# app.R ---------------------------------------------------------------
#source("clean_data.R")

# ---- load data ------------------------------------------------------


numbers <- c("one","two","three","four","five","six","seven","eight","nine","ten")

# ---- helpers --------------------------------------------------------
find_courses <- function(sentence){
  spec <- str_extract_all(sentence, "\\b\\w{4}\\d{4}\\b")[[1]]
  gen  <- str_match_all(sentence, "\\S+?\\s\\d{4}\\s[\\w\\s]*(?=course|courses)")[[1]][,1]
  gen  <- vapply(gen, \(g){
    w1 <- word(g, 1)
    if (w1 %in% numbers) str_trim(str_remove(g, paste0("^", w1, "\\s"))) else g
  }, "")
  toupper(unique(c(spec, gen)))
}

find_prereqs <- function(code){
  txt  <- course_info[course_info$code == code, "blurb"]
  snts <- str_split(txt, "[.!;]")[[1]] |> str_to_lower()
  keep <- !str_detect(snts, "incompatible| not | cannot |n't")
  snts <- snts[keep]
  setdiff(unique(unlist(lapply(snts, find_courses))), code)
}

create_all_graph <- function(){
  edges <- lapply(all_codes, \(c){
    prs <- find_prereqs(c)
    if (length(prs))
      data.frame(from = prs, to = c, stringsAsFactors = FALSE)
  }) |> purrr::compact() |> bind_rows()
  
  vert <- union(all_codes, unique(unlist(edges)))
  graph_from_data_frame(edges, directed = TRUE, vertices = vert)
}

get_family <- function(g, target, direction = "in"){
  if (direction == "in"){
    fam   <- union(target, names(subcomponent(g, target, mode = "in")))
    depth <- distances(g, v = fam, to = target)[,1]
  } else {
    fam   <- union(target, names(subcomponent(g, target, mode = "out")))
    depth <- distances(g, v = target, to = fam)[1,]
  }
  list(nodes = fam, depth = depth[fam])
}




# ---- UI -------------------------------------------------------------
ui <- page_sidebar(
  title = "ANU Course Prerequisite Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 500,
    textInput("target", "Course code", ""),
    radioButtons("direction", "Direction",
                 choices = c("Out", "In"), inline = TRUE),
    selectInput("color", "Colour by:", choices = c("depth", "college", "subject"), selected="depth"),
    sliderInput("node_spread", "Node & label size", min = 5, max = 30, value = 15),
    sliderInput("layer_gap",   "Squish/stretch layout",   min = 0.3, max = 0.9, value = 0.5, step=0.05),
    checkboxGroupInput("career", "Career", 
                       choices = c("Undergraduate", "Research", "Postgraduate"), 
                       selected = c("Undergraduate", "Research", "Postgraduate")),
    checkboxGroupInput("assessment", "Assessments must include:",
                       choices = assessment_type,
                       selected = NULL),
    virtualSelectInput(
      inputId = "session_filter",
      label = "Session Offered",
      choices = all_sessions,
      selected = unlist(all_sessions),
      multiple = TRUE
    ),
    virtualSelectInput(
      inputId = "college_school",
      label = "Select Colleges/Schools",
      choices = college_school_choices,
      selected = unlist(college_school_choices),
      search = TRUE,
      multiple = TRUE
    ),
    actionButton("go", "Submit", class = "btn-primary")
  ),
  tags$script(HTML("
  $(document).on('keypress', function(e) {
    if (e.which == 13 && $('#target').is(':focus')) {
      $('#go').click();
    }
  });
")),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel("Graph",
               visNetworkOutput("graph", height = "700px"),
               br(),
               htmlOutput("blurb", class = "ps-3")
      ),
      tabPanel("Table",
               DT::dataTableOutput("filtered_table")
      ),
      tabPanel(
        "Course input",
        rHandsontableOutput("course_table", height = "220px", width="700px")
      )
    )
  )
)

# ---- server ---------------------------------------------------------
server <- function(input, output, session) {
  
  g  <- create_all_graph()
  rv <- reactiveValues(subg = NULL, depth = NULL, highlight_nodes = character(0))
  
  filtered_course_info <- reactive({
    meta <- course_info
    
    # session filtering: include rows with NA session_list
    meta <- meta |> 
      filter(map_lgl(session_list, ~ any(is.na(.x)) || any(.x %in% input$session_filter)))
    
    # career filtering: include NA or matching
    meta <- meta |> 
      filter(is.na(career) | career %in% input$career)
    
    # assessment filtering: include only if all selected assessments are TRUE (ignore NAs)
    if (length(input$assessment) > 0) {
      meta <- meta |> 
        filter(if_all(all_of(input$assessment), ~ isTRUE(.x)))
    }
    
    # college/school filtering: include rows with NA or at least one school in selection
    meta <- meta |> 
      filter(map_lgl(school_list, ~ any(is.na(.x)) || any(.x %in% input$college_school)))
    
    meta
  })
  
  # build sub-graph ---------------------------------------------------
  observeEvent(input$go, {
    tgt <- toupper(input$target)
    req(tgt, tgt %in% all_codes)
    
    fam <- get_family(g, tgt, tolower(input$direction))
    rv$subg  <- induced_subgraph(g, fam$nodes)
    rv$depth <- fam$depth
    rv$highlight_nodes <- character(0)
    output$blurb <- renderUI("")
    
    meta <- filtered_course_info()
    all_ids <- fam$nodes
    filtered_ids <- intersect(all_ids, meta$code)
    filtered_out_ids <- setdiff(all_ids, filtered_ids)
    
    if (length(filtered_out_ids) > 0) {
      message("Nodes filtered out: ", paste(filtered_out_ids, collapse = ", "))
    } else {
      message("No nodes filtered out.")
    }
  })
  
  # render graph ------------------------------------------------------
  output$graph <- renderVisNetwork({
    req(rv$subg)
    
    meta <- filtered_course_info()
    ids <- intersect(V(rv$subg)$name, meta$code)
    
    subg <- induced_subgraph(rv$subg, vids = ids)
    
    depth_vec <- rv$depth[V(subg)$name]
    base <- layout_with_centrality(
      subg,
      cent  = max(depth_vec) - depth_vec,
      scale = TRUE
    )
    
    theta  <- atan2(base[,2], base[,1])
    raw    <- depth_vec^(2*(1 - input$layer_gap))
    radii  <- 300 * raw / max(raw)
    lay    <- cbind(radii * cos(theta), radii * sin(theta))
    
    coords <- data.frame(id = V(subg)$name,
                         x  = lay[,1],
                         y  = lay[,2])
    
    ## nodes ----------------------------------------------------------
    
    pal_code <- setNames(pal[seq_along(colleges)], colleges)
    
    subject <- numeric(length(unique(ids)))
    for (i in seq_along(unique(ids))) {
      course <- unique(ids)[i]
      subject[i] <- substr(course, 1, 4)
    }
    unique_subjects <- unique(subject)
    pal_subject <- setNames(pal[seq_along(unique_subjects)], unique_subjects)
    
    nodes <- coords |>
      left_join(meta, by = c("id" = "code")) |>
      mutate(
        title = paste0(
          "<b>", id, ": ", title, "</b><br/>",
          ifelse(!is.na(session), paste0("<b>Session:</b> ", session, "<br/>"), ""),
          ifelse(!is.na(college), paste0("<b>College:</b> ", college, "<br/>"), ""),
          ifelse(!is.na(school), paste0("<b>School:</b> ", school, "<br/>"), ""),
          ifelse(!is.na(areas_of_interest), paste0("<b>Areas of interest:</b> ", areas_of_interest, "<br/>"), ""),
          ifelse(!is.na(max_assessment_name) & !is.na(max_assessment_weight), paste0("<b>Largest assessment:</b> ", max_assessment_name, " (", max_assessment_weight, "%)<br/>"), ""),
          ifelse(!is.na(selt_link), paste0("<a href='", selt_link, "' target='_blank'>SELT results</a> <br/>"), ""),
          ifelse(!is.na(summary_link), paste0("<a href='", summary_link, "' target='_blank'>Most recent course summary</a>"), "")
        )
      ) |>
      mutate(
        label = id,
        color = case_when(
          input$color == "depth" ~ pal[(depth_vec[id] %% length(pal)) + 1], ### Fix later
          input$color == "college" ~ pal_college[college],
          input$color == "subject" ~ pal_subject[subject]
        ),
        size = input$node_spread,
        font.size = input$node_spread,
        physics = FALSE
      ) |>
      select(id, label, title, color, x, y, size, font.size, physics)
    
    # Filter nodes to match metadata
    nodes <- nodes[nodes$id %in% meta$code, ]
    
    print(nodes[!nodes$id %in% meta$code])
    ## edges ----------------------------------------------------------
    edges <- igraph::as_data_frame(subg, what = "edges") |>
      filter(from %in% nodes$id, to %in% nodes$id) |>
      mutate(
        id    = paste0(from, "→", to),
        color = ifelse(from %in% rv$highlight_nodes & to %in% rv$highlight_nodes, "#000000", "#d3d3d3"),
        width = input$node_spread/7,
        arrows = "to"
      )
    
      visNetwork(nodes, edges, height = "700px") %>%
        visEdges(smooth = FALSE) %>%
        visOptions(nodesIdSelection = list(enabled = TRUE, main = "Pick a course")) %>%
        visPhysics(
          enabled = TRUE,                 # turn physics ON briefly
          solver  = "repulsion",
          repulsion = list(
            nodeDistance =  10 * input$node_spread,  # how hard to push apart
            centralGravity = 0,
            springLength   = 0,
            springConstant = 0
          )
        ) %>%
        visEvents(                         # after first stabilisation, freeze
          stabilized = "function () {
          this.setOptions({physics: {enabled: false}});
      }"
        )
  })
  
  # click node --------------------------------------------------------
  observeEvent(input$graph_selected, {
    code <- input$graph_selected
    title <- course_info[course_info$code == code, "title"]
    blurb <- course_info[course_info$code == code, "blurb"]
    
    req(code, rv$subg)
    
    anc <- names(subcomponent(rv$subg, code, mode = "in"))
    des <- names(subcomponent(rv$subg, code, mode = "out"))
    rv$highlight_nodes <- unique(c(code, anc, des))
    
    output$blurb <- renderUI({
      HTML(paste0("<b>", code, ": ", title, "<br/><br/>",
                  blurb))
    })
  })
  
  output$filtered_table <- DT::renderDataTable({
    filtered_course_info() |> 
      select(code, title, college, school, career, session, areas_of_interest)
  })
  
  # course data table
  code_choices  <- unique(course_info$code) |> sort()
  title_choices <- unique(course_info$title) |> sort()
  
  rv <- reactiveValues(
    data = data.frame(
      code  = "",
      title = "",
      units = NA_integer_,
      grade = 100,
      stringsAsFactors = FALSE
    )
  )
  
  output$course_table <- renderRHandsontable({
    rhandsontable(rv$data, stretchH = "all") %>%
      hot_col("code",
              type          = "autocomplete",   # <─ searchable
              source        = code_choices,
              allowInvalid  = FALSE) %>%
      hot_col("title",
              type          = "autocomplete",   # <─ searchable
              source        = title_choices,
              allowInvalid  = FALSE)
  })
  
  observeEvent(input$course_table, ignoreNULL = TRUE, {
    df <- hot_to_r(input$course_table)
    df$code <- toupper(df$code)
    ## 1 ─ user changed/typed a *code*  -------------------------------
    idx_code <- match(df$code, course_info$code)
    has_code <- !is.na(idx_code)
    df$title[has_code] <- course_info$title[idx_code[has_code]]   # autofill title
    df$units[has_code] <- course_info$units[idx_code[has_code]]   # autofill units
    
    ## 2 ─ user changed/typed a *title* ------------------------------
    idx_title <- match(df$title, course_info$title)
    has_title   <- !is.na(idx_title)
    df$code[has_title]  <- course_info$code[idx_title[has_title]]     # autofill code
    df$units[has_title] <- course_info$units[idx_title[has_title]]    # autofill units
    df$grade[has_title] <- 100
    
    rv$data <- df                                                # update store
  })
}



shinyApp(ui, server)

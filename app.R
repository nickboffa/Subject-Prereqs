# app.R ---------------------------------------------------------------
library(shiny)
library(bslib)
library(visNetwork)
library(igraph)
library(graphlayouts)
library(jsonlite)
library(stringr)
library(dplyr)

# ---- load data ------------------------------------------------------
all_codes <- scan("data/current/course_codes.txt", what = character())
blurbs    <- fromJSON("data/current/blurbs.txt")
names_vec <- fromJSON("data/current/names.txt")

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
  txt  <- blurbs[[code]]
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

pal <- c("red","orange","yellow","chartreuse","cyan",
         "dodgerblue","mediumslateblue","violet","magenta")

# ---- UI -------------------------------------------------------------
ui <- page_sidebar(
  title = "ANU Course Prerequisite Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    textInput("target", "Course code", ""),
    radioButtons("direction", "Direction",
                 choices = c("Out", "In"), inline = TRUE),
    checkboxGroupInput("levels", "Show",
                       choices = c("Undergraduate", "Postgraduate"),
                       selected = c("Undergraduate")),
    sliderInput("node_spread", "Node & label size", min = 5, max = 30, value = 15),
    sliderInput("layer_gap",   "Squish/stretch layout",   min = 0.3, max = 0.9, value = 0.5, step=0.05),
    actionButton("go", "Submit", class = "btn-primary")
  ),
  tags$script(HTML("
  $(document).on('keypress', function(e) {
    if (e.which == 13 && $('#target').is(':focus')) {
      $('#go').click();
    }
  });
")),
  visNetworkOutput("graph", height = "700px"),
  br(),
  htmlOutput("blurb", class = "ps-3")
  
)

# ---- server ---------------------------------------------------------
server <- function(input, output, session){
  
  g  <- create_all_graph()
  rv <- reactiveValues(subg = NULL, depth = NULL, highlight_nodes = character(0))
  
  # build sub-graph ---------------------------------------------------
  observeEvent(input$go, {
    tgt <- toupper(input$target)
    req(tgt, tgt %in% all_codes)
    
    fam <- get_family(g, tgt, tolower(input$direction))
    rv$subg  <- induced_subgraph(g, fam$nodes)
    rv$depth <- fam$depth
    rv$highlight_nodes <- character(0)
    output$blurb <- renderUI("")
  })
  
  # render graph ------------------------------------------------------
  output$graph <- renderVisNetwork({
    req(rv$subg)
    
    ## ring radii -----------------------------------------------------
    depth_vec <- rv$depth[V(rv$subg)$name]        # 0,1,2,...
    base      <- layout_with_centrality(
      rv$subg,
      cent  = max(depth_vec) - depth_vec,
      scale = TRUE,
      tseq =
    )
    theta  <- atan2(base[,2], base[,1])           # keep angular order

    raw   <- depth_vec^(2*(1-input$layer_gap))   # any shape you like
    radii <- 300 * raw / max(raw)
    lay    <- cbind(radii * cos(theta), radii * sin(theta))
    
    coords <- data.frame(id = V(rv$subg)$name,
                         x  = lay[,1],
                         y  = lay[,2])
    
    ## nodes ----------------------------------------------------------
    nodes <- data.frame(
      id        = coords$id,
      label     = coords$id,
      title     = paste0(coords$id, ": ", names_vec[coords$id]),
      color     = pal[(depth_vec %% length(pal)) + 1],
      x         = coords$x,
      y         = coords$y,
      size      = input$node_spread,      # one slider drives both
      font.size = input$node_spread,
      physics   = FALSE,
      stringsAsFactors = FALSE
    )
    
    ## UG / PG filter -------------------------------------------------
    is_ug <- substr(nodes$id, 5, 5) < "6"
    keep  <- (is_ug & "Undergraduate" %in% input$levels) |
      (!is_ug & "Postgraduate" %in% input$levels)
    nodes <- nodes[keep, ]
    
    ## edges ----------------------------------------------------------
    edges <- igraph::as_data_frame(rv$subg, what = "edges") |>
      filter(from %in% nodes$id, to %in% nodes$id) |>
      mutate(
        id    = paste0(from, "→", to),
        color = ifelse(from %in% rv$highlight_nodes & to %in% rv$highlight_nodes, "#000000", "#d3d3d3"),
        width = 2,
        arrows = "to"
      )
    
      visNetwork(nodes, edges, height = "700px") %>%
        visEdges(smooth = FALSE) %>%
        visOptions(nodesIdSelection = list(enabled = TRUE, main = "Pick a course")) %>%
        visPhysics(
          enabled = TRUE,                 # turn physics ON briefly
          solver  = "repulsion",
          repulsion = list(
            nodeDistance =  2 * input$node_spread,  # how hard to push apart
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
    req(code, rv$subg)
    
    anc <- names(subcomponent(rv$subg, code, mode = "in"))
    des <- names(subcomponent(rv$subg, code, mode = "out"))
    rv$highlight_nodes <- unique(c(code, anc, des))
    
    output$blurb <- renderUI({
      HTML(paste0("<b>", code, ": ", names_vec[[code]], "<br/><br/>",
                  blurbs[[code]]))
    })
  })
}

shinyApp(ui, server)

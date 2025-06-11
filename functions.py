import re 
import json
import numpy as np
import networkx as nx
from networkx.drawing.nx_agraph import graphviz_layout
import matplotlib.pyplot as plt

with open("data/current/course_codes.txt", "r") as f:
   all_codes = sorted([line.strip() for line in f if line.strip()])

u_codes = [code for code in all_codes if int(code[4]) < 6]
p_codes = [code for code in all_codes if int(code[4]) >= 6]

numbers = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten']

with open("data/current/blurbs.txt", "r") as file:
    blurbs = json.load(file)

with open("data/current/names.txt", "r") as file:
    names = json.load(file)

def get_codes(type):
    # to account for spelling errors and different ways of writing undergraduate/postgraduate, only the first letter is looked at
    if type[0].lower() == 'u':
        return u_codes
    elif type[0].lower() == 'p':
        return p_codes 
    else:
        return all_codes 

def find_courses(sentence):
    specific_courses = re.findall(r'\w{4}\d{4}', sentence)
    general_courses = re.findall(r'\S*\s\d{4}\s[\w\s]*(?=course|courses)', sentence)
    for i in range(len(general_courses)):
        course = general_courses[i]
        try:
            if course[:course.index(' ')] not in numbers:
                    general_courses[i] = course[course.index(' ')+1:]
        except:
            a = 1 #dummy
    general_courses = [c + 'course(s)' for c in general_courses]

    return specific_courses + general_courses

def contains(phrase, sentence):
    return phrase in sentence

def find_prereqs(code):
    requisite_text = blurbs[code]
    sentences = re.findall(r'[^;.!?]*', requisite_text)
    sentences = [sentence.lower() for sentence in sentences if sentence != '']
    prereqs = []
    for sentence in sentences:
        courses = find_courses(sentence)
        courses = [course.upper() for course in courses if course.upper() != code]
        if not (contains('incompatible', sentence) or contains(' not ', sentence) 
                or contains(' cannot ', sentence) or contains("n't", sentence)):
            prereqs += courses
    prereqs = list(set(prereqs))

    return prereqs

#colors = ['red', 'orange', 'yellow', 'chartreuse', 'cyan', 'dodgerblue', 'mediumslateblue', 'violet', 'magenta']

colors = [[1,0,0,1],[1,1,0,1], [0,1,0,1], [0,1,1,1], [0,0,1,1], [1,0,1,1]]

def wrap_text(labels):
    for j in range(len(labels)):
        cutoff1 = len(labels[j]) // 3
        cutoff2 = 2*cutoff1
        if len(labels[j]) > cutoff1:
            indices = [i for i, x in enumerate(list(labels[j])) if x == ' ']
            indices = np.asarray(indices)
            try:
                idx1 = indices[indices >= cutoff1][0]
                
                newlabel = list(labels[j])
                newlabel.insert(idx1, ' \n')

                try:
                    idx2 = indices[indices >= cutoff2][0]
                    if idx2 != idx1:
                        newlabel.insert(idx2+1, ' \n') #DODGY
                except:
                    continue

                newlabel = ''.join(newlabel)
                labels[j] = newlabel
            except:
                continue

    return labels

#fig.savefig(f'/Users/nicholasboffa/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Uni/Prereqs/{stem}/{code}.png')

BREAKING_SUBJECTS = ['CHEM4005']

def get_layout_info(code, G, direction=str()):
    family = [code]
    if direction == 'in':
        family += list(nx.ancestors(G,code))
        depth = {member: nx.shortest_path_length(G, source=member, target=code) for member in family}
    elif direction == 'out':
        family += list(nx.descendants(G,code))
        depth = {member: nx.shortest_path_length(G, source=code, target=member) for member in family}
    else:
        raise ValueError
    
    H = G.subgraph(family)

    try:
        shells = [sorted([code for code in family if depth[code]==i]) for i in range(max(depth.values())+1)]
    except ValueError: #if no descendants/ancestors
        pass

    color_map = {node: colors[depth[node]] for node in H.nodes()}

    labels = wrap_text(family)
    labels = {family[i]:labels[i] for i in range(len(family))}

    if H.number_of_nodes() >= 8:
        pos = nx.shell_layout(H, shells)
    else:
        pos = nx.spring_layout(H)

    
    pos = graphviz_layout(H, prog='twopi', root=code)

    #min max normalization of pos coordinates
    x_min = min([x[0] for x in pos.values()])
    x_max = max([x[0] for x in pos.values()])
    y_min = min([y[1] for y in pos.values()])
    y_max = max([y[1] for y in pos.values()])
    mins = (x_min, y_min)
    maxs = (x_max, y_max)

    try:
        layout = {node: ((x[0]-mins[0])/(maxs[0]-mins[0]), (x[1]-mins[1])/(maxs[1]-mins[1])) for node,x in pos.items()}
    except ZeroDivisionError:
        nodes = list(H.nodes())
        if len(nodes) > 1:
            layout = {node: (nodes.index(node)/len(nodes), float(np.sin(nodes.index(node)))**2) for node in nodes}
        else:
            layout = {node: (0.5, 0.5) for node in nodes}

    
    k = 0.9 #shrink_factor
    layout = {node: (k*(x[0]-0.5)+0.5,k*(x[1]-0.5)+0.5) for node, x in layout.items()}

    color_map = {node: colors[depth[node]] for node in H.nodes()}

    return H, layout, color_map
    
def draw(code, G, direction=str()):
    family = [code]
    if direction == 'in':
        family += list(nx.ancestors(G,code))
        depth = {member: nx.shortest_path_length(G, source=member, target=code) for member in family}
    elif direction == 'out':
        family += list(nx.descendants(G,code))
        depth = {member: nx.shortest_path_length(G, source=code, target=member) for member in family}
    else:
        raise ValueError
    
    H = G.subgraph(family)

    try:
        shells = [sorted([code for code in family if depth[code]==i]) for i in range(max(depth.values())+1)]
    except ValueError: #if no descendants/ancestors
        pass

    colors = ['red', 'orange', 'yellow', 'chartreuse', 'cyan', 'dodgerblue', 'mediumslateblue', 'violet', 'magenta']
    color_map = {node: colors[depth[node]] for node in H.nodes()}

    labels = wrap_text(family)
    labels = {family[i]:labels[i] for i in range(len(family))}

    if G.number_of_nodes() >= 8:
        pos = nx.shell_layout(H, shells)
    else:
        pos = nx.spring_layout(H)

    pos = graphviz_layout(H, prog='twopi', root=code)

    fig = plt.figure(figsize=(15,15))
    nx.draw_networkx(H,pos=pos, labels=labels, node_size=3500,
                    node_color=color_map.values(), font_size=8, 
                    edge_color='grey', font_weight='bold', arrowsize=25)

    plt.show()

def create_all_graph():
    G = nx.DiGraph()

    codes = all_codes #can also choose u_codes or p_codes

    for code in codes:
        prereqs = find_prereqs(code)
        for prereq in prereqs:
            G.add_edge(prereq,code)

    for code in codes:
        if code not in G.nodes():
            G.add_node(code)

    return G

def find_courses_about(topic, cutoff=None):
    courses = []
    for course, name in names.items():
        if topic.lower() in name.lower():
            courses.append(course)

    if cutoff:
        courses = [course for course in courses if int(course[4])<= cutoff]
    
    for course in courses:
        print(course, names[course])
    

# functions for vector mathematics used to draw triangles in correct spot (to create arrows)
def add(t1,t2):
    if len(t1) == len(t2):
        return tuple([t1[n]+t2[n] for n in range(len(t1))])
    else:
        raise ValueError
    
def scale(t, k): #tuple, scalar
    return tuple([k*t_i for t_i in t])
    

def eligible_courses(G, completed_courses, prefix=None):
    completed_courses = set(completed_courses)
    eligible = []

    for course in G.nodes:
        if prefix and not course.startswith(prefix):
            continue

        prereqs = set(G.predecessors(course))
        if prereqs.issubset(completed_courses) and course not in completed_courses:
            eligible.append(course)

    return sorted(eligible)

def list_options(completed, interested_prefixes):

    G = create_all_graph()

    for prefix in prefixes:
        print(f"\n{prefix}")
        elig = eligible_courses(G, nick_completed, prefix=prefix)
        elig_u = [course for course in elig if int(course[4]) < 4]
        
        for course in elig_u:
            title = names.get(course, '[No title available]')
            print(f"{course}: {title}")

    
nick_completed = ["CHEM1101", "BIOL1008", "MATH1115", "HLTH1001", "CHEM1201", "BIOL1004", "BIOL2202", "MATH1116", "STAT2001", "BIOL3207", "BIOL2161", "MEDN2001", "POPH3000", "SCNC2101", "MEDN2002", "STAT2005", "STAT3040", "HLTH3001", "SCNC3101", "MATH2305"]

prefixes = ["STAT", "BIOL", "MATH", "HLTH", "MEDN", "POPH", "COMP"]

# e.g. list_options(nick_completed, prefixes)

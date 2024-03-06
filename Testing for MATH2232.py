from bs4 import BeautifulSoup
import requests
import re
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np

numbers = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten']
test_codes = ['math2322', 'laws2201', 'stat4027', 'math2222', 'BIOL3108', 'comp4528', 'JPNS3102']
code = 'BIAN4011'

def find_courses(sentence):
    sentence = sentence.replace('\n', '')
    print(sentence)
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
    url = f'https://programsandcourses.anu.edu.au/course/{code}'
    page = requests.get(url)
    soup = BeautifulSoup(page.text, 'html.parser')

    try:
        requisite_text = soup.find('div', class_='requisite').text
        requisite_text = requisite_text.lower()
    except AttributeError: #if no requisite section
        requisite_text = ''

    sentences = re.findall(r'[^;.!?]*', requisite_text)

    prereqs = []
    for sentence in sentences:
        courses = find_courses(sentence)
        courses = [course.upper() for course in courses]
        if not (contains('incompatible', sentence) or contains(' not ', sentence) 
                or contains(' cannot ', sentence) or contains("n't", sentence)):
            prereqs += courses
    prereqs = list(set(prereqs))

    return prereqs


G = nx.DiGraph()

explored = []

def recur(code):
    explored.append(code)

    if code == 'MATH1115' or len(code) > 8:
        prereqs = []
    else:
        prereqs = find_prereqs(code)
    
    print(code, prereqs)
    for prereq in prereqs:
        G.add_edge(prereq,code)
        
        if prereq not in explored:
            recur(prereq)

recur(code)

depth = {node:nx.shortest_path_length(G, source=node, target=code) for node in list(G.nodes)}
shells = [sorted([code for code in explored if depth[code]==i]) for i in range(max(depth.values())+1)]

colours = ['red', 'orange', 'yellow', 'chartreuse', 'cyan', 'dodgerblue', 'mediumslateblue', 'violet', 'magenta']
color_map = {node: colours[depth[node]] for node in list(G.nodes)}

labels = list(G.nodes)

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
labels = {list(G.nodes)[i]:labels[i] for i in range(len(G.nodes))}

#if below looks bad, change to spring_layout(G)
if G.number_of_nodes() >= 6:
    pos = nx.shell_layout(G, shells)
else:
    pos = nx.spring_layout(G)

plt.figure(figsize=(15,15))
nx.draw_networkx(G,pos=pos, labels=labels, node_size=3500,
                 node_color=color_map.values(), cmap=colours,font_size=8, 
                 edge_color='grey', font_weight='bold', arrowsize=25)

plt.show()
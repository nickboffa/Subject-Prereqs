import networkx as nx
import matplotlib.pyplot as plt
from functions import *

G = nx.DiGraph()

codes = all_codes #can also choose u_codes or p_codes

for code in codes:
    prereqs = find_prereqs(code)
    for prereq in prereqs:
        G.add_edge(prereq,code)

for code in codes:
    if code not in G.nodes():
        G.add_node(code)

#if a course doesn't have any prereqs and isn't a prereq for anything else, it isn't in the graph

#Also JPNS is the only one with cycles

doesnt_exist = []
for code in G.nodes():
    if code not in all_codes and len(code) == 8:
        doesnt_exist.append(code)

descendants = {}
ancestors = {}
for code in G.nodes():
   descendants[code] = list(nx.descendants(G,code))
   ancestors[code] = list(nx.ancestors(G,code))






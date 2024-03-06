import networkx as nx

G = nx.DiGraph()
coins = [1,2,5,10,20,50,100,200]

for n in range(200, -1, -1):
    for c in coins:
        if n >= c:
            G.add_edge(str(n), str(n-c), weight=c) 

print(sum([1 for _ in nx.all_simple_paths(G, '200', '0')]))
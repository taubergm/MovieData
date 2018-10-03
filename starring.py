import csv
import sys
reload(sys)  
sys.setdefaultencoding('UTF8')
import re
import networkx as nx
import matplotlib.pyplot as plt
import io

filename = sys.argv[1]

# needed to remove carriage returns - %s/\r//g


MG = nx.MultiGraph()
G = nx.Graph()

i = 0
j = 0

# create set of all actors
seen_actors = set()


def clean_plainlist(list_data):
    



with io.open(filename, encoding='latin-1', errors='ignore') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
    for row in readCSV:

        # get movie name
        movie_name = row[5]
        #print movie_name


        # fix up the bad formatting of actors
        starring = row[16].encode('utf8')
        
        # remove <!.*|, [[ ]] , plainlist , ubl, * , {{ }}, | , plain list, <br />, br/>,<br> , (actor)
        # replace with spaces

        #starring = re.sub("\[.*\|", " ", starring)
        starring = re.sub("<!.*\|", " ", starring)
        starring = re.sub("<!.*\>", " ", starring)
        starring = re.sub("\[\[", " ", starring)
        starring = re.sub("\]\]", " ", starring)
        starring = re.sub("\}\}", " ", starring)
        starring = re.sub("\{\{", " ", starring)
        starring = re.sub("plainlist", " ", starring, flags=re.IGNORECASE)
        starring = re.sub("plain list", " ", starring, flags=re.IGNORECASE)
        starring = re.sub("ubl", " ", starring)
        starring = re.sub("<br />", " ", starring)
        starring = re.sub("<br/>", " ", starring)
        starring = re.sub("<br>", " ", starring)
        starring = re.sub("\(actor\)", " ", starring)
        starring = re.sub("\(actress\)", " ", starring)
        starring = re.sub("\(entertainer\)", " ", starring)
        starring = re.sub("\|", " ", starring)
        starring = re.sub("\*", " ", starring)
        starring = re.sub("-", " ", starring)
        starring = re.sub("<br.*>", " ", starring)
        starring = re.sub("-", " ", starring)
        starring = re.sub("\(.*\)", " ", starring)
        starring = re.sub("\".*\"", " ", starring)
        starring = re.sub("unbulleted list", " ", starring)

        # now parse actors from whitespace
        actors = re.split(r'\s{2,}', starring)
        if '' in actors:
            actors.remove('')
        if '' in actors:
            actors.remove('')

        
        for actor in actors:
            actor = actor.strip()
            if actor not in seen_actors:
                #print actor
                MG.add_node(actor)
                G.add_node(actor)
                seen_actors.add(actor)
            for other_actor in actors:
                other_actor = other_actor.strip()

                # get existing weight of the connection and increment for new connection (ie movie together)
                weight = G.get_edge_data(actor,other_actor,default=0)
                #weight = weight + 1
                MG.add_edge(actor, other_actor, attribute=movie_name)
                G.add_edge(actor, other_actor)
                #G.add_edge(0,1,key='movie_name',weight=weight)

                print "movie=%s,actor=%s,other_actor=%s,weight=%s" % (movie_name, actor, other_actor, weight)

        i = i + 1
        #if (i > 100):
        #    break

        
        #write to new csv in utf8 - also parse movie name


         # fix up the bad formatting of directors, producers



        # now create a graph in nx, where each actor is a node and we add edges between them with movie name



#from networkx.drawing.nx_pydot import write_dot
#write_dot(G,'multi.dot')

#nx.draw(G, with_labels=True, font_weight='bold')
#nx.draw_networkx(G)

print len(seen_actors)
print G.number_of_nodes()
print G.number_of_edges()

#a = max(G.degree().items(), key = lambda x: x[1])
#print a 

#nx.draw_networkx_edges(G)
#plt.show()


# plot using bokeh

from bokeh.io import show, output_file
from bokeh.models import Plot, Range1d, MultiLine, Circle, HoverTool, TapTool, BoxSelectTool
from bokeh.models.graphs import from_networkx, NodesAndLinkedEdges, EdgesAndLinkedNodes
from bokeh.palettes import Spectral4


#graph_renderer = from_networkx(G, nx.spring_layout)

#plot = Plot(plot_width=400, plot_height=400,
#            x_range=Range1d(-1.1,1.1), y_range=Range1d(-1.1,1.1))
#plot.title.text = "Graph Interaction Demonstration"
#plot.renderers.append(graph_renderer)
#output_file("interactive_graphs.html")
#show(plot)


# who is most connected
actor_degrees =  dict(G.degree)
fieldnames = ['actor', 'degrees']
print actor_degrees

with open('mycsvfile.csv', 'wb') as f:  
    w = csv.DictWriter(f, actor_degrees.keys())
    w.writeheader()
    w.writerow(actor_degrees)

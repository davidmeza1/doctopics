library(arcdiagram)

# location of 'gml' file
mis_file <- "~/Dropbox/RData/data/lesmiserables.txt"

# read 'gml' file
mis_graph <- read.graph(mis_file, format="gml")
#####Extraticng Graph Attributes########
# get edgelist
edgelist <- get.edgelist(mis_graph)

# get vertex labels
vlabels <- get.vertex.attribute(mis_graph, "label")

# get vertex groups
vgroups <- get.vertex.attribute(mis_graph, "group")

# get vertex fill color
vfill <- get.vertex.attribute(mis_graph, "fill")

# get vertex border color
vborders <- get.vertex.attribute(mis_graph, "border")

# get vertex degree
degrees <- degree(mis_graph)

# get edges value
values <- get.edge.attribute(mis_graph, "value")

####Nodes Oredring#######
# load reshape
library(reshape)

# data frame with vgroups, degree, vlabels and ind
x <- data.frame(vgroups, degrees, vlabels, ind=1:vcount(mis_graph))

# arranging by vgroups and degrees
y <- arrange(x, desc(vgroups), desc(degrees))

# get ordering 'ind'
new_ord <- y$ind

###Plot Arc Diagram########
# plot arc diagram
arcplot(edgelist, ordering=new_ord, labels=vlabels, cex.labels=0.8,
        show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
        cex.nodes = log(degrees)+0.5, pch.nodes=21,
        lwd.nodes = 2, line=-0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)
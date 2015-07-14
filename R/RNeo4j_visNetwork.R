## This is an example of using Rneo4j by Nocole White.
## http://nicolewhite.github.io/2015/06/18/visualize-your-graph-with-rneo4j-and-visNetwork.html
devtools::install_github("nicolewhite/RNeo4j")
devtools::install_github("dataknowledge/visNetwork")
library(RNeo4j)
library(visNetwork)

## Creates the connection to the the neo4j database. In this case it is running locally
graph <- startGraph("http://localhost:7476/db/data/", username = "neo4j", password = "linkurious")
## Use this to browse the neo4j db in the internal viewer
browse(graph)
## This will clear out the entire db.
clear(graph)

nicole <- createNode(graph, "Person", name="Nicole")
greta <- createNode(graph, "Person", name="Greta")
kenny <- createNode(graph, "Person", name="Kenny")
adam <- createNode(graph, "Person", name="Adam")

neo4j <- createNode(graph, "Company", name="Neo4j")
digital <- createNode(graph, "Company", name="Digital Insights")
docker <- createNode(graph, "Company", name="Docker")

createRel(nicole, "WORKS_FOR", neo4j)
createRel(greta, "WORKS_FOR", neo4j)
createRel(kenny, "WORKS_FOR", digital)
createRel(adam, "WORKS_FOR", docker)

createRel(greta, "KNOWS", adam)
createRel(nicole, "KNOWS", kenny)
createRel(kenny, "KNOWS", adam)

# My attempt to create nodes and releationships directly from a dataframe
for (i in 1:nrow(people.df)){

     p <- createNode(graph, "Person", name = people.df[i,2])
     e <- createNode(graph, "Employer", name = people.df[i,3])
     createRel(p, "WORKS_FOR", e)
}





node_query <- "
MATCH n
RETURN n.name AS id,
       n.name AS label,
       LABELS(n)[0] AS group
"

edge_query <- "
MATCH (n)-[r]->(m)
RETURN n.name AS from,
       m.name AS to,
       TYPE(r) AS label
"

nodes <- cypher(graph, node_query)
edges <- cypher(graph, edge_query)

nodes

edges

visNetwork(nodes, edges)

neo4j <- updateProp(neo4j, employees=1)
digital <- updateProp(digital, employees=2)
docker <- updateProp(docker, employees=3)

node_query <- "
MATCH n
RETURN n.name AS id,
       n.name AS label,
       LABELS(n)[0] AS group,
       n.employees AS value
"

nodes <- cypher(graph, node_query)
nodes[is.na(nodes)] <- 1

nodes

visNetwork(nodes, edges)

importSample(graph, "movies", input=F)

LIMIT <- 30

node_query <- "
MATCH n
WITH n, RAND() AS random
ORDER BY random
LIMIT {limit}
RETURN ID(n) AS id,
COALESCE(n.name, n.title) AS label,
LABELS(n)[0] AS group
"

nodes <- cypher(graph, node_query, limit=LIMIT)

head(nodes)

nrow(nodes)

edge_query <- "
MATCH (n)-[r]->(m)
WHERE ID(n) IN {ids} AND ID(m) IN {ids}
RETURN ID(n) AS from,
ID(m) AS to,
TYPE(r) AS label
"

edges <- cypher(graph, edge_query, ids=nodes$id)

head(edges)

visNetwork(nodes, edges)

nodes$connected <- nodes$id %in% c(edges$from, edges$to)
nodes <- nodes[nodes$connected, ]

visNetwork(nodes, edges)

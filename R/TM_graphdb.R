## This is an example of using Rneo4j by Nocole White.
## http://nicolewhite.github.io/2015/06/18/visualize-your-graph-with-rneo4j-and-visNetwork.html
devtools::install_github("nicolewhite/RNeo4j")
devtools::install_github("dataknowledge/visNetwork")
library(RNeo4j)
library(visNetwork)
library(lubridate)
## Creates the connection to the the neo4j database. You need to start the db first.
## In this case it is running locally onmy machine

graph <- startGraph("http://localhost:7474/db/data/", username = "neo4j", password = "rneo4jdb")
#graph <- startGraph("http://localhost:7478/db/data/", username = "neo4j", password = "Rneo4j")
## Use this to browse the neo4j db in the internal viewer
browse(graph)
## This will clear out the entire db. Be careful!
clear(graph)


## I first start by importing the csv files I created to store the lessons learned, Category and Topic,
## Topoic correlations and the top 30 terms for each topic.
llis <- read.csv("~/OneDrive/GitHub/doctopics/data/llis.csv", comment.char="#")
topicCategory <- read.csv("~/OneDrive/GitHub/doctopics/data/topicCategory.csv")
topicCorr <- read.csv("~/OneDrive/GitHub/doctopics/data/topicCorr.csv")
topicTerms <- read.csv("~/OneDrive/GitHub/doctopics/data/topicTerms.csv")
## Convert the LessonDate field to date format
llis$LessonDate <- as.Date(llis$LessonDate, "%m/%d/%y")

# Add constraint
addConstraint(graph, "Lesson", "name")
addConstraint(graph, "Submitter", "name")
addConstraint(graph, "Topic", "name")
addConstraint(graph, "Center", "name")

## My attempt to create nodes and releationships directly from a dataframe using a loop

## Create the nodes from the llis data frame

## System time for running the code below
## user   system  elapsed
## 4272.577   39.062 4594.435
system.time(for (i in 1:nrow(llis)){
     lesson <- getOrCreateNode(graph, "Lesson", name = llis[i,"LessonId"], year = year(llis[i,"LessonDate"]),
                          month = month(llis[i,"LessonDate"]), day = day(llis[i,"LessonDate"]),
                          title = llis[i,"Title"], abstract = llis[i,"Abstract"],
                          lesson = llis[i,"Lesson"], org = llis[i,"MissionDirectorate"],
                          safety = llis[i,"SafetyIssue"])
     submitter <- getOrCreateNode(graph, "Submitter", name = llis[i, "Submitter1"])
     center <- getOrCreateNode(graph, "Center", name = llis[i, "Organization"])
     topic <- getOrCreateNode(graph, "Topic", name =llis[i,"Topic"])

     createRel(topic, "Contains", lesson)
     createRel(submitter, "Wrote", lesson)
     createRel(lesson, "OccuredAt", center)
})

## Create the Category node and relate to topic
addConstraint(graph, "Category", "name")
for (t in 1:nrow(topicCategory)){
     topic <- getOrCreateNode(graph, "Topic", name = topicCategory[t, "Topic"])
     category <- getOrCreateNode(graph, "Category", name = topicCategory[t, "Category"])
     createRel(topic, "AssociatedTo", category)
}

## Relate topics by correlation

for (c in 1:nrow(topicCorr)){
     topic <- getOrCreateNode(graph, "Topic", name = topicCorr[c, "Topic"])
     Totopic <- getOrCreateNode(graph, "Topic", name = topicCorr[c, "ToTopic"])
     createRel(topic, "CorrelatedTo", Totopic, corr = topicCorr[c, "Correlation"])
}

## Create Term nodes
addConstraint(graph, "Term", "name")
for (e in 1:nrow(topicTerms)){
     topic <- getOrCreateNode(graph, "Topic", name = topicTerms[e, "Topic"])
     term <- getOrCreateNode(graph, "Term", name = topicTerms[e, "Terms"])
     createRel(term, "RankIn", topic, rank = topicTerms[e, "Rank"])
}

## Find a specific term and the topics it is in.

## Example
node_query <- "
MATCH n
RETURN n.name AS id,
n.name AS label,
LABELS(n)[0] AS group
"
##
topic_query <- "
MATCH (n:Term)-[:RankIn]->( m:Topic)
WHERE n.name = 'contamin'
RETURN m.name as id,
m.name as label,
LABELS(m)[0] AS group
"

term_query <- "
MATCH (n:Term)
WHERE n.name = 'contamin'
RETURN n.name AS id,
n.name AS label,
LABELS(n)[0] AS group
"

edge_query <- "
MATCH (n:Term)-[r:RankIn]->(m:Topic)
WHERE n.name = 'contamin'
RETURN n.name AS from,
m.name AS to,
-(r.rank) AS value,
r.rank AS title,
TYPE(r) AS label
"
topic.nodes <- cypher(graph, topic_query)
term.nodes <- cypher(graph, term_query)
edges <- cypher(graph, edge_query)

nodes <- rbind(topic.nodes, term.nodes)
edges

visNetwork(nodes, edges)
#######################################
## Find the Lessons in the topic the term is most prevalent in.

lesson_query <- "
MATCH (n:Topic)-[:Contains]->( m:Lesson)
WHERE n.name = 27
RETURN m.id AS id,
m.title AS label,
m.title AS title,
LABELS(m)[0] AS group
"

topic2_query <- "
MATCH (n:Topic)
WHERE n.name = 27
RETURN n.name AS id,
n.name AS label,
n.name AS title,
LABELS(n)[0] AS group
"

edge2_query <- "
MATCH (n:Topic)-[r:Contains]->(m:Lesson)
WHERE n.name = 27
RETURN n.name AS from,
m.id AS to,
m.title AS title,
(m.year) AS value
"
topic2.nodes <- cypher(graph, topic2_query)
lesson.nodes <- cypher(graph, lesson_query)
edges2 <- cypher(graph, edge2_query)

nodes2 <- rbind(topic2.nodes, lesson.nodes)
edges2
visNetwork(nodes2, edges2) %>%
     visLayout(randomSeed = 0622, hierarchical = FALSE)
##########################################################
## Find Topics Correlated to my primary topic
corr_query <- "
MATCH (n:Topic)-[r:CorrelatedTo]->( m:Topic)
WHERE r.corr > 0.40
RETURN n.name AS id,
n.name AS label,
LABELS(m)[0] AS group
"

edge3_query <- "
MATCH (n:Topic)-[r:CorrelatedTo]->(m:Topic)
WHERE r.corr > 0.40
RETURN n.name AS from,
m.name AS to,
r.corr AS title,
(r.corr) AS value
"
corr.nodes <- cypher(graph, corr_query)
edges3 <- cypher(graph, edge3_query)

visNetwork(corr.nodes, edges3) %>%
visLayout(randomSeed = 0622, hierarchical = FALSE)

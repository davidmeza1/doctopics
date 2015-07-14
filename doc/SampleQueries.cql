MATCH (n:Topic)-[r:Contains]->(m:Lesson)
WHERE n.name = 27
RETURN n, m,
n.name AS from,
m.id AS to,
m.title AS title,
(m.year) AS value,
"In" AS label

MATCH (n:Topic)-[r:CorrelatedTo]->(m:Topic)
WHERE n.name = 27 AND (r.corr > 0.02)
RETURN n, m,
n.name AS from,
m.id AS to,
m.title AS title,
(m.year) AS value,
"In" AS label


MATCH (n:Topic)-[r:CorrelatedTo]->(m:Topic)
WHERE r.corr > 0.40
RETURN n, m,
n.name AS from,
m.name AS to,
(r.corr) AS value

MATCH (n:Topic)-[r:CorrelatedTo]->(m:Topic)
WHERE n.name = 27
RETURN n, m,
n.name AS from,
m.name AS to,
(r.corr) AS value
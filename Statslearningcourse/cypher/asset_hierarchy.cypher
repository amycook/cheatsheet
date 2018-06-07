
CREATE (s: Site {name: "Grosvenor"});

LOAD CSV WITH HEADERS FROM "file:///scope_reduced.csv" AS row
WITH DISTINCT row.Module as Mod
MATCH (s:Site {name: "Grosvenor"})
MERGE (m:Mod:Module {name:Mod})-[r:SUB_MOD_TO]->(s);

LOAD CSV WITH HEADERS FROM "file:///scope_reduced.csv" AS row
WITH DISTINCT row.Module as Mod, row.SubModule as SubMod
MATCH (m:Mod {name: Mod})
MERGE (u:SubMod:SubModule {name:SubMod})-[r:SUB_MOD_TO]->(m);

Match (n:SubMod) WITH n LIMIT 1
match p = shortestpath ((n)-[*]->(s:Site))
return p;

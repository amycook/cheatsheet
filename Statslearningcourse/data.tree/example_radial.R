
library(data.tree)
library(networkD3)

fileName <- system.file("extdata", "useR15.csv", package="data.tree")
useRdf <- read.csv(fileName, stringsAsFactors = FALSE)

#define the hierarchy (Session/Room/Speaker)
useRdf$pathString <- paste("useR", useRdf$session, useRdf$room, useRdf$speaker, sep="|")
#convert to Node
useRtree <- as.Node(useRdf, pathDelimiter = "|")

#plot with networkD3
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radialNetwork( useRtreeList)
diagonalNetwork( useRtreeList)
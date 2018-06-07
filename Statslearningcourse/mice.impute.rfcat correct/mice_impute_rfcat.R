mice.impute.rfcat<- function (y, ry, x, ntree_cat = NULL, nodesize_cat = NULL, maxnodes_cat = NULL, 
                              ntree = NULL, ...) 
{
        # x <- as.matrix(x)
        bootsample <- sample(sum(ry), replace = TRUE)
        yobs <- y[ry][bootsample]
        xobs <- x[ry, , drop = FALSE][bootsample, , drop = FALSE]
        miss <- x[!ry, , drop = FALSE]
        if (is.null(ntree_cat) & !is.null(ntree)) {
                ntree_cat <- ntree
        }
        if (is.null(ntree_cat)) {
                if (is.null(getOption("CALIBERrfimpute_ntree_cat"))) {
                        ntree_cat <- 10
                }
                else {
                        ntree_cat <- getOption("CALIBERrfimpute_ntree_cat")
                }
        }
        if (is.null(nodesize_cat)) {
                if (is.null(getOption("CALIBERrfimpute_nodesize_cat"))) {
                        nodesize_cat <- 1
                }
                else {
                        nodesize_cat <- getOption("CALIBERrfimpute_nodesize_cat")
                }
        }
        if (is.null(maxnodes_cat)) {
                maxnodes_cat <- getOption("CALIBERrfimpute_maxnodes_cat")
        }
        missinglevels <- (table(yobs) == 0)
        newlevels <- rep(NA_integer_, length(levels(y)))
        newlevels[!missinglevels] <- 1:sum(!missinglevels)
        labels <- levels(y)
        oldlevels <- 1:length(levels(y))
        changes <- !identical(newlevels, 1:length(levels(y)))
        if (changes) {
                temp <- data.frame(id_yobs = 1:length(yobs), fac = as.integer(yobs))
                lookup <- data.frame(fac = oldlevels, new = factor(newlevels))
                temp <- merge(temp, lookup, all.x = TRUE)
                yobs <- temp[order(temp$id_yobs), "new"]
        }
        trees <- lapply(1:ntree_cat, function(x) {
                if (length(unique(yobs)) == 1) {
                        yobs[1]
                }
                else {
                        randomForest(xobs, yobs, ntree = 1, nodesize = nodesize_cat, 
                                     maxnodes = maxnodes_cat)
                }
        })
        yimp<- rep(0, nrow(xmiss))
        for(i in 1:nrow(xmiss)){
                thetree <- trees[[sample(ntree_cat, 1)]]
                if ("randomForest" %in% class(thetree)) {
                        index = predict(thetree, xmiss[i,])
                        yimp[i] = levels(y)[index]
                } else {
                        yimp[i] = thetree
                }
        }
        
        if (changes) {
                temp <- data.frame(id_yimp = 1:length(yimp), fac = as.integer(yimp))
                lookup <- data.frame(fac = newlevels, old = factor(oldlevels))
                levels(lookup$old) <- labels
                temp <- merge(temp, lookup, all.x = TRUE)
                yimp <- temp[order(temp$id_yimp), "old"]
        }
        return(yimp)
}
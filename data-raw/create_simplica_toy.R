# data-raw/create_simplica_toy.R
# Creates a small toy matrix with one multiplicative and one additive bicluster.

library(SIMPLICA.simtools)

set.seed(1)
# simplicaToy <- simulateBiclusterData(
#   matrixSize       = c(30, 50),
#   clusterTypes     = c("constant", "additive"),
#   clusterSize      = c(10, 20),
#   backgroundSignal = c(10),
#   backgroundNoise  = c(5),
#   signalLevel      = c(25),
#   signalStructure  = c(20),
#   signalNoise      = c(5),
#   rowShape         = "sin",
#   colShape         = "exp",
#   verbose = TRUE)


load("results/20250811_100526_bigGAExpDes.RData")
simplicaToy <- simResults[[276]]$simData

subset(gridResults, popSize==200 & pCrossover==0.5 & pMutation==0.025 & zeroFraction==0.9 & elitism==20 & numSimClus==5)


df <- simplicaToy$data
image(t(df))

simplicaToy$description = "60x120 matrix with one constant, one additive and one multiplicative bicluster"

dir.create("data", showWarnings = FALSE)
save(simplicaToy, file = "C:/Users/josha/OneDrive - Wageningen University & Research/2. My R/SIMPLICA/data/simplicaToy.rda", compress = "xz", version = 2)

data("simplicaToy")

fit <- simplica(df = simplicaToy$data, verbose = TRUE)

plotClusterResult(df = df,
                  string          = fit$best$string,
                  clusterPatterns = fit$best$clusterPatternsUpdated,
                  clusterScores   = fit$best$clusterScores,
                  showLabels      = FALSE,
                  title           = "SIMPLICA on simplicaToy",
                  scoreCutoff = 0, rearrange = FALSE)


summaryClusters(fit$best)
printBestClusters(fit$best)




# Run from the package root with SIMPLICA loaded from this checkout.
vignetteSimulation <- readRDS(file.path("data-raw", "vignetteData-simulation.rds"))
gaSettings <- list(numSimComp = 6, mySeeds = 1:4, popSize = 100, elitism = 40,
                   maxIter = 500, run = 150)
analysis <- do.call(simplica, c(list(df = vignetteSimulation$data), gaSettings))
fit <- analysis$best
seedResults <- do.call(rbind, lapply(seq_along(analysis$raw), function(step) {
  runs <- analysis$raw[[step]]
  data.frame(step = step, seed = as.integer(names(runs)), fitness = unname(fit$seedFitness[[step]]),
             generations = vapply(runs, function(result) as.numeric(result@iter), numeric(1)),
             selected = seq_along(runs) == if (step <= length(fit$bestSeeds)) which.max(fit$seedFitness[[step]]) else 0)
}))
write.csv(seedResults, file.path("data-raw", "vignetteData-seedResults.csv"), row.names = FALSE)

# The fitted object contains the residuals, CV results and scores by step and seed.
simplicaVignette <- list(data = vignetteSimulation$data, fit = fit,
  info = list(gaSettings = gaSettings, datasetId = vignetteSimulation$datasetId,
              simulationSeed = vignetteSimulation$simulationSeed,
              missingSeed = vignetteSimulation$missingSeed,
              packageVersion = as.character(utils::packageVersion("SIMPLICA")),
              rVersion = R.version.string))
save(simplicaVignette, file = file.path("data", "simplicaVignette.rda"), compress = "xz", version = 2)
print(summaryComponents(fit))
print(seedResults, row.names = FALSE)

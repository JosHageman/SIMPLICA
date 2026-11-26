# Restore the original vignette matrix and simulation metadata.
# Run from the package root in the project checkout.
original <- readRDS(file.path("..", "packageSimplica", "data-raw", "vignetteData-simulation.rds"))
vignetteSimulation <- list(data = original$data, truth = original$simulated$clusters,
                           datasetId = original$datasetId, simulationSeed = original$simulationSeed,
                           missingSeed = original$missingSeed)
write.csv(vignetteSimulation$data, file.path("data-raw", "vignetteData.csv"))
saveRDS(vignetteSimulation, file.path("data-raw", "vignetteData-simulation.rds"), compress = "xz", version = 2)

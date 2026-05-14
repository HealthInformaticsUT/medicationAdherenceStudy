# Define output folder ----
outputFolder <- here::here("results")

# Define shiny folder ----
shinyFolder <- here::here("shiny/rawData")

# Create output folder if it doesn't exist
if (!file.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}

# Start log ----
log_file <- here::here(outputFolder, paste0(dbName, "_log.txt"))
logger <- log4r::create.logger()
log4r::logfile(logger) <- log_file
log4r::level(logger) <- "INFO"

# Create cdm object ----
log4r::info(logger, "CREATE CDM OBJECT")
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = c(schema = cdmSchema),
  writeSchema = c(schema = writeSchema, prefix = writePrefix),
  cdmName = dbName
)

# cdm snapshot ----
log4r::info(logger, "CREATE SNAPSHOT")
write.csv(
  x = OmopSketch::summariseOmopSnapshot(cdm),
  file = here::here(outputFolder, paste0("snapshot_", CDMConnector::cdmName(cdm), ".csv")),
  row.names = FALSE
)

# Adherence step  ----
log4r::info(logger, "ADHERENCE FROM OMOP")

ingredient_concepts_ids <- read.csv("concepts.csv")[,1]

drugIngredientCodes <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = ingredient_concepts_ids, nameStyle = "{concept_name}")

concepts_ids <- unlist(drugIngredientCodes, use.names = F)

chronicDrugExposure <- AdherenceFromOMOP::generateChronicDrugExposure(
  cdm = cdm,
  conceptSet = concepts_ids,
  name = drugExposureTable,
  overwrite = T
)

adherence <- AdherenceFromOMOP::calculateAdherenceSlidingWindowBatched(
  cdm = cdm,
  drugExposure = chronicDrugExposure,
  name = adherenceResultTable,
  cma = c("CMA5","CMA6", "CMA7"),
  medicationGroup = drugIngredientCodes,
  batchSize = 7000,
  delayObservationWindowStart = TRUE,
  sliding.window.duration = 1,
  sliding.window.duration.unit = c("days", "weeks", "months", "years")[4],
  sliding.window.step.duration = 1,
  sliding.window.step.unit = c("days", "weeks", "months", "years")[4],
  cleanRows = FALSE
)
log4r::info(logger, "OBTAINED RESULTS")


log4r::info(logger, "FORMAT SUMMARY RESULTS")

summary <- summariseAdherence(adherence)

log4r::info(logger, "WRITE SUMMARY RESULTS TO OUTPUT FOLDER")

omopgenerics::exportSummarisedResult(summary, path = outputFolder)

log4r::info(logger, "WRITE SUMMARY RESULTS TO SHINY FOLDER")

omopgenerics::exportSummarisedResult(summary, path = shinyFolder)

log4r::info(logger, "SAVED SUMMARY RESULTS")

## zip everything together ---
zip::zip(
  zipfile = here::here(paste0("Results_", CDMConnector::cdmName(cdm), ".zip")),
  files = list.files(outputFolder),
  root = outputFolder
)


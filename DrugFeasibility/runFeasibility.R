# Define output folder ----
outputFolder <- here::here("results")

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

# ingredients
concepts <- read.csv(paste0(getwd(), "/concepts.csv"))[,1]

# Feasibility step  ----
log4r::info(logger, "QUERY DED PACKAGE")
output_feasibility <- DrugExposureDiagnostics::executeChecks(
  cdm,
  ingredients = concepts,
  checks = c("missing", "exposureDuration", "type", "route", "sourceConcept", "daysSupply",  "dose","quantity", "daysBetween",  "diagnosticsSummary"),
  verbose = FALSE
)

log4r::info(logger, "OBTAINED DED RESULTS")

## save DED results ---
log4r::info(logger, "WRITE DED RESULTS")
result <- DrugExposureDiagnostics::writeResultToDisk(
  resultList = output_feasibility,
  databaseId = dbName,
  outputFolder = outputFolder
)

log4r::info(logger, "SAVED DED RESULTS")

## zip everything together ---
zip::zip(
  zipfile = here::here(paste0("Results_", CDMConnector::cdmName(cdm), ".zip")),
  files = list.files(outputFolder),
  root = outputFolder
)

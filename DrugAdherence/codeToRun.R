# Load renv
renv::activate()
renv::restore()

library(dplyr) 
devtools::install_github("HealthInformaticsUT/AdherenceFromOMOP@HEAD", force = TRUE)


# Connect to database

# A DBI database connection to a database where an OMOP CDM v5.4 or v5.3 instance is located.
con <- DBI::dbConnect(...)

# Parameters to connect to create cdm object
# The schema where the OMOP CDM tables are located.
cdmSchema <- ""

# Schema in the CDM database that the user has write access to.
writeSchema <- ""

# A prefix that will be added to all tables created in the write_schema.
writePrefix <- ""

# name of the database, use acronym in capital letters (eg. "CPRD GOLD")
dbName <- ""

drugExposureTable <- ""
adherenceResultTable <- ""

# helper function
source(here::here("summariseAdherence.R"))
# Run the study
source(here::here("RunAdherence.R"))

# run shiny to see results
shiny::runApp(here::here("./shiny"))

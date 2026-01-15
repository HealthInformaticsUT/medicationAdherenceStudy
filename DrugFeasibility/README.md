# DrugFeasibility

## Introduction

This part of the repository contains the code for DrugExposureDiagnostics of the drug ingredients which we want to study. The number of ingredients is high, so the running time of the code can be long.

## How to run the analysis

1.  Download the repository as zip folder or you can use also Github Desktop.
2.  Open the project DrugFeasibility.Rproj in RStudio.
3.  Use renv::activate() and renv::restore() to install and load all necessary libraries.
4.  Open the codeToRun.R file and fill the necessary parameters for you database. You might need the the right driver package for your database (e.g. RPostgres for PostgreSQL) to connect to the database. The last line of CodeToRun.R will run the diagnostics (source(here::here("runFeasibility.R"))).
5.  After running there will be a zip file with results (Results\_{Your database name}.zip).
6.  We have provided also the Shiny app which comes together with [DrugExposureDiagnostics](https://github.com/darwin-eu/DrugExposureDiagnostics)
    1.  Copy the resulting zip file from results folder to shiny/ResultsExplorer/data folder

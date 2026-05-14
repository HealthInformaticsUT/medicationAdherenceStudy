# DrugAdherence

## Introduction

This part of the repository contains the code for adherence study. It is still a work in progress, do not use for medication adherence study. 

## How to run the analysis

1.  Download the repository as zip folder or you can use also Github Desktop.
2.  Open the project DrugAdherence.Rproj in RStudio.
3.  Use renv::activate() and renv::restore() to install and load all necessary libraries.
4.  Open the codeToRun.R file and fill the necessary parameters for you database. You might need the the right driver package for your database (e.g. RPostgres for PostgreSQL) to connect to the database.
5.  After running there will be a zip file with results (Results\_{Your database name}.zip).
6.  We have provided also the Shiny app with instructions in codeToRun file. 
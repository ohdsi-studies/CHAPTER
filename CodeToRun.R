# This is the only code the user should interact with
# Runs the Studyathon Long Covid and PASC Project

# Manage project dependencies
# the following will prompt you to install the various packages used in the study
# You must have renv package installed. Otherwise, run: install.packages("renv")
renv::activate()
renv::restore()

# Required packages from CRAN:
library(RPostgres)
library(tools)
library(DBI)
library(dplyr)
library(dbplyr)
library(CDMConnector)
library(here)
library(log4r)
library(zip)
library(ggplot2)
library(IncidencePrevalence)
library(PatientProfiles)
library(tibble)
library(reshape2)
library(readr)

#devtools::install_github("OHDSI/CirceR")
library(CirceR)

# Database name or acronym (e.g. for CPRD AURUM use "CPRUAurum")
db.name <- "..."

# Name of the output folder to save the results. Change to "output" or any other
# desired path
output.folder <- here::here()

# Stem to use for the cohort tables in the database
table_stem <- "..."

# Change the following parameters with your own database information
user <- Sys.getenv("...")
password <- Sys.getenv("...")
port <- Sys.getenv("...")
host <- Sys.getenv("...")
server_dbi <- Sys.getenv("...")

# Create database connection
# We use the DBI package to create a cdm_reference
db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)

# Name of the schema with the patient-level data
cdm_database_schema <- "..."

# Name of the schema where the result table will be created
results_database_schema <- "..."

# Study start date, should not change this
study_start_date <- as.Date("2018-01-01")

# Decide which parts of the study you want to run
readInitialCohorts <- TRUE
getStudyCohorts <- TRUE
doIncidencePrevalence <- TRUE
doTimeSeries <- TRUE

# Run the study
source(here("RunStudy.R"))

# After this is run you should have a zip file in your output folder to share

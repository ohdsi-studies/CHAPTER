# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of CHAPTER
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Get the incidence results of the CHAPTER study
#' @details 
#' This function performs the incidence calculations and characterisation
#' of the relevant cohorts of the CHAPTER study.
#' 
#' @param cdm                  A cdm object as created using the
#'                             \code{\link[CDMConnector]{cdmFromCon}} function 
#'                             in the CDMConnector package.
#' @param analyses             A tibble specifying the incidence analyses to
#'                             conduct, retrieved from the settings folder  
#' @param cohortNames          Names of all the cohorts stored in the cdm  
#' @param tempDir              Directory where the output is stored   
#' @param latestDataAvailability    Date of the latest data availability
#' 
#' @importFrom dplyr "%>%"
#' @export
#' 
getIncidenceResults <- function(cdm,
                                analyses,
                                cohortNames,
                                tempDir,
                                latestDataAvailability) {

  # Output folder for Incidence
  outputIp <- file.path(tempDir,"Incidence")
  if (!file.exists(outputIp))
    dir.create(outputIp, recursive = TRUE)

  # Output folder for TableOnes
  outputTo <- file.path(tempDir,"TableOne")
  if (!file.exists(outputTo))
    dir.create(outputTo, recursive = TRUE)
  
  ######################################################
  ParallelLogger::logInfo("- Getting table ones")
  
  for(tn in cohortNames) {
    result <- CohortCharacteristics::summariseCharacteristics(
      cohort = cdm[[tn]],
      ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)) 
    )
    
    write.csv(result, file = here::here(outputTo, paste0("tableOne_",tn,".csv")))
  }
  
  ######################################################
  ParallelLogger::logInfo("- Getting incidence all population denominator")
  
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    name = "denominator",
    cohortDateRange = c(as.Date("2018-01-01"), as.Date(latestDataAvailability)),
    daysPriorObservation = 365,
    sex = c("Male", "Female", "Both"),
    ageGroup = list(c(0,39),c(40,65),c(66,150),c(0,150))
  )
  
  # Get tableOne all population denominator too
  result <- PatientProfiles::summariseCharacteristics(
    cohort = cdm[["denominator"]],
    ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)) 
  )
  
  write.csv(result, file = here::here(outputTo, "tableOne_allpop.csv"))
  
  # Incidence for all diagnoses in the whole population
  outcomesAllPop <- analyses %>%
    dplyr::filter(denominator_table_name == "all_population") %>%
    dplyr::select(outcome_id, outcome_table_name)
  
  for(tn in outcomesAllPop %>% dplyr::pull(outcome_table_name) %>% unique()) {
    outcomeIdsAllPop <- outcomesAllPop %>%
      dplyr::filter(outcome_table_name == tn) %>%
      dplyr::pull(outcome_id) %>%
      unique()
    
    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm,
      denominatorTable = "denominator",
      outcomeTable = tn,
      outcomeCohortId = outcomeIdsAllPop,
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE,
      minCellCount = 5)
    
    write.csv(inc, file = here::here(outputIp, paste0("AllPop_",tn, ".csv")))
    write.csv(attr(inc, "attrition"), file = here::here(outputIp, paste0("AllPop_",tn,"_attrition.csv")))
  }
  
  ######################################################
    # Incidence of death on acute cohorts as denominator
    
    # So that death as an outcome can be picked up
    cdm[["acute_cohorts"]] <- cdm[["acute_cohorts"]] %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(cohort_end_date + lubridate::days(1))) %>% # check (K)
      dplyr::mutate(cohort_start_date = lubridate::as_date(cohort_start_date + lubridate::days(1))) %>% # check (K)
      dplyr::compute()
    
  denominatorsRest <- analyses %>%
    dplyr::filter(denominator_table_name != "all_population") %>%
    dplyr::select(denominator_id, denominator_name, denominator_table_name)
  
  for(cnd in denominatorsRest %>% dplyr::pull(denominator_name) %>% unique()) {
    ParallelLogger::logInfo(paste0("- Calculating incidence ",cnd," denominator"))
    
    workingDenominator <- denominatorsRest %>% 
      dplyr::filter(denominator_name == cnd)
    
    cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
      cdm =  cdm,
      name = "denominator",
      targetCohortTable = workingDenominator %>% dplyr::pull(denominator_table_name),
      targetCohortId = workingDenominator %>% dplyr::pull(denominator_id),
      cohortDateRange = c(as.Date("2018-01-01"), as.Date(latestDataAvailability)),
      daysPriorObservation = 365,
      sex = c("Male", "Female", "Both"),
      ageGroup = list(c(0,39),c(40,65),c(66,150),c(0,150))
    )
    
    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm,
      denominatorTable = "denominator",
      outcomeTable = analyses %>% dplyr::filter(denominator_name == cnd) %>% dplyr::pull(outcome_table_name),
      outcomeCohortId = analyses %>% dplyr::filter(denominator_name == cnd) %>% dplyr::pull(outcome_id),
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE,
      minCellCount = 5)
    
    write.csv(inc, file = here::here(outputIp, paste0(cnd, ".csv")))
    write.csv(attr(inc, "attrition"), file = here::here(outputIp, paste0(cnd,"_attrition.csv")))
  }
  
  #############################################################################
  # Same but now time at risk 365 days
  cdm[["acute_cohorts"]] <- cdm[["acute_cohorts"]] %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      cohort_end_date - cohort_start_date > 365,
      lubridate::as_date(cohort_start_date + lubridate::days(365)),
      cohort_end_date)) %>% # check (K)
    dplyr::compute()
  
  for(cnd in denominatorsRest %>% dplyr::pull(denominator_name) %>% unique()) {
    ParallelLogger::logInfo(paste0("- Calculating incidence ",cnd," denominator TAR 365"))
    
    workingDenominator <- denominatorsRest %>% 
      dplyr::filter(denominator_name == cnd)
    
    cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
      cdm =  cdm,
      name = "denominator",
      targetCohortTable = workingDenominator %>% dplyr::pull(denominator_table_name),
      targetCohortId = workingDenominator %>% dplyr::pull(denominator_id),
      cohortDateRange = c(as.Date("2018-01-01"), as.Date(latestDataAvailability)),
      daysPriorObservation = 365,
      sex = c("Male", "Female", "Both"),
      ageGroup = list(c(0,39),c(40,65),c(66,150),c(0,150))
    )

    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm,
      denominatorTable = "denominator",
      outcomeTable = analyses %>% dplyr::filter(denominator_name == cnd) %>% dplyr::pull(outcome_table_name),
      outcomeCohortId = analyses %>% dplyr::filter(denominator_name == cnd) %>% dplyr::pull(outcome_id),
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE,
      minCellCount = 5)

    write.csv(inc, file = here::here(outputIp, paste0(cnd, "_tar365.csv")))
    write.csv(attr(inc, "attrition"), file = here::here(outputIp, paste0(cnd,"_tar365_attrition.csv")))
  }
  
  #########################################################################################
  # Same but now time at risk 30 days
  cdm[["acute_cohorts"]] <- cdm[["acute_cohorts"]] %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(
      cohort_end_date - cohort_start_date > 30,
      lubridate::as_date(cohort_start_date + lubridate::days(30)),
      cohort_end_date)) %>% # check (K)
    dplyr::compute()
  
  for(cnd in denominatorsRest %>% dplyr::pull(denominator_name) %>% unique()) {
    ParallelLogger::logInfo(paste0("- Calculating incidence ",cnd," denominator TAR 30"))
    
    workingDenominator <- denominatorsRest %>% 
      dplyr::filter(denominator_name == cnd)
    
    cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
      cdm =  cdm,
      name = "denominator",
      targetCohortTable = workingDenominator %>% dplyr::pull(denominator_table_name),
      targetCohortId = workingDenominator %>% dplyr::pull(denominator_id),
      cohortDateRange = c(as.Date("2018-01-01"), as.Date(latestDataAvailability)),
      daysPriorObservation = 365,
      sex = c("Male", "Female", "Both"),
      ageGroup = list(c(0,39),c(40,65),c(66,150),c(0,150))
    )
    
    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm,
      denominatorTable = "denominator",
      outcomeTable = analyses %>% dplyr::filter(denominator_name == cnd) %>% dplyr::pull(outcome_table_name),
      outcomeCohortId = analyses %>% dplyr::filter(denominator_name == cnd) %>% dplyr::pull(outcome_id),
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE,
      minCellCount = 5)
    
    write.csv(inc, file = here::here(outputIp, paste0(cnd, "_tar30.csv")))
    write.csv(attr(inc, "attrition"), file = here::here(outputIp, paste0(cnd,"_tar30_attrition.csv")))
  }

}

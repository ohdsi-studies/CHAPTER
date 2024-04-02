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

#' Get the COVID related cohorts of the CHAPTER study
#' @details 
#' This function applies the inclusion criteria for some cohorts which
#' must be created with bespoke code, and gets their attrition.
#' 
#' @param cdm                  A cdm object as created using the
#'                             \code{\link[CDMConnector]{cdmFromCon}} function 
#'                             in the CDMConnector package.
#' 
#' @importFrom dplyr "%>%"
#' @export
#' 
createCovidCohorts <- function(cdm) {

  # observation period + death table
  observationDeath <- cdm$observation_period %>%
    dplyr::select("subject_id" = "person_id", "observation_period_end_date") %>%
    dplyr::left_join(cdm$death %>% dplyr::select("subject_id" = "person_id", "death_date"),
              by = "subject_id") %>%
    dplyr::mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
    dplyr::compute()

  # Output folder for Attrition
  outputAt <- file.path(tempDir,"Attrition")
  if (!file.exists(outputAt)){
    dir.create(outputAt, recursive = TRUE)}

  # ------------------------------------------------------------------------------
  # INFECTION COHORT FOR COVID RELATED CONDITIONS
  ParallelLogger::logInfo("- Getting COVID related outcome cohorts")
  
  newinfInit <- cdm[["acute_cohorts"]] %>%
    dplyr::filter(.data$cohort_definition_id == 2) %>% 
    dplyr::select(
      "subject_id",
      "cohort_definition_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>% 
    dplyr::compute()

  attrition <- dplyr::tibble(
    number_records = newinfInit %>% 
      dplyr::tally() %>% 
      dplyr::pull(),
    reason = "Initial events"
  )

  # No COVID infection previous 42 days
  newinfInit <- newinfInit %>%
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "acute_cohorts",
      targetCohortId = 2, 
      window = list(c(-Inf,-1)),
      nameStyle = "date_previous") %>%
    dplyr::filter(is.na(.data$date_previous) | .data$date_previous < -42) %>%
    dplyr::select(-"date_previous") %>%
    dplyr::compute()

  attrition <- dplyr::union_all(
    attrition,
    dplyr::tibble(
      number_records = newinfInit %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "Event washout"
    )
  )

  # Check the individuals are in observation at cohort entry
  newinfInit <- newinfInit %>%
    PatientProfiles::addInObservation() %>%
    dplyr::filter(.data$in_observation == 1) %>%
    dplyr::select(-"in_observation") %>%
    dplyr::compute()

  attrition <- dplyr::union_all(
    attrition,
    dplyr::tibble(
      number_records = newinfInit %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "In observation at cohort entry"
    )
  )

  # Prior history 365 days
  newinfInit <- newinfInit %>%
    PatientProfiles::addPriorObservation() %>%
    dplyr::filter(.data$prior_observation >= 365) %>%
    dplyr::select(-c("prior_observation")) %>%
    dplyr::compute()

  attrition <- dplyr::union_all(
    attrition,
    dplyr::tibble(
      number_records = newinfInit %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "365 days of prior history"
    )
  )

  # Historical influenza 90 days
  newinfInit <- newinfInit %>%
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "acute_cohorts",
      targetCohortId = 4, 
      window = list(c(-90,-1)),
      nameStyle = "last_flu") %>%
    dplyr::filter(is.na(last_flu)) %>%
    dplyr::select(-c("last_flu")) %>%
    dplyr::compute()

  attrition <- dplyr::union_all(
    attrition,
    dplyr::tibble(
      number_records = newinfInit %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "Historical influenza"
    )
  )

  # keep only people starting after study_start_date and 1 year before latest data availability
  oneYearLda <- latestDataAvailability - 365
  newinfInit <- newinfInit %>%
    dplyr::filter(.data$cohort_start_date > as.Date("2018-01-01") & # studyStartDate
                    .data$cohort_start_date < .env$oneYearLda) %>%
    dplyr::compute()

  attrition <- dplyr::union_all(
    attrition,
    dplyr::tibble(
      number_records = newinfInit %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = paste0("Entry after 2018-01-01 & before ",
                      oneYearLda)
    )
  )

  # censor on observation_end, death, end of covid testing or study end date
  newinfInit <- newinfInit %>%
    PatientProfiles::addCohortIntersectDate(
      targetCohortTable = "acute_cohorts",
      targetCohortId = 2, 
      window = list(c(1, 365)),
      order = "first", 
      nameStyle = "next_covid") %>%
    dplyr::compute()
  # censor on observation_end, death, study end date, or covid (re)infection
  newinfInit <- newinfInit %>%
    dplyr::left_join(observationDeath, by = c("subject_id")) %>%
    dplyr::compute()

  newinfInit <- newinfInit %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(dplyr::if_else(
      !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(dplyr::if_else(
      !(is.na(.data$next_covid)) & .data$cohort_end_date > .data$next_covid, .data$next_covid, .data$cohort_end_date))) %>%
    dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>%
    dplyr::mutate(reason_censoring = dplyr::if_else(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
                                                   dplyr::if_else(cohort_end_date == .data$observation_period_end_date,
                                                   "end of data collection or exit from database",
                                                   dplyr::if_else(cohort_end_date == .data$next_covid, "next covid infection", NA )))) %>%
    compute()

  # exclude if follow-up < 120 days
  excludedFollowup <- newinfInit %>%
    dplyr::filter(.data$follow_up_days < 120) %>%
    dplyr::compute()
  reasonExclusion <- excludedFollowup %>%
    dplyr::group_by(.data$reason_censoring) %>%
    dplyr::tally() %>%
    dplyr::collect()
  # Do we want sub >120d follow-up attrition? (K)
  
  newinfInit <- newinfInit %>%
    dplyr::filter(.data$follow_up_days >= 120) %>%
    dplyr::select(-follow_up_days, -reason_censoring, -next_covid, -death_date, -death, -observation_period_end_date) %>%
    dplyr::compute()

  attrition <- dplyr::union_all(
    attrition,
    dplyr::tibble(
      number_records = newinfInit %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "> 120 days follow-up"
    )
  )

  newinfInit <- newinfInit %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute()

  write.csv(
    attrition,
    file = here::here(outputAt, "attrition_infection.csv")
  )

  # ------------------------------------------------------------------------------
  # GETTING FINAL OUTCOME COHORTS
  numLcSymp <- as.integer(cdm[["long_covid_cohorts"]] %>%
                              dplyr::select("cohort_definition_id") %>% 
                            dplyr::distinct() %>% 
                            dplyr::tally() %>% 
                            dplyr::pull())
  numPasc <- as.integer(cdm[["pasc_cohorts"]] %>%
                           dplyr::select("cohort_definition_id") %>% 
                           dplyr::distinct() %>% 
                           dplyr::tally() %>% 
                           dplyr::pull())

  overlap <- list()
  # Right now I am treating the "LC code" as another symptom, can do it as its own thing (K)
  for(i in 1:numLcSymp) {
    outcome <- cdm[["long_covid_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i) %>%
      dplyr::compute()

    overlap[[i]] <- newinfInit %>%
      dplyr::inner_join(
        outcome %>%
          dplyr::select(subject_id, outcome_date = cohort_start_date,
                        outcome_end = cohort_end_date),
        by = "subject_id"
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::compute()

    attritionOutcome <- dplyr::tibble(
      number_records = overlap[[i]] %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "Initial events"
    )

    overlap[[i]] <- overlap[[i]] %>%
      dplyr::mutate(cohort_definition_id = i) %>%
      dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
      dplyr::filter(time_diff < -90 & time_diff > -366) %>%
      dplyr::select(-time_diff) %>%
      dplyr::compute()

    attritionOutcome <- dplyr::union_all(
      attritionOutcome,
      dplyr::tibble(
        number_records = overlap[[i]] %>% 
          dplyr::tally() %>% 
          dplyr::pull(),
        reason = "Outcome in window (90,365)"
      ))

    overlap[[i]] <- overlap[[i]] %>%
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "long_covid_cohorts",
        targetCohortId = i,
        window = list(c(-180,-1)), 
        nameStyle = "event") %>%
      dplyr::compute()

    if("event" %in% colnames(overlap[[i]])) {
      overlap[[i]] <- overlap[[i]] %>%
        dplyr::filter(event == 0) %>%
        dplyr::select(-c(event)) %>%
        dplyr::compute()
    }

    attritionOutcome <- dplyr::union_all(
      attritionOutcome,
      dplyr::tibble(
        number_records = overlap[[i]] %>% 
          dplyr::tally() %>% 
          dplyr::pull(),
        reason = "180 days of washout for the outcome"
      ))

    overlap[[i]] <- overlap[[i]] %>%
      dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
      dplyr::rename("cohort_start_date" = "outcome_date") %>%
      dplyr::rename("cohort_end_date" = "outcome_end") %>%
      dplyr::distinct() %>%
      dplyr::compute()

    write.csv(
      attritionOutcome,
      file = here::here(outputAt, paste0("attrition_",
                                          attr(cdm[["long_covid_cohorts"]], "cohort_set") %>%
                                            dplyr::filter(cohort_definition_id == i) %>%
                                            dplyr::pull("cohort_name")
                                          ,".csv"))
    )
  }

  overlapFinal <- overlap[[1]]
  for(i in 2:numLcSymp) {
    overlapFinal <- dplyr::union_all(
      overlapFinal,
      overlap[[i]]
    )
  }

  overlap <- overlapFinal %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute()

  overlapLcOne <- overlap %>%
    dplyr::group_by(subject_id) %>%
    dbplyr::window_order(cohort_definition_id) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapLcTwo <- overlapLcOne %>%
    dplyr::inner_join(overlap %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_definition_id) %>%
                 dplyr::filter(row_number() > 1) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 2) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapLcThree <- overlapLcOne %>%
    dplyr::inner_join(overlap %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_definition_id) %>%
                 dplyr::filter(row_number() > 2) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 3) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapPasc <- list()
  for(i in 1:numPasc) {
    outcome <- cdm[["pasc_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i) %>%
      dplyr::compute()

    overlapPasc[[i]] <- newinfInit %>%
      dplyr::inner_join(
        outcome %>%
          dplyr::select(subject_id, outcome_date = cohort_start_date,
                        outcome_end = cohort_end_date),
        by = "subject_id"
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::compute()

    attritionOutcome <- dplyr::tibble(
      number_records = overlapPasc[[i]] %>% 
        dplyr::tally() %>% 
        dplyr::pull(),
      reason = "Initial events"
    )

    overlapPasc[[i]] <- overlapPasc[[i]] %>%
      dplyr::mutate(cohort_definition_id = i) %>%
      dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
      dplyr::filter(time_diff < -90 & time_diff > -366) %>%
      dplyr::select(-time_diff) %>%
      dplyr::compute()

    attritionOutcome <- dplyr::union_all(
      attritionOutcome,
      dplyr::tibble(
        number_records = overlapPasc[[i]] %>% 
          dplyr::tally() %>% 
          dplyr::pull(),
        reason = "Outcome in window (90,365)"
      ))

    overlapPasc[[i]] <- overlapPasc[[i]] %>%
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "pasc_cohorts",
        targetCohortId = i,
        window = list(c(-180,-1)), 
        nameStyle = "event") %>%
      dplyr::compute()

    if("event" %in% colnames(overlapPasc[[i]])) {
      overlapPasc[[i]] <- overlapPasc[[i]] %>%
        dplyr::filter(event == 0) %>%
        dplyr::select(-c(event)) %>%
        dplyr::compute()
    }

    attritionOutcome <- dplyr::union_all(
      attritionOutcome,
      dplyr::tibble(
        number_records = overlapPasc[[i]] %>% 
          dplyr::tally() %>% 
          dplyr::pull(),
        reason = "180 days of washout for the outcome"
      ))

    overlapPasc[[i]] <- overlapPasc[[i]] %>%
      dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
      dplyr::rename("cohort_start_date" = "outcome_date") %>%
      dplyr::rename("cohort_end_date" = "outcome_end") %>%
      dplyr::distinct() %>%
      dplyr::compute()

    write.csv(
      attritionOutcome,
      file = here::here(outputAt, paste0("attrition_",
                                          attr(cdm[["pasc_cohorts"]], "cohort_set") %>%
                                            dplyr::filter(cohort_definition_id == i) %>%
                                            dplyr::pull("cohort_name")
                                          ,".csv"))
    )
  }

  overlapPascFinal <- overlapPasc[[1]]
  for(i in 2:numPasc) {
    overlapPascFinal <- dplyr::union_all(
      overlapPascFinal,
      overlapPasc[[i]]
    )
  }

  overlapPasc <- overlapPascFinal %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute()

  overlapPascOne <- overlapPasc %>%
    dplyr::group_by(subject_id) %>%
    dbplyr::window_order(cohort_definition_id) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::mutate(cohort_definition_id = 4) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapPascTwo <- overlapPascOne %>%
    dplyr::inner_join(overlapPasc %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_definition_id) %>%
                 dplyr::filter(row_number() > 1) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 5) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapPascThree <- overlapPascOne %>%
    dplyr::inner_join(overlapPasc %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_definition_id) %>%
                 dplyr::filter(row_number() > 2) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 3) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapCohorts <- overlapLcOne %>%
    dplyr::union_all(overlapLcTwo) %>%
    dplyr::union_all(overlapLcThree) %>%
    dplyr::union_all(overlapPascOne) %>%
    dplyr::union_all(overlapPascTwo) %>%
    dplyr::union_all(overlapPascThree)

  # Check here cohort_attrition and cohort_set and all (K)
  attr(overlapCohorts, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1:6),
    cohort_name = c("long_covid_one_symptom",
                    "long_covid_two_symptoms",
                    "long_covid_three_symptoms",
                    "pasc_one_event",
                    "pasc_two_events",
                    "pasc_three_events")
  )
  attr(overlapCohorts, "cohort_count") <- getCohortCount(overlapCohorts)
  attr(overlapCohorts, "cohort_attrition") <- attr(overlapCohorts, "cohort_count") %>%
    dplyr::mutate(number_subjects = number_records,
                  reason_id = 0L,
                  reason = "Qualifying events",
                  excluded_records = 0L,
                  excluded_subjects = 0L)
  attr(overlapCohorts, "tbl_name") <- "overlap_cohorts"

  cdm[["overlap_cohorts"]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(overlapCohorts, "overlap_cohorts", FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = attr(overlapCohorts, "cohort_set"),
    cohortAttritionRef = attr(overlapCohorts, "cohort_attrition")
  )

  # GET PASC OUTCOMES AS ACUTE OUTCOMES ON THEIR OWN TOO
  if(attr(cdm[["acute_cohorts"]], "cohort_set") %>% 
     dplyr::tally() %>% dplyr::pull() < 6) {
    acuteCohortsAll <- cdm[["acute_cohorts"]] %>%
      dplyr::union_all(cdm[["pasc_cohorts"]] %>%
                         dplyr::mutate(cohort_definition_id = cohort_definition_id + 5)) %>%
      dplyr::compute()
    attr(acuteCohortsAll, "cohort_set") <- attr(cdm[["acute_cohorts"]], "cohort_set") %>%
      dplyr::union_all(attr(cdm[["pasc_cohorts"]], "cohort_set") %>%
                         dplyr::mutate(cohort_definition_id = cohort_definition_id + 5)) %>%
      dplyr::compute()
    attr(acuteCohortsAll, "cohort_count") <- getCohortCount(acuteCohortsAll)
    attr(acuteCohortsAll, "cohort_attrition") <- attr(acuteCohortsAll, "cohort_count") %>%
      dplyr::mutate(number_subjects = number_records,
                    reason_id = 0L,
                    reason = "Qualifying events",
                    excluded_records = 0L,
                    excluded_subjects = 0L)
    attr(acuteCohortsAll, "tbl_name") <- "acute_cohorts"
    
    cdm[["acute_cohorts"]] <- newGeneratedCohortSet(
      cohortRef = computeQuery(acuteCohortsAll, "acute_cohorts", FALSE, attr(cdm, "write_schema"), TRUE),
      cohortSetRef = attr(acuteCohortsAll, "cohort_set"),
      cohortAttritionRef = attr(acuteCohortsAll, "cohort_attrition")
    )
  }
  
  ParallelLogger::logInfo("- Getting acute prognosis cohorts")
  # Generate mortality cohorts
  acutePrognosisCcohorts <- cdm$death %>%
    PatientProfiles::addInObservation(indexDate = "death_date") %>%
    dplyr::filter(.data$in_observation == 1) %>%
    dplyr::select("person_id", "death_date") %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(cdm[["acute_cohorts"]] %>%
                        dplyr::select("subject_id", "cohort_definition_id"),
                      by = c("subject_id")) %>%
    dplyr::select("subject_id", "death_date") %>%
    dplyr::group_by(.data$subject_id) %>%
    dbplyr::window_order(.data$death_date) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::rename("cohort_start_date" = "death_date") %>%
    dplyr::mutate(cohort_definition_id = 1L ,
                  cohort_end_date = .data$cohort_start_date) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  
  attr(acutePrognosisCcohorts, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1),
    cohort_name = c("all_cause_mortality")
  )
  attr(acutePrognosisCcohorts, "cohort_count") <- getCohortCount(acutePrognosisCcohorts)
  attr(acutePrognosisCcohorts, "cohort_attrition") <- attr(acutePrognosisCcohorts, "cohort_count") %>%
    dplyr::mutate(number_subjects = number_records,
                  reason_id = 0L,
                  reason = "Qualifying events",
                  excluded_records = 0L,
                  excluded_subjects = 0L)
  attr(acutePrognosisCcohorts, "tbl_name") <- "acute_prognosis_cohorts"
  
  cdm[["acute_prognosis_cohorts"]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(acutePrognosisCcohorts, "acute_prognosis_cohorts", FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = attr(acutePrognosisCcohorts, "cohort_set"),
    cohortAttritionRef = attr(acutePrognosisCcohorts, "cohort_attrition")
  )

  # ------------------------------------------------------------------------------
  # Print counts of all cohorts (if >5)
  ParallelLogger::logInfo("- Getting cohort counts")

  finalCounts <- list()
  for(i in 1:length(cohortNames)) {
    finalCounts[[i]] <- attr(cdm[[cohortNames[i]]], "cohort_attrition") %>%
      dplyr::group_by(cohort_definition_id) %>%
      dplyr::filter(reason_id == max(reason_id)) %>%
      dplyr::ungroup() %>%
      dplyr::select("cohort_definition_id", "number_records", "number_subjects") %>%
      dplyr::left_join(attr(cdm[[cohortNames[i]]], "cohort_set"), by = "cohort_definition_id") %>%
      dplyr::collect() %>%
      dplyr::mutate(table_name = cohortNames[i]) %>%
      dplyr::select(c("table_name", "cohort_name", "number_records", "number_subjects"))
  }

  finalCounts <- dplyr::bind_rows(finalCounts)

  # Export csv
  write.csv(finalCounts,
            file = file.path(tempDir,
                             paste0("finalcounts.csv")
            )
  )

}

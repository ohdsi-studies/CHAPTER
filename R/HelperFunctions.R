# helper functions

getCohortCount <- function(cohort) {
  cohort %>%
    group_by(cohort_definition_id) %>%
    summarise(
      number_records = dplyr::n() %>% as.integer(),
      number_subjects = dplyr::n_distinct(.data$subject_id) %>% as.integer(),
      .groups = "drop"
    ) %>%
    collect() %>%
    dplyr::right_join(attr(cohort, "cohort_set") %>% dplyr::select("cohort_definition_id") %>%
                        dplyr::collect(),
                     by = "cohort_definition_id") %>%
    dplyr::mutate(number_records = dplyr::if_else(is.na(number_records), 0, number_records),
                  number_subjects = dplyr::if_else(is.na(number_subjects), 0, number_subjects)) %>%
    arrange(cohort_definition_id) %>%
    dplyr::distinct()
}

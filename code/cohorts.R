#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'

#' function to return visits and essential info based on visit_concept_id and date parameters

get_visits <-
  function(visit_codes=c(9201, 9202, 9203, 2000000048, 2000000088), start_date="2010-01-01", end_date="2021-12-31") {
    cdm_tbl("visit_occurrence") %>%
      filter( visit_concept_id %in% visit_codes, visit_start_date >=start_date, visit_end_date <= end_date) %>%
      dplyr::select(person_id, visit_occurrence_id, visit_concept_id, visit_concept_name, visit_start_date, visit_end_date, provider_id, care_site_id) %>%
      compute_new(indexes = list("provider_id", "care_site_id", "person_id", "visit_concept_id"))
  }


# Vector of additional packages to load before executing the request
config_append("extra_packages", c("tidyverse"))

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run <- function() {
  setup_pkgs() # Load runtime packages as specified above

  message(
    "Starting execution with framework version ",
    config("framework_version")
  )

  message("Get codesets")
  general_codes <- read_codeset("general_visits_codesets") %>%
    filter(concept_id != 38004477) %>%
    pull(concept_id)

  general_providers <- cdm_tbl("provider") %>%
    filter(specialty_concept_id %in% general_codes) %>%
    distinct(provider_id) %>%
    compute_new()

  general_sites <- cdm_tbl("care_site") %>%
    filter(specialty_concept_id %in% general_codes) %>%
    distinct(care_site_id) %>%
    compute_new()

  specialist_codes <-
    load_codeset("specialist_visits_codesets") %>%
    filter(!is.na(specialty)) %>%
    filter(specialty != "Pain") %>% #dropped from study
    compute_new()

  specialist_providers <-
    cdm_tbl("provider") %>%
    inner_join(specialist_codes, by = c("specialty_concept_id" = "concept_id")) %>%
    distinct(provider_id, specialty) %>%
    anti_join(read_codeset("omit_providers"), copy = TRUE) %>% #get rid of providers we know have issues
    compute_new()

  specialist_sites <-
    cdm_tbl("care_site") %>%
    inner_join(specialist_codes, by = c("specialty_concept_id" = "concept_id")) %>%
    distinct(care_site_id, specialty) %>%
    compute_new()
  
  message(Sys.time())
  message("Temp Visits_tbl")
  
  tmp_visits <-
    get_visits() #defined in cohort
  
  message(Sys.time())
  message("Temp Outpatient Table")

  tmp_outpatient <-
    tmp_visits %>%
    filter(visit_concept_id == 9202, visit_start_date >="2010-01-01") %>%
    compute_new(indexes = list("provider_id", "care_site_id", "person_id"))

  message(Sys.time())
  message("General by Provider")

  general_outpatient_provider <-
    tmp_outpatient %>%
    inner_join(general_providers) %>%
    dplyr::select(visit_occurrence_id) %>%
    compute_new()

  message(Sys.time())
  message("General by Site")

  general_outpatient_site <-
    tmp_outpatient %>%
    inner_join(general_sites) %>%
    dplyr::select(visit_occurrence_id) %>%
    compute_new()

  message(Sys.time())
  message("Save General")

  general_outpatient_visits <-
    general_outpatient_provider %>%
    dplyr::union(general_outpatient_site) %>%
    inner_join(tmp_outpatient) %>%
    compute_new()
  
  

  message(Sys.time())
  message("Get cohort")

  cohort <-
    general_outpatient_visits %>%
    dplyr::select(person_id, visit_start_date) %>%
    inner_join(cdm_tbl("person")) %>%
    mutate(date_21 = as.Date(birth_date+years(21))) %>% 
    mutate(eth_cat = case_when(
      ethnicity_concept_id == 38003563L ~ "Hispanic",
      race_concept_id == 8516L ~ "Black/AA, non-Hispanic",
      race_concept_id %in% c(8515L, 8557L) ~
        "Asian/PI",
      race_concept_id == 8567L ~ "Native American",
      race_concept_id == 8527L ~ "White, non-Hispanic",
      race_concept_id == 44814659L ~ "Multiple",
      TRUE ~ "Other/Unknown"
    )) %>% #limit to known gender and to age >=0, < 21
    filter(gender_concept_name %in% c("MALE", "FEMALE"), visit_start_date >= birth_date, visit_start_date < date_21)  %>%
    distinct(birth_date, person_id, gender_concept_name, eth_cat, date_21) %>%
    compute_new(index = "person_id")
  
 
    
  general_op_payer <-
    cohort %>%
    inner_join( general_outpatient_visits) %>%
    filter(visit_start_date >= birth_date, visit_start_date < date_21) %>%
    dplyr::select(person_id, visit_occurrence_id) %>%
    inner_join(cdm_tbl("visit_payer")) %>%
    dplyr::select(visit_occurrence_id, plan_class) %>%
    right_join(general_outpatient_visits) %>%
    mutate(rank_payer = case_when(is.na(plan_class) ~ 0, plan_class == "Private/Commercial" ~ 1, plan_class %in% c("Medicaid/sCHIP", "Medicare", "Other public") ~ 2, TRUE ~ 0)) %>%
    group_by(person_id) %>%
    summarise(payer = max(rank_payer)) %>%
    mutate(payer_class = case_when(payer == 0 ~ "Unknown/Self-Pay", payer == 1 ~ "Private", payer == 2 ~ "Public")) %>%
    compute_new()
  
  cohort %>%
    inner_join(general_op_payer) %>%
    output_tbl("final_cohort", index="person_id")
  
  
  denominator <-
    cohort %>%
    inner_join( general_outpatient_visits) %>%
    filter(visit_start_date >= birth_date, visit_start_date < date_21) %>%
    mutate(visit_year = year(visit_start_date)) %>%
    mutate(age_yr = (visit_start_date - birth_date) / 365.25) %>%
    group_by(person_id, visit_year) %>%
    summarise(age_yr = floor(min(age_yr))) %>%
    ungroup() %>%
    compute_new()  
  
  denominator_final <-
    denominator %>%
    mutate(visit_year = visit_year + 1) %>%
    dplyr::union(denominator) %>%
    filter(visit_year %in% 2011:2021) %>%
    group_by(person_id, visit_year) %>%
    summarise(age_yr = min(age_yr)) %>%
    compute_new()
  
  denominator_final %>%
    output_tbl("final_denominator", indexes=list("person_id","visit_year"))
    

  message(Sys.time())
  message("Specialist by Provider")

  specialist_visits_provider <-
    tmp_outpatient %>%
    inner_join(specialist_providers) %>%
    dplyr::select(visit_occurrence_id, specialty) %>%
    mutate(source = "provider") %>%
    compute_new()

  message(Sys.time())
  message("Specialist by Site")

  specialist_visits_site <-
    tmp_outpatient %>%
    anti_join(specialist_visits_provider) %>%
    inner_join(specialist_sites) %>%
    dplyr::select(visit_occurrence_id, specialty) %>%
    mutate(source = "site") %>%
    compute_new()

  message(Sys.time())
  message("Save Specialist")

  specialist_outpatient_visits <-
    specialist_visits_provider %>%
    dplyr::union_all(specialist_visits_site) %>%
    inner_join(tmp_outpatient) %>%
    inner_join(cohort) %>%
    filter(visit_start_date >= birth_date, visit_start_date < date_21) %>%
    compute_new()

  specialist_outpatient_visits %>%
    mutate(visit_year = year(visit_start_date)) %>%
    distinct(specialty, visit_year, person_id) %>%
    output_tbl("final_specialist_outpatient", indexes=list("person_id","visit_year"))
 
  message(Sys.time())
  message("Save Specialist Outpatient Conditions")
  
  #save distinct conditions and years

  specialist_outpatient_visits %>%
    mutate(visit_year = year(visit_start_date)) %>%
    dplyr::select(specialty, visit_occurrence_id, visit_year) %>%
    inner_join(cdm_tbl("condition_occurrence")) %>%
    anti_join(load_codeset("omit_conds"), by = c("condition_concept_id" = "concept_id")) %>%
    distinct(specialty, visit_year, person_id, condition_concept_id, condition_concept_name) %>%
    output_tbl("visit_conditions_year", indexes = list("person_id", "condition_concept_id"))



  # all inpatient or ED visits to specialists


  message(Sys.time())
  message("Get Inpatient and ED visits")

  other_visits_tbl <-
    tmp_visits%>%
    filter(visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
    inner_join(cohort) %>%
    filter(visit_start_date >= birth_date, visit_start_date < date_21) %>%
    compute_new()

  message(Sys.time())
  message("Specialist by Provider")

  other_visits_provider <-
    other_visits_tbl %>%
    inner_join(specialist_providers) %>%
    dplyr::select(visit_occurrence_id, specialty) %>%
    mutate(source = "provider") %>%
    compute_new()

  message(Sys.time())
  message("Specialist by Site")

  other_visits_site <-
    other_visits_tbl %>%
    anti_join(other_visits_provider) %>%
    inner_join(specialist_sites) %>%
    dplyr::select(visit_occurrence_id, specialty) %>%
    mutate(source = "site") %>%
    compute_new()

  message(Sys.time())
  message("Save Specialist ED/Inpatient")

  other_visits_provider %>%
    dplyr::union_all(other_visits_site) %>%
    inner_join(other_visits_tbl) %>%
    mutate(visit_year = year(visit_start_date)) %>%
    distinct(specialty, visit_year, person_id) %>%
    output_tbl("final_specialist_other")
  
  attrition_row_1 <-
    tibble(step = 1, cohort = "all patients", n = cdm_tbl("person") %>% summarise(n = n()) %>% pull())

  attrition_row_2 <-
    tibble(step = 2, cohort = "all patients w general outpatient pediatric visit in study period", n = results_tbl("final_cohort") %>% summarise(n = n()) %>% pull())

  bind_rows(attrition_row_1, attrition_row_2) %>%
    output_tbl("attrition_tbl")



  # Set up the step log with as many attrition columns as you need.
  # For example, this call sets up the log with a `persons` count that will be
  # required at each step.

  message("Done.")

  message(Sys.time())
}

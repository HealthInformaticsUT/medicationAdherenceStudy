summariseAdherence <- function(data) {

  # ── Age groups ────────────────────────────────────────────────────────────────
  data_with_age_groups <- data %>%
    dplyr::mutate(
      age = as.integer(as.integer(window.start - date_of_birth) / 365),
      age_group = cut(
        age,
        breaks = c(0, 19, 39, 59, 80, Inf),
        labels = c("0-19", "20-39", "40-59", "60-80", "80<"),
        right  = FALSE
      )
    ) %>%
    dplyr::collect()

  # ── Helper: format a summarised tibble into the omop result schema ─────
  format_result <- function(df,
                            result_id_val,
                            strata_name_val,
                            strata_level_col,
                            additional_name_val  = "overall",
                            additional_level_val = "overall") {
    df %>%
      dplyr::mutate(
        strata_name      = strata_name_val,
        strata_level     = {{ strata_level_col }},
        group_name       = "ingredient",
        group_level      = group,
        variable_name    = name,
        # same for every estimate_name case
        variable_level   = NA_character_,
        # BUG FIX: original used `paste(name)` inside case_when for every branch,
        # which is identical to just `name` — the case_when added no value.
        estimate_type    = dplyr::if_else(
          estimate_name %in% c("count", "count_all", "number_of_pts"),
          "integer",
          "numeric"
        ),
        result_id        = as.character(result_id_val),
        cdm_name         = dbName,
        additional_name  = additional_name_val,
        additional_level = additional_level_val
      ) %>%
      dplyr::select(omopgenerics::resultColumns())
  }

  # RESULT 1: Mean CMA ####
  # For each person, first compute their personal mean CMA (averaging across all
  # measurement windows). Then aggregate those personal means by stratum.
  # This two-step approach avoids giving extra weight to people with more windows.

  # ── Helper: aggregate person-level means into group-level summary stats ───────
  summarise_cma <- function(person_means_df, group_cols) {
    person_means_df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        group_mean_cma = mean(mean_CMA, na.rm = TRUE),
        group_sd_cma   = sd(mean_CMA, na.rm = TRUE),
        n_people       = n(),
        ci             = list(bootstrap_ci(mean_CMA)),
        .groups        = "drop"
      ) %>%
      tidyr::unnest_wider(ci) %>%
      # k=5 suppression: drop strata with 5 or fewer people
      dplyr::filter(n_people > 5) %>%
      tidyr::pivot_longer(
        cols      = c(
          n_people,
          group_mean_cma,
          group_sd_cma,
          ci_lower_95,
          ci_upper_95
        ),
        names_to  = "estimate_name",
        values_to = "estimate_value"
      )
  }

  # ── Overall (no stratification) ──────────────────────────────────────────
  person_mean_cma_overall <- data_with_age_groups %>%
    dplyr::group_by(group, name, person_id) %>%
    dplyr::summarise(mean_CMA = mean(CMA, na.rm = TRUE), .groups = "drop")

  mean_cma_overall <- summarise_cma(person_mean_cma_overall, c("group", "name")) %>%
    format_result(
      result_id_val    = 1,
      strata_name_val  = "overall",
      strata_level_col = "overall"   # literal string — no stratification
    )

  # ── Stratified by age group ───────────────────────────────────────────────
  person_mean_cma_by_age <- data_with_age_groups %>%
    dplyr::group_by(group, name, person_id, age_group) %>%
    dplyr::summarise(mean_CMA = mean(CMA, na.rm = TRUE), .groups = "drop")

  mean_cma_by_age_group <- summarise_cma(person_mean_cma_by_age, c("group", "name", "age_group")) %>%
    format_result(
      result_id_val    = 1,
      strata_name_val  = "age_group",
      strata_level_col = age_group
    )

  # ── Stratified by sex ─────────────────────────────────────────────────────
  person_mean_cma_by_sex <- data_with_age_groups %>%
    dplyr::group_by(group, name, person_id, sex) %>%
    dplyr::summarise(mean_CMA = mean(CMA, na.rm = TRUE), .groups = "drop")

  mean_cma_by_sex <- summarise_cma(person_mean_cma_by_sex, c("group", "name", "sex")) %>%
    format_result(
      result_id_val    = 1,
      strata_name_val  = "sex",
      strata_level_col = sex
    )

  # ── Stratified by age group × sex ────────────────────────────────────────
  person_mean_cma_by_age_sex <- data_with_age_groups %>%
    dplyr::group_by(group, name, person_id, sex, age_group) %>%
    dplyr::summarise(mean_CMA = mean(CMA, na.rm = TRUE), .groups = "drop")

  mean_cma_by_age_group_sex <- summarise_cma(person_mean_cma_by_age_sex,
                                             c("group", "name", "age_group", "sex")) %>%
    format_result(
      result_id_val    = 1,
      strata_name_val  = "age_group &&& sex",
      strata_level_col = paste(age_group, "&&&", sex)
    )

  # RESULT 2: Proportion with mean CMA ≥ 0.8   #####
  # A person is "adherent" if their personal mean CMA across all windows is ≥ 0.8.
  # count of adherent people, total people, and percentage.
  # ── Helper: compute adherence proportions given a person-level summary ────────
  summarise_proportion_adherent <- function(person_means_df, group_cols) {
    person_means_df %>%
      dplyr::mutate(is_adherent = mean_CMA >= 0.8) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        n_people_total    = n(),
        n_people_adherent = sum(is_adherent, na.rm = TRUE),
        pct_adherent      = n_people_adherent / n_people_total * 100,
        .groups           = "drop"
      ) %>%
      # k=5 suppression on the adherent sub-count (the smaller, riskier number)
      dplyr::filter(n_people_adherent > 5) %>%
      tidyr::pivot_longer(
        cols      = c(n_people_total, n_people_adherent, pct_adherent),
        names_to  = "estimate_name",
        values_to = "estimate_value"
      )
  }

  # ── 2a. Overall ───────────────────────────────────────────────────────────────
  proportion_adherent_overall <- summarise_proportion_adherent(
    person_mean_cma_overall,
    # reuse from result 1a
    c("group", "name")
  ) %>%
    format_result(
      result_id_val    = 2,
      strata_name_val  = "overall",
      strata_level_col = "overall"
    )

  # ── 2b. By sex ────────────────────────────────────────────────────────────────
  proportion_adherent_by_sex <- summarise_proportion_adherent(
    person_mean_cma_by_sex,
    # reuse from result 1c
    c("group", "name", "sex")
  ) %>%
    format_result(
      result_id_val    = 2,
      strata_name_val  = "sex",
      strata_level_col = sex
    )

  # ── 2c. By age group ─────────────────────────────────────────────────────────
  proportion_adherent_by_age_group <- summarise_proportion_adherent(
    person_mean_cma_by_age,
    # reuse from result 1b
    c("group", "name", "age_group")
  ) %>%
    format_result(
      result_id_val    = 2,
      strata_name_val  = "age_group",
      strata_level_col = age_group
    )

  # ── 2d. By age group × sex ───────────────────────────────────────────────────
  proportion_adherent_by_age_group_sex <- summarise_proportion_adherent(
    person_mean_cma_by_age_sex,
    # reuse from result 1d
    c("group", "name", "sex", "age_group")
  ) %>%
    format_result(
      result_id_val    = 2,
      strata_name_val  = "age_group &&& sex",
      strata_level_col = paste(age_group, "&&&", sex)
    )

  # RESULT 3: Kaplan-Meier curves for continuous adherence duration   #####
  # "Continuous adherence" = an uninterrupted run of windows (window.ID never
  # resets to 1 after the first window). The event of interest is discontinuation
  # (a second run starting). Only "new chronic patients" are included: those whose
  # very first prescription started more than 364 days after observation began,
  # ensuring they are incident users rather than prevalent users.

  # ── 3-1. Isolate the first continuous run per person × drug ───────────────────
  first_adherence_run <- data %>%
    dplyr::collect() %>%
    dplyr::arrange(person_id, name, window.start) %>%
    dplyr::group_by(person_id, name, group) %>%
    dplyr::mutate(
      # A reset happens when window.ID goes back to 1 after the very first window
      is_reset       = window.ID == 1 & dplyr::row_number() > 1,
      n_resets_so_far = cumsum(is_reset)
    ) %>%
    # Keep only rows belonging to the first uninterrupted run (before any reset)
    dplyr::filter(n_resets_so_far == 0) %>%
    dplyr::ungroup()

  # ── 3-2. Flag whether each person ever started a second run ───────────────────
  # A second run means window.ID resets to 1 at least once after the first window.
  # cum_resets reaches 1 at the very first window.ID==1 row, so >1 means a RESET .
  discontinuation_flag <- first_adherence_run %>%
    dplyr::collect() %>%
    dplyr::arrange(person_id, name, window.start) %>%
    dplyr::group_by(person_id, name, group) %>%
    dplyr::mutate(discontinuation = as.numeric(observation_period_end_date - max(window.end)) > 365) %>%
    dplyr::ungroup() %>%
    dplyr::select(person_id, name, group, discontinuation) %>%
    dplyr::distinct()

  # ── 3-3. Build the survival data frame ────────────────────────────────────────
  surv_data <- first_adherence_run %>%
    dplyr::group_by(person_id, name, sex, group, date_of_birth) %>%
    dplyr::mutate(
      # Incident user flag: first window starts more than a year into observation
      is_incident_window = as.numeric(window.start - observation_period_start_date) > 364
      & window.ID == 1,
      is_incident_user   = any(is_incident_window)
    ) %>%
    dplyr::filter(is_incident_user) %>%
    dplyr::select(-is_incident_window, -is_incident_user) %>%
    dplyr::summarise(
      run_start = min(window.start),
      run_end   = max(window.end),
      .groups   = "drop"
    ) %>%
    dplyr::left_join(discontinuation_flag, by = c("person_id", "name", "group")) %>%
    dplyr::mutate(
      # Follow-up time in whole years; event = 1 if a second run was ever started
      followup_years     = round(as.numeric(run_end - run_start) / 365),
      discontinuation    = as.integer(discontinuation)
    )

  # ── 3-4. Helper: KM fit → anonymised long tibble ─────────────────────────────
  extract_km <- function(km_fit,
                         source_data,
                         strata_label,
                         baseline_risk_counts) {

    km_summary_df <- survminer::surv_summary(km_fit, data = source_data) %>%
      tibble::as_tibble()

    # survminer prefixes strata values with "varname="; strip to keep only the level
    if ("strata" %in% colnames(km_summary_df)) {
      km_summary_df <- km_summary_df %>%
        dplyr::mutate(strata = factor(sub(".*=", "", as.character(strata))))
    } else {
      # Single-stratum fit: reconstruct from the fit object directly
      single_level <- sub(".*=", "", names(km_fit$strata)[1])
      km_summary_df <- km_summary_df %>%
        dplyr::mutate(strata = factor(single_level))
    }

    if (nrow(km_summary_df) == 0)
      return(NULL)

    # Anchor rows at time=0: surv=1 and n.risk = full cohort size per stratum.
    # time=0 rows are never suppressed — they define the curve's starting point.
    time0_anchor_rows <- km_summary_df %>%
      dplyr::distinct(strata) %>%
      dplyr::left_join(baseline_risk_counts %>% dplyr::mutate(strata = factor(sex)),
                       by = "strata") %>%
      dplyr::transmute(
        strata   = strata,
        time     = 0,
        n.risk   = count,
        n.event  = 0L,
        n.censor = 0L,
        surv     = 1,
        upper    = 1,
        lower    = 1
      )

    # Apply k=5 suppression to all post-baseline rows:
    # suppress any time point where n.risk < 5 OR where 0 < n.event < 5.
    km_summary_df <- dplyr::bind_rows(time0_anchor_rows, km_summary_df) %>%
      dplyr::arrange(strata, time) %>%
      dplyr::group_by(strata) %>%
      dplyr::mutate(
        is_suppressed = time > 0 &
          (n.risk < 5 | (n.event > 0 & n.event < 5)),
        surv    = ifelse(is_suppressed, NA_real_, surv),
        upper   = ifelse(is_suppressed, NA_real_, upper),
        lower   = ifelse(is_suppressed, NA_real_, lower),
        n.event = ifelse(is_suppressed, NA_integer_, n.event),
        n.risk  = ifelse(is_suppressed, NA_integer_, n.risk)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-is_suppressed)

    km_summary_df %>%
      tidyr::pivot_longer(
        cols      = c(surv, lower, upper, n.risk, n.event),
        names_to  = "estimate_name",
        values_to = "estimate_value"
      ) %>%
      dplyr::mutate(
        estimate_value = as.character(estimate_value),
        estimate_type  = dplyr::case_when(
          estimate_name %in% c("surv", "lower", "upper") ~ "numeric",
          estimate_name %in% c("n.risk", "n.event")      ~ "integer"
        ),
        strata_name = strata_label
      )
  }

  # ── 3-5. Fit KM models per CMA measure × ingredient ──────────────────────────
  km_results_list <- list()

  for (cma_name in unique(surv_data$name)) {
    for (ingredient in unique(surv_data$group)) {
      km_input <- surv_data %>%
        dplyr::filter(group == ingredient, name == cma_name)

      # Skip strata too small to fit a KM curve
      if (nrow(km_input) < 5)
        next

      km_fit <- tryCatch(
        survival::survfit(
          survival::Surv(followup_years, discontinuation) ~ sex,
          data = km_input
        ),
        error = function(e)
          NULL
      )

      if (is.null(km_fit))
        next

      # Baseline risk table: distinct people per sex at time=0
      baseline_risk_by_sex <- km_input %>%
        dplyr::distinct(person_id, sex) %>%
        dplyr::group_by(sex) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop")

      km_by_sex <- extract_km(
        km_fit              = km_fit,
        source_data         = km_input,
        strata_label        = "sex",
        baseline_risk_counts = baseline_risk_by_sex
      )

      if (is.null(km_by_sex))
        next

      km_results_list[[length(km_results_list) + 1]] <- km_by_sex %>%
        dplyr::mutate(
          result_id     = 3L,
          group_name    = "ingredient",
          group_level   = ingredient,
          variable_name = cma_name
        )
    }
  }

  all_km_results <- dplyr::bind_rows(km_results_list)

  # ── 3-6. Cast to the omop result schema ──────────────────────────────────────
  # `time` here is the KM time point (in years), stored in additional_level
  # so downstream visualisation knows which year each estimate belongs to.
  km_sr_tibble <- all_km_results %>%
    dplyr::transmute(
      result_id        = result_id,
      cdm_name         = dbName,
      group_name       = group_name,
      group_level      = group_level,
      strata_name      = strata_name,
      strata_level     = sub(".*=", "", as.character(strata)),
      variable_name    = variable_name,
      variable_level   = NA_character_,
      estimate_name    = estimate_name,
      estimate_type    = estimate_type,
      estimate_value   = estimate_value,
      additional_name  = "time_years",
      additional_level = as.character(time)   # KM time point, not follow-up years
    )

  # RESULT 4: Distribution of number of continuous adherence periods  #####
  adherence_breaks_first <- data %>%
    dplyr::collect() %>%
    dplyr::group_by(person_id, name, group, sex) %>%
    dplyr::arrange(person_id, name, group, window.start) %>%
    dplyr::mutate(
      is_reset        = window.ID == 1 & dplyr::row_number() > 1,
      definite_break_years = dplyr::if_else(dplyr::lead(is_reset),
        as.numeric(dplyr::lead(window.start) - window.end) %/% 365, NA_real_),
      indefinte_break_years = dplyr::if_else(max(window.start) == window.start, as.numeric(observation_period_end_date - max(window.end)) %/% 365, NA_real_),
      number_years_remaining = as.numeric(observation_period_end_date - window.end) %/% 365) %>%
    dplyr::ungroup() %>%
    dplyr::filter(definite_break_years > 0 | indefinte_break_years > 0) %>%
    dplyr::group_by(person_id, name, group, sex) %>% ##
    dplyr::arrange(person_id, name, group, window.start) %>% ##
    dplyr::slice(1) %>%
    dplyr::ungroup()

  summarise_adherence_breaks <- purrr::map_dfr(c(2, 3, 4), function(x) {
    adherence_breaks_first %>%
      # Prerequisite: at least X years of observation after break started
      dplyr::filter(number_years_remaining > x) %>%
      dplyr::group_by(name, group, sex) %>%
      dplyr::summarise(
        # Denominator: everyone with >= X years observation after break
        n_could_have      = dplyr::n(),
        # Numerator: restarted within X years
        # Late restarters (break > X) stay in denominator but not numerator
        n_restarted       = sum(
          !is.na(definite_break_years) & definite_break_years < x + 1,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        threshold_years     = x,
        pct_restarted       = dplyr::if_else(
          n_could_have > 0,
          n_restarted / n_could_have * 100,
          NA_real_
        ),
        # k=5 suppression on numerator
        pct_restarted = ifelse(n_restarted   < 5, NA_real_, pct_restarted),
        n_restarted   = ifelse(n_restarted   < 5, NA_real_, as.numeric(n_restarted)),
        n_could_have  = ifelse(n_could_have  < 5, NA_real_, as.numeric(n_could_have))
      )
  }) %>%
    tidyr::pivot_longer(
      cols      = c(n_could_have, n_restarted, pct_restarted),
      names_to  = "estimate_name",
      values_to = "estimate_value"
    ) %>%
    dplyr::mutate(
      strata_level     = sex,
      group_name       = "ingredient",
      group_level      = group,
      strata_name      = "sex",
      variable_name    = name,
      variable_level   = NA_character_,
      estimate_type    = dplyr::if_else(
        estimate_name == "pct_restarted", "numeric", "integer"
      ),
      result_id        = "4",
      cdm_name         = dbName,
      additional_name  = "threshold_years",
      additional_level = as.character(threshold_years)
    ) %>%
    dplyr::select(omopgenerics::resultColumns())

  # RESULT 5: People who never discontinued (always on first run  #####
  # A person is "always adherent" if they never had a window.ID reset to 1 after
  # their first window — i.e. they stayed on a single continuous run throughout.

  always_adherent <- data %>%
    dplyr::collect() %>%
    dplyr::arrange(person_id, name, group, window.start) %>%
    dplyr::group_by(person_id, name, group, sex) %>%
    dplyr::summarise(
      rows_in_this_group = n(),
      #var = as.integer(observation_period_end_date - min(window.start)) %/%365,
      never_discontinued = as.integer(observation_period_end_date - min(window.start)) %/% 365 == rows_in_this_group,
      .groups            = "drop"
    ) %>%
    dplyr::filter(never_discontinued) %>%
    dplyr::distinct(pick(person_id,name,group,sex), .keep_all = TRUE) %>%
    dplyr::group_by(name, group, sex) %>%
    dplyr::summarise(n_always_adherent = dplyr::n(), .groups           = "drop")

  total_per_group <- data %>%
    dplyr::collect() %>%
    dplyr::distinct(person_id, name, group, sex) %>%
    dplyr::group_by(name, group, sex) %>%
    dplyr::summarise(n_all_people = dplyr::n(), .groups = "drop")

  summarise_always_adherent <- always_adherent %>%
    dplyr::left_join(total_per_group,    by = c("name", "group", "sex")) %>%
    # k=5 suppression
    dplyr::mutate(n_always_adherent = ifelse(n_always_adherent < 5, NA_real_, n_always_adherent),
                  n_all_people = ifelse(n_all_people < 5, NA_real_, n_all_people),
                  pct_adherent = round((n_always_adherent / n_all_people)*100),2) %>%
    tidyr::pivot_longer(
      cols      = c(n_always_adherent,n_all_people,pct_adherent),
      names_to  = "estimate_name",
      values_to = "estimate_value"
    ) %>%
    dplyr::mutate(
      strata_level     = sex,
      group_name       = "ingredient",
      group_level      = group,
      strata_name      = "sex",
      variable_name    = name,
      variable_level   = NA_character_,
      estimate_type    = dplyr::if_else(estimate_name == "pct_adherent", "numeric", "integer"),
      result_id        = "5",
      cdm_name         = dbName,
      additional_name  = "overall",
      additional_level = "overall"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(omopgenerics::resultColumns())

  # Combine all result blocks and wrap in omop SummarisedResult  #####
  combined_summary <- rbind(
    mean_cma_overall,
    mean_cma_by_age_group,
    mean_cma_by_sex,
    mean_cma_by_age_group_sex,
    proportion_adherent_by_age_group,
    proportion_adherent_by_sex,
    proportion_adherent_by_age_group_sex,
    km_sr_tibble,
    summarise_adherence_breaks,
    summarise_always_adherent
  )

  summarised_result <- combined_summary %>%
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = c(1L, 2L, 3L, 4L, 5L),
        result_type = c(
          "summarise_medication_adherence",
          "summarise_medication_adherence_cma_over_0.8",
          "summarise_mean_continuous_adherence_window",
          "summarise_adherence_breaks",
          "summarise_always_adherent"
        ),
        package_name    = "AdherenceFromOMOP",
        package_version = "1.0"
      )
    )

  return(summarised_result)
}


# Bootstrap 95 % CI for the mean  #####
# Returns a named vector c(ci_lower_95, ci_upper_95).
# Falls back to NA if x has fewer than 2 non-missing values or if boot.ci fails.
bootstrap_ci <- function(x, R = 1000, conf = 0.95) {
  x <- x[!is.na(x)]

  if (length(x) < 2) {
    return(c(ci_lower_95 = NA_real_, ci_upper_95 = NA_real_))
  }

  boot_mean <- function(data, indices)
    mean(data[indices])

  boot_result <- boot::boot(x, boot_mean, R = R)

  ci <- tryCatch({
    bc <- boot::boot.ci(boot_result, conf = conf, type = "perc")
    if (is.null(bc$percent))
      c(NA_real_, NA_real_)
    else
      bc$percent[4:5]
  }, error = function(e) {
    c(NA_real_, NA_real_)
  })

  names(ci) <- c("ci_lower_95", "ci_upper_95")
  ci
}

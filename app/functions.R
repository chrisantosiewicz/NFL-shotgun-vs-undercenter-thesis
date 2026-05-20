# Load all models and data needed by functions
rf_shotgun_pass       <- readRDS("models/rf_shotgun_pass.rds")
rf_shotgun_run        <- readRDS("models/rf_shotgun_run.rds")
rf_uc_pass            <- readRDS("models/rf_uc_pass.rds")
rf_uc_run             <- readRDS("models/rf_uc_run.rds")
rf_yards_shotgun_pass <- readRDS("models/rf_yards_shotgun_pass.rds")
rf_yards_shotgun_run  <- readRDS("models/rf_yards_shotgun_run.rds")
rf_yards_uc_pass      <- readRDS("models/rf_yards_uc_pass.rds")
rf_yards_uc_run       <- readRDS("models/rf_yards_uc_run.rds")
residual_stats        <- readRDS("models/residual_stats.rds")
yards_residual_stats  <- readRDS("models/yards_residual_stats.rds")
reliability_lookup    <- readRDS("models/reliability_lookup.rds")

# ============================================
# STEP 4: SITUATION CLASSIFIER HELPER
# ============================================

classify_situation <- function(down, ydstogo, yardline_100,
                               score_differential,
                               half_seconds_remaining, qtr) {
  
  dist_cat <- case_when(
    ydstogo <= 3  ~ "Short",
    ydstogo <= 7  ~ "Medium",
    ydstogo <= 10 ~ "Long",
    TRUE          ~ "Very Long"
  )
  
  field_zone <- case_when(
    yardline_100 >= 95 ~ "Own Goal Line",
    yardline_100 >= 60 ~ "Own Territory",
    yardline_100 <= 5  ~ "Goal Line",
    yardline_100 <= 20 ~ "Red Zone",
    yardline_100 <= 40 ~ "Opponent Territory",
    TRUE               ~ "Midfield"
  )
  
  is_two_minute <- half_seconds_remaining <= 120
  is_critical   <- down >= 3
  
  return(list(
    dist_cat      = dist_cat,
    field_zone    = field_zone,
    is_two_minute = is_two_minute,
    is_critical   = is_critical
  ))
}

# ============================================
# STEP 5: FORMATION RELIABILITY CHECKER
# ============================================

get_formation_reliability <- function(down, dist_cat) {
  
  situation_data <- reliability_lookup %>%
    filter(down == !!down, dist_cat == !!dist_cat)
  
  if (nrow(situation_data) == 0) {
    warning(sprintf("No reliability data found for down=%d, dist=%s",
                    down, dist_cat))
    return(NULL)
  }
  
  list(
    high         = situation_data %>%
      filter(reliability_tier == "High") %>%
      pull(formation_play),
    moderate     = situation_data %>%
      filter(reliability_tier == "Moderate") %>%
      pull(formation_play),
    low          = situation_data %>%
      filter(reliability_tier == "Low") %>%
      pull(formation_play),
    insufficient = situation_data %>%
      filter(reliability_tier == "Insufficient") %>%
      pull(formation_play),
    is_ambiguous = first(situation_data$is_ambiguous),
    full_data    = situation_data
  )
}

# ============================================
# SIMULATE PLAY BASED ON YARDS GAINED
# ============================================

simulate_play_yards_based <- function(down, ydstogo, yardline_100, 
                                      score_differential, half_seconds_remaining, 
                                      qtr, qb_quality_score, formation_play, n_sims = 10000) {
  
  # Create input situation
  situation <- matrix(
    c(down, ydstogo, yardline_100, score_differential, 
      half_seconds_remaining, qtr, qb_quality_score),
    nrow = 1,
    ncol = 7
  )
  colnames(situation) <- c("down", "ydstogo", "yardline_100", 
                           "score_differential", "half_seconds_remaining", "qtr", "qb_quality_score")
  
  # Select appropriate models and residuals
  if (formation_play == "Shotgun Pass") {
    rf_yards_model <- rf_yards_shotgun_pass
    rf_epa_model <- rf_shotgun_pass
    residual_pool_yards <- yards_residual_stats$sg_pass$residuals
    residual_pool_epa <- residual_stats$sg_pass$residuals
  } else if (formation_play == "Shotgun Run") {
    rf_yards_model <- rf_yards_shotgun_run
    rf_epa_model <- rf_shotgun_run
    residual_pool_yards <- yards_residual_stats$sg_run$residuals
    residual_pool_epa <- residual_stats$sg_run$residuals
  } else if (formation_play == "Under Center Pass") {
    rf_yards_model <- rf_yards_uc_pass
    rf_epa_model <- rf_uc_pass
    residual_pool_yards <- yards_residual_stats$uc_pass$residuals
    residual_pool_epa <- residual_stats$uc_pass$residuals
  } else if (formation_play == "Under Center Run") {
    rf_yards_model <- rf_yards_uc_run
    rf_epa_model <- rf_uc_run
    residual_pool_yards <- yards_residual_stats$uc_run$residuals
    residual_pool_epa <- residual_stats$uc_run$residuals
  } else {
    stop("Invalid formation_play type")
  }
  
  # Predict expected yards and EPA
  pred_yards <- predict(rf_yards_model, situation)
  pred_epa <- predict(rf_epa_model, situation)
  
  # Simulate yards gained (add random noise from actual residuals)
  sampled_residuals_yards <- sample(residual_pool_yards, n_sims, replace = TRUE)
  simulated_yards <- pred_yards + sampled_residuals_yards
  
  # Calculate conversion probability from simulated yards
  prob_conversion <- mean(simulated_yards >= ydstogo)
  
  # Simulate EPA (using EPA residuals)
  sampled_residuals_epa <- sample(residual_pool_epa, n_sims, replace = TRUE)
  simulated_epa <- pred_epa + sampled_residuals_epa
  
  # Return comprehensive results
  return(list(
    formation_play = formation_play,
    
    # Yards metrics
    mean_yards = mean(simulated_yards),
    median_yards = median(simulated_yards),
    sd_yards = sd(simulated_yards),
    yards_q05 = quantile(simulated_yards, 0.05),
    yards_q95 = quantile(simulated_yards, 0.95),
    
    # Conversion metrics (derived from yards)
    prob_conversion = prob_conversion,
    yards_needed = ydstogo,
    
    # EPA metrics
    mean_epa = mean(simulated_epa),
    median_epa = median(simulated_epa),
    sd_epa = sd(simulated_epa),
    q05 = quantile(simulated_epa, 0.05),
    q10 = quantile(simulated_epa, 0.10),
    q25 = quantile(simulated_epa, 0.25),
    q75 = quantile(simulated_epa, 0.75),
    q90 = quantile(simulated_epa, 0.90),
    q95 = quantile(simulated_epa, 0.95),
    
    # Other EPA metrics
    prob_positive_epa = mean(simulated_epa > 0),
    prob_big_play = mean(simulated_epa > 1.5),
    prob_disaster = mean(simulated_epa < -1.5),
    
    # Raw simulations
    simulated_yards_values = simulated_yards,
    simulated_epa_values = simulated_epa
  ))
}


# ============================================
# STEP 6: SIMULATION RUNNER
# ============================================

run_all_simulations <- function(down, ydstogo, yardline_100,
                                score_differential,
                                half_seconds_remaining,
                                qtr, qb_quality_score,
                                n_sims = 1000) {
  
  formations <- c("Shotgun Pass", "Shotgun Run",
                  "Under Center Pass", "Under Center Run")
  
  results <- lapply(formations, function(fp) {
    simulate_play_yards_based(
      down, ydstogo, yardline_100, score_differential,
      half_seconds_remaining, qtr, qb_quality_score, fp, n_sims
    )
  })
  
  names(results) <- formations
  return(results)
}



# ============================================
# STEP 7: RANKER
# ============================================

rank_formations <- function(all_results, insufficient_formations) {
  
  scores <- sapply(all_results, function(x) x$mean_epa)
  
  # Separate reliable and insufficient scores
  reliable_scores     <- scores
  insufficient_scores <- scores
  
  # Zero out each group for separate ranking
  reliable_scores[names(reliable_scores) %in% insufficient_formations] <- NA
  insufficient_scores[!names(insufficient_scores) %in% insufficient_formations] <- NA
  
  # Rank reliable formations first by EPA
  reliable_ranked <- names(sort(reliable_scores, decreasing = TRUE, na.last = NA))
  
  # Rank insufficient formations by EPA too
  insufficient_ranked <- names(sort(insufficient_scores, decreasing = TRUE, na.last = NA))
  
  # Reliable formations first, then insufficient ranked by EPA
  ranked_plays <- c(reliable_ranked, insufficient_ranked)
  
  return(ranked_plays)
}


# ============================================
# STEP 8: SUMMARY TABLE BUILDER
# ============================================

build_summary_table <- function(all_results, ranked_plays,
                                reliability_info) {
  
  data.frame(
    Rank            = 1:4,
    Formation_Play  = ranked_plays,
    Mean_EPA        = sapply(all_results[ranked_plays],
                             function(x) round(x$mean_epa, 3)),
    CI_Lower        = sapply(all_results[ranked_plays],
                             function(x) round(x$q05, 3)),
    CI_Upper        = sapply(all_results[ranked_plays],
                             function(x) round(x$q95, 3)),
    Prob_Conversion = sapply(all_results[ranked_plays],
                             function(x) round(x$prob_conversion * 100, 1)),
    Prob_Boom = sapply(all_results[ranked_plays],
                       function(x) round(x$prob_big_play * 100, 1)),
    Prob_Bust = sapply(all_results[ranked_plays],
                       function(x) round(x$prob_disaster * 100, 1)),
    Reliability     = sapply(ranked_plays, function(fp) {
      tier <- reliability_info$full_data$reliability_tier[
        reliability_info$full_data$formation_play == fp]
      if (length(tier) == 0) "Unknown" else tier
    }),
    N_Plays         = sapply(ranked_plays, function(fp) {
      n <- reliability_info$full_data$n_plays[
        reliability_info$full_data$formation_play == fp]
      if (length(n) == 0) NA else n
    }),
    stringsAsFactors = FALSE
  )
}



# ============================================
# STEP 9: PRINTER
# ============================================

print_recommendation <- function(down, ydstogo, yardline_100,
                                 score_differential,
                                 half_seconds_remaining,
                                 qtr, qb_quality_score,
                                 situation_info, reliability_info,
                                 ranked_plays, summary_df,
                                 all_results) {
  
  best_play   <- ranked_plays[1]
  backup_play <- ranked_plays[2]
  down_label  <- c("1st", "2nd", "3rd", "4th")[down]
  
  cat("\n==============================================================\n")
  cat("                   PLAY RECOMMENDATION\n")
  cat("==============================================================\n")
  cat(sprintf("Situation:  %s & %d, Yard Line: %d\n",
              down_label, ydstogo, 100 - yardline_100))
  cat(sprintf("Score:      %+d | Time: %.1f min | Quarter: %d\n",
              score_differential, half_seconds_remaining / 60, qtr))
  cat(sprintf("QB Quality: %.2f | Zone: %s\n",
              qb_quality_score, situation_info$field_zone))
  
  if (situation_info$is_two_minute) {
    cat("🕐 TWO MINUTE DRILL\n")
  }
  
  cat("--------------------------------------------------------------\n")
  
  # Reliability warnings
  if (length(reliability_info$insufficient) > 0) {
    cat(sprintf("🚫 EXCLUDED — insufficient data: %s\n",
                paste(reliability_info$insufficient, collapse = ", ")))
  }
  if (length(reliability_info$low) > 0) {
    cat(sprintf("⚠  LOW CONFIDENCE: %s\n",
                paste(reliability_info$low, collapse = ", ")))
  }
  if (length(reliability_info$moderate) > 0) {
    cat(sprintf("📊 MODERATE CONFIDENCE: %s\n",
                paste(reliability_info$moderate, collapse = ", ")))
  }
  if (reliability_info$is_ambiguous) {
    cat("⚡ Top options are statistically similar\n")
  }
  
  cat("--------------------------------------------------------------\n")
  cat(sprintf("\nPRIMARY:  %s\n", best_play))
  cat(sprintf("  Reliability:  %s (n = %d plays)\n",
              summary_df$Reliability[1], summary_df$N_Plays[1]))
  cat(sprintf("  Expected EPA: %.3f (± %.3f)\n",
              all_results[[best_play]]$mean_epa,
              all_results[[best_play]]$sd_epa))
  cat(sprintf("  90%% CI:      [%.3f, %.3f]\n",
              all_results[[best_play]]$q05,
              all_results[[best_play]]$q95))
  cat(sprintf("  Conversion:   %.1f%%\n",
              all_results[[best_play]]$prob_conversion * 100))
  cat(sprintf("  Boom Probability:   %.1f%%\n",
              all_results[[best_play]]$prob_big_play * 100))
  cat(sprintf("  Bust Probability:   %.1f%%\n",
              all_results[[best_play]]$prob_disaster * 100))
  
  cat(sprintf("\nBACKUP:   %s\n", backup_play))
  cat(sprintf("  Reliability:  %s (n = %d plays)\n",
              summary_df$Reliability[2], summary_df$N_Plays[2]))
  cat(sprintf("  Expected EPA: %.3f (± %.3f)\n",
              all_results[[backup_play]]$mean_epa,
              all_results[[backup_play]]$sd_epa))
  cat(sprintf("  Conversion:   %.1f%%\n",
              all_results[[backup_play]]$prob_conversion * 100))
  cat(sprintf("  Boom Probability:   %.1f%%\n",
              all_results[[backup_play]]$prob_big_play * 100))
  cat(sprintf("  Bust Probability:   %.1f%%\n",
              all_results[[backup_play]]$prob_disaster * 100))
  
  cat("\n--- ALL OPTIONS ---\n")
  print(summary_df %>%
          select(Rank, Formation_Play, Mean_EPA,
                 Prob_Conversion, Reliability, N_Plays))
  
  cat("==============================================================\n\n")
}


##--------------------------------------
## PLOT FUNCTION
##-------------------------------------
plot_recommendation <- function(all_results, summary_df,
                                down, ydstogo, yardline_100) {
  
  library(ggplot2)
  library(gridExtra)
  
  down_label      <- c("1st", "2nd", "3rd", "4th")[down]
  situation_title <- sprintf("%s & %d, Yard Line %d",
                             down_label, ydstogo, 100 - yardline_100)
  dist_cat <- case_when(
    ydstogo <= 3  ~ "Short",
    ydstogo <= 7  ~ "Medium",
    ydstogo <= 10 ~ "Long",
    TRUE          ~ "Very Long"
  )
  
  formation_colors <- c(
    "Shotgun Pass"      = "#0072B2",
    "Shotgun Run"       = "#009E73",
    "Under Center Pass" = "#D55E00",
    "Under Center Run"  = "#CC79A7"
  )
  
  n_sims <- length(all_results[["Shotgun Pass"]]$simulated_epa_values)
  
  epa_vals <- c(
    all_results[["Shotgun Pass"]]$simulated_epa_values,
    all_results[["Shotgun Run"]]$simulated_epa_values,
    all_results[["Under Center Pass"]]$simulated_epa_values,
    all_results[["Under Center Run"]]$simulated_epa_values
  )
  
  fp_labels <- rep(
    c("Shotgun Pass", "Shotgun Run",
      "Under Center Pass", "Under Center Run"),
    each = n_sims
  )
  
  sim_data <- data.frame(
    epa_val        = epa_vals,
    Formation_Play = fp_labels,
    stringsAsFactors = FALSE
  )
  
  p1 <- ggplot(sim_data, aes(x = epa_val, fill = Formation_Play)) +
    geom_density(alpha = 0.5) +
    geom_vline(
      data     = summary_df,
      aes(xintercept = Mean_EPA, color = Formation_Play),
      linetype = "dashed", linewidth = 1
    ) +
    scale_fill_manual(values  = formation_colors) +
    scale_color_manual(values = formation_colors) +
    labs(
      title    = "Simulated EPA Distributions",
      subtitle = situation_title,
      x        = "Expected Points Added (EPA)",
      y        = "Density"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p2 <- ggplot(summary_df,
               aes(x     = reorder(Formation_Play, Mean_EPA),
                   y     = Mean_EPA,
                   color = Formation_Play)) +
    geom_point(size = 4) +
    geom_errorbar(
      aes(ymin = CI_Lower, ymax = CI_Upper),
      width = 0.2, linewidth = 1.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    scale_color_manual(values = formation_colors) +
    labs(
      title = "Mean EPA with 90% CI",
      x     = "",
      y     = "Expected Points Added"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  p3 <- ggplot(summary_df,
               aes(x    = reorder(Formation_Play, Prob_Conversion),
                   y    = Prob_Conversion,
                   fill = Formation_Play)) +
    geom_col() +
    geom_text(
      aes(label = sprintf("%.1f%%", Prob_Conversion)),
      hjust = -0.2, size = 3.5
    ) +
    coord_flip() +
    scale_fill_manual(values = formation_colors) +
    ylim(0, 100) +
    labs(
      title = "Conversion Probability",
      x     = "",
      y     = "Probability (%)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  bucket_label <- sprintf("All %s & %s plays 2016-2023",
                          down_label, dist_cat)
  
  p4 <- ggplot(summary_df,
               aes(x    = reorder(Formation_Play, Mean_EPA),
                   y    = N_Plays,
                   fill = Reliability)) +
    geom_col() +
    geom_text(
      aes(label = scales::comma(N_Plays)),
      hjust = -0.2, size = 3.5
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "High"         = "#2ecc71",
        "Moderate"     = "#f39c12",
        "Low"          = "#e74c3c",
        "Insufficient" = "#95a5a6",
        "Unknown"      = "#bdc3c7"
      )
    ) +
    labs(
      title = "Training Sample Size",
      subtitle = bucket_label,
      caption  = "Sample size reflects situational bucket, not exact scenario",
      x     = "",
      y     = "Number of Training Plays"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  return(list(
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4
  ))
}


# ============================================
# STEP 10: MASTER RECOMMEND FUNCTION
# ============================================

recommend_play <- function(down, ydstogo, yardline_100,
                           score_differential,
                           half_seconds_remaining,
                           qtr, qb_quality_score,
                           n_sims = 1000,
                           show_plot = TRUE) {
  
  # 1 — Classify situation
  situation_info <- classify_situation(
    down, ydstogo, yardline_100,
    score_differential, half_seconds_remaining, qtr
  )
  
  # 2 — Get reliability info
  reliability_info <- get_formation_reliability(
    down, situation_info$dist_cat
  )
  
  # 3 — Run simulations
  all_results <- run_all_simulations(
    down, ydstogo, yardline_100, score_differential,
    half_seconds_remaining, qtr, qb_quality_score, n_sims
  )
  
  # 4 — Rank formations
  ranked_plays <- rank_formations(
    all_results, reliability_info$insufficient
  )
  
  # 5 — Build summary table
  summary_df <- build_summary_table(
    all_results, ranked_plays, reliability_info
  )
  
  # 6 — Print recommendation
  print_recommendation(
    down, ydstogo, yardline_100, score_differential,
    half_seconds_remaining, qtr, qb_quality_score,
    situation_info, reliability_info,
    ranked_plays, summary_df, all_results
  )
  
  # 7 — Plot if requested
  if (show_plot) {
    plot_recommendation(all_results, summary_df, down,
                        ydstogo, yardline_100)
  }
  
  # 8 — Return results
  return(invisible(list(
    primary             = ranked_plays[1],
    backup              = ranked_plays[2],
    ranked_plays        = ranked_plays,
    situation           = situation_info,
    reliability         = reliability_info,
    summary             = summary_df,
    full_results        = all_results
  )))
}
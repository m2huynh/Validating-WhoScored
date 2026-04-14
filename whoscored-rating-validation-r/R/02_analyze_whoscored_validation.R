# Author: Minh Huynh

source(file.path("R", "01_helpers.R"))

study1 <- read.csv(STUDY1_FILE, check.names = FALSE)
if ("" %in% names(study1)) {
  names(study1)[names(study1) == ""] <- "row_id"
}
study1$ws_rating <- to_num(study1$ws_rating)
study1$Min <- to_num(study1$Min)

study1$broad_position <- mapply(
  broad_pos_from_study1,
  study1$ws_position,
  study1$First_pos_fbref,
  study1$fotmob_position_short
)

non_numeric_cols <- c(
  "row_id", "MatchURL", "League", "Match_Date", "Matchweek", "Home_Team", "Home_Formation",
  "Away_Team", "Away_Formation", "Away_Goals", "Game_URL", "Team", "Home_Away",
  "Player", "Player_Href", "Nation", "First_pos_fbref", "Other_pos_fbref",
  "Competition_Name", "Gender", "Country", "Tier", "ws_position",
  "fotmob_position_short", "fotmob_position", "sofa_position", "broad_position"
)

for (col_name in setdiff(names(study1), non_numeric_cols)) {
  study1[[col_name]] <- to_num(study1[[col_name]])
}

s1 <- subset(study1, !is.na(ws_rating) & Min >= 30)
s1$team_goals <- ifelse(s1$Home_Away == "Home", s1$Home_Score, s1$Away_Score)
s1$opp_goals <- ifelse(s1$Home_Away == "Home", s1$Away_Score, s1$Home_Score)
s1$result <- ifelse(s1$team_goals > s1$opp_goals, "win", ifelse(s1$team_goals < s1$opp_goals, "loss", "draw"))

attack <- c("Gls","Ast","PK","Sh","SoT","xG_Expected","npxG_Expected","xAG_Expected","SCA_SCA","GCA_SCA","xAG","xA","KP","PPA","CrsPA","Att.Pen_Touches","CPA_Carries","PKwon","Off")
passing <- c("Cmp_Passes","Att_Passes","Cmp_percent_Passes","PrgP_Passes","Cmp_Total","Att_Total","Cmp_percent_Total","Cmp_Short","Att_Short","Cmp_percent_Short","Cmp_Medium","Att_Medium","Cmp_percent_Medium","Cmp_Long","Att_Long","Cmp_percent_Long","PrgP","Final_Third","TB_Pass_Types","Sw_Pass_Types","Crs_Pass_Types","CK_Pass_Types","Live_Pass_Types","Dead_Pass_Types")
defense <- c("Tkl","Int","Blocks","Tkl_Tackles","TklW_Tackles","Def.3rd_Tackles","Mid.3rd_Tackles","Att.3rd_Tackles","Tkl_Challenges","Tkl_percent_Challenges","Blocks_Blocks","Sh_Blocks","Pass_Blocks","Tkl.Int","Clr","Recov","Won_Aerial_Duels","Won_percent_Aerial_Duels")
progression <- c("Touches","Touches_Touches","Carries_Carries","PrgC_Carries","TotDist_Carries","PrgDist_Carries","Final_Third_Carries","Rec_Receiving","PrgR_Receiving","Att_Take_Ons","Succ_Take_Ons","Succ_percent_Take_Ons","Mid.3rd_Touches","Att.3rd_Touches","Live_Touches")
negative_events <- c("CrdY","CrdR","X2CrdY","Fls","Err","PKcon","OG","Mis_Carries","Dis_Carries","Lost_Aerial_Duels","Lost_Challenges")

groups <- list(
  attack = intersect(attack, names(s1)),
  passing = intersect(passing, names(s1)),
  defense = intersect(defense, names(s1)),
  progression = intersect(progression, names(s1)),
  negative_events = intersect(negative_events, names(s1))
)
all_features <- sort(unique(unlist(groups, use.names = FALSE)))

corr_features <- unique(c(all_features, "Min", "team_goals", "opp_goals"))
corr_rows <- do.call(rbind, lapply(corr_features, function(v) {
  if (!v %in% names(s1)) return(NULL)
  data.frame(
    variable = v,
    spearman_r = spearman_corr(s1[[v]], s1$ws_rating),
    mean = safe_mean(s1[[v]]),
    sd = stats::sd(s1[[v]], na.rm = TRUE),
    n_non_missing = sum(!is.na(s1[[v]]))
  )
}))
cor_df <- corr_rows[order(corr_rows$spearman_r, decreasing = TRUE), ]
write.csv(cor_df, file.path(TABLE_DIR, "study1_ws_rating_correlations.csv"), row.names = FALSE)

full_r2 <- ridge_cv_r2(s1, "ws_rating", all_features)
model_rows <- data.frame(model = character(), r2_cv = numeric(), features = integer())
model_rows <- rbind(model_rows, data.frame(model = "full_model_all_groups", r2_cv = full_r2, features = length(all_features)))
for (group_name in names(groups)) {
  feats <- groups[[group_name]]
  model_rows <- rbind(model_rows, data.frame(model = paste0(group_name, "_only"), r2_cv = ridge_cv_r2(s1, "ws_rating", feats), features = length(feats)))
  reduced <- setdiff(all_features, feats)
  model_rows <- rbind(model_rows, data.frame(model = paste0("full_minus_", group_name), r2_cv = ridge_cv_r2(s1, "ws_rating", reduced), features = length(reduced)))
}
model_rows$drop_from_full <- full_r2 - model_rows$r2_cv
write.csv(model_rows, file.path(TABLE_DIR, "study1_group_model_r2.csv"), row.names = FALSE)

pos_summary <- aggregate(ws_rating ~ broad_position, data = s1, FUN = function(x) c(n = length(x), mean = mean(x), sd = stats::sd(x)))
pos_df <- data.frame(
  broad_position = pos_summary$broad_position,
  n = pos_summary$ws_rating[, "n"],
  mean_rating = pos_summary$ws_rating[, "mean"],
  sd_rating = pos_summary$ws_rating[, "sd"]
)
mean_minutes <- aggregate(Min ~ broad_position, data = s1, FUN = mean)
pos_df <- merge(pos_df, mean_minutes, by = "broad_position", all.x = TRUE)
names(pos_df)[names(pos_df) == "Min"] <- "mean_minutes"

study1_mean <- mean(s1$ws_rating, na.rm = TRUE)
study1_sd <- stats::sd(s1$ws_rating, na.rm = TRUE)
study1_threshold <- study1_mean + 2 * study1_sd
s1$exceptional_2sd <- s1$ws_rating >= study1_threshold
overall_rate <- aggregate(exceptional_2sd ~ broad_position, data = s1, FUN = mean)
names(overall_rate)[2] <- "share_exceptional_overall_2sd"
pos_df <- merge(pos_df, overall_rate, by = "broad_position", all.x = TRUE)

s1$exceptional_2sd_within_position <- FALSE
for (pos in unique(s1$broad_position)) {
  idx <- which(s1$broad_position == pos)
  thr <- mean(s1$ws_rating[idx], na.rm = TRUE) + 2 * stats::sd(s1$ws_rating[idx], na.rm = TRUE)
  s1$exceptional_2sd_within_position[idx] <- s1$ws_rating[idx] >= thr
}
within_rate <- aggregate(exceptional_2sd_within_position ~ broad_position, data = s1, FUN = mean)
names(within_rate)[2] <- "share_exceptional_within_position_2sd"
pos_df <- merge(pos_df, within_rate, by = "broad_position", all.x = TRUE)
write.csv(pos_df, file.path(TABLE_DIR, "study1_rating_by_position.csv"), row.names = FALSE)

selected_metrics <- intersect(c("Gls","Ast","Sh","SoT","xG_Expected","xA","KP","Cmp_Passes","PrgP","TklW_Tackles","Int","Clr","Recov","Won_Aerial_Duels"), names(s1))
for (metric in selected_metrics) {
  s1[[paste0("pctl_", metric)]] <- percentile_within_group(s1[[metric]], s1$broad_position)
}
pctl_cols <- paste0("pctl_", selected_metrics)
s1$elite_stat_count_90 <- rowSums(s1[, pctl_cols, drop = FALSE] >= 0.90, na.rm = TRUE)
s1$elite_stat_count_95 <- rowSums(s1[, pctl_cols, drop = FALSE] >= 0.95, na.rm = TRUE)

study1_compare_metrics <- c("ws_rating", "Min", "elite_stat_count_90", "elite_stat_count_95", selected_metrics)
exc_compare_rows <- do.call(rbind, lapply(study1_compare_metrics, function(metric) {
  exc <- s1[s1$exceptional_2sd, metric]
  other <- s1[!s1$exceptional_2sd, metric]
  data.frame(
    metric = metric,
    exceptional_mean = safe_mean(exc),
    non_exceptional_mean = safe_mean(other),
    exceptional_median = stats::median(exc, na.rm = TRUE),
    non_exceptional_median = stats::median(other, na.rm = TRUE)
  )
}))
write.csv(exc_compare_rows, file.path(TABLE_DIR, "study1_exceptional_vs_others.csv"), row.names = FALSE)

exc_example_cols <- intersect(c("Player","Team","Match_Date","Home_Team","Away_Team","broad_position","Min","ws_rating", selected_metrics), names(s1))
exc_examples <- s1[s1$exceptional_2sd, exc_example_cols, drop = FALSE]
exc_examples <- exc_examples[order(exc_examples$ws_rating, decreasing = TRUE), ]
write.csv(exc_examples, file.path(TABLE_DIR, "study1_exceptional_examples.csv"), row.names = FALSE)

result_summary <- aggregate(ws_rating ~ result, data = s1, FUN = function(x) c(mean = mean(x), median = stats::median(x), count = length(x)))
result_df <- data.frame(
  result = result_summary$result,
  mean = result_summary$ws_rating[, "mean"],
  median = result_summary$ws_rating[, "median"],
  count = result_summary$ws_rating[, "count"]
)
write.csv(result_df, file.path(TABLE_DIR, "study1_rating_by_result.csv"), row.names = FALSE)

t_list <- lapply(TOURNAMENT_FILES, function(fp) {
  df <- read.csv(fp, check.names = FALSE)
  df$source_file <- basename(fp)
  df
})
t <- do.call(rbind, t_list)

for (col_name in intersect(c("Shots","ShotsOT","KeyPasses","PassAccruacy","PassAccuracy","AerialsWon","Touches","Rating","home_goals","away_goals","went_to_extra","went_to_penalties","shirt_number","row_in_match","gk_count"), names(t))) {
  t[[col_name]] <- to_num(t[[col_name]])
}
if (!"PassAccuracy" %in% names(t) && "PassAccruacy" %in% names(t)) {
  t$PassAccuracy <- t$PassAccruacy
} else if ("PassAccruacy" %in% names(t)) {
  t$PassAccuracy <- ifelse(is.na(t$PassAccuracy), t$PassAccruacy, t$PassAccuracy)
}

t$broad_position <- vapply(t$player_position, broad_pos_from_tournament, character(1))
t$team_result <- ifelse(
  (t$team_side == "home" & t$home_goals > t$away_goals) | (t$team_side == "away" & t$away_goals > t$home_goals),
  "win",
  ifelse(
    (t$team_side == "home" & t$home_goals < t$away_goals) | (t$team_side == "away" & t$away_goals < t$home_goals),
    "loss",
    "draw"
  )
)

t <- t[!is.na(t$Rating), ]
pooled_mean <- mean(t$Rating, na.rm = TRUE)
pooled_sd <- stats::sd(t$Rating, na.rm = TRUE)
pooled_threshold <- pooled_mean + 2 * pooled_sd
t$exceptional_2sd_pooled <- t$Rating >= pooled_threshold

t$exceptional_2sd_by_tournament <- FALSE
for (source in unique(t$source_file)) {
  idx <- which(t$source_file == source)
  thr <- mean(t$Rating[idx], na.rm = TRUE) + 2 * stats::sd(t$Rating[idx], na.rm = TRUE)
  t$exceptional_2sd_by_tournament[idx] <- t$Rating[idx] >= thr
}

t$exceptional_2sd_by_tournament_position <- FALSE
combo_keys <- unique(paste(t$source_file, t$broad_position, sep = "||"))
for (key in combo_keys) {
  parts <- strsplit(key, "\\|\\|")[[1]]
  idx <- which(t$source_file == parts[1] & t$broad_position == parts[2])
  if (length(idx) >= 20) {
    thr <- mean(t$Rating[idx], na.rm = TRUE) + 2 * stats::sd(t$Rating[idx], na.rm = TRUE)
    t$exceptional_2sd_by_tournament_position[idx] <- t$Rating[idx] >= thr
  }
}

turn_summary <- do.call(rbind, lapply(split(t, t$source_file), function(sub) {
  data.frame(
    source_file = unique(sub$source_file),
    n = nrow(sub),
    mean_rating = mean(sub$Rating, na.rm = TRUE),
    sd_rating = stats::sd(sub$Rating, na.rm = TRUE),
    threshold_2sd = mean(sub$Rating, na.rm = TRUE) + 2 * stats::sd(sub$Rating, na.rm = TRUE),
    n_exceptional_tournament_2sd = sum(sub$exceptional_2sd_by_tournament, na.rm = TRUE),
    share_exceptional_tournament_2sd = mean(sub$exceptional_2sd_by_tournament, na.rm = TRUE)
  )
}))
write.csv(turn_summary, file.path(TABLE_DIR, "study3_tournament_threshold_summary.csv"), row.names = FALSE)

turn_pos <- aggregate(Rating ~ source_file + broad_position, data = t, FUN = function(x) c(n = length(x), mean = mean(x), sd = stats::sd(x)))
turn_pos_df <- data.frame(
  source_file = turn_pos$source_file,
  broad_position = turn_pos$broad_position,
  n = turn_pos$Rating[, "n"],
  mean_rating = turn_pos$Rating[, "mean"],
  sd_rating = turn_pos$Rating[, "sd"]
)
turn_pos_df <- merge(turn_pos_df, aggregate(exceptional_2sd_pooled ~ source_file + broad_position, data = t, FUN = mean), by = c("source_file", "broad_position"), all.x = TRUE)
turn_pos_df <- merge(turn_pos_df, aggregate(exceptional_2sd_by_tournament ~ source_file + broad_position, data = t, FUN = mean), by = c("source_file", "broad_position"), all.x = TRUE)
turn_pos_df <- merge(turn_pos_df, aggregate(exceptional_2sd_by_tournament_position ~ source_file + broad_position, data = t, FUN = mean), by = c("source_file", "broad_position"), all.x = TRUE)
names(turn_pos_df)[names(turn_pos_df) == "exceptional_2sd_pooled"] <- "share_exceptional_pooled"
names(turn_pos_df)[names(turn_pos_df) == "exceptional_2sd_by_tournament"] <- "share_exceptional_by_tournament"
names(turn_pos_df)[names(turn_pos_df) == "exceptional_2sd_by_tournament_position"] <- "share_exceptional_by_tournament_position"
write.csv(turn_pos_df, file.path(TABLE_DIR, "study3_rating_by_tournament_position.csv"), row.names = FALSE)

obs_metrics <- intersect(c("Shots","ShotsOT","KeyPasses","PassAccuracy","AerialsWon","Touches"), names(t))
for (metric in obs_metrics) {
  t[[paste0("pctl_", metric)]] <- percentile_within_group(t[[metric]], t$broad_position)
}
obs_pctl_cols <- paste0("pctl_", obs_metrics)
t$elite_obs_count_90 <- rowSums(t[, obs_pctl_cols, drop = FALSE] >= 0.90, na.rm = TRUE)
t$elite_obs_count_95 <- rowSums(t[, obs_pctl_cols, drop = FALSE] >= 0.95, na.rm = TRUE)

mask_exc_tourn <- t$exceptional_2sd_by_tournament
study3_compare_metrics <- c("Rating", "elite_obs_count_90", "elite_obs_count_95", obs_metrics)
study3_compare_rows <- do.call(rbind, lapply(study3_compare_metrics, function(metric) {
  exc <- t[mask_exc_tourn, metric]
  other <- t[!mask_exc_tourn, metric]
  data.frame(
    metric = metric,
    exceptional_mean = safe_mean(exc),
    non_exceptional_mean = safe_mean(other),
    exceptional_median = stats::median(exc, na.rm = TRUE),
    non_exceptional_median = stats::median(other, na.rm = TRUE)
  )
}))
write.csv(study3_compare_rows, file.path(TABLE_DIR, "study3_exceptional_vs_others.csv"), row.names = FALSE)

study3_example_cols <- intersect(c("source_file","date","Stage","team","Player","player_position","Rating", obs_metrics, "team_result","went_to_extra","went_to_penalties"), names(t))
study3_examples <- t[mask_exc_tourn, study3_example_cols, drop = FALSE]
study3_examples <- study3_examples[order(study3_examples$source_file, -study3_examples$Rating), ]
write.csv(study3_examples, file.path(TABLE_DIR, "study3_exceptional_examples.csv"), row.names = FALSE)

png(file.path(FIGURE_DIR, "study1_ws_rating_distribution.png"), width = 1440, height = 900, res = 180)
hist(s1$ws_rating, breaks = 40, col = "#4C78A8", border = "white", main = "Study 1: WhoScored rating distribution", xlab = "WhoScored rating", ylab = "Player-match count")
abline(v = study1_threshold, col = "#DC143C", lty = 2, lwd = 2)
legend("topright", legend = sprintf("2 SD threshold = %.2f", study1_threshold), col = "#DC143C", lty = 2, lwd = 2, bty = "n")
dev.off()

pos_order <- c("GK", "DEF", "MID", "FWD", "UNK")
pos_plot <- pos_df[match(pos_order, pos_df$broad_position), ]
pos_plot <- pos_plot[!is.na(pos_plot$broad_position), ]
png(file.path(FIGURE_DIR, "study1_mean_rating_by_position.png"), width = 1440, height = 900, res = 180)
barplot(height = pos_plot$mean_rating, names.arg = pos_plot$broad_position, col = "#72B7B2", main = "Study 1: Mean WhoScored rating by broad position", xlab = "Broad position", ylab = "Mean rating")
dev.off()

model_plot <- subset(model_rows, grepl("_only$", model) | model == "full_model_all_groups")
model_plot$label <- ifelse(model_plot$model == "full_model_all_groups", "full model", gsub("_only$", "", model_plot$model))
plot_cols <- ifelse(model_plot$label == "full model", "#4C78A8", "#F58518")
png(file.path(FIGURE_DIR, "study1_group_model_r2.png"), width = 1440, height = 900, res = 180)
barplot(height = model_plot$r2_cv, names.arg = model_plot$label, col = plot_cols, las = 2, main = "Study 1: How much objective-stat information predicts the rating", ylab = "Cross-validated R-squared")
dev.off()

png(file.path(FIGURE_DIR, "study1_exceptional_elite_stat_counts.png"), width = 1440, height = 900, res = 180)
barplot(
  height = c(mean(s1$elite_stat_count_90[s1$exceptional_2sd], na.rm = TRUE), mean(s1$elite_stat_count_90[!s1$exceptional_2sd], na.rm = TRUE)),
  names.arg = c("2 SD exceptional", "Other"),
  col = c("#E45756", "#4C78A8"),
  main = "Study 1: Exceptional ratings have more elite stat percentiles",
  ylab = "Mean count of >=90th percentile selected stats"
)
dev.off()

png(file.path(FIGURE_DIR, "study3_exceptional_elite_obs_counts.png"), width = 1440, height = 900, res = 180)
barplot(
  height = c(mean(t$elite_obs_count_90[mask_exc_tourn], na.rm = TRUE), mean(t$elite_obs_count_90[!mask_exc_tourn], na.rm = TRUE)),
  names.arg = c("2 SD exceptional", "Other"),
  col = c("#E45756", "#4C78A8"),
  main = "Study 3: Exceptional ratings have more elite observable stats",
  ylab = "Mean count of >=90th percentile observable stats"
)
dev.off()

summary_list <- list(
  study1 = list(
    n_player_matches_main = nrow(s1),
    mean_ws_rating = study1_mean,
    sd_ws_rating = study1_sd,
    threshold_2sd = study1_threshold,
    n_exceptional_2sd = sum(s1$exceptional_2sd, na.rm = TRUE),
    share_exceptional_2sd = mean(s1$exceptional_2sd, na.rm = TRUE),
    top_correlations = utils::head(cor_df, 12),
    group_model_r2 = model_rows
  ),
  study3 = list(
    n_player_matches = nrow(t),
    mean_rating_pooled = pooled_mean,
    sd_rating_pooled = pooled_sd,
    threshold_2sd_pooled = pooled_threshold,
    n_exceptional_2sd_pooled = sum(t$exceptional_2sd_pooled, na.rm = TRUE),
    share_exceptional_2sd_pooled = mean(t$exceptional_2sd_pooled, na.rm = TRUE),
    n_exceptional_2sd_by_tournament = sum(t$exceptional_2sd_by_tournament, na.rm = TRUE),
    share_exceptional_2sd_by_tournament = mean(t$exceptional_2sd_by_tournament, na.rm = TRUE),
    observable_metrics_used = obs_metrics
  )
)
write_json_pretty(summary_list, file.path(TABLE_DIR, "whoscored_validation_summary.json"))

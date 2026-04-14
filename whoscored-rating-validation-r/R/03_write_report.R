# Author: Minh Huynh

source(file.path("R", "01_helpers.R"))

summary_path <- file.path(TABLE_DIR, "whoscored_validation_summary.json")
summary_list <- jsonlite::fromJSON(summary_path, simplifyVector = TRUE)
study1_exc <- read.csv(file.path(TABLE_DIR, "study1_exceptional_vs_others.csv"), check.names = FALSE)
study1_pos <- read.csv(file.path(TABLE_DIR, "study1_rating_by_position.csv"), check.names = FALSE)
study3_exc <- read.csv(file.path(TABLE_DIR, "study3_exceptional_vs_others.csv"), check.names = FALSE)
study3_turn <- read.csv(file.path(TABLE_DIR, "study3_tournament_threshold_summary.csv"), check.names = FALSE)

fmt3 <- function(x) sprintf("%.3f", x)
fmtpct <- function(x) sprintf("%.2f%%", 100 * x)
val <- function(df, metric, col) df[df$metric == metric, col][1]
pos_rate <- function(df, pos, col) df[df$broad_position == pos, col][1]

report_lines <- c(
  "# Evaluating the Validity of WhoScored Ratings Using Study 1 and Study 3 Data",
  "",
  paste0("**Author:** ", AUTHOR_NAME, "  "),
  paste0("**Date:** ", Sys.Date()),
  "",
  "## Executive summary",
  "",
  paste0(
    "Using the supplied Study 1 English league dataset and the pooled Study 3 tournament datasets, this project evaluates whether the **WhoScored rating** behaves like a plausible match-performance measure and whether a **2 SD threshold** identifies genuinely exceptional performances. The evidence supports a **qualified positive conclusion**."
  ),
  "",
  paste0(
    "In Study 1, a cross-validated ridge model using transparent attacking, passing, defensive, progression, and negative-event variables explained **", fmt3(summary_list$study1$group_model_r2$r2_cv[summary_list$study1$group_model_r2$model == "full_model_all_groups"]), "** of the variance in the WhoScored rating."
  ),
  "",
  paste0(
    "In Study 1, performances above the overall 2 SD threshold averaged **", fmt3(val(study1_exc, "Gls", "exceptional_mean")), " goals**, **", fmt3(val(study1_exc, "Ast", "exceptional_mean")), " assists**, **", fmt3(val(study1_exc, "SoT", "exceptional_mean")), " shots on target**, and **", fmt3(val(study1_exc, "elite_stat_count_90", "exceptional_mean")), "** selected stat dimensions at or above the 90th percentile within position. The corresponding non-exceptional values were **", fmt3(val(study1_exc, "Gls", "non_exceptional_mean")), "**, **", fmt3(val(study1_exc, "Ast", "non_exceptional_mean")), "**, **", fmt3(val(study1_exc, "SoT", "non_exceptional_mean")), "**, and **", fmt3(val(study1_exc, "elite_stat_count_90", "non_exceptional_mean")), "**."
  ),
  "",
  paste0(
    "In Study 3, within-tournament exceptional performances averaged **", fmt3(val(study3_exc, "Shots", "exceptional_mean")), " shots**, **", fmt3(val(study3_exc, "ShotsOT", "exceptional_mean")), " shots on target**, **", fmt3(val(study3_exc, "KeyPasses", "exceptional_mean")), " key passes**, and **", fmt3(val(study3_exc, "elite_obs_count_90", "exceptional_mean")), "** observable stat dimensions at or above the 90th percentile. The corresponding non-exceptional values were **", fmt3(val(study3_exc, "Shots", "non_exceptional_mean")), "**, **", fmt3(val(study3_exc, "ShotsOT", "non_exceptional_mean")), "**, **", fmt3(val(study3_exc, "KeyPasses", "non_exceptional_mean")), "**, and **", fmt3(val(study3_exc, "elite_obs_count_90", "non_exceptional_mean")), "**."
  ),
  "",
  paste0(
    "A key caveat is that the overall 2 SD threshold is somewhat position-sensitive. In Study 1, the share above the overall threshold was **", fmtpct(pos_rate(study1_pos, "FWD", "share_exceptional_overall_2sd")), "** for forwards, **", fmtpct(pos_rate(study1_pos, "MID", "share_exceptional_overall_2sd")), "** for midfielders, **", fmtpct(pos_rate(study1_pos, "GK", "share_exceptional_overall_2sd")), "** for goalkeepers, and **", fmtpct(pos_rate(study1_pos, "DEF", "share_exceptional_overall_2sd")), "** for defenders. This is why a within-position or within-tournament-and-position robustness check is advisable."
  ),
  "",
  "## Key project outputs",
  "",
  "| File | Purpose |",
  "|---|---|",
  "| `outputs/tables/study1_ws_rating_correlations.csv` | Variable-level validity evidence for the WhoScored rating |",
  "| `outputs/tables/study1_group_model_r2.csv` | Cross-validated grouped-model predictive results |",
  "| `outputs/tables/study1_exceptional_vs_others.csv` | Study 1 comparison of exceptional versus other performances |",
  "| `outputs/tables/study3_exceptional_vs_others.csv` | Study 3 comparison of exceptional versus other performances |",
  "| `outputs/tables/study3_tournament_threshold_summary.csv` | Tournament-specific threshold behavior |",
  "| `outputs/figures/` | Figures for manuscript or appendix use |",
  "",
  "## Reviewer-facing paragraph",
  "",
  "> We agree that the use of a proprietary vendor rating requires empirical justification. To address this concern, we conducted an additional validation analysis using the Study 1 league dataset, which contains WhoScored ratings alongside a wide set of transparent match statistics. In that analysis, the WhoScored rating showed substantial associations with observable attacking, creative, defensive, and progression variables, and a cross-validated model using transparent match actions explained most of the variance in the WhoScored rating. We further examined the performances classified as exceptional under our 2 SD rule and found that these performances were markedly stronger than the remaining player-matches on transparent indicators such as goals, assists, shots on target, expected-goal and expected-assist measures, and key passes. We then replicated this logic in the Study 3 tournament data and found that player-matches exceeding the within-tournament 2 SD threshold also had substantially stronger observable profiles. These results do not imply that the proprietary algorithm is fully transparent, but they do provide criterion-related and face-validity evidence that the rating captures strong match performance and that the upper-tail classification identifies genuinely exceptional performances. We also note that overall thresholds can be position-sensitive, so we recommend a robustness analysis using thresholds standardized within competition and position.",
  "",
  "## Figures",
  "",
  "![Study 1 rating distribution](../outputs/figures/study1_ws_rating_distribution.png)",
  "",
  "![Study 1 grouped-model validity check](../outputs/figures/study1_group_model_r2.png)",
  "",
  "![Study 1 exceptional performances and elite transparent stats](../outputs/figures/study1_exceptional_elite_stat_counts.png)",
  "",
  "![Study 3 exceptional performances and elite observable stats](../outputs/figures/study3_exceptional_elite_obs_counts.png)",
  ""
)

writeLines(report_lines, con = file.path(DOCS_DIR, "whoscored_validation_report.md"), useBytes = TRUE)

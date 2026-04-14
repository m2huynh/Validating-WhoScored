# Author: Minh Huynh

source(file.path("R", "00_config.R"))

to_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

spearman_corr <- function(x, y) {
  keep <- stats::complete.cases(x, y)
  if (sum(keep) < 30) return(NA_real_)
  suppressWarnings(stats::cor(x[keep], y[keep], method = "spearman"))
}

r2_score <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  if (is.na(ss_tot) || ss_tot <= 0) return(NA_real_)
  1 - ss_res / ss_tot
}

ridge_cv_r2 <- function(df, target, features, repeats = 15, alpha = 5, train_frac = 0.8) {
  features <- intersect(features, names(df))
  if (length(features) == 0) return(NA_real_)
  use <- df[, c(target, features), drop = FALSE]
  use <- use[!is.na(use[[target]]), , drop = FALSE]
  if (nrow(use) < 200) return(NA_real_)

  for (feature in features) {
    if (all(is.na(use[[feature]]))) {
      use[[feature]] <- 0
    } else {
      med <- stats::median(use[[feature]], na.rm = TRUE)
      use[[feature]][is.na(use[[feature]])] <- med
    }
  }

  nunique <- vapply(use[, features, drop = FALSE], function(z) length(unique(z)), numeric(1))
  features <- features[nunique > 1]
  if (length(features) == 0) return(NA_real_)

  X <- as.matrix(use[, features, drop = FALSE])
  y <- as.numeric(use[[target]])
  n <- nrow(X)
  r2_values <- c()

  for (i in seq_len(repeats)) {
    set.seed(1000 + i)
    idx <- sample.int(n)
    split <- floor(n * train_frac)
    tr <- idx[seq_len(split)]
    te <- idx[(split + 1):n]

    Xtr <- X[tr, , drop = FALSE]
    Xte <- X[te, , drop = FALSE]
    ytr <- y[tr]
    yte <- y[te]

    mu <- colMeans(Xtr)
    sdv <- apply(Xtr, 2, stats::sd)
    sdv[is.na(sdv) | sdv == 0] <- 1

    Xtrz <- scale(Xtr, center = mu, scale = sdv)
    Xtez <- scale(Xte, center = mu, scale = sdv)
    ymu <- mean(ytr)
    ytrc <- ytr - ymu

    XtX <- t(Xtrz) %*% Xtrz
    beta <- solve(XtX + alpha * diag(ncol(Xtrz)), t(Xtrz) %*% ytrc)
    pred <- Xtez %*% beta + ymu
    r2_values <- c(r2_values, r2_score(yte, as.numeric(pred)))
  }

  mean(r2_values, na.rm = TRUE)
}

percentile_within_group <- function(values, groups) {
  out <- rep(NA_real_, length(values))
  for (g in unique(groups)) {
    idx <- which(groups == g)
    ranks <- rank(values[idx], na.last = "keep", ties.method = "average")
    denom <- sum(!is.na(values[idx]))
    if (denom > 0) {
      out[idx] <- ranks / denom
    }
  }
  out
}

broad_pos_from_study1 <- function(ws_position, first_pos_fbref, fotmob_position_short) {
  vals <- c(toupper(as.character(ws_position)), toupper(as.character(first_pos_fbref)), toupper(as.character(fotmob_position_short)))
  txt <- paste(vals, collapse = " ")
  if (grepl("GK", txt, fixed = TRUE)) return("GK")
  if (grepl("CB|LB|RB|WB|DF", txt) || grepl("(^| )D", txt)) return("DEF")
  if (grepl("FW", txt) || grepl("(^| )(ST|CF)", txt)) return("FWD")
  if (grepl("AM|DM|MF", txt) || grepl("(^| )M", txt) || grepl("W", txt)) return("MID")
  "UNK"
}

broad_pos_from_tournament <- function(x) {
  s <- toupper(trimws(as.character(x)))
  if (grepl("GK", s, fixed = TRUE)) return("GK")
  if (grepl("^D", s)) return("DEF")
  if (grepl("^(M|W)", s)) return("MID")
  if (grepl("^(F|S|A)", s)) return("FWD")
  "UNK"
}

write_json_pretty <- function(object, path) {
  json_text <- jsonlite::toJSON(object, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  writeLines(json_text, con = path, useBytes = TRUE)
}

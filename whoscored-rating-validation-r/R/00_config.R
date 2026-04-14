# Author: Minh Huynh

options(stringsAsFactors = FALSE)

AUTHOR_NAME <- "Minh Huynh"
PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
RAW_DIR <- file.path(PROJECT_ROOT, "data", "raw")
PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
TABLE_DIR <- file.path(PROJECT_ROOT, "outputs", "tables")
FIGURE_DIR <- file.path(PROJECT_ROOT, "outputs", "figures")
DOCS_DIR <- file.path(PROJECT_ROOT, "docs")

for (dir_path in c(PROCESSED_DIR, TABLE_DIR, FIGURE_DIR, DOCS_DIR)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

STUDY1_FILE <- file.path(RAW_DIR, "pasted_file_6LKvMs_ENG_complete.csv")
TOURNAMENT_FILES <- file.path(
  RAW_DIR,
  c(
    "Euros2024_cleaned.csv",
    "WEuros2025_cleaned.csv",
    "WomenWC2023_cleaned.csv",
    "WorldCup2018_cleaned.csv",
    "WorldCup2022_cleaned.csv",
    "AfricaNations2023_cleaned.csv",
    "CopaAmerica2024_cleaned.csv"
  )
)

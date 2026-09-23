# Open active_lives_survey.Rproj in RStudio, edit these settings, then Source.
# First run: install.packages("haven")
settings <- list(
  surveys = c("adult", "cyp"),
  latest_year = "2024/25",
  mode = "report",             # "audit", "import", or "report"
  raw_manifest = "config/datasets.csv", # Edit file paths here; no need to rename .sav files.
  cache_dir = "rds_data/annual_pipeline",
  output_dir = "output/annual_pipeline",
  rebuild = FALSE,              # TRUE forces re-import; otherwise unchanged caches are reused.
  local_area = "Somerset",
  local_code = 21,
  min_cell = 8L,                # Retained from the old reports; confirm disclosure guidance.
  round_base = 5L
)

settings$local_codes <- c(
  "adult|2022/23" = 312,
  "cyp|2022/23" = 312,
  "adult|2023/24" = 312,
  "cyp|2023/24" = 312,
  "adult|2024/25" = 312,
  "cyp|2024/25" = 312
)

settings$latest_year <- "2024/25"
# settings$skip_datasets <- c( "adult|2023/24", "cyp|2023/24" )
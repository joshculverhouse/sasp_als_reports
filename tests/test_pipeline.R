# Run from the project root: source("tests/test_pipeline.R")
# Synthetic records only; no survey data are required.
source("R/annual_pipeline.R")
fails <- function(expr) inherits(tryCatch({force(expr); NULL}, error = identity), "error")
stopifnot(identical(als_year(c("2022/23", "2023/24")), c(2022L, 2023L)))
stopifnot(fails(als_year("2023/25")))
settings <- list(surveys = c("adult", "cyp"), latest_year = "2023/24", mode = "report",
                 raw_manifest = "config/datasets.csv", output_dir = tempfile("als-test-"),
                 cache_dir = tempfile("als-cache-"), rebuild = FALSE,
                 local_area = "Somerset", local_code = 21, min_cell = 8L, round_base = 5L)
cfg <- als_config(settings)
stopifnot(nrow(cfg$datasets) == 16L, !anyDuplicated(als_key(cfg$variables)))
stopifnot(all(cfg$variables$source_name[cfg$variables$survey == "cyp" & cfg$variables$variable == "wt_gross"] == "wt_gross"))
stopifnot(sum(cfg$variables$variable == "fasbandf" & cfg$variables$status == "not_comparable") == 2L)

# Missing and unavailable responses must never become zero participation.
rules <- data.frame(code = c("1", "2", "-99"), include = c("TRUE", "TRUE", "FALSE"), analysis_label = c("Yes", "No", ""))
stopifnot(identical(als_recode(c(1, 2, -99, NA), rules, "test"), c("Yes", "No", NA_character_, NA_character_)))
stopifnot(fails(als_recode(3, rules, "test")))
count <- function(response, w = rep(1, length(response)), levels = c("Yes", "No")) {
  als_counts(response, rep("All", length(response)), w, rep(TRUE, length(response)), levels, "All",
             "adult", "2023/24", "test", "overall", "weight", settings)
}
out <- count(c(rep("Yes", 11), rep("No", 19), rep(NA_character_, 10)))
stopifnot(abs(out$percent[1] - 100 * 11/30) < 1e-10, out$sample_n[1] == 10, out$base_n[1] == 30)
stopifnot(all(is.na(count(rep(NA_character_, 20))$percent)))
stopifnot(all(is.na(count(c(rep("Yes", 7), rep("No", 20)))$percent)))
stopifnot(all(is.na(count(c(rep("Yes", 12), rep("No", 12)), rep(0, 24))$percent)))
stopifnot(fails(count(c("Yes", "No"), c(-1, 2))))
stopifnot(als_escape('<script>"&') == "&lt;script&gt;&quot;&amp;")

if (requireNamespace("haven", quietly = TRUE)) {
  # Build a miniature real .sav from approved dictionary labels, uppercase all names.
  ds <- cfg$datasets[1, ]; ds$file <- tempfile(fileext = ".sav")
  m <- cfg$variables[cfg$variables$survey == ds$survey & cfg$variables$year == ds$year, ]
  present <- m[nzchar(m$source_name), ]
  synthetic <- list()
  for (i in seq_len(nrow(present))) {
    row <- present[i, ]; vr <- cfg$values[als_key(cfg$values) == als_key(row), ]
    valid <- as.numeric(vr$code[vr$include == "TRUE"])
    vals <- if (length(valid)) rep(valid, length.out = 120) else rep(1, 120)
    if (row$role == "id") vals <- seq_len(120)
    if (row$role == "geography") vals <- rep(21, 120)
    labs <- setNames(as.numeric(vr$code), vr$source_label)
    synthetic[[toupper(row$source_name)]] <- haven::labelled_spss(as.numeric(vals), labels = labs, na_values = -99, label = row$label)
  }
  haven::write_sav(as.data.frame(synthetic), ds$file)
  small <- cfg; small$datasets <- ds
  audit <- als_audit(small, settings)
  stopifnot(!any(audit$audit$status == "BLOCK"))
  header <- audit$headers[[paste(ds$survey, ds$year, sep = "|")]]
  saved <- als_import_year(ds, m, header, settings)
  stopifnot(nrow(saved$data) == 120, inherits(saved$data$serial, "haven_labelled"))
  again <- als_import_year(ds, m, header, settings)
  stopifnot(identical(saved$imported_at, again$imported_at), identical(saved$fingerprint, again$fingerprint))
  result <- als_summarise_year(saved, small, settings)
  stopifnot(nrow(result$estimates) > 0, all(result$estimates$percent >= 0 & result$estimates$percent <= 100, na.rm = TRUE))
  path <- als_report("adult", result$estimates, small, audit$audit, settings)
  stopifnot(file.exists(path), any(grepl("Internal validation draft", readLines(path))))
  # A changed response label in an otherwise readable SPSS file must block.
  labs <- attr(synthetic[["GEND3"]], "labels")
  names(labs)[as.numeric(labs) == 1] <- "Unexpected new category"
  attr(synthetic[["GEND3"]], "labels") <- labs
  haven::write_sav(as.data.frame(synthetic), ds$file)
  bad <- als_audit(small, settings)
  stopifnot(any(bad$audit$status == "BLOCK"))
  unlink(ds$file)
  # Latest CYP fixture covers the different weight, names and report footnotes.
  ds <- cfg$datasets[cfg$datasets$survey == "cyp" & cfg$datasets$year == "2023/24", ]; ds$file <- tempfile(fileext = ".sav")
  m <- cfg$variables[cfg$variables$survey == ds$survey & cfg$variables$year == ds$year, ]
  synthetic <- list()
  for (i in which(nzchar(m$source_name))) {
    row <- m[i, ]; vr <- cfg$values[als_key(cfg$values) == als_key(row), ]
    valid <- as.numeric(vr$code[vr$include == "TRUE"])
    vals <- if (length(valid)) rep(valid, length.out = 120) else rep(1, 120)
    if (row$role == "id") vals <- seq_len(120)
    if (row$role == "geography") vals <- rep(21, 120)
    synthetic[[row$source_name]] <- haven::labelled_spss(as.numeric(vals),
      labels = setNames(as.numeric(vr$code), vr$source_label), na_values = -99, label = row$label)
  }
  haven::write_sav(as.data.frame(synthetic), ds$file)
  small <- cfg; small$datasets <- ds
  audit <- als_audit(small, settings)
  stopifnot(!any(audit$audit$status == "BLOCK"))
  saved <- als_import_year(ds, m, audit$headers[["cyp|2023/24"]], settings)
  result <- als_summarise_year(saved, small, settings)
  stopifnot(all(result$estimates$weight == "wt_gross"))
  path <- als_report("cyp", result$estimates, small, audit$audit, settings)
  html <- paste(readLines(path), collapse = "\n")
  stopifnot(grepl("Table 8: Family Affluence Scale", html, fixed = TRUE))
  source("R/prepare_year.R")
  proposal_dir <- file.path(settings$output_dir, "next_year")
  proposal <- als_prepare_year("cyp", "2024/25", ds$file, "fixture-guide.pdf", "fixture-dictionary.rtf", proposal_dir)
  stopifnot(all(proposal$status == "unreviewed"), all(proposal$year == "2024/25"),
            file.exists(file.path(proposal_dir, "review_checklist.csv")))
  # Missing participant responses are kept in the cache, but excluded from estimates.
  raw <- haven::labelled_spss(c(1, 2, -99), labels = c(Yes = 1, No = 2, Missing = -99), na_values = -99)
  stopifnot(identical(as.numeric(raw), c(1, 2, -99)), is.na(als_recode(raw, rules, "fixture")[3]))
  unlink(ds$file)
} else {
  message("SKIPPED: SPSS import/cache integration tests require haven.")
}
unlink(c(settings$output_dir, settings$cache_dir), recursive = TRUE)
message("Annual pipeline tests passed.")

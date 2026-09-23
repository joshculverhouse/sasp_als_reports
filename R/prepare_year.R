# Propose next year's mapping from the actual .sav header. Never approves changes.
# source("R/prepare_year.R")
# als_prepare_year("cyp", "2024/25", "raw_data/cyp/my_new_file.sav",
#                  guide_file = "new_user_guide.pdf", dictionary_file = "new_dictionary.rtf")
source("R/annual_pipeline.R")

als_prepare_year <- function(survey, year, file, guide_file, dictionary_file,
                             output_dir = NULL) {
  if (!survey %in% c("adult", "cyp")) stop("survey must be adult or cyp.")
  if (!requireNamespace("haven", quietly = TRUE)) stop("Install haven first.")
  als_year(year)
  if (is.null(output_dir)) output_dir <- file.path("output/year_review", survey, gsub("/", "", year))
  old <- als_csv("config/variables.csv")
  old <- old[old$survey == survey & als_year(old$year) < als_year(year), , drop = FALSE]
  if (!nrow(old)) stop("No previous mapping available.")
  prev <- old[als_year(old$year) == max(als_year(old$year)), , drop = FALSE]
  header <- haven::read_sav(file, n_max = 0, user_na = TRUE, .name_repair = "minimal")
  if (anyDuplicated(tolower(names(header)))) stop("Names collide after lowercasing.")
  values <- als_csv("config/values.csv")
  values <- values[values$survey == survey & values$year == prev$year[1], , drop = FALSE]
  proposed_values <- list(); review <- list()
  source_id <- paste(survey, als_year(year), sep = "_")
  for (i in seq_len(nrow(prev))) {
    v <- prev$variable[i]
    idx <- match(tolower(prev$source_name[i]), tolower(names(header)))
    if (!nzchar(prev$source_name[i])) idx <- match(v, tolower(names(header)))
    if (is.na(idx)) {
      prev$source_name[i] <- ""
      prev$status[i] <- "unreviewed"
      prev$note[i] <- "Source not found: inspect raw_variable_inventory.csv; distinguish rename, changed question and genuine absence."
      review[[length(review) + 1L]] <- data.frame(variable = v, change = "Missing source", decision = "")
      next
    }
    src <- names(header)[idx]
    prev$source_name[i] <- src
    raw_label <- attr(header[[idx]], "label")
    if (!is.null(raw_label)) {
      if (als_plain(raw_label) != als_plain(prev$label[i]))
        review[[length(review) + 1L]] <- data.frame(variable = v, change = "Variable/question label changed", decision = "")
      prev$label[i] <- raw_label
    }
    # Every new year needs explicit review of definitions/weights and availability.
    prev$status[i] <- "unreviewed"
    prev$note[i] <- "Check annual guide, comparability, eligibility and weight before setting status to available."
    labs <- attr(header[[idx]], "labels")
    old_rules <- values[values$variable == v, , drop = FALSE]
    at <- match(as.numeric(labs), as.numeric(old_rules$code))
    same <- !is.na(at) & als_plain(names(labs)) == als_plain(old_rules$source_label[at])
    same[is.na(same)] <- FALSE
    if (length(labs)) {
      proposed_values[[length(proposed_values) + 1L]] <- data.frame(survey, year, variable = v,
        code = as.numeric(labs), source_label = names(labs),
        analysis_label = ifelse(same, old_rules$analysis_label[at], ""),
        include = ifelse(same, old_rules$include[at], "FALSE"), source_id, stringsAsFactors = FALSE)
      if (any(!same)) review[[length(review) + 1L]] <- data.frame(variable = v,
        change = paste("New/changed codes:", paste(as.numeric(labs)[!same], collapse = ", ")), decision = "")
    }
    review[[length(review) + 1L]] <- data.frame(variable = v,
      change = if (length(labs) && all(same)) "Codes unchanged; verify annual definition and weight" else "Verify mapping and annual guidance", decision = "")
  }
  prev$year <- year; prev$source_id <- source_id; prev$candidate_names <- ""
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  als_write_csv(prev, file.path(output_dir, "proposed_variables.csv"))
  if (length(proposed_values)) als_write_csv(do.call(rbind, proposed_values), file.path(output_dir, "proposed_values.csv"))
  als_write_csv(do.call(rbind, review), file.path(output_dir, "review_checklist.csv"))
  als_write_csv(data.frame(source_name = names(header), label = vapply(header, function(x) {
    z <- attr(x, "label"); if (is.null(z)) "" else z
  }, character(1))), file.path(output_dir, "raw_variable_inventory.csv"))
  als_write_csv(data.frame(survey, year, file, source_id), file.path(output_dir, "proposed_datasets.csv"))
  als_write_csv(data.frame(source_id, survey, year, guide_file, dictionary_file, url = "",
                           citation = "ADD the correct study reference and URL"), file.path(output_dir, "proposed_sources.csv"))
  message("Proposals written to ", output_dir, ". Review and append to config CSVs; unreviewed rows cannot run.")
  invisible(prev)
}

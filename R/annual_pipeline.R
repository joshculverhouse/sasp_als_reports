# Annual ALS workflow. Raw files and labelled caches are never edited in place.
# Uses base R plus haven; no dependency on the global objects in the legacy reports.

als_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
           colClasses = "character", na.strings = character(), fileEncoding = "UTF-8")
}

als_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

als_year <- function(x) {
  good <- grepl("^[0-9]{4}/[0-9]{2}$", x)
  start <- suppressWarnings(as.integer(substr(x, 1, 4)))
  good <- good & substr(x, 6, 7) == sprintf("%02d", (start + 1L) %% 100L)
  if (any(!good | is.na(good))) stop("Use consecutive year labels such as 2023/24.", call. = FALSE)
  start
}

als_key <- function(x) paste(x$survey, x$year, x$variable, sep = "|")
als_plain <- function(x) trimws(gsub("[[:space:]]+", " ", tolower(x)))
als_number <- function(x) as.numeric(x) # Explicitly retain codes; do not turn labels into factors.

als_config <- function(settings) {
  if (!all(settings$surveys %in% c("adult", "cyp")) || !length(settings$surveys))
    stop("surveys must contain adult and/or cyp.")
  if (!settings$mode %in% c("audit", "import", "report")) stop("Unknown mode.")
  if (settings$min_cell < 1 || settings$round_base < 1) stop("Invalid disclosure settings.")
  cfg <- list(datasets = als_csv(settings$raw_manifest),
              variables = als_csv("config/variables.csv"),
              values = als_csv("config/values.csv"),
              sources = als_csv("config/sources.csv"),
              notes = als_csv("config/notes.csv"))
  for (name in c("datasets", "variables", "values", "sources")) {
    d <- cfg[[name]]
    d <- d[d$survey %in% settings$surveys & als_year(d$year) <= als_year(settings$latest_year), , drop = FALSE]
    cfg[[name]] <- d
  }
  if (anyDuplicated(paste(cfg$datasets$survey, cfg$datasets$year))) stop("Duplicate survey/year in datasets.csv.")
  if (anyDuplicated(als_key(cfg$variables))) stop("Duplicate variable/year in variables.csv.")
  if (anyDuplicated(paste(als_key(cfg$values), cfg$values$code))) stop("Duplicate code in values.csv.")
  allowed <- c(
    "available", "not_in_dictionary", "not_comparable", "not_in_file"
  )
  if (any(!cfg$variables$status %in% allowed)) stop("Unreviewed mapping: resolve status in variables.csv.")
  if (any(!cfg$variables$weight_status %in% c("documented_family", "review", "not_applicable", "approved")))
    stop("Unknown weight_status in variables.csv.")
  if (any(!cfg$values$include %in% c("TRUE", "FALSE"))) stop("include must be TRUE or FALSE in values.csv.")
  if (any(!cfg$variables$source_id %in% cfg$sources$source_id)) stop("Missing source citation.")
  for (s in settings$surveys) {
    years <- cfg$datasets$year[cfg$datasets$survey == s]
    expected <- seq(if (s == "adult") 2015L else 2017L, als_year(settings$latest_year))
    if (!identical(sort(als_year(years)), expected))
      stop("datasets.csv must contain every annual file through latest_year for ", s,
           ". New years need explicit rows in datasets.csv, variables.csv, values.csv and sources.csv.")
  }
  cfg
}

als_issue <- function(survey, year, variable, status, detail) {
  data.frame(survey, year, variable, status, detail, stringsAsFactors = FALSE)
}

als_audit <- function(cfg, settings) {
  issues <- list(); inventory <- list(); headers <- list()
  for (i in seq_len(nrow(cfg$datasets))) {
    ds <- cfg$datasets[i, ]; tag <- paste(ds$survey, ds$year, sep = "|")
    cat("Checking ", tag, "\n", sep = "")
    if (!file.exists(ds$file)) {
      issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, "file", "BLOCK", paste("File not found:", ds$file))
      next
    }
    h <- haven::read_sav(ds$file, n_max = 0, user_na = TRUE, .name_repair = "minimal")
    if (anyDuplicated(tolower(names(h)))) {
      issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, "names", "BLOCK", "Names collide after lowercasing; resolve explicitly.")
      next
    }
    headers[[tag]] <- h
    inv <- data.frame(survey = ds$survey, year = ds$year, source_name = names(h),
                      label = vapply(h, function(x) {z <- attr(x, "label", exact = TRUE); if (is.null(z)) "" else z}, character(1)))
    inventory[[length(inventory) + 1L]] <- inv
    m <- cfg$variables[cfg$variables$survey == ds$survey & cfg$variables$year == ds$year, , drop = FALSE]
    mandatory <- c(if (ds$survey == "adult") c("serial", "wt_final", "mems7gr_sportcount_a01") else
                     c("respondent_serial", "wt_gross", "mins_modplus_everywhere_week_gr3_all"), "countycounc")
    if (!all(mandatory %in% m$variable[m$status == "available"])) {
      issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, "mapping", "BLOCK", "Missing required ID, weight, geography or activity mapping.")
    }
    for (j in seq_len(nrow(m))) {
      row <- m[j, ]; matched <- match(tolower(row$source_name), tolower(names(h)))
      if (!nzchar(row$source_name)) {
        issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "GAP", row$note)
        next
      }
      if (is.na(matched)) {
        issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "BLOCK", paste("Expected source missing:", row$source_name))
        next
      }
      labs <- attr(h[[matched]], "labels")
      known <- cfg$values[als_key(cfg$values) == als_key(row), , drop = FALSE]
      if (row$role %in% c("measure", "demographic")) {
        raw_label <- attr(h[[matched]], "label")
        if (is.null(raw_label) || als_plain(raw_label) != als_plain(row$label))
          issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "BLOCK",
            paste("Variable label changed or absent:", if (is.null(raw_label)) "(none)" else raw_label))
        if (!length(labs) || !nrow(known)) {
          issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "BLOCK", "Value labels unavailable; check dictionary and raw file.")
        } else {
          at <- match(as.numeric(labs), as.numeric(known$code))
          drift <- is.na(at) | als_plain(names(labs)) != als_plain(known$source_label[at])
          if (any(drift)) {
            detail <- paste(paste(as.numeric(labs)[drift], names(labs)[drift], sep = " = "), collapse = "; ")
            issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "BLOCK", paste("New/changed value labels:", detail))
          }
        }
      }
      if (row$status == "not_comparable")
        issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "GAP", row$note)
      if (row$weight_status == "review")
        issues[[length(issues) + 1L]] <- als_issue(ds$survey, ds$year, row$variable, "REVIEW", "Imported, but excluded from draft estimates until the indicator weight is approved.")
    }
  }
  audit <- if (length(issues)) do.call(rbind, issues) else als_issue(character(), character(), character(), character(), character())
  als_write_csv(audit, file.path(settings$output_dir, "variable_audit.csv"))
  if (length(inventory)) als_write_csv(do.call(rbind, inventory), file.path(settings$output_dir, "raw_variable_inventory.csv"))
  list(audit = audit, headers = headers)
}

als_fingerprint <- function(ds, settings) {
  files <- c(ds$file, settings$raw_manifest, list.files("config", pattern = "\\.csv$", full.names = TRUE), "R/annual_pipeline.R")
  hashes <- tools::md5sum(files)
  if (anyNA(hashes)) stop("Could not fingerprint inputs.")
  paste(names(hashes), hashes, collapse = "|")
}

als_import_year <- function(ds, m, header, settings) {
  cache <- file.path(settings$cache_dir, ds$survey, paste0(gsub("/", "", ds$year), ".rds"))
  key <- als_fingerprint(ds, settings)
  if (!settings$rebuild && file.exists(cache)) {
    saved <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (is.list(saved) && identical(saved$fingerprint, key)) {
      cat("Reusing ", ds$survey, " ", ds$year, "\n", sep = "")
      return(saved)
    }
  }
  m <- m[nzchar(m$source_name), , drop = FALSE]
  src <- names(header)[match(tolower(m$source_name), tolower(names(header)))]
  if (anyNA(src)) stop("Unresolved mapping reached import.")
  cat("Importing ", ds$survey, " ", ds$year, " (", length(unique(src)), " columns)\n", sep = "")
  raw <- haven::read_sav(ds$file, col_select = tidyselect::all_of(unique(src)), user_na = TRUE, .name_repair = "minimal")
  data <- as.data.frame(raw[src], check.names = FALSE)
  names(data) <- m$variable
  id <- if (ds$survey == "adult") "serial" else "respondent_serial"
  ids <- als_number(data[[id]])
  if (anyNA(ids) || any(ids < 0) || anyDuplicated(ids)) stop("Missing/duplicate respondent IDs in ", ds$file)
  saved <- list(data = data, survey = ds$survey, year = ds$year, raw_file = ds$file,
                source_names = setNames(src, m$variable), fingerprint = key,
                imported_at = format(Sys.time(), tz = "UTC", usetz = TRUE), pipeline_version = 1L)
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(tmpdir = dirname(cache), fileext = ".rds")
  on.exit(unlink(temp), add = TRUE)
  saveRDS(saved, temp)
  # file.copy(overwrite=TRUE) works on Windows where rename cannot replace an existing file.
  if (!file.copy(temp, cache, overwrite = TRUE)) stop("Could not write cache: ", cache)
  saved
}

als_recode <- function(x, rules, variable) {
  code <- als_number(haven::zap_missing(x))
  unexpected <- setdiff(unique(code[!is.na(code)]), as.numeric(rules$code))
  if (length(unexpected)) stop("Unrecognised observed codes in ", variable, ": ", paste(unexpected, collapse = ", "))
  at <- match(code, as.numeric(rules$code))
  valid <- !is.na(at) & rules$include[at] == "TRUE"
  result <- rep(NA_character_, length(code))
  result[valid] <- rules$analysis_label[at[valid]]
  result
}

als_counts <- function(response, group, weights, area, levels, group_levels,
                       survey, year, variable, group_variable, weight_name, settings, cluster = NULL, local = FALSE) {
  if (any(is.finite(weights) & weights < 0)) stop("Negative analysis weight: ", weight_name)
  rows <- list()
  for (g in group_levels) {
    use <- area & !is.na(group) & group == g & !is.na(response) & is.finite(weights) & weights > 0
    n <- sum(use); total <- sum(weights[use])
    school_n <- NA_integer_
    school_missing <- NA_integer_
    reliability <- "Not assessed"
    if (survey == "cyp") {
      if (!is.null(cluster)) {
        stopifnot(length(cluster) == length(response))
        school_missing <- sum(is.na(cluster[use]))
        school_n <- length(unique(cluster[use & !is.na(cluster)]))
      }
      if (local) {
        reasons <- character()
        if (n == 0) reasons <- c(reasons, "No eligible valid responses")
        else if (n < 150) reasons <- c(reasons, "Base below 150")
        if (is.na(school_n) || school_missing > 0) {
          reasons <- c(reasons, "School identifiers unavailable or missing")
        } else if (school_n < 3) {
          reasons <- c(reasons, "Fewer than 3 contributing schools")
        }
        reliability <- if (length(reasons)) paste(reasons, collapse = "; ") else "Threshold checks passed"
      } else reliability <- "National: local thresholds not applied"
    }
    counts <- vapply(levels, function(z) sum(use & !is.na(response) & response == z), numeric(1))
    small <- counts < settings$min_cell
    # Complementary suppression avoids revealing one small category by subtraction.
    if (sum(small) == 1L && length(small) > 1L) {
      others <- which(!small); small[others[which.min(counts[others])]] <- TRUE
    }
    for (j in seq_along(levels)) {
      numerator <- sum(weights[use & response == levels[j]], na.rm = TRUE)
      suppressed <- small[j] || n < settings$min_cell || !is.finite(total) || total <= 0
      rows[[length(rows) + 1L]] <- data.frame(
        survey, year, variable, group_variable, group = g, response = levels[j],
        percent = if (suppressed) NA_real_ else 100 * numerator / total,
        sample_n = if (suppressed) NA_real_ else round(counts[j] / settings$round_base) * settings$round_base,
        base_n = if (n < settings$min_cell) NA_real_ else round(n / settings$round_base) * settings$round_base,
        status = if (n == 0) "No eligible valid responses" else if (suppressed) "Suppressed" else "Available",
        weight = weight_name, contributing_schools = school_n,
        missing_school_ids = school_missing, reliability_review = reliability,
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

als_summarise_year <- function(saved, cfg, settings) {
  survey <- saved$survey; year <- saved$year; d <- saved$data
  # The first two adult datasets also contain under-16 respondents.
  if (survey == "adult" && year %in% c("2015/16", "2016/17")) {
    stopifnot("age16plus" %in% names(d))
    
    eligible <- as.numeric(haven::zap_missing(d$age16plus))
    stopifnot(
      !anyNA(eligible),
      all(eligible %in% c(0, 1))
    )
    
    cat(
      "Adult eligibility:", year, "— excluding",
      sum(eligible == 0), "under-16 respondents\n"
    )
    
    d <- d[eligible == 1, , drop = FALSE]
  }
  m <- cfg$variables[cfg$variables$survey == survey & cfg$variables$year == year, , drop = FALSE]
  rules <- cfg$values[cfg$values$survey == survey & cfg$values$year == year, , drop = FALSE]
  usable <- m$status == "available" & m$role %in% c("measure", "demographic")
  decoded <- list(); level_map <- list()
  for (v in m$variable[usable]) {
    vr <- rules[rules$variable == v, , drop = FALSE]
    decoded[[v]] <- als_recode(d[[v]], vr, v)
    level_map[[v]] <- unique(vr$analysis_label[vr$include == "TRUE"])
  }
  county <- als_number(d$countycounc)
  areas <- list(England = rep(TRUE, nrow(d)))
  local_code <- settings$local_code
  key <- paste(survey, year, sep = "|")
  
  if (key %in% names(settings$local_codes)) {
    local_code <- settings$local_codes[[key]]
  }
  
  areas[[settings$local_area]] <- !is.na(county) & county == local_code
  if (!any(areas[[settings$local_area]])) stop("No records match the local geography in ", year, ". Check config/variables.csv and local_code.")
  primary <- if (survey == "adult") "mems7gr_sportcount_a01" else "mins_modplus_everywhere_week_gr3_all"
  cluster <- if (survey == "cyp" && "xcluster" %in% names(d))
    haven::zap_missing(d$xcluster) else NULL
  result <- list(); quality <- list()
  # All core measures have their own distributions. Also show activity by each core subgroup.
  for (v in names(decoded)) {
    row <- m[m$variable == v, ]; wname <- row$weight
    if (row$weight_status == "review") next
    if (!wname %in% names(d)) stop("Weight missing for ", v, ": ", wname)
    w <- als_number(haven::zap_missing(d[[wname]]))
    if (any(is.finite(w) & w < 0)) stop("Negative weights in ", wname)
    quality[[length(quality) + 1L]] <- data.frame(survey, year, variable = v,
      rows = nrow(d), valid_response = sum(!is.na(decoded[[v]])),
      missing_response = sum(is.na(decoded[[v]])), invalid_or_zero_weight = sum(!is.finite(w) | w <= 0), weight = wname)
    if (!length(level_map[[v]])) next
    for (area_name in names(areas)) {
      block <- als_counts(decoded[[v]], rep("All", nrow(d)), w, areas[[area_name]],
                          level_map[[v]], "All", survey, year, v, "overall", wname, settings, cluster, area_name != "England")
      if (!is.null(block)) {block$area <- area_name; result[[length(result) + 1L]] <- block}
      if (v != primary) {
        block <- als_counts(decoded[[primary]], decoded[[v]], w, areas[[area_name]],
                            level_map[[primary]], level_map[[v]], survey, year, primary, v, wname, settings, cluster, area_name != "England")
        if (!is.null(block)) {block$area <- area_name; result[[length(result) + 1L]] <- block}
      }
    }
  }
  list(estimates = do.call(rbind, result), quality = do.call(rbind, quality))
}

als_escape <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

als_table <- function(d) {
  if (!nrow(d)) return("<p>No data available.</p>")
  cells <- apply(d, 1L, function(row) paste0("<tr><td>", paste(als_escape(row), collapse = "</td><td>"), "</td></tr>"))
  paste0('<div class="scroll"><table><thead><tr><th>', paste(als_escape(names(d)), collapse = "</th><th>"),
         "</th></tr></thead><tbody>", paste(cells, collapse = "\n"), "</tbody></table></div>")
}

als_bars <- function(d) {
  if (!nrow(d)) return("")
  paste(vapply(seq_len(nrow(d)), function(i) {
    x <- d[i, ]; val <- if (is.na(x$percent)) x$status else sprintf("%.1f%%", x$percent)
    width <- if (is.na(x$percent)) 0 else max(0, min(100, x$percent))
    paste0('<div class="bar-row"><span>', als_escape(paste(x$area, x$response, sep = " — ")),
           '</span><div class="track"><div class="bar" style="width:', sprintf("%.2f", width),
           '%"></div></div><b>', als_escape(val), "</b></div>")
  }, character(1)), collapse = "\n")
}

# Render local/analyst citations as text rather than empty hyperlinks.
als_source_link <- function(src) {
  label <- als_escape(src$guide_file[1L])
  url <- src$url[1L]
  if (is.na(url) || !nzchar(trimws(url))) return(label)
  paste0('<a href="', als_escape(url), '">', label, '</a>')
}

als_note_html <- function(survey, variable, cfg) {
  notes <- cfg$notes[cfg$notes$survey == survey & cfg$notes$variable == variable, , drop = FALSE]
  text <- character()
  for (i in seq_len(nrow(notes))) {
    n <- notes[i, ]; src <- cfg$sources[cfg$sources$source_id == n$source_id, , drop = FALSE]
    if (!nrow(src)) next
    text <- c(text, paste0("<p><small><b>", als_escape(n$years), ":</b> ", als_escape(n$note),
      ' <a href="#', als_escape(src$source_id), '">', als_escape(src$source_id), "</a>, ",
      als_escape(n$section), ".</small></p>"))
  }
  paste(text, collapse = "\n")
}

als_report <- function(survey, estimates, cfg, audit, settings) {
  latest <- settings$latest_year
  title <- paste("Active Lives", if (survey == "adult") "Adults" else "Children and Young People",
                 "—", settings$local_area, latest)
  parts <- c('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    paste0("<title>", als_escape(title), "</title>"),
    '<style>body{font:16px/1.5 system-ui,sans-serif;color:#213047;max-width:1200px;margin:auto;padding:28px}h1,h2{color:#155cac}section{border-top:1px solid #d5dfeb;margin-top:32px;padding-top:16px}.notice{background:#fff2cf;padding:18px;border-left:5px solid #ab7214}table{border-collapse:collapse;width:100%;font-size:14px}th,td{text-align:left;padding:9px;border-bottom:1px solid #dae1eb}th{background:#edf3fa;position:sticky;top:0}.scroll{overflow:auto;max-height:600px}details{margin:16px 0}summary{cursor:pointer;font-weight:600}.bar-row{display:grid;grid-template-columns:minmax(180px,2fr) 3fr 110px;gap:14px;align-items:center;margin:10px 0}.track{background:#edf1f6;height:20px}.bar{height:100%;background:#155cac}small{color:#526176}a{color:#155cac}</style>',
    paste0("<h1>", als_escape(title), "</h1>"),
    paste0("<p>Annual data through ", als_escape(latest), ". Generated ", Sys.Date(), ".</p>"),
    '<div class="notice"><b>Internal validation draft.</b> These are weighted descriptive percentages, not verified replicas of Sport England tables. Confidence intervals and significance claims are intentionally absent pending verification of the survey-design method. Unapproved indicator weights are omitted and listed below. Check the Somerset geography across years before circulation.</div>',
    paste0("<p>Percentages use valid coded responses with a positive finite indicator weight. Counts are rounded only after calculation. Cells below ",
           settings$min_cell, " and complementary cells are suppressed; displayed counts are rounded to ", settings$round_base,
           ". These disclosure settings carry forward the old project convention and require confirmation against the prescribed guidance. A blank is never interpreted as zero participation.</p>"))
  e <- estimates[estimates$survey == survey, , drop = FALSE]
  if (survey == "cyp" && "reliability_review" %in% names(e)) {
    reviews <- unique(e[e$area != "England", c("year", "area", "variable",
      "group_variable", "group", "base_n", "contributing_schools",
      "missing_school_ids", "reliability_review"), drop = FALSE])
    flagged <- reviews[reviews$reliability_review != "Threshold checks passed", , drop = FALSE]
    parts <- c(parts, '<div class="notice"><b>CYP reliability review.</b> Local results are screened for fewer than 150 eligible valid responses and fewer than three contributing schools. This diagnostic applies the thresholds to each displayed denominator, including subgroups, across all years; it is not a verified reproduction of the year-specific publication rules. Flags do not yet alter estimates or suppression. Passing these checks does not validate confidence intervals, phase/term coverage or historical comparability. See <a href="https://sportengland-production-files.s3.eu-west-2.amazonaws.com/s3fs-public/2024-03/Active%20Lives%20Child%20Y6%20Technical%20Report%20%2822_23%29_v1%20PUBLIC.pdf">2022/23 technical report, section 7.8</a>.</div>',
      '<details open><summary>Local reliability flags — review before circulation</summary>',
      als_table(flagged), '</details>')
  }
  vars <- cfg$variables[cfg$variables$survey == survey, , drop = FALSE]
  indicators <- unique(e$variable[e$group_variable == "overall"])
  primary <- if (survey == "adult") "mems7gr_sportcount_a01" else "mins_modplus_everywhere_week_gr3_all"
  indicators <- unique(c(primary, indicators))
  for (v in indicators) {
    vm <- vars[vars$variable == v, , drop = FALSE]
    label <- vm$label[which.max(als_year(vm$year))]
    if (v == primary) label <- "Physical activity"
    d <- e[e$variable == v & e$group_variable == "overall", , drop = FALSE]
    parts <- c(parts, paste0("<section><h2>", als_escape(label), "</h2>"),
      if (any(d$year == latest)) als_bars(d[d$year == latest, , drop = FALSE]) else
        "<p>No comparable estimate is available for the latest year; see availability notes.</p>",
      als_note_html(survey, v, cfg))
    show <- d[c("year", "area", "response", "percent", "sample_n", "base_n", "status", "weight")]
    show$percent <- ifelse(is.na(show$percent), "", sprintf("%.1f", show$percent))
    parts <- c(parts, "<details><summary>Annual results and sample sizes</summary>", als_table(show), "</details></section>")
  }
  parts <- c(parts, "<section><h2>Physical activity by subgroup</h2>")
  for (g in unique(e$group_variable[e$group_variable != "overall"])) {
    gm <- vars[vars$variable == g, , drop = FALSE]; label <- gm$label[which.max(als_year(gm$year))]
    d <- e[e$group_variable == g, c("year", "area", "group", "response", "percent", "sample_n", "base_n", "status"), drop = FALSE]
    d$percent <- ifelse(is.na(d$percent), "", sprintf("%.1f", d$percent))
    parts <- c(parts, paste0("<details><summary>", als_escape(label), "</summary>"),
               als_note_html(survey, g, cfg), als_table(d), "</details>")
  }
  parts <- c(parts, "</section><section><h2>Historical notes and data gaps</h2>")
  notes <- cfg$notes[cfg$notes$survey == survey, , drop = FALSE]
  for (i in seq_len(nrow(notes))) {
    n <- notes[i, ]; src <- cfg$sources[cfg$sources$source_id == n$source_id, , drop = FALSE]
    if (!nrow(src)) next
    parts <- c(parts, paste0("<p><b>", als_escape(paste(n$years, n$variable, sep = " — ")), ":</b> ",
      als_escape(n$note), " ", als_source_link(src),
      ", ", als_escape(n$section), ".</p>"))
  }
  gaps <- vars[vars$status != "available", c("year", "variable", "status", "note", "source_id"), drop = FALSE]
  parts <- c(parts, "<details><summary>Variable availability by year</summary>", als_table(gaps), "</details>")
  reviews <- vars[vars$weight_status == "review" & vars$status == "available", c("year", "variable", "weight", "source_id"), drop = FALSE]
  parts <- c(parts, "<details open><summary>Indicator weights awaiting review — estimates omitted</summary>", als_table(reviews), "</details>")
  geog <- vars[vars$variable == "countycounc", c("year", "source_name", "source_id"), drop = FALSE]
  geog$local_code <- vapply(geog$year, function(year) {
    key <- paste(survey, year, sep = "|")
    code <- settings$local_code
    if (key %in% names(settings$local_codes)) code <- settings$local_codes[[key]]
    as.character(code)
  }, character(1))
  parts <- c(parts, "<p>Local selection uses the annual source fields and codes below:</p>", als_table(geog))
  parts <- c(parts, "</section><section><h2>Sources</h2>")
  for (i in which(cfg$sources$survey == survey)) {
    s <- cfg$sources[i, ]
    parts <- c(parts, paste0('<p id="', als_escape(s$source_id), '"><b>', als_escape(s$source_id),
      ':</b> <a href="', als_escape(s$url), '">', als_escape(s$citation), "</a><br>Dictionary: ", als_escape(s$dictionary_file), "</p>"))
  }
  parts <- c(parts, "</section></html>")
  path <- file.path(settings$output_dir, paste0(survey, "_", gsub("/", "", latest), "_draft.html"))
  writeLines(enc2utf8(parts), path, useBytes = TRUE)
  path
}

als_run <- function(settings) {
  if (!requireNamespace("haven", quietly = TRUE)) stop('Install haven first: install.packages("haven")')
  dir.create(settings$output_dir, recursive = TRUE, showWarnings = FALSE)
  completed <- FALSE
  status_file <- file.path(settings$output_dir, "run_status.txt")
  writeLines(paste("RUNNING", format(Sys.time(), tz = "UTC", usetz = TRUE)), status_file)
  on.exit({if (!completed) writeLines("FAILED: inspect the R error/audit. Any earlier reports in this directory have not been refreshed.", status_file)}, add = TRUE)
  cfg <- als_config(settings)
  checks <- als_audit(cfg, settings)
  if (settings$mode == "audit") {
    message("Audit written to ", settings$output_dir, "; BLOCK entries need correction before import.")
    completed <- TRUE
    writeLines(paste("AUDIT COMPLETE; blocking issues:", sum(checks$audit$status == "BLOCK")), status_file)
    return(invisible(checks$audit))
  }
  if (any(checks$audit$status == "BLOCK")) stop("Preflight stopped. See variable_audit.csv; no data have been imported.", call. = FALSE)
  all_results <- list(); quality <- list(); run_log <- list()
  for (i in seq_len(nrow(cfg$datasets))) {
    ds <- cfg$datasets[i, ]; m <- cfg$variables[cfg$variables$survey == ds$survey & cfg$variables$year == ds$year, , drop = FALSE]
    saved <- als_import_year(ds, m, checks$headers[[paste(ds$survey, ds$year, sep = "|")]], settings)
    run_log[[i]] <- data.frame(survey = ds$survey, year = ds$year, raw_file = ds$file,
                               rows = nrow(saved$data), columns = ncol(saved$data), imported_at = saved$imported_at)
    if (settings$mode == "report") {
      result <- als_summarise_year(saved, cfg, settings)
      all_results[[i]] <- result$estimates; quality[[i]] <- result$quality
    }
    rm(saved); gc(verbose = FALSE)
  }
  als_write_csv(do.call(rbind, run_log), file.path(settings$output_dir, "run_log.csv"))
  writeLines(c(capture.output(str(settings)), capture.output(sessionInfo())), file.path(settings$output_dir, "session_info.txt"))
  if (settings$mode == "import") {
    completed <- TRUE; writeLines("IMPORT COMPLETE", status_file)
    return(invisible(run_log))
  }
  estimates <- do.call(rbind, all_results)
  als_write_csv(estimates, file.path(settings$output_dir, "estimates.csv"))
  reliability <- unique(estimates[estimates$survey == "cyp" & estimates$area != "England",
    c("survey", "year", "area", "variable", "group_variable", "group", "base_n",
      "contributing_schools", "missing_school_ids", "reliability_review"), drop = FALSE])
  als_write_csv(reliability, file.path(settings$output_dir, "cyp_reliability_review.csv"))
  als_write_csv(do.call(rbind, quality), file.path(settings$output_dir, "data_quality.csv"))
  paths <- vapply(settings$surveys, als_report, character(1), estimates = estimates, cfg = cfg, audit = checks$audit, settings = settings)
  message("Draft reports created:\n", paste(paths, collapse = "\n"))
  completed <- TRUE; writeLines("DRAFT REPORTS COMPLETE", status_file)
  invisible(paths)
}

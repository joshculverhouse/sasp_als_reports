# Source AFTER annual_pipeline.R. Adds only the missing measure-by-demographic rows.
# Re-sourcing is safe: capture the unwrapped function once per fresh pipeline source.
if (!isTRUE(attr(als_summarise_year, "report_extension"))) {
  als_summarise_year_core <- als_summarise_year
}
als_summarise_year <- function(saved, cfg, settings) {
  result <- als_summarise_year_core(saved, cfg, settings)
  s <- saved$survey; y <- saved$year; d <- saved$data
  m <- cfg$variables[cfg$variables$survey == s & cfg$variables$year == y, , drop=FALSE]
  rules <- cfg$values[cfg$values$survey == s & cfg$values$year == y, , drop=FALSE]
  target <- if(s == "adult") "muscle7gr2_sportcount_a01" else "swm25can_bcd_pop"
  row <- m[m$variable == target & m$status == "available", , drop=FALSE]
  if (!nrow(row) || row$weight_status == "review") return(result)
  if (s == "adult" && y %in% c("2015/16","2016/17"))
    d <- d[as.numeric(haven::zap_missing(d$age16plus)) == 1, , drop=FALSE]
  vr <- rules[rules$variable == target, , drop=FALSE]
  response <- als_recode(d[[target]], vr, target)
  categories <- unique(vr$analysis_label[vr$include == "TRUE"])
  weight <- als_number(haven::zap_missing(d[[row$weight]]))
  if(any(is.finite(weight) & weight < 0)) stop("Negative measure weights")
  groups <- if(s == "adult") c("gend3","age4","nssec5","disab2_pop","eth2") else c("gend3","schyr_5_2","fasbandf")
  groups <- intersect(groups,m$variable[m$status == "available" & m$role == "demographic"])
  local_code <- settings$local_code; key <- paste(s,y,sep="|")
  if(key %in% names(settings$local_codes)) local_code <- settings$local_codes[[key]]
  geo <- als_number(d$countycounc)
  areas <- list(England=rep(TRUE,nrow(d)))
  areas[[settings$local_area]] <- !is.na(geo) & geo == local_code
  cluster <- if(s == "cyp" && "xcluster" %in% names(d)) haven::zap_missing(d$xcluster) else NULL
  for(g in groups) {
    gr <- rules[rules$variable == g, , drop=FALSE]
    subgroup <- als_recode(d[[g]],gr,g)
    for(area in names(areas)) {
      block <- als_counts(response,subgroup,weight,areas[[area]],categories,
        unique(gr$analysis_label[gr$include == "TRUE"]),s,y,target,g,row$weight,settings,cluster,area != "England")
      if(!is.null(block)) {
        block$area <- area
        result$estimates <- rbind(result$estimates,block)
      }
    }
  }
  result
}
attr(als_summarise_year,"report_extension") <- TRUE

# Source after annual_pipeline.R. Explicit, survey-specific skips only.
if (!isTRUE(attr(als_config, 'missing_year_extension'))) als_config_with_all_years <- als_config
als_config <- function(settings) {
  cfg <- als_config_with_all_years(settings)
  keys <- paste(cfg$datasets$survey,cfg$datasets$year,sep='|')
  skip <- settings$skip_datasets
  if(is.null(skip)) skip <- character()
  if(!is.character(skip) || anyNA(skip) || anyDuplicated(skip)) stop('skip_datasets must be unique survey|year strings.')
  # Keep declarations for other survey selections/years, but reject unknown keys.
  manifest <- als_csv(settings$raw_manifest)
  all_keys <- paste(manifest$survey,manifest$year,sep='|')
  unknown <- setdiff(skip,all_keys)
  if(length(unknown)) stop('Skipped datasets need placeholder rows in datasets.csv: ',paste(unknown,collapse=', '))
  missing <- keys %in% skip
  if(any(missing & cfg$datasets$year==settings$latest_year)) stop('latest_year must have data; do not skip the latest report year.')
  cfg$missing_years <- cfg$datasets[missing,c('survey','year'),drop=FALSE]
  cfg$datasets <- cfg$datasets[!missing,,drop=FALSE]
  for(s in settings$surveys) if(!any(cfg$datasets$survey==s)) stop('No datasets remain for ',s)
  cfg
}
attr(als_config,'missing_year_extension') <- TRUE
if (!isTRUE(attr(als_audit,'missing_year_extension'))) als_audit_available_years <- als_audit
als_audit <- function(cfg,settings) {
  checked <- als_audit_available_years(cfg,settings)
  skipped <- cfg$missing_years
  if(!is.null(skipped) && nrow(skipped)) for(i in seq_len(nrow(skipped))) {
    message('Skipping ',skipped$survey[i],'|',skipped$year[i],' (explicitly declared missing)')
    checked$audit <- rbind(checked$audit,als_issue(skipped$survey[i],skipped$year[i],'file','GAP',
      'Dataset explicitly skipped in settings$skip_datasets. No import or estimates; leave a gap in trends.'))
  }
  als_write_csv(checked$audit,file.path(settings$output_dir,'variable_audit.csv'))
  als_write_csv(skipped,file.path(settings$output_dir,'skipped_datasets.csv'))
  checked
}
attr(als_audit,'missing_year_extension') <- TRUE

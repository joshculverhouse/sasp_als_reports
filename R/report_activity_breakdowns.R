# Source after report_summaries.R and before report_confidence_intervals.R.
if(!isTRUE(attr(als_summarise_year,'activity_breakdowns'))) als_summarise_before_activity <- als_summarise_year
als_summarise_year <- function(saved,cfg,settings) {
  out <- als_summarise_before_activity(saved,cfg,settings)
  s <- saved$survey; y <- saved$year; d <- saved$data
  if(s=='adult' && y %in% c('2015/16','2016/17')) {
    a <- as.numeric(haven::zap_missing(d$age16plus)); d <- d[!is.na(a)&a==1,,drop=FALSE]
  }
  m <- cfg$variables[cfg$variables$survey==s & cfg$variables$year==y,,drop=FALSE]
  rules <- cfg$values[cfg$values$survey==s & cfg$values$year==y,,drop=FALSE]
  primary <- if(s=='adult') 'mems7gr_sportcount_a01' else 'mins_modplus_everywhere_week_gr3_all'
  targets <- m$variable[m$role=='measure' & m$status=='available' & m$weight_status!='review' &
    grepl('^wellb_|^lone|^indev|^comm|^ready|^motiv|^try_|^trust_|^pl_|^outdoor',m$variable)]
  pr <- rules[rules$variable==primary,,drop=FALSE]
  activity <- als_recode(d[[primary]],pr,primary)
  activity_levels <- unique(pr$analysis_label[pr$include=='TRUE'])
  code <- settings$local_code; key <- paste(s,y,sep='|')
  if(key %in% names(settings$local_codes)) code <- settings$local_codes[[key]]
  geo <- als_number(d$countycounc); areas <- list(England=rep(TRUE,nrow(d)))
  areas[[settings$local_area]] <- !is.na(geo)&geo==code
  cluster <- if(s=='cyp' && 'xcluster'%in%names(d)) haven::zap_missing(d$xcluster) else NULL
  for(v in targets) {
    row <- m[m$variable==v,,drop=FALSE]; vr <- rules[rules$variable==v,,drop=FALSE]
    response <- als_recode(d[[v]],vr,v); levels <- unique(vr$analysis_label[vr$include=='TRUE'])
    w <- als_number(haven::zap_missing(d[[row$weight]]))
    # Replace the pre-existing activity-within-response block with the jointly protected pair.
    out$estimates <- out$estimates[!(out$estimates$variable==primary & out$estimates$group_variable==v),,drop=FALSE]
    for(area in names(areas)) {
      forward <- als_counts(activity,response,w,areas[[area]],activity_levels,levels,s,y,primary,v,row$weight,settings,cluster,area!='England')
      reverse <- als_counts(response,activity,w,areas[[area]],levels,activity_levels,s,y,v,primary,row$weight,settings,cluster,area!='England')
      if(is.null(forward)||is.null(reverse)) next
      # Conservative linked-table protection: publishing both normalisations must
      # not reveal a small cell hidden in the other view. Withhold the whole
      # area/year/indicator matrix if either direction requires cell suppression.
      if(any(forward$status!='Available')||any(reverse$status!='Available')) {
        for(n in c('percent','sample_n','base_n')) {forward[[n]]<-NA_real_;reverse[[n]]<-NA_real_}
        forward$status <- reverse$status <- 'Suppressed: linked activity breakdown'
      }
      forward$area <- reverse$area <- area
      out$estimates <- rbind(out$estimates,forward,reverse)
    }
  }
  out
}
attr(als_summarise_year,'activity_breakdowns') <- TRUE

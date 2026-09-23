# Source after report_summaries.R. Verified scope: Somerset CYP overall activity,
# 2020/21, 2021/22 and 2022/23 only. Other results carry an explicit omission reason.
if(!isTRUE(attr(als_summarise_year,'ci_extension'))) als_summarise_without_ci <- als_summarise_year
als_summarise_year <- function(saved,cfg,settings) {
  result <- als_summarise_without_ci(saved,cfg,settings)
  result$estimates$lower_95 <- NA_real_; result$estimates$upper_95 <- NA_real_
  result$estimates$ci_status <- 'Not yet verified'
  if(saved$survey=='cyp' && saved$year=='2019/20') result$estimates$ci_status <- 'Unresolved: 2019/20 variance method'
  if(saved$survey!='cyp' || !saved$year %in% c('2020/21','2021/22','2022/23') || settings$local_area!='Somerset') return(result)
  if(!requireNamespace('survey',quietly=TRUE)) stop('Install the survey package to calculate the verified CYP headline intervals: install.packages("survey")')
  v <- 'mins_modplus_everywhere_week_gr3_all'; d <- saved$data
  m <- cfg$variables[cfg$variables$survey=='cyp' & cfg$variables$year==saved$year & cfg$variables$variable==v,,drop=FALSE]
  vr <- cfg$values[cfg$values$survey=='cyp' & cfg$values$year==saved$year & cfg$values$variable==v,,drop=FALSE]
  if(nrow(m)!=1 || m$weight_status=='review') return(result)
  if(!all(c('xstrata','xschyr_5_2',m$weight,'countycounc',v) %in% names(d))) stop('Verified CYP CI calculation needs xstrata, xschyr_5_2 and the approved weight in the cache.')
  number <- function(x) as.numeric(haven::zap_missing(x))
  local_code <- settings$local_code; key <- paste('cyp',saved$year,sep='|')
  if(key %in% names(settings$local_codes)) local_code <- settings$local_codes[[key]]
  geo <- number(d$countycounc)
  dat <- data.frame(response=als_recode(d[[v]],vr,v),weight=number(d[[m$weight]]),
    strata_geo=number(d$xstrata),strata_school=number(d$xschyr_5_2),local=!is.na(geo)&geo==local_code)
  dat <- dat[!is.na(dat$response)&is.finite(dat$weight)&dat$weight>0,,drop=FALSE]
  if(anyNA(dat[,c('strata_geo','strata_school')])) stop('Missing strata among eligible CYP respondents; CI not verified for this run.')
  dat$stratum <- interaction(dat$strata_geo,dat$strata_school,drop=TRUE)
  old <- options(survey.lonely.psu='remove',survey.adjust.domain.lonely=FALSE); on.exit(options(old),add=TRUE)
  design <- survey::svydesign(ids=~1,strata=~stratum,weights=~weight,data=dat)
  local <- subset(design,local)
  # Benchmarks are the previously reproduced results, not intervals inserted into the output.
  benchmark <- list(
    '2020/21'=rbind(c(34.1623,30.2904,38.2575),c(22.2078,18.7337,26.1191),c(43.6299,39.2847,48.0751)),
    '2021/22'=rbind(c(23.7210,20.7254,27.0022),c(26.0478,22.0974,30.4285),c(50.2312,45.7382,54.7204)),
    '2022/23'=rbind(c(24.4234,21.9304,27.1013),c(26.7677,23.9455,29.7922),c(48.8090,45.5587,52.0693)))
  categories <- c('Less active','Fairly active','Active')
  for(i in seq_along(categories)) {
    category <- categories[i]
    local$variables$indicator <- as.numeric(local$variables$response==category)
    estimate <- survey::svyciprop(~indicator,local,method='xlogit',df=Inf)
    calc <- c(as.numeric(coef(estimate)),as.numeric(confint(estimate)))*100
    idx <- which(result$estimates$variable==v & result$estimates$group_variable=='overall' & result$estimates$group=='All' & result$estimates$area==settings$local_area & result$estimates$response==category)
    if(length(idx)!=1) stop('Expected one CYP headline estimate per response')
    if(result$estimates$status[idx]!='Available') next
    if(abs(calc[1]-result$estimates$percent[idx])>1e-6) stop('CI and report point estimates disagree; check eligibility and weights.')
    if(any(abs(calc-benchmark[[saved$year]][i,])>.01)) {
      result$estimates$ci_status[idx] <- 'Review required: differs from verified benchmark'
      warning('CYP ',saved$year,' ',category,': CI differs from verified benchmark; interval withheld.',call.=FALSE)
    } else {
      result$estimates$lower_95[idx] <- calc[2]; result$estimates$upper_95[idx] <- calc[3]
      result$estimates$ci_status[idx] <- 'Verified'
    }
  }
  result
}
attr(als_summarise_year,'ci_extension') <- TRUE

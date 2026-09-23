# Small base-R JSON encoder; data frames are arrays of row objects.
als_json <- function(x) {
  quote <- function(s) {
    s <- gsub('\\','\\\\',enc2utf8(s),fixed=TRUE)
    s <- gsub('"','\\"',s,fixed=TRUE)
    if(grepl('[[:cntrl:]]',s)) for(i in 1:31) s <- gsub(intToUtf8(i),sprintf('\\u%04x',i),s,fixed=TRUE)
    paste0('"',s,'"')
  }
  if(is.null(x)) return('null')
  if(is.data.frame(x)) return(paste0('[',paste(vapply(seq_len(nrow(x)),function(i) als_json(as.list(x[i,,drop=FALSE])),''),collapse=','),']'))
  if(is.list(x)) {
    values <- vapply(x,als_json,'')
    if(!is.null(names(x))) return(paste0('{',paste(paste0(vapply(names(x),quote,''),':',values),collapse=','),'}'))
    return(paste0('[',paste(values,collapse=','),']'))
  }
  if(length(x)!=1) return(paste0('[',paste(vapply(as.list(x),als_json,''),collapse=','),']'))
  if(is.na(x)) return('null')
  if(is.logical(x)) return(if(x) 'true' else 'false')
  if(is.numeric(x)) return(if(is.finite(x)) format(x,digits=15,scientific=FALSE,trim=TRUE) else 'null')
  quote(as.character(x))
}
# Source after report_builder.R. Plotly's local JS bundle is embedded once per report.
als_plotly_bundle <- function() {
  path <- 'R/vendor/plotly-2.35.2.min.js'
  if(!file.exists(path)) stop('Missing ',path,'. Extract the complete als_plotly_fix.zip into the project root.')
  # Windows text connections interpret the literal Ctrl+Z in Plotly's PNG
  # signature as EOF. Read bytes to preserve the complete JavaScript library.
  size <- file.info(path)$size
  js <- rawToChar(readBin(path,what='raw',n=size))
  Encoding(js) <- 'UTF-8'
  # This occurrence is inside a JavaScript string; the escape has identical
  # runtime meaning and avoids Ctrl+Z in the generated HTML as well.
  js <- gsub(intToUtf8(26),'\\u001a',js,fixed=TRUE)
  if(!grepl('plotly.js v2.35.2',js,fixed=TRUE) || nchar(js,type='bytes')<1000000)
    stop('The bundled Plotly file is incomplete. Extract the update again.')
  # Force the standalone browser branch even when another script defines AMD/CommonJS.
  paste0('(function(define,module,exports){',js,'\n}).call(window,undefined,undefined,undefined);')
}
als_colour_theme <- function(d) {
  if(!nrow(d)) return('activity')
  v <- as.character(d$variable[1])
  if(grepl('^muscle|^vol',v)) return('purple')
  if(grepl('^days10p|^onceawk|^swm25|^schtrav',v)) return('types')
  if(grepl('^wellb|^lone|^indev|^comm|^try_|^trust_',v)) return('orange')
  if(grepl('^ready|^motiv|^pl_|^outdoor',v)) return('attitudes')
  'activity'
}
als_r_panel <- function(d,label,years,latest,area,notes='') {
  if(!nrow(d)) return(paste0('<details class="als-indicator"><summary>',als_r_escape(label),'</summary>',notes,'<p>No estimates available. See the year-specific notes.</p></details>'))
  suppress <- d$status!='Available' | !is.finite(d$percent)
  d$percent[suppress] <- NA_real_
  fields <- c('year','area','group','response','percent','status')
  payload <- list(denominator=attr(d,'denominator'),theme=als_colour_theme(d),activity_types=isTRUE(attr(d,'activity_types')),survey=as.character(d$survey[1]),label=label,latest=latest,years=unname(as.list(years)),areas=list(area,'England'),groups=unname(as.list(unique(d$group))),categories=unname(as.list(unique(d$response))),rows=d[,fields])
  json <- als_json(payload)
  json <- gsub('<','\\u003c',json,fixed=TRUE)
  table <- d[,intersect(c('year','area','group','response','percent','base_n'),names(d)),drop=FALSE]
  table$percent <- als_r_value(d)
  paste0('<details class="als-indicator als-plotly-panel als-theme-',als_colour_theme(d),'"><summary>',als_r_escape(label),'</summary>',notes,
    '<script type="application/json" class="als-spec">',json,'</script>',
    '<div class="als-tabs"><button type="button" class="selected">',als_r_escape(latest),'</button><button type="button">Trends</button><button type="button">Data table</button></div>',
    '<div class="als-interactive-body"><div class="als-controls"></div><div class="als-interactive-chart"></div><p class="als-summary"></p><p class="als-chart-error" role="alert" hidden></p></div>',
    '<div class="als-data-body" hidden>',als_r_table(table),'</div></details>')
}
if(!isTRUE(attr(als_report_content,'plotly_extension'))) als_report_content_base <- als_report_content
als_report_content <- function(survey,settings,config_dir='config',image_dir='images') {
  html <- als_report_content_base(survey,settings,config_dir,image_dir)
  html <- sub('Purple denotes the local area and grey denotes England. Hover over trend points for values.',
    'Somerset is selected first, with all groups shown. Use the Area and Group checkboxes to compare any selection, including both areas together. Somerset uses the section colour family; England uses blue shades, dashed trend lines and hatched bars. Hover for values and use Download PNG to save the current selection.',html,fixed=TRUE)
  runtime <- paste(readLines('R/report_interactive.js',warn=FALSE),collapse='\n')
  paste0('<script>',als_plotly_bundle(),'</script>',html,
    '<style>.als-help{margin:16px 0;padding:12px;border:1px solid #dbe3eb;border-radius:6px}.als-help>summary{cursor:pointer;font-weight:600}.als-dual>.als-view>.als-indicator>summary{display:none}.als-dual>.als-view>.als-indicator{border:0;margin:0}.als-denominator-label{display:block;padding:14px}.als-denominator{padding:8px}.als-controls{display:flex;flex-wrap:wrap;gap:12px;padding:14px}.als-controls select,.als-controls button{padding:9px;font-size:15px;max-width:100%}.als-summary{padding:0 18px;white-space:pre-line}.als-interactive-chart{width:100%;min-height:510px}.als-data-body{padding:14px}.als-controls fieldset{border:1px solid #c9d4df;border-radius:6px;min-width:180px;max-width:100%;display:flex;flex-wrap:wrap;gap:10px;padding:10px 14px}.als-controls legend{font-weight:650;color:#003E69}.als-controls fieldset label{display:inline-flex;align-items:center;gap:5px;font-size:14px;padding:3px}.als-controls input[type=checkbox]{width:17px;height:17px;accent-color:#003E69}.als-controls fieldset button{font-size:12px;padding:5px}.als-controls button:disabled{opacity:.5;cursor:default}.als-theme-activity{--als-section:#00A984}.als-theme-purple{--als-section:#A8589E}.als-theme-types{--als-section:#1D71B8}.als-theme-orange{--als-section:#EC6725}.als-theme-attitudes{--als-section:#E62249}.als-plotly-panel>summary{border-left:5px solid var(--als-section);color:#003E69}.als-plotly-panel .als-note{border-left-color:var(--als-section)}.als-controls [hidden]{display:none!important}</style><script>',runtime,'</script>')
}
attr(als_report_content,'plotly_extension') <- TRUE

als_activity_panel <- function(forward,reverse,label,years,latest,area) {
  if(!nrow(forward) || !nrow(reverse)) return(paste0('<details class="als-indicator"><summary>',als_r_escape(label),'</summary><p>This breakdown needs recalculating. Run the updated run_report.R.</p></details>'))
  attr(forward,'denominator') <- 'response category'
  attr(reverse,'denominator') <- 'activity group'
  views <- lapply(list(forward,reverse),function(d) {
    h <- als_r_panel(d,label,years,latest,area)
    sub('<details class="als-indicator als-plotly-panel','<details open class="als-indicator als-plotly-panel',h,fixed=TRUE)
  })
  # The hidden second view is initialised on demand, avoiding zero-width rendering.
  views[[2]] <- sub('<details open','<details',views[[2]],fixed=TRUE)
  paste0('<details class="als-indicator als-dual"><summary>',als_r_escape(label),'</summary>',
    '<label class="als-denominator-label">Show percentages within <select class="als-denominator" onchange="alsDenominator(this)"><option value="0">Response category</option><option value="1">Activity group</option></select></label>',
    '<div class="als-view">',views[[1]],'</div><div class="als-view" hidden>',views[[2]],'</div></details>')
}

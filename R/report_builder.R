# Self-contained HTML report content shared by the Quarto templates and preview runner.
als_r_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  for(pair in list(c("&","&amp;"),c("<","&lt;"),c(">","&gt;"),c('"',"&quot;")))
    x <- gsub(pair[1],pair[2],x,fixed=TRUE)
  x
}
als_r_table <- function(d) {
  d <- d[,!names(d) %in% c('status','reliability_review'),drop=FALSE]
  if(!nrow(d)) return('<p>No estimates available.</p>')
  d[] <- lapply(d,as.character)
  rows <- apply(d,1,function(z) paste0('<tr><td>',paste(als_r_escape(z),collapse='</td><td>'),'</td></tr>'))
  paste0('<div class="als-scroll"><table><thead><tr><th>',paste(als_r_escape(names(d)),collapse='</th><th>'),
         '</th></tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>')
}
als_r_value <- function(d) ifelse(d$status == "Available" & is.finite(d$percent),sprintf("%.1f%%",d$percent),d$status)
als_r_catalog <- function(s,vars) {
  activity <- if(s=='adult') 'mems7gr_sportcount_a01' else 'mins_modplus_everywhere_week_gr3_all'
  sections <- if(s=='adult') list(
    'Levels of activity'=activity,
    'Muscle strengthening'='muscle7gr2_sportcount_a01',
    'Types of activity'=grep('^days10p60gr_',vars,value=TRUE),
    'Wellbeing and development'=grep('^wellb_|^lone$|^indev|^comm',vars,value=TRUE),
    'Attitudes and motivation'=grep('^ready|^motiv',vars,value=TRUE),
    'Volunteering'=c('volany','volfrqb_pop','voldur_gr2')) else list(
    'Levels of activity'=activity,
    'Types of activity'=c(grep('^onceawk_',vars,value=TRUE),'swm25can_bcd_pop',grep('^schtrav_',vars,value=TRUE)),
    'Wellbeing and development'=grep('^wellb_|^lonely|^try_|^trust_',vars,value=TRUE),
    'Attitudes and opportunities'=c(grep('^pl_',vars,value=TRUE),'outdoor_bcd_overall'),
    'Volunteering'='voln_bc_pop')
  lapply(sections,unique)
}
als_r_label <- function(v,m) {
  labels <- c(mems7gr_sportcount_a01='Overall activity',mins_modplus_everywhere_week_gr3_all='Overall activity',
    muscle7gr2_sportcount_a01='Muscle strengthening',swm25can_bcd_pop='Swimming 25 metres unaided',
    wellb_happy_gr='Happiness',wellb_lifesat_gr='Life satisfaction',wellb_worthw_gr='Feeling worthwhile',
    wellb_anxious_gr='Anxiety',wellb_numberhigh_gr4='Number of high wellbeing scores',lone='Loneliness',
    lonely_c='Loneliness: frequency',lonely_gr='Often or always lonely',try_bc_sa_gr2='Resilience',
    trust_bc_sa_gr2='Trust',gend3='Gender',age4='Age',nssec5='Socioeconomic group',eth2='Ethnicity',
    disab2_pop='Disability',disab_all_pop='Disability',fasbandf='Family affluence',schyr_5_2='School year',
    voln_bc_pop='Volunteering twice in the last 12 months',volany='Any volunteering in the last 12 months')
  if(v %in% names(labels)) return(unname(labels[v]))
  r <- m[m$variable == v,,drop=FALSE]; r <- r[order(r$year,decreasing=TRUE),,drop=FALSE]
  if(nrow(r)) r$label[1] else v
}
als_r_notes <- function(s,v,cfg,years) {
  n <- cfg$notes[cfg$notes$survey == s & cfg$notes$variable %in% c(v,'all','*',''),,drop=FALSE]
  if(!nrow(n)) return('')
  n <- n[vapply(strsplit(n$years,';',fixed=TRUE),function(y) any(trimws(y) %in% years) || any(y %in% c('all','All','*','')),TRUE),,drop=FALSE]
  paste(vapply(seq_len(nrow(n)),function(i) paste0('<aside class="als-note"><strong>',als_r_escape(n$years[i]),':</strong> ',
    als_r_escape(n$note[i]),' <a href="#source-',als_r_escape(n$source_id[i]),'">',als_r_escape(n$source_id[i]),'</a>',
    if(nzchar(n$section[i])) paste0(' — ',als_r_escape(n$section[i])) else '', '</aside>'),''),collapse='')
}
als_r_plot <- function(d,years,area,trend=FALSE) {
  if(!nrow(d)) return('<p class="als-note">No estimates are available for this selection.</p>')
  d <- d[d$area %in% c(area,'England'),,drop=FALSE]
  categories <- unique(d$response)
  colours <- setNames(c('#85448a','#687887'),c(area,'England'))
  e <- als_r_escape
  text <- function(x,y,label,size=14,anchor='start') sprintf('<text x="%s" y="%s" font-size="%s" text-anchor="%s">%s</text>',x,y,size,anchor,e(label))
  svg <- character()
  for(category in categories) {
    a <- d[d$response==category,,drop=FALSE]
    groups <- unique(a$group)
    if(trend) {
      for(g in groups) {
        z <- a[a$group == g,,drop=FALSE]
        title <- paste(category,if(g!='All') paste('—',g) else '')
        body <- c(text(60,24,title,16),text(60,46,paste(area,'(purple) · England (grey)'),12))
        xs <- seq(85,870,length.out=max(2,length(years)))[seq_along(years)]
        for(tick in seq(0,100,25)) {
          yy <- 265-tick*1.9
          body <- c(body,sprintf('<path d="M85 %s H870" stroke="#e0e5eb"/>',yy),text(75,yy+4,paste0(tick,'%'),12,'end'))
        }
        for(i in seq_along(years)) body <- c(body,text(xs[i],291,years[i],11,'middle'))
        for(ar in c(area,'England')) {
          zz <- z[z$area==ar,,drop=FALSE]; idx <- match(years,zz$year)
          p <- zz$percent[idx]; ok <- !is.na(idx) & !is.na(zz$status[idx]) & zz$status[idx]=='Available' & is.finite(p)
          for(i in seq_along(years)) if(ok[i]) {
            if(i>1 && ok[i-1]) body <- c(body,sprintf('<path d="M%f %f L%f %f" stroke="%s" stroke-width="2"/>',xs[i-1],265-p[i-1]*1.9,xs[i],265-p[i]*1.9,colours[ar]))
            body <- c(body,sprintf('<circle tabindex="0" cx="%f" cy="%f" r="5" fill="%s"><title>%s</title></circle>',xs[i],265-p[i]*1.9,colours[ar],e(paste(ar,years[i],sprintf('%.1f%%',p[i])))))
          }
        }
        svg <- c(svg,paste0('<svg class="als-chart" viewBox="0 0 950 310" role="img"><title>',e(title),' trend. Missing or suppressed values create gaps.</title>',paste(body,collapse=''),'</svg>'))
      }
    } else {
      for(g in groups) {
        z <- a[a$group==g,,drop=FALSE]
        body <- c(text(25,23,paste(category,if(g!='All') paste('—',g) else ''),16))
        for(i in 1:2) {
          ar <- c(area,'England')[i]; r <- z[z$area==ar,,drop=FALSE]; y <- 35+i*38
          body <- c(body,text(25,y+5,ar,14),sprintf('<path d="M190 %s H770" stroke="#e2e6ec" stroke-width="10"/>',y))
          if(nrow(r) && r$status[1]=='Available' && is.finite(r$percent[1]))
            body <- c(body,sprintf('<circle cx="%f" cy="%s" r="7" fill="%s"><title>%s</title></circle>',190+5.8*r$percent[1],y,colours[ar],e(als_r_value(r)[1])))
          body <- c(body,text(795,y+5,if(nrow(r)) als_r_value(r)[1] else 'Unavailable',14))
        }
        body <- c(body,text(190,140,'0%',11),text(770,140,'100%',11,'end'))
        svg <- c(svg,paste0('<svg class="als-chart" viewBox="0 0 950 152" role="img"><title>',e(category),' comparison</title>',paste(body,collapse=''),'</svg>'))
      }
    }
  }
  paste(svg,collapse='')
}
als_r_panel <- function(d,label,years,latest,area,notes='') {
  latest_d <- d[d$year==latest,,drop=FALSE]
  table <- d[,intersect(c('year','area','group','response','percent','base_n','status','reliability_review'),names(d)),drop=FALSE]
  if(nrow(table)) {
    table$percent <- als_r_value(d)
    names(table) <- c(year='Year',area='Area',group='Group',response='Response',percent='Percentage',base_n='Rounded base',status='Status',reliability_review='Reliability review')[names(table)]
  }
  paste0('<details class="als-indicator"><summary>',als_r_escape(label),'</summary>',notes,
    '<div class="als-tabs"><button type="button" class="selected" onclick="alsTab(this,0)">',als_r_escape(latest),'</button>',
    '<button type="button" onclick="alsTab(this,1)">Trends</button><button type="button" onclick="alsTab(this,2)">Data table</button></div>',
    '<div class="als-pane">',als_r_plot(latest_d,years,area),'</div>',
    '<div class="als-pane" hidden>',als_r_plot(d,years,area,TRUE),'</div>',
    '<div class="als-pane" hidden>',als_r_table(table),'</div></details>')
}
als_r_image <- function(file,alt) {
  if(!file.exists(file)) return(paste0('<p class="als-note">Definition image unavailable: ',als_r_escape(basename(file)),'</p>'))
  raw <- as.integer(readBin(file,'raw',n=file.info(file)$size)); n <- length(raw)
  raw <- c(raw,rep(0,(3-n%%3)%%3)); m <- matrix(raw,nrow=3)
  codes <- rbind(m[1,]%/%4,(m[1,]%%4)*16+m[2,]%/%16,(m[2,]%%16)*4+m[3,]%/%64,m[3,]%%64)
  alphabet <- strsplit('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/','',fixed=TRUE)[[1]]
  encoded <- alphabet[as.vector(codes)+1]
  if(n%%3) encoded[(length(encoded)-(3-n%%3)+1):length(encoded)] <- '='
  mime <- if(grepl('\\.png$',file,ignore.case=TRUE)) 'image/png' else 'image/jpeg'
  paste0('<img class="als-definition" alt="',als_r_escape(alt),'" src="data:',mime,';base64,',paste(encoded,collapse=''),'"/>')
}
als_r_style <- function() paste0('<style>
.als-report{font:16px/1.6 system-ui,Arial,sans-serif;color:#283344;max-width:1120px;margin:auto;padding:24px}.als-report *{box-sizing:border-box}.als-report h1,.als-report h2{color:#155cac;line-height:1.2}.als-report h1{font-size:36px}.als-report h2{margin-top:48px;padding-top:18px;border-top:2px solid #e6edf4}.als-report h3{font-size:21px}.als-report nav{display:flex;flex-wrap:wrap;gap:10px;margin:22px 0}.als-report nav a{color:#155cac;background:#eef4fa;padding:6px 12px;text-decoration:none;border-radius:6px}.als-note{background:#f4f0f7;border-left:4px solid #85448a;padding:12px 16px;margin:12px 0;font-size:14px}.als-warning{background:#fff5dc;border-left-color:#a47400}.als-indicator{border:1px solid #dbe3eb;border-radius:8px;margin:16px 0;overflow:hidden}.als-indicator>summary{padding:15px 18px;font-weight:650;cursor:pointer;background:#f5f8fc;overflow-wrap:anywhere}.als-pane{padding:14px}.als-tabs{display:flex;gap:8px;padding:12px;border-bottom:1px solid #e3e8ef}.als-tabs button{border:0;padding:10px 18px;border-radius:6px;background:#edf0f4;cursor:pointer;font-size:14px}.als-tabs button.selected{background:#155cac;color:white}.als-chart{display:block;width:100%;height:auto;font-family:Arial,sans-serif;fill:#283344}.als-scroll{overflow:auto;max-height:500px}.als-report table{border-collapse:collapse;font-size:13px;width:100%}.als-report th,.als-report td{padding:9px;text-align:left;border-bottom:1px solid #e1e6ed;min-width:70px}.als-report th{background:#edf3fa;position:sticky;top:0}.als-definition{max-width:100%;height:auto;display:block;margin:18px auto}.als-logo{max-width:170px;margin-left:auto}.als-source{overflow-wrap:anywhere;font-size:13px}.als-report [hidden]{display:none!important}@media(max-width:650px){.als-report{padding:12px}.als-report h1{font-size:27px}.als-pane{overflow-x:auto}.als-chart{min-width:650px}.als-tabs button{padding:8px}}@media print{.als-tabs{display:none}.als-pane[hidden]{display:block!important}.als-report{max-width:none}}
</style><script>function alsTab(b,n){const p=b.closest("details");const tabs=p.querySelector(".als-tabs");Array.from(tabs.children).forEach((x,i)=>{x.classList.toggle("selected",i===n);x.setAttribute("aria-pressed",i===n)});Array.from(p.children).filter(x=>x.classList.contains("als-pane")).forEach((x,i)=>x.hidden=i!==n)}</script>')
als_report_content <- function(survey,settings,config_dir='config',image_dir='images') {
  read <- function(f) read.csv(f,stringsAsFactors=FALSE,check.names=FALSE,na.strings='')
  estimates <- read(file.path(settings$output_dir,'estimates.csv'))
  cfg <- lapply(setNames(c('variables','notes','sources'),c('variables','notes','sources')),function(n) {
    z <- read(file.path(config_dir,paste0(n,'.csv'))); z[] <- lapply(z,function(x) {x[is.na(x)]<-'';x});z
  })
  latest <- settings$latest_year; area <- settings$local_area
  estimates$percent <- as.numeric(estimates$percent)
  estimates <- estimates[estimates$survey==survey & estimates$year<=latest & estimates$area %in% c(area,'England'),,drop=FALSE]
  if(!nrow(estimates) || !latest %in% estimates$year) stop('No estimates for requested survey/latest year')
  key <- estimates[,c('year','area','variable','group_variable','group','response')]
  if(anyDuplicated(key)) stop('Duplicate estimate rows: resolve before reporting')
  if(any(estimates$status=='Available' & (!is.finite(estimates$percent)|estimates$percent<0|estimates$percent>100))) stop('Invalid available percentages')
  estimates$percent[estimates$status!='Available'] <- NA_real_
  starts <- seq.int(as.integer(substr(min(estimates$year),1,4)),as.integer(substr(latest,1,4)))
  years <- sprintf('%04d/%02d',starts,(starts+1L)%%100L)
  missing_years <- setdiff(years,unique(estimates$year))
  m <- cfg$variables[cfg$variables$survey==survey & cfg$variables$year<=latest,,drop=FALSE]
  catalog <- als_r_catalog(survey,unique(m$variable[m$role=='measure']))
  e <- als_r_escape; parts <- c(als_r_style(),'<article class="als-report">')
  parts <- c(parts,paste0('<div class="als-logo">',als_r_image(file.path(image_dir,'SASP_Logo_Blue_PNG.png'),'SASP logo'),'</div>'),
    paste0('<h1>Active Lives ',if(survey=='adult') 'Adult' else 'Children and Young People',' Survey</h1><p>',e(area),' and England · ',e(latest),' · Working draft</p>'))
  parts <- c(parts,paste0('<nav aria-label="Report sections">',paste(vapply(seq_along(catalog),function(i) paste0('<a href="#section-',i,'">',e(names(catalog)[i]),'</a>'),''),collapse=''),'<a href="#data-notes">Definitions and notes</a></nav>'))
  help <- paste0('<details class="als-help"><summary>About the data and how to use this report</summary>',
    '<p>These are weighted estimates from Sport England’s Active Lives Survey. Each measure uses eligible respondents with valid answers and an approved positive weight.</p>',
    '<p>Select Somerset, England or both; select groups; switch between current-year charts, trends and tables. Download PNG saves the displayed chart. England uses blue shades, hatched bars and dashed trend lines.</p>',
    '<p>For wellbeing and motivation, choose whether percentages are calculated within each response category or within each activity group. Filtering visible groups does not recalculate denominators.</p>',
    '<p>Changes are descriptive and are not significance tests. Missing values are not zero.</p></details>')
  parts <- c(parts,'<h2>Executive summary</h2>',als_headline_arrow(estimates,survey,latest,area),help)
  if(length(missing_years)) parts <- c(parts,paste0('<aside class="als-note"><strong>Missing survey data:</strong> ',e(paste(missing_years,collapse=', ')),'. No data are included for these years. Trend charts show gaps; no values are estimated or carried forward.</aside>'))
  primary <- catalog[[1]][1]
  active <- estimates[estimates$variable==primary & estimates$group_variable=='overall' & estimates$response=='Active' & estimates$status=='Available',,drop=FALSE]
  cur <- active[active$year==latest & active$area==area,,drop=FALSE]
  england <- active[active$year==latest & active$area=='England',,drop=FALSE]
  if(nrow(cur)==1 && nrow(england)==1) parts <- c(parts,sprintf('<p>In %s, <strong>%.1f%%</strong> were active in %s, compared with <strong>%.1f%%</strong> in England.</p>',e(latest),cur$percent,e(area),england$percent))
  previous <- if(length(years)>1) years[length(years)-1] else ''
  prev <- active[active$year==previous & active$area==area,,drop=FALSE]
  if(nrow(cur)==1 && nrow(prev)==1) parts <- c(parts,sprintf('<p>The local active percentage changed by <strong>%+.1f percentage points</strong> from %s (%.1f%%). This is a descriptive comparison.</p>',cur$percent-prev$percent,e(previous),prev$percent))
  images <- if(survey=='adult') c('adult_pa_def-1.png','adult_mus_def-1.png','adult_type_def-1.png','adult_dev_def-1.png','adult_att_def-1.png','adult_vol_def-1.png') else
    c('als_pa_definition.jpg','als_type_definition.jpg','als_wb_definition.jpg','als_pl_definition.jpg','als_vol_definition.jpg')
  demographic <- if(survey=='adult') c('gend3','age4','nssec5','disab2_pop','eth2') else c('gend3','schyr_5_2','fasbandf','disab_all_pop','eth2')
  for(i in seq_along(catalog)) {
    section_colours <- c('Levels of activity'='#00A984','Muscle strengthening'='#A8589E','Types of activity'='#1D71B8','Wellbeing and development'='#EC6725','Attitudes and motivation'='#E62249','Attitudes and opportunities'='#E62249','Volunteering'='#A8589E')
    parts <- c(parts,paste0('<h2 style="border-top:5px solid ',section_colours[names(catalog)[i]],';color:#003E69" id="section-',i,'">',e(names(catalog)[i]),'</h2>'),als_r_image(file.path(image_dir,images[i]),paste(names(catalog)[i],'definition')))
    section_vars <- catalog[[i]]
    type_vars <- grep(if(survey=='adult') '^days10p60gr_' else '^onceawk_',section_vars,value=TRUE)
    if(length(type_vars)) {
      td <- estimates[estimates$variable %in% type_vars & estimates$group_variable=='overall',,drop=FALSE]
      positive <- if(survey=='adult') 'Participated at least twice in the last 28 days' else 'Participated once a week or more'
      td <- td[td$response==positive,,drop=FALSE]
      type_labels <- c(
        days10p60gr_acttrav_c03='Active travel',days10p60gr_cycleissport_b03='Cycling',days10p60gr_walkleisure_b01='Walking for leisure',
        days10p60gr_teamsport_c05='Team sports',days10p60gr_swimjust_u01='Swimming',days10p60gr_runjog_u07='Running / jogging',days10p60gr_fitness_b06='Fitness activities',
        onceawk_modplus_everywhere_gr_acttrav_ca210='Active travel',onceawk_modplus_everywhere_gr_actplay_ca140='Active play',
        onceawk_modplus_everywhere_gr_walkleisure_cc002='Walking for leisure',onceawk_modplus_everywhere_gr_dance_ca170='Dance',
        onceawk_modplus_everywhere_gr_swim_cb156='Swimming, diving and water polo',onceawk_modplus_everywhere_gr_teamsport_cb151='Team sports',
        onceawk_modplus_everywhere_gr_runathmulti_cb154='Running, athletics or multi-sports')
      td$group <- unname(type_labels[td$variable])
      if(anyNA(td$group)) stop('Missing activity type display label')
      # Stable order across both areas and years, including absent latest-year rows.
      td <- td[order(match(td$variable,type_vars),td$year,td$area),,drop=FALSE]
      attr(td,'activity_types') <- TRUE
      tn <- ''
      tn <- paste0('<aside class="als-note">',positive,'. Each activity is a separate measure; percentages do not sum to 100%.</aside>',tn)
      parts <- c(parts,als_r_panel(td,'Participation by activity type',years,latest,area,tn))
      section_vars <- setdiff(section_vars,type_vars)
    }
    for(v in section_vars) {
      d <- estimates[estimates$variable==v & estimates$group_variable=='overall',,drop=FALSE]
      notes <- ''
      current <- m[m$variable==v & m$year==latest,,drop=FALSE]
      if(!nrow(d[d$year==latest,,drop=FALSE])) notes <- paste0('<p class="als-note">',if(nrow(current) && any(current$weight_status=='review')) 'Current-year results await weighting review.' else 'Not currently available in this dataset; historical results are retained. This does not confirm that the survey question was discontinued.',' See <a href="#data-notes">data notes</a>.</p>')
      parts <- c(parts,als_r_panel(d,als_r_label(v,m),years,latest,area,notes))
      if(names(catalog)[i] %in% c('Wellbeing and development','Attitudes and motivation','Attitudes and opportunities')) {
        f <- estimates[estimates$variable==primary & estimates$group_variable==v,,drop=FALSE]
        r <- estimates[estimates$variable==v & estimates$group_variable==primary,,drop=FALSE]
        parts <- c(parts,als_activity_panel(f,r,paste(als_r_label(v,m),'and activity level'),years,latest,area))
      }
      if(v %in% c(primary,'muscle7gr2_sportcount_a01','swm25can_bcd_pop')) {
        groups <- if(v=='swm25can_bcd_pop') intersect(demographic,c('gend3','schyr_5_2','fasbandf')) else demographic
        for(g in groups) {
          gd <- estimates[estimates$variable==v & estimates$group_variable==g,,drop=FALSE]
          gn <- ''
          if(!nrow(gd) && v != primary) gn <- paste0(gn,'<aside class="als-note">This breakdown is not in the current estimates file. Run the updated run_report.R to generate it from the cached data.</aside>')
          parts <- c(parts,als_r_panel(gd,paste(als_r_label(v,m),'by',als_r_label(g,m)),years,latest,area,gn))
        }
      }
    }
  }
  parts <- c(parts,'<h2 id="data-notes">Definitions, data notes and sources</h2>',
    '<details class="als-help"><summary>Definitions and interpretation</summary><p>Percentages may not sum to 100% because of rounding. Filter selections do not change denominators. Breakdown percentages use respondents with valid answers to both questions and the outcome question’s weight.</p><p>Small cells and complementary cells are suppressed. For the two-way activity comparisons, if either view requires suppression, both views for that indicator, area and year are withheld to protect the same underlying cells.</p>')
  parts <- c(parts,'<p>Confidence intervals are not presented because verified intervals are available for only a limited subset of results.</p>')
  parts <- c(parts,'</details><details class="als-help"><summary>Survey changes and measure availability</summary>')
  n <- cfg$notes[cfg$notes$survey==survey,,drop=FALSE]
  if(nrow(n)) for(txt in unique(n$note)) {
    z <- n[n$note==txt,,drop=FALSE]
    refs <- unique(z$source_id)
    parts <- c(parts,paste0('<p><strong>',e(paste(unique(z$years),collapse='; ')),':</strong> ',e(txt),' ',
      paste(paste0('<a href="#source-',e(refs),'">',e(refs),'</a>'),collapse='; '),'</p>'))
  }
  # Group repeated mapping notes across variables and years into one entry.
  nm <- m[nzchar(m$note),,drop=FALSE]
  if(nrow(nm)) for(txt in unique(nm$note)) {
    z <- nm[nm$note==txt,,drop=FALSE]
    parts <- c(parts,paste0('<p><strong>',e(paste(unique(z$year),collapse=', ')),': ',e(paste(unique(vapply(z$variable,als_r_label,'',m=m)),collapse='; ')),'.</strong> ',e(txt),'</p>'))
  }
  gaps <- m[m$status!='available' | m$weight_status=='review',c('year','variable','status','weight_status'),drop=FALSE]
  parts <- c(parts,'<details><summary>Availability by year</summary>',als_r_table(gaps),'</details></details>',
    '<details class="als-help"><summary>Sources</summary>')
  src <- cfg$sources[cfg$sources$survey==survey & cfg$sources$year<=latest,,drop=FALSE]
  for(i in seq_len(nrow(src))) parts <- c(parts,paste0('<p class="als-source" id="source-',e(src$source_id[i]),'"><strong>',e(src$source_id[i]),'</strong> ',e(src$citation[i]),
    if(grepl('^https?://',src$url[i])) paste0(' <a href="',e(src$url[i]),'">Source record</a>') else '', '</p>'))
  parts <- c(parts,'</details>')
  paste(c(parts,'</article>'),collapse='\n')
}
als_render_reports <- function(settings,config_dir='config',image_dir='images') {
  paths <- character()
  for(s in settings$surveys) {
    body <- als_report_content(s,settings,config_dir,image_dir)
    path <- file.path(settings$output_dir,paste0(s,'_',gsub('/','',settings$latest_year),'_report.html'))
    writeLines(paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>',
      als_r_escape(paste('Active Lives',s,settings$latest_year)),'</title></head><body>',body,'</body></html>'),path,useBytes=TRUE)
    paths <- c(paths,path)
  }
  message('Rebuilt reports created:\n',paste(paths,collapse='\n'))
  invisible(paths)
}

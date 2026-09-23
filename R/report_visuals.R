# Data-driven headline arrow. Base R only; returns SVG for an HTML Quarto report.
als_headline_arrow <- function(estimates, survey, year, local_area = "Somerset",
                               note = character()) {
  stopifnot(length(survey) == 1L, survey %in% c("adult", "cyp"),
            length(year) == 1L, length(local_area) == 1L, local_area != "England")
  required <- c("survey", "year", "variable", "group_variable", "group",
                "area", "response", "percent", "status")
  if (!all(required %in% names(estimates))) stop("estimates.csv lacks required columns")
  esc <- function(x) {
    x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  primary <- if (survey == "adult") "mems7gr_sportcount_a01" else "mins_modplus_everywhere_week_gr3_all"
  categories <- c(if (survey == "adult") "Inactive" else "Less active", "Fairly active", "Active")
  keep <- estimates$survey == survey & estimates$year == year &
    estimates$variable == primary & estimates$group_variable == "overall" & estimates$group == "All"
  d <- estimates[which(keep), , drop = FALSE]
  areas <- c(local_area, "England")
  labels <- matrix("Unavailable", 2, 3)
  for (i in seq_along(areas)) {
    a <- d[which(d$area == areas[i]), , drop = FALSE]
    if (anyDuplicated(a$response)) stop("Duplicate headline categories for ", areas[i])
    if (any(!a$response %in% categories)) stop("Unexpected headline response labels; inspect estimates.csv")
    for (j in seq_along(categories)) {
      r <- a[which(a$response == categories[j]), , drop = FALSE]
      if (!nrow(r)) next
      if (identical(as.character(r$status), "Suppressed")) labels[i,j] <- "Suppressed"
      if (identical(as.character(r$status), "Available")) {
        p <- suppressWarnings(as.numeric(as.character(r$percent)))
        if (!is.finite(p) || p < 0 || p > 100) stop("Invalid available headline percentage")
        labels[i,j] <- sprintf("%.1f%%", p)
      }
    }
  }
  title <- paste(local_area, "and England", year, "activity levels")
  description <- paste(vapply(1:2, function(i) paste(areas[i], paste(categories, labels[i,], collapse = "; ")), ""), collapse = ". ")
  out <- c('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1000 405" role="img" style="width:100%;height:auto;max-width:1100px">',
           paste0('<title>',esc(title),'</title><desc>',esc(description),'. Segment widths are decorative, not proportional.</desc>'),
           '<rect width="1000" height="405" fill="white"/>')
  txt <- function(x,y,text,size=20,fill="#333333",anchor="middle",weight="normal")
    sprintf('<text x="%s" y="%s" text-anchor="%s" font-family="Arial, sans-serif" font-size="%s" font-weight="%s" fill="%s">%s</text>',x,y,anchor,size,weight,fill,esc(text))
  out <- c(out, txt(30,32,paste("Activity levels",year),23,anchor="start",weight="bold"))
  bounds <- c(30,315,580,945); centres <- (head(bounds,-1)+tail(bounds,-1))/2+14
  for (j in 1:3) out <- c(out,txt(centres[j],75,categories[j],24,fill="#005F4B",weight="bold"))
  colours <- list(c("#A7D5C2","#5CBA9D","#00A984"),c("#7699BC","#376C98","#003E69"))
  for (i in 1:2) {
    y <- c(115,250)[i]; h <- c(95,58)[i]
    out <- c(out,txt(30,y-10,areas[i],17,anchor="start",weight="bold"))
    for (j in 1:3) {
      l <- bounds[j]; r <- bounds[j+1]; tip <- 24
      points <- sprintf('%s,%s %s,%s %s,%s %s,%s %s,%s %s,%s',l,y,r,y,r+tip,y+h/2,r,y+h,l,y+h,l+tip,y+h/2)
      out <- c(out,sprintf('<polygon points="%s" fill="%s"/>',points,colours[[i]][j]),
               txt(centres[j],y+h/2+10,labels[i,j],if(nchar(labels[i,j])>6) 21 else c(36,27)[i],
                   fill=if(i==2 && j>=2) "white" else "#16352E",weight="bold"))
    }
  }
  thresholds <- if(survey=="adult") c("Under 30 minutes per week","30–149 minutes per week","150+ minutes per week") else
    c("Under 30 minutes per day","30–59 minutes per day","60+ minutes per day")
  for(j in 1:3) out <- c(out,txt(centres[j],346,thresholds[j],17))
  out <- c(out,txt(30,382,"Weighted percentages. Arrow widths do not represent percentages.",14,anchor="start"),'</svg>')
  if(length(note)) out <- c(out,paste0('<p class="als-data-note"><strong>Data note:</strong> ',paste(esc(note),collapse=" "),'</p>'))
  paste(out,collapse="\n")
}

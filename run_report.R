# Run from the project root in RStudio. Edit report_settings.R each year.
source("report_settings.R")
source("R/annual_pipeline.R")
source("R/report_missing_years.R")
source("R/report_summaries.R")
source("R/report_activity_breakdowns.R")
source("R/report_confidence_intervals.R")
als_run(settings)
if (settings$mode == "report") {
  source("R/report_visuals.R")
  source("R/report_builder.R")
  source("R/report_plotly.R")
  als_render_reports(settings)
}

# Run once from the project root. Adds/replaces 2024/25 configuration only.
files <- c('variables.csv', 'values.csv', 'sources.csv')
read_config <- function(p) read.csv(p, colClasses='character', check.names=FALSE, na.strings=NULL)
updates <- lapply(files, function(n) {
  old <- read_config(file.path('config',n))
  add <- read_config(file.path('config_202425',n))
  stopifnot(setequal(names(old),names(add)))
  keep <- !(old$survey %in% c('adult','cyp') & old$year=='2024/25')
  rbind(old[keep,,drop=FALSE],add[,names(old),drop=FALSE])
})
backup <- tempfile('before_202425_',tmpdir='config')
dir.create(backup)
for (i in seq_along(files)) {
  path <- file.path('config',files[i])
  stopifnot(file.copy(path,file.path(backup,files[i])))
  write.csv(updates[[i]],path,row.names=FALSE,na='',fileEncoding='UTF-8')
}
message('2024/25 configuration installed. Previous files saved in ',backup)
message('Keep skip_datasets for 2023/24. Source run_report.R next.')

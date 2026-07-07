#Set our working directory to the folder this script lives in (docs/).
#Resolving it from the script's own location keeps this working no matter
#where the repo sits on disk or what the current working directory is.
this_file <- tryCatch(
  # When run via Rscript / source(): use the --file= arg or the sourced path.
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]),
    mustWork = TRUE),
  error = function(e) NULL
)
if (is.null(this_file) || is.na(this_file)) {
  # Fallback for RStudio "Source" button, which sets this variable.
  this_file <- normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE)
}
setwd(dirname(this_file))

#If pandoc isn't already on the path (e.g. running from a plain terminal
#rather than RStudio), point R at RStudio's bundled copy.
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
}

#render your sweet site.
rmarkdown::render_site()


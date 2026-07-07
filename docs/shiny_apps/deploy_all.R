# Consolidated shinylive export for ALL STAT251 apps.
#
# Why this exists: previously each app was exported on its own into its own
# `webfiles/` folder, so every app shipped a FULL ~30MB copy of the webR runtime
# at a different URL. The browser cache could not share them, so opening each app
# paid the full cold-start download again.
#
# This script exports every app into a SINGLE shinylive site (shiny_apps/site/),
# with each app in its own subdir. shinylive writes the shared runtime ONCE at
# site/shinylive/ and every app references it. Students download the runtime only
# on the first app they open; every other app then loads from cache.
#
# Run from the repo root (working dir = repo root):
#   Rscript docs/shiny_apps/deploy_all.R
# or from an R session:
#   source("docs/shiny_apps/deploy_all.R")
#
# Requires the `shinylive` package: install.packages("shinylive")

library(shinylive)

# Resolve the shiny_apps dir relative to this script so it works regardless of
# the working directory.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(file_arg) == 1) {
  base <- normalizePath(dirname(file_arg), winslash = "/", mustWork = TRUE)
} else {
  # Fallback when source()'d interactively.
  base <- normalizePath("docs/shiny_apps", winslash = "/", mustWork = TRUE)
}

site <- file.path(base, "site")

# App source folders (each contains a single app.R). The subdir under site/ that
# an app is served from is the same as its folder name.
apps <- c(
  "Convert_file_to_text",
  "HistoPlot",
  "Stat_calculator",
  "Freq_table",
  "Make_Graph",
  "LLN",
  "Compute_probs",
  "one_sample_hypothesis_tests",
  "two_sample_hypothesis_tests",
  "categorical_tests",
  "non_parametric_tests",
  "Regression"
)

# Start from a clean site so removed/renamed apps don't linger.
if (dir.exists(site)) unlink(site, recursive = TRUE)
dir.create(site, recursive = TRUE)

for (app in apps) {
  app_r <- file.path(base, app, "app.R")
  if (!file.exists(app_r)) {
    warning("Skipping ", app, ": no app.R found at ", app_r)
    next
  }
  message("Exporting ", app, " ...")

  # Export from a clean temp dir containing ONLY app.R, so the old webfiles/ and
  # deploy scripts sitting in the source folder are never bundled into the app.
  src <- file.path(tempdir(), paste0("stat251_", app))
  if (dir.exists(src)) unlink(src, recursive = TRUE)
  dir.create(src)
  file.copy(app_r, file.path(src, "app.R"), overwrite = TRUE)

  # subdir places the app under site/<app>/ while sharing site/shinylive/ assets.
  shinylive::export(appdir = src, destdir = site, subdir = app)
}

message("\nDone. Consolidated site written to: ", site)
message("Preview locally with:")
message('  httpuv::runStaticServer("', site, '")')

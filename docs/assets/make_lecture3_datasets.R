#Writes the datasets that the Week 2 notes offer for download, so the examples
#worked on the Lecture 3 slides can be pulled into R or the stat-tools site
#during class instead of only being pictures of tables.
#
#  docs/Data/old_faithful.csv   - all 272 rows of R's built-in `faithful`,
#                                 the eruption/waiting data on slides 13, 14 and 28.
#
#The MPG and engine-cylinders example (slides 19 and 22) is R's `mtcars` and is
#already committed as docs/Data/carsdata.csv, so it is not regenerated here -
#it only needed a .gitignore exception to publish.
#
#Column names match the headers printed on the slides, so a student comparing
#the download against the projector sees the same words.

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"),
                      error = function(e) NA_character_)
if (is.na(this_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  this_file <- if (length(m)) normalizePath(sub("^--file=", "", m[[1]]),
                                            winslash = "/") else NA_character_
}
if (is.na(this_file)) stop("Could not determine the script location; run it with Rscript.")
data_dir <- file.path(dirname(dirname(this_file)), "Data")

stopifnot(nrow(faithful) == 272L)

old_faithful <- data.frame(
  Observation = seq_len(nrow(faithful)),
  `Eruption Time (min)` = round(faithful$eruptions, 3),
  `Waiting Time (min)`  = faithful$waiting,
  check.names = FALSE)

out <- file.path(data_dir, "old_faithful.csv")
write.csv(old_faithful, out, row.names = FALSE)

message("Wrote ", out, " (", nrow(old_faithful), " rows)")
message("  eruption time: ", sprintf("%.3f", min(faithful$eruptions)), " to ",
        sprintf("%.3f", max(faithful$eruptions)), " min")
message("  waiting time:  ", min(faithful$waiting), " to ", max(faithful$waiting), " min")

#Builds docs/Data/teens_cell_phone_focus_simulated.csv - the student-level file
#offered for download beside Figure 1 in the Week 1 notes (docs/lecturemenu1.Rmd)
#and reused for the bar graph and pie chart in the Week 2 notes.
#
#IMPORTANT - this file is SIMULATED. Pew Research Center published the marginal
#percentages for the survey question ("Are you losing focus in class by checking
#your cell phone?", n = 743 U.S. teens aged 13-17) but never released row-level
#responses. What is real here is the margin:
#
#    Never 289 (0.39) · Rarely 216 (0.29) · Sometimes 178 (0.24) · Often 60 (0.08)
#
#Those four counts are reproduced exactly, so any frequency table a student
#builds from the file matches the one on the lecture slide. The pairing of a
#response with a particular student, and every value of Age, is invented. The
#filename says so, and so does the link text in the notes.
#
#The first four rows and the last row are pinned to the values already printed
#in the illustrative table in lecturemenu1.Rmd, so that table is literally the
#head and tail of the file students download.

set.seed(2026)

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"),
                      error = function(e) NA_character_)
if (is.na(this_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  this_file <- if (length(m)) normalizePath(sub("^--file=", "", m[[1]]),
                                            winslash = "/") else NA_character_
}
if (is.na(this_file)) stop("Could not determine the script location; run it with Rscript.")
out_path <- file.path(dirname(dirname(this_file)), "Data",
                      "teens_cell_phone_focus_simulated.csv")

COUNTS <- c(Never = 289L, Rarely = 216L, Sometimes = 178L, Often = 60L)
N <- sum(COUNTS)
stopifnot(N == 743L)

Response <- sample(rep(names(COUNTS), COUNTS))
Age <- sample(13:17, N, replace = TRUE)

#Pin the rows the notes already display, by swapping rather than overwriting so
#the four counts above survive untouched.
pin <- list(list(i = 1L,   response = "Never",     age = 13L),
            list(i = 2L,   response = "Sometimes", age = 13L),
            list(i = 3L,   response = "Never",     age = 15L),
            list(i = 4L,   response = "Often",     age = 17L),
            list(i = 743L, response = "Rarely",    age = 16L))

for (p in pin) {
  if (Response[[p$i]] != p$response) {
    donor <- setdiff(which(Response == p$response), vapply(pin, `[[`, integer(1), "i"))
    stopifnot(length(donor) > 0)
    j <- donor[[1]]
    Response[c(p$i, j)] <- Response[c(j, p$i)]
  }
  Age[[p$i]] <- p$age
}

teens <- data.frame(Student = seq_len(N), Age = Age, Response = Response,
                    stringsAsFactors = FALSE)

stopifnot(identical(table(teens$Response)[names(COUNTS)], table(rep(names(COUNTS), COUNTS))[names(COUNTS)]))
stopifnot(all(teens$Age >= 13 & teens$Age <= 17))

write.csv(teens, out_path, row.names = FALSE, quote = FALSE)

message("Wrote ", out_path, " (", nrow(teens), " rows)")
print(table(factor(teens$Response, levels = names(COUNTS))))
message("relative frequencies: ",
        paste(sprintf("%s %.2f", names(COUNTS), COUNTS / N), collapse = " · "))

#Checks the numbers on the Week 12 Lecture 29 deck (11/11/2026): the introduction to categorical data
#(one-way and two-way tables, conditional distributions, observed and expected counts, the chi-square
#distribution and the X^2 statistic). The matched-pairs slides carried over from Lecture 28 are checked
#in make_lecture28_figures.R. Nov 13 2026: Lecture 29 stopped at slide 15, so the slides from "Tables for
#categorical data" on (including the l29_chisq.png slide) moved into the Lecture 30 deck. It draws:
#
#  l29_chisq.png         chi-square density curves for df = 1, 2, 4 and 8
#  l29_figure_sizes.csv  the size, in points, each figure is drawn at
#
#The same numbers are asserted on the Week 12 notes page (lecturemenu12.Rmd).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under
#16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and every text
#size goes through tsz() or el(), which refuse anything smaller.

library(ggplot2)

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"),
                      error = function(e) NA_character_)
if (is.na(this_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  this_file <- if (length(m)) normalizePath(sub("^--file=", "", m[[1]]),
                                            winslash = "/") else NA_character_
}
if (is.na(this_file)) stop("Could not determine the script location; run it with Rscript.")
assets_dir <- dirname(this_file)

#---- numbers (the same checks as lecturemenu12.Rmd)
#Pew: "Are you losing focus in class by checking your cell phone?", n = 743 U.S. teens (published margins)
pew <- c(Never = 289, Rarely = 216, Sometimes = 178, Often = 60)
stopifnot(sum(pew) == 743, identical(unname(round(pew / 743, 2)), c(0.39, 0.29, 0.24, 0.08)))
#H0: all four answers equally common (p_i = 0.25): E = n p_i, X^2 = sum (O - E)^2 / E, df = k - 1
E <- 743 * 0.25
terms <- round((pew - E)^2 / E, 2)
stopifnot(E == 185.75, identical(unname(terms), c(57.39, 4.93, 0.32, 85.13)),
          round(sum((pew - E)^2 / E), 2) == 147.77, round(sum(terms), 2) == 147.77,
          length(pew) - 1 == 3)
#the mean of chi-square(df) is df and its SD is sqrt(2 df): 147.77 is ~ 59 SDs above the mean of chi-square(3)
stopifnot(round((147.77 - 3) / sqrt(6), 0) == 59, pchisq(147.77, 3, lower.tail = FALSE) < 1e-30)

#OpenStax Example 11.6: hours volunteered per week by type of volunteer (n = 839)
vol <- matrix(c(111, 96, 48,
                96, 133, 61,
                91, 150, 53), nrow = 3, byrow = TRUE,
              dimnames = list(c('Community college', 'Four-year college', 'Nonstudents'),
                              c('1-3', '4-6', '7-9')))
stopifnot(identical(unname(rowSums(vol)), c(255, 290, 294)), identical(unname(colSums(vol)), c(298, 379, 162)),
          sum(vol) == 839)
cond <- round(vol / rowSums(vol), 3)          # conditional distribution of hours within each type
marg <- round(colSums(vol) / 839, 3)          # marginal distribution of hours
stopifnot(identical(unname(cond[1, ]), c(0.435, 0.376, 0.188)),
          identical(unname(cond[2, ]), c(0.331, 0.459, 0.210)),
          identical(unname(cond[3, ]), c(0.310, 0.510, 0.180)),
          identical(unname(marg), c(0.355, 0.452, 0.193)))
#the book's expected count for the top-left cell (Friday's test of independence)
stopifnot(round(255 * 298 / 839, 2) == 90.57)

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }     # geom text size (mm)
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

NAVY <- "#1F3864"; RED <- "#C00000"; GOLD <- "#B8860B"; GREEN <- "#2E7D32"

sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}

#---- the chi-square family: right-skewed, never below 0, one curve per df; the peak moves right and the
#curve spreads out as df grows (mean = df)
x <- seq(0.02, 16, length.out = 900)
dfs <- c(1, 2, 4, 8)
cols <- c('1' = RED, '2' = GOLD, '4' = GREEN, '8' = NAVY)
cur <- do.call(rbind, lapply(dfs, function(k) data.frame(x = x, y = dchisq(x, k), df = factor(k, levels = dfs))))
#label each curve beside its own peak (df = 1 and 2 peak at 0, so label them on the way down)
lp <- data.frame(df = factor(dfs, levels = dfs), x = c(0.6, 1.6, 2.6, 8.6),
                 y = c(0.55, 0.32, 0.23, 0.13))
lp$lab <- paste0('df = ', dfs)
g <- ggplot(cur, aes(x, y, colour = df)) +
  geom_line(linewidth = 1.2) +
  geom_label(data = lp, aes(x, y, label = lab, colour = df), size = tsz(16), fontface = 'bold', hjust = 0,
             fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'), show.legend = FALSE) +
  scale_colour_manual(values = cols, guide = 'none') +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16)) +
  coord_cartesian(xlim = c(0, 16), ylim = c(0, 0.62), expand = FALSE) +
  labs(x = expression(chi^2~'value (never below 0)'), y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.x = el(16, colour = 'black'), axis.title.x = el(16),
        plot.margin = margin(8, 14, 4, 8))
save_fig(g, 'l29_chisq.png', 840, 270)

write.csv(sizes, file.path(assets_dir, 'l29_figure_sizes.csv'), row.names = FALSE)
print(sizes)

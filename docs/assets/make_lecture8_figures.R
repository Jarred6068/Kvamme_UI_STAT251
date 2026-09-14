#Builds the figures for the Week 4 Lecture 8 deck and notes, which run across two
#Mondays (9/14/2026 and 9/21/2026): sampling designs, then experimental design
#and causation.
#
#  l8_design_srs.png             simple random sample, n = 20 of N = 100
#  l8_design_systematic.png      every 5th entry of a numbered list, random start
#  l8_design_stratified.png      20% from each class year: 8, 6, 4, 2
#  l8_design_cluster.png         two whole columns (clusters) of ten
#  l8_design_two_stage.png       four columns, then five people from each
#  l8_three_designs.png          SRS, stratified and cluster side by side
#  l8_bias_vs_n.png              a big biased sample vs a small random one
#  l8_heights_allocation.png     proportional allocation for docs/Data/heights.csv
#  l8_confounding.png            a lurking variable pointing at both X and Y
#  l8_crd.png                    completely randomized design
#  l8_rcbd.png                   randomized block design
#  l8_matched_pairs.png          matched pairs design
#  l8_sampling_vs_assignment.png what random sampling and random assignment each buy
#
#  l8_figure_sizes.csv           the size, in points, each figure is drawn at
#
#TYPE SIZE. Nothing on a slide may be smaller than 16pt, and that includes the text
#inside a picture. So every figure is drawn at EXACTLY the size it occupies on the
#slide (width_in = points / 72) and the build script places it at that size, read
#from l8_figure_sizes.csv. Every text size in this file goes through tsz() or el(),
#which refuse anything under 16pt. Scaled 1:1, 16pt in the figure is 16pt on the
#wall.
#
#ONE POPULATION FOR THE GRID PICTURES. N = 100 students on a 10 x 10 grid. The
#rows are class years - 40 first-years, 30 sophomores, 20 juniors, 10 seniors - so
#the strata are horizontal bands (alike within) and the clusters are the columns,
#each of which cuts across all four years (a mini-population). Every design draws
#n = 20.
#
#The SYSTEMATIC sample is deliberately NOT drawn on that grid. Colouring by class
#year makes any scattered set of red dots read as a stratified sample, and counting
#every 5th seat along rows of 10 picks whole columns, which reads as a cluster
#sample. A systematic sample is a statement about a LIST, so it is drawn as one:
#the numbers 1 to 100 in order, every 5th highlighted, with the +5 steps shown.
#
#Every number the slides and notes print is asserted below.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
})

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
data_dir   <- file.path(dirname(assets_dir), "Data")

NAVY <- "#1F3864"
BLUE <- "#4472C4"
PALE <- "#D9E5F7"
RED  <- "#C00000"
GREY <- "#595959"

MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / ggplot2::.pt }
el  <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

sizes <- data.frame(file = character(), w_pt = numeric(), h_pt = numeric(),
                    stringsAsFactors = FALSE)
save_fig <- function(p, file, w_pt, h_pt) {
  ggsave(file.path(assets_dir, file), p, width = w_pt / 72, height = h_pt / 72,
         dpi = 300, bg = "white")
  sizes[nrow(sizes) + 1, ] <<- list(file, w_pt, h_pt)
}

YEARS <- c("First-year", "Sophomore", "Junior", "Senior")
TINTS <- setNames(c("#D9E5F7", "#FCE4D6", "#E2EFDA", "#EDE3F5"), YEARS)

#----------------------------------------------------------------------------
#The population.

pop <- expand.grid(col = 1:10, row = 1:10)
pop <- pop[order(pop$row, pop$col), ]
pop$id <- seq_len(nrow(pop))
pop$year <- factor(YEARS[findInterval(pop$row, c(1, 5, 8, 10))], levels = YEARS)
stopifnot(nrow(pop) == 100,
          identical(as.integer(table(pop$year)), c(40L, 30L, 20L, 10L)))

N <- 100L
n <- 20L

design_panel <- function(sel, extras = list(), title = NULL, legend_rows = 2) {
  d <- pop
  d$picked <- d$id %in% sel
  p <- ggplot(d, aes(x = col, y = -row)) +
    extras +
    geom_point(data = d[!d$picked, ], aes(fill = year), shape = 21, size = 5.4,
               colour = "grey55", stroke = 0.5) +
    geom_point(data = d[d$picked, ], shape = 21, size = 5.4, fill = RED,
               colour = "black", stroke = 0.9) +
    scale_fill_manual(values = TINTS, limits = YEARS, name = NULL, drop = FALSE) +
    guides(fill = guide_legend(nrow = legend_rows, override.aes = list(size = 5.4))) +
    coord_equal(xlim = c(0.4, 10.6), ylim = c(-10.6, -0.4), clip = "off") +
    theme_void() +
    theme(legend.position = "bottom", legend.text = el(16),
          legend.key.size = unit(18, "pt"),
          plot.margin = margin(4, 4, 4, 4))
  if (!is.null(title)) {
    p <- p + labs(title = title) +
      theme(plot.title = el(18, colour = NAVY, face = "bold", hjust = 0.5,
                            margin = margin(b = 6)))
  }
  p
}

col_rects <- function(highlight) list(
  annotate("rect", xmin = (1:10) - 0.46, xmax = (1:10) + 0.46, ymin = -10.5, ymax = -0.5,
           fill = NA, colour = "grey80", linewidth = 0.4),
  annotate("rect", xmin = highlight - 0.46, xmax = highlight + 0.46, ymin = -10.5, ymax = -0.5,
           fill = NA, colour = BLUE, linewidth = 1.4)
)
band_lines <- list(annotate("segment", x = 0.45, xend = 10.55, y = -c(4.5, 7.5, 9.5),
                            yend = -c(4.5, 7.5, 9.5), colour = NAVY, linewidth = 0.9,
                            linetype = "dashed"))

PANEL_W <- 360
PANEL_H <- 360

#----------------------------------------------------------------------------
#1. simple random sample - every set of 20 is equally likely

set.seed(251)
srs <- sort(sample(pop$id, n))
stopifnot(length(unique(srs)) == n)
srs_by_year <- table(pop$year[pop$id %in% srs])
#The slide points out that this SRS drew 4 seniors but only 2 juniors.
stopifnot(identical(as.integer(srs_by_year), c(8L, 6L, 2L, 4L)))

p_srs <- design_panel(srs)
save_fig(p_srs, "l8_design_srs.png", PANEL_W, PANEL_H)

#----------------------------------------------------------------------------
#2. systematic sample - a numbered list, random start in 1..k, then every kth

k <- N %/% n
stopifnot(k == 5)
set.seed(9142026)
start <- sample(1:k, 1)
sys_pos <- seq(start, by = k, length.out = n)
stopifnot(start == 2, length(sys_pos) == n, max(sys_pos) <= N)

PER_ROW <- 25
strip <- data.frame(pos = 1:N)
strip$row <- (strip$pos - 1) %/% PER_ROW
strip$col <- (strip$pos - 1) %% PER_ROW
strip$picked <- strip$pos %in% sys_pos
stopifnot(sum(strip$picked) == n)

first_row_picks <- strip$col[strip$picked & strip$row == 0]
arcs <- data.frame(x = head(first_row_picks, -1), xend = tail(first_row_picks, -1))

p_sys <- ggplot(strip, aes(x = col, y = -row)) +
  geom_tile(aes(fill = picked), colour = "white", width = 0.94, height = 0.84) +
  geom_text(aes(label = pos, colour = picked), size = tsz(16), fontface = "bold") +
  geom_curve(data = arcs, aes(x = x, xend = xend, y = 0.5, yend = 0.5),
             inherit.aes = FALSE, curvature = -0.32, colour = RED, linewidth = 0.8,
             arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  annotate("text", x = (arcs$x + arcs$xend) / 2, y = 1.78, label = "+5", colour = RED,
           size = tsz(16), fontface = "bold") +
  annotate("text", x = first_row_picks[1], y = 1.78, label = "start", colour = RED,
           size = tsz(16), fontface = "bold", hjust = 0.8) +
  scale_fill_manual(values = c(`FALSE` = "#E7EBF2", `TRUE` = RED), guide = "none") +
  scale_colour_manual(values = c(`FALSE` = "grey25", `TRUE` = "white"), guide = "none") +
  coord_cartesian(xlim = c(-0.55, PER_ROW - 0.45), ylim = c(-3.55, 2.15), expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
save_fig(p_sys, "l8_design_systematic.png", 840, 230)

#----------------------------------------------------------------------------
#3. stratified random sample - an SRS inside every class year, proportional

alloc <- setNames(as.integer(table(pop$year)) * n / N, YEARS)
stopifnot(identical(unname(alloc), c(8, 6, 4, 2)), sum(alloc) == n)
set.seed(251)
strat <- sort(unlist(lapply(YEARS, function(y) sample(pop$id[pop$year == y], alloc[[y]]))))
stopifnot(length(strat) == n,
          identical(as.integer(table(pop$year[pop$id %in% strat])), c(8L, 6L, 4L, 2L)))

p_strat <- design_panel(strat, band_lines)
save_fig(p_strat, "l8_design_stratified.png", PANEL_W, PANEL_H)

#----------------------------------------------------------------------------
#4. cluster sample - an SRS of whole columns, everyone in them

set.seed(4)
clusters <- sort(sample(1:10, n / 10))
clus <- pop$id[pop$col %in% clusters]
stopifnot(length(clusters) == 2, length(clus) == n)

p_clus <- design_panel(clus, col_rects(clusters))
save_fig(p_clus, "l8_design_cluster.png", PANEL_W, PANEL_H)

#----------------------------------------------------------------------------
#5. two-stage cluster sample - an SRS of columns, then an SRS inside each

set.seed(21)
clusters2 <- sort(sample(1:10, 4))
two <- sort(unlist(lapply(clusters2, function(cc) sample(pop$id[pop$col == cc], 5))))
stopifnot(length(clusters2) == 4, length(two) == n,
          all(table(pop$col[pop$id %in% two]) == 5))

p_two <- design_panel(two, col_rects(clusters2))
save_fig(p_two, "l8_design_two_stage.png", PANEL_W, PANEL_H)

#----------------------------------------------------------------------------
#6. SRS, stratified and cluster side by side - the strata vs clusters slide

p_three <- ggpubr::ggarrange(
  design_panel(srs, title = "Simple random", legend_rows = 1),
  design_panel(strat, band_lines, title = "Stratified", legend_rows = 1),
  design_panel(clus, col_rects(clusters), title = "Cluster", legend_rows = 1),
  nrow = 1, common.legend = TRUE, legend = "bottom")
save_fig(p_three, "l8_three_designs.png", 860, 330)

#----------------------------------------------------------------------------
#7. a bigger sample does not fix bias
#
#   The truth: 40% of students exercise at least three times a week. One survey
#   asks n = 2000 people at the Student Recreation Center, where 75% do; the other
#   is a simple random sample of n = 200. Repeat each survey 5000 times.

TRUTH <- 0.40
FRAME <- 0.75
REPS  <- 5000L
set.seed(1936)
big_biased <- rbinom(REPS, 2000, FRAME) / 2000
small_srs  <- rbinom(REPS, 200, TRUTH) / 200
stopifnot(abs(mean(big_biased) - FRAME) < 0.005,
          abs(mean(small_srs) - TRUTH) < 0.005,
          sd(big_biased) < sd(small_srs),
          all(big_biased > TRUTH))

bias_df <- rbind(
  data.frame(est = small_srs,  survey = "Simple random sample, n = 200"),
  data.frame(est = big_biased, survey = "Rec Center survey, n = 2000"))
bias_df$survey <- factor(bias_df$survey, levels = c("Simple random sample, n = 200",
                                                    "Rec Center survey, n = 2000"))

p_bias <- ggplot(bias_df, aes(x = est, fill = survey)) +
  geom_histogram(binwidth = 0.01, boundary = 0, colour = "white", linewidth = 0.15,
                 position = "identity", alpha = 0.9) +
  geom_vline(xintercept = TRUTH, colour = RED, linewidth = 1.1) +
  annotate("text", x = TRUTH - 0.01, y = Inf, vjust = 1.5, hjust = 1, colour = RED,
           size = tsz(16), fontface = "bold", label = "truth: 40%") +
  scale_fill_manual(values = c(BLUE, GREY), name = NULL) +
  scale_x_continuous(labels = function(v) paste0(round(v * 100), "%"),
                     breaks = seq(0.2, 0.9, 0.1)) +
  coord_cartesian(xlim = c(0.25, 0.85)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Estimate from each of 5000 repeated surveys", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(), axis.text.x = el(16),
        axis.title.x = el(16, colour = NAVY),
        legend.position = "top", legend.text = el(16),
        plot.margin = margin(2, 8, 2, 2))
save_fig(p_bias, "l8_bias_vs_n.png", 540, 330)

#----------------------------------------------------------------------------
#8. proportional allocation on real data - docs/Data/heights.csv

heights <- read.csv(file.path(data_dir, "heights.csv"), stringsAsFactors = FALSE)
h_counts <- table(heights$GENDER)
stopifnot(nrow(heights) == 379,
          h_counts[["Female"]] == 262, h_counts[["Male"]] == 117)
H_N <- 379; H_n <- 30
h_raw   <- c(Female = 262, Male = 117) * H_n / H_N
h_alloc <- round(h_raw)
stopifnot(identical(unname(h_alloc), c(21, 9)), sum(h_alloc) == H_n)

alloc_df <- data.frame(
  group = rep(c("Population, N = 379", "Sample, n = 30"), each = 2),
  sex   = rep(c("Female", "Male"), 2),
  count = c(262, 117, 21, 9))
alloc_df$share <- alloc_df$count / ave(alloc_df$count, alloc_df$group, FUN = sum)
alloc_df$group <- factor(alloc_df$group, levels = rev(unique(alloc_df$group)))
alloc_df$label <- sprintf("%s: %d (%.0f%%)", alloc_df$sex, alloc_df$count, 100 * alloc_df$share)
stopifnot(identical(sprintf("%.0f", 100 * alloc_df$share), c("69", "31", "70", "30")))

p_alloc <- ggplot(alloc_df, aes(x = share, y = group, fill = sex)) +
  geom_col(width = 0.7, colour = "white", position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5, reverse = TRUE),
            size = tsz(16), colour = "black") +
  scale_fill_manual(values = c(Female = PALE, Male = "#FCE4D6"), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),
        axis.text.y = el(16, colour = NAVY), plot.margin = margin(2, 4, 2, 2))
save_fig(p_alloc, "l8_heights_allocation.png", 720, 150)

#----------------------------------------------------------------------------
#Boxes and arrows for the diagrams. These are drawn in POINT coordinates: the
#canvas runs 0..W by 0..H with no margins, so a box 150 wide is 150pt wide on
#the slide and a label can be checked to fit by eye.

box <- function(x, y, w, h, label, fill = PALE, colour = NAVY, pt = 16, face = "plain") {
  list(annotate("rect", xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
                fill = fill, colour = colour, linewidth = 0.9),
       annotate("text", x = x, y = y, label = label, colour = "black", size = tsz(pt),
                lineheight = 0.95, fontface = face))
}
arr <- function(x, y, xend, yend, colour = BLUE, linetype = "solid") {
  annotate("segment", x = x, y = y, xend = xend, yend = yend, colour = colour,
           linewidth = 1, linetype = linetype,
           arrow = arrow(length = unit(7, "pt"), type = "closed"))
}
canvas <- function(layers, W, H) {
  ggplot() + layers + coord_cartesian(xlim = c(0, W), ylim = c(0, H), expand = FALSE) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
}

#----------------------------------------------------------------------------
#9. confounding

p_conf <- canvas(c(
  box(230, 222, 230, 46, "Lurking variable Z", fill = "#FCE4D6", colour = RED, face = "bold"),
  box(85, 60, 160, 66, "Explanatory\nvariable X", face = "bold"),
  box(375, 60, 160, 66, "Response\nvariable Y", face = "bold"),
  list(arr(175, 199, 110, 95, colour = RED),
       arr(285, 199, 350, 95, colour = RED),
       arr(167, 60, 293, 60, colour = "grey45", linetype = "dashed"),
       annotate("text", x = 230, y = 80, label = "association", colour = GREY, size = tsz(16)),
       annotate("text", x = 230, y = 38, label = "cause?", colour = GREY, size = tsz(16)))
), 460, 250)
save_fig(p_conf, "l8_confounding.png", 460, 250)

#----------------------------------------------------------------------------
#10. completely randomized design

p_crd <- canvas(c(
  box(80, 125, 145, 72, "40 experimental\nunits"),
  box(268, 125, 150, 72, "Random\nassignment", fill = "white", colour = BLUE, face = "bold"),
  box(505, 195, 230, 70, "Group 1: 20 units\nTreatment"),
  box(505, 55, 230, 70, "Group 2: 20 units\nControl (placebo)"),
  box(752, 125, 150, 72, "Compare\nresponses"),
  list(arr(153, 125, 191, 125), arr(343, 145, 388, 185), arr(343, 105, 388, 65),
       arr(620, 185, 675, 145), arr(620, 65, 675, 105))
), 840, 250)
save_fig(p_crd, "l8_crd.png", 840, 250)

#----------------------------------------------------------------------------
#11. randomized block design - randomize SEPARATELY inside each block

rcbd_block <- function(y, label) c(
  box(80, y, 145, 64, label),
  box(268, y, 150, 64, "Random\nassignment", fill = "white", colour = BLUE, face = "bold"),
  box(492, y + 25, 170, 40, "Treatment"),
  box(492, y - 25, 170, 40, "Control"),
  box(725, y, 190, 64, "Compare within\nthe block"),
  list(arr(153, y, 191, y), arr(343, y + 12, 405, y + 24), arr(343, y - 12, 405, y - 24),
       arr(577, y + 24, 628, y + 12), arr(577, y - 24, 628, y - 12))
)
p_rcbd <- canvas(c(
  rcbd_block(172, "Block 1\n30 women"),
  rcbd_block(56, "Block 2\n20 men"),
  list(annotate("segment", x = 0, xend = 840, y = 114, yend = 114, colour = "grey70",
                linetype = "dotted", linewidth = 0.7))
), 840, 226)
save_fig(p_rcbd, "l8_rcbd.png", 840, 226)

#----------------------------------------------------------------------------
#12. matched pairs - the coin flip happens INSIDE each pair

mp_row <- function(y, label) c(
  box(90, y, 165, 46, label),
  box(283, y, 130, 46, "Coin flip", fill = "white", colour = BLUE, face = "bold"),
  box(505, y, 215, 46, "one gets Treatment"),
  box(742, y, 196, 46, "other gets Control"),
  list(arr(173, y, 216, y), arr(349, y, 396, y),
       annotate("text", x = 626, y = y, label = "+", size = tsz(20), colour = NAVY))
)
p_mp <- canvas(c(
  mp_row(180, "Pair 1 (twins)"),
  mp_row(116, "Pair 2 (twins)"),
  mp_row(52, "Pair 3 (twins)")
), 840, 230)
save_fig(p_mp, "l8_matched_pairs.png", 840, 230)

#----------------------------------------------------------------------------
#13. what each kind of randomization buys you

cell <- function(x0, y0, w, h, fill, head, eg) list(
  annotate("rect", xmin = x0, xmax = x0 + w, ymin = y0, ymax = y0 + h, fill = fill, colour = NA),
  annotate("text", x = x0 + w / 2, y = y0 + h * 0.68, label = head, colour = NAVY,
           size = tsz(16), fontface = "bold", lineheight = 0.95),
  annotate("text", x = x0 + w / 2, y = y0 + h * 0.25, label = eg, colour = GREY,
           size = tsz(16), lineheight = 0.95)
)
GW <- 610; GH <- 350
p_grid <- canvas(c(
  cell(100, 160, 250, 140, "#E2EFDA", "Cause, AND generalize\nto the population", "rare: an experiment\non a random sample"),
  cell(356, 160, 250, 140, PALE, "Generalize,\nbut not cause", "e.g. a random-sample\nsurvey"),
  cell(100, 14, 250, 140, PALE, "Cause, but only for\nunits like these", "e.g. most clinical trials,\nwhich use volunteers"),
  cell(356, 14, 250, 140, "#FCE4D6", "Association in\nthese units only", "e.g. volunteers who chose\ntheir own treatment"),
  list(annotate("text", x = 353, y = 336, label = "Randomly ASSIGNED to treatments?",
                colour = RED, fontface = "bold", size = tsz(16)),
       annotate("text", x = c(225, 481), y = 312, label = c("Yes", "No"),
                colour = NAVY, fontface = "bold", size = tsz(16)),
       annotate("text", x = 18, y = 157, label = "Randomly SAMPLED?", angle = 90,
                colour = RED, fontface = "bold", size = tsz(16)),
       annotate("text", x = 66, y = c(230, 84), label = c("Yes", "No"),
                colour = NAVY, fontface = "bold", size = tsz(16)))
), GW, GH)
save_fig(p_grid, "l8_sampling_vs_assignment.png", GW, GH)

#----------------------------------------------------------------------------
#14. bias and standard error as a bullseye
#
#   The bullseye is the parameter; each shot is the estimate from one sample.
#   Rows are accuracy (is the group of shots centred on the bullseye? = bias),
#   columns are precision (how tightly are the shots grouped? = standard error).
#   Drawn in point coordinates on a 440 x 360 canvas, so the circles are round.

ring <- function(cx, cy, r, npts = 181) {
  t <- seq(0, 2 * pi, length.out = npts)
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}
target <- function(cx, cy, R, shots) {
  radii <- R * c(1, 0.72, 0.44, 0.16)
  fills <- c("white", PALE, "white", RED)
  layers <- lapply(seq_along(radii), function(i)
    geom_polygon(data = ring(cx, cy, radii[i]), aes(x = x, y = y),
                 fill = fills[i], colour = NAVY, linewidth = 0.6, inherit.aes = FALSE))
  c(layers, list(geom_point(data = shots, aes(x = x, y = y), inherit.aes = FALSE,
                            shape = 21, size = 3.2, fill = "black", colour = "white",
                            stroke = 0.6)))
}

BW <- 440; BH <- 360; R_T <- 72
set.seed(251)
make_shots <- function(cx, cy, bias_xy, sd) {
  s <- data.frame(x = cx + bias_xy[1] + rnorm(10, 0, sd), y = cy + bias_xy[2] + rnorm(10, 0, sd))
  d <- sqrt((s$x - cx)^2 + (s$y - cy)^2); f <- pmin(1, (R_T - 6) / d)
  s$x <- cx + (s$x - cx) * f; s$y <- cy + (s$y - cy) * f
  s
}
CX <- c(150, 340); CY <- c(235, 78)
BIAS <- c(34, 28); SD_SMALL <- 6; SD_LARGE <- 24
p_bull <- canvas(c(
  target(CX[1], CY[1], R_T, make_shots(CX[1], CY[1], c(0, 0), SD_SMALL)),
  target(CX[2], CY[1], R_T, make_shots(CX[2], CY[1], c(0, 0), SD_LARGE)),
  target(CX[1], CY[2], R_T, make_shots(CX[1], CY[2], BIAS, SD_SMALL)),
  target(CX[2], CY[2], R_T, make_shots(CX[2], CY[2], BIAS, SD_LARGE)),
  list(annotate("text", x = CX, y = 340, label = c("precise", "not precise"),
                colour = NAVY, fontface = "bold", size = tsz(16)),
       annotate("text", x = 20, y = CY, label = c("accurate", "not accurate"), angle = 90,
                colour = NAVY, fontface = "bold", size = tsz(16)))
), BW, BH)
save_fig(p_bull, "l8_bullseye.png", BW, BH)

#----------------------------------------------------------------------------
#15. census, sampling frame, sample and sampling error on one small population
#
#   100 students, each with the hours they exercised last week. The registrar's
#   list (the sampling frame) holds 90 of them; the last row enrolled late and is
#   not on it. A census of all 100 gives the parameter; an SRS of 10 from the list
#   gives the statistic; the gap between them is the sampling error. The numbers
#   are written to l8_frame_example.csv so the slide and the notes print exactly
#   these values.

set.seed(2026)
hours <- round(rgamma(100, shape = 2.5, scale = 1.8), 1)
set.seed(88)
frame_sample <- sort(sample(1:90, 10))
mu_hours   <- round(mean(hours), 1)
xbar_hours <- round(mean(hours[frame_sample]), 1)
err_hours  <- round(xbar_hours - mu_hours, 1)
stopifnot(length(frame_sample) == 10, all(frame_sample <= 90), abs(err_hours) >= 0.2)
#Column names must differ by more than case: PowerShell's Import-Csv treats N and n as one.
write.csv(data.frame(pop_size = 100, frame_size = 90, sample_size = 10, mu = sprintf("%.1f", mu_hours),
                     xbar = sprintf("%.1f", xbar_hours), error = sprintf("%.1f", err_hours)),
          file.path(assets_dir, "l8_frame_example.csv"), row.names = FALSE)

FW <- 440; FH <- 360
fr <- data.frame(id = 1:100)
fr$row <- (fr$id - 1) %/% 10 + 1
fr$col <- (fr$id - 1) %% 10 + 1
fr$x <- 40 + (fr$col - 1) * 40
fr$y <- 300 - (fr$row - 1) * 29
#Two versions: the setup slide shows only the population and the frame; the next
#slide adds the random sample in red.
frame_plot <- function(show_sample) {
  fr$status <- ifelse(show_sample & fr$id %in% frame_sample, "sample",
                      ifelse(fr$id <= 90, "frame", "off"))
  canvas(list(
    annotate("rect", xmin = 18, xmax = 422, ymin = 300 - 8 * 29 - 15, ymax = 316,
             fill = NA, colour = NAVY, linewidth = 1.1, linetype = "dashed"),
    geom_point(data = fr, aes(x = x, y = y, fill = status), shape = 21, size = 5,
               colour = "grey40", stroke = 0.5),
    scale_fill_manual(values = c(frame = PALE, sample = RED, off = "grey80"), guide = "none"),
    annotate("text", x = 220, y = 338, label = "sampling frame: the registrar's list",
             colour = NAVY, fontface = "bold", size = tsz(16)),
    annotate("text", x = 220, y = 14, label = "10 late enrollees - not on the list",
             colour = GREY, size = tsz(16))
  ), FW, FH)
}
save_fig(frame_plot(FALSE), "l8_frame_setup.png", FW, FH)
save_fig(frame_plot(TRUE), "l8_frame_example.png", FW, FH)

#----------------------------------------------------------------------------
#16. sampling without and with replacement
#
#   Five numbered balls in a bag, three draws. Without replacement each ball that
#   is drawn leaves the bag (drawn as a dashed empty circle), so no number can
#   repeat: 3, 1, 5. With replacement every ball goes back, the bag is full before
#   every draw, and a repeat is possible: 3, 1, 3.

RW <- 860; RH <- 180
BAG_W <- 136
ball <- function(x, y, lab, gone = FALSE) {
  list(geom_polygon(data = ring(x, y, 11), aes(x = x, y = y), inherit.aes = FALSE,
                    fill = if (gone) "white" else PALE, colour = if (gone) "grey60" else NAVY,
                    linewidth = 0.7, linetype = if (gone) "dashed" else "solid"),
       annotate("text", x = x, y = y, label = lab, size = tsz(16), fontface = "bold",
                colour = if (gone) "grey70" else "black"))
}
bag <- function(x0, yc, removed = integer(0)) {
  c(list(annotate("rect", xmin = x0, xmax = x0 + BAG_W, ymin = yc - 22, ymax = yc + 22,
                  fill = "white", colour = "grey55", linewidth = 0.6)),
    unlist(lapply(1:5, function(i) ball(x0 + 18 + (i - 1) * 25, yc, i, i %in% removed)),
           recursive = FALSE))
}
draw_arrow <- function(x, xend, yc, lab) list(
  arr(x, yc, xend, yc),
  annotate("text", x = (x + xend) / 2, y = yc + 33, label = lab, size = tsz(16), colour = BLUE)
)
BAGS_X <- c(150, 346, 542)
replacement_row <- function(yc, label, removed_seq, draws, colour) {
  c(list(annotate("text", x = 70, y = yc, label = label, size = tsz(16), fontface = "bold",
                  colour = colour, lineheight = 0.95)),
    bag(BAGS_X[1], yc, removed_seq[[1]]), bag(BAGS_X[2], yc, removed_seq[[2]]),
    bag(BAGS_X[3], yc, removed_seq[[3]]),
    draw_arrow(BAGS_X[1] + BAG_W + 4, BAGS_X[2] - 4, yc, paste("draw", draws[1])),
    draw_arrow(BAGS_X[2] + BAG_W + 4, BAGS_X[3] - 4, yc, paste("draw", draws[2])),
    draw_arrow(BAGS_X[3] + BAG_W + 4, 700, yc, paste("draw", draws[3])),
    list(annotate("rect", xmin = 704, xmax = 856, ymin = yc - 22, ymax = yc + 22,
                  fill = "#FCE4D6", colour = colour, linewidth = 0.9),
         annotate("text", x = 780, y = yc, label = paste("sample:", paste(draws, collapse = ", ")),
                  size = tsz(16), fontface = "bold", colour = "black")))
}
draws_without <- c(3, 1, 5)
draws_with    <- c(3, 1, 3)
stopifnot(!anyDuplicated(draws_without), anyDuplicated(draws_with) > 0)
p_repl <- canvas(c(
  replacement_row(128, "Without\nreplacement", list(integer(0), 3, c(3, 1)), draws_without, NAVY),
  replacement_row(42, "With\nreplacement", list(integer(0), integer(0), integer(0)), draws_with, RED)
), RW, RH)
save_fig(p_repl, "l8_replacement.png", RW, RH)

#17. the same drug-concentration curve on a log axis and a linear axis
#
#   ILLUSTRATIVE, not Purdue's data (no published series exists). A one-compartment
#   absorption/elimination curve matched to two values in the 2008 FDA OxyContin
#   label: a 10 mg dose peaks at 10.6 ng/mL at about 2.7 hours, and the elimination
#   half-life is 4.5 hours. The log panel uses a 1-100 axis, as the label figure did.

ke <- log(2) / 4.5
ka <- uniroot(function(k) log(k / ke) / (k - ke) - 2.7, c(ke + 1e-3, 20))$root
oxy_t <- seq(0.05, 12, by = 0.05)
oxy_shape <- exp(-ke * oxy_t) - exp(-ka * oxy_t)
oxy_c <- oxy_shape / max(oxy_shape) * 10.6
oxy_peak_t <- oxy_t[which.max(oxy_c)]
oxy_c12 <- oxy_c[length(oxy_c)]
oxy_drop <- round(100 * (1 - oxy_c12 / 10.6))
stopifnot(abs(oxy_peak_t - 2.7) < 0.1, oxy_c12 > 2, oxy_c12 < 3.5)
write.csv(data.frame(peak = "10.6", c12 = sprintf("%.1f", oxy_c12), drop = oxy_drop),
          file.path(assets_dir, "l8_oxy_example.csv"), row.names = FALSE)

oxy_df <- data.frame(t = oxy_t, conc = oxy_c)
oxy_panel <- function(logy) {
  d <- if (logy) oxy_df[oxy_df$conc >= 1, ] else oxy_df
  p <- ggplot(d, aes(x = t, y = conc)) +
    geom_line(colour = NAVY, linewidth = 1.3) +
    annotate("point", x = c(oxy_peak_t, 12), y = c(10.6, oxy_c12), colour = RED, size = 3) +
    scale_x_continuous(breaks = c(0, 4, 8, 12), limits = c(0, 12.3)) +
    labs(x = NULL, y = NULL, title = if (logy) "ng/mL, log axis" else "ng/mL, linear axis") +
    theme_minimal() +
    theme(axis.text = el(16), panel.grid.minor = element_blank(),
          plot.title = el(16, colour = if (logy) RED else NAVY, face = "bold"),
          plot.margin = margin(2, 10, 2, 2))
  if (logy) p + scale_y_log10(limits = c(1, 100), breaks = c(1, 10, 100))
  else p + scale_y_continuous(limits = c(0, 12), breaks = c(0, 5, 10))
}
p_oxy <- ggpubr::ggarrange(
  oxy_panel(TRUE) + theme(axis.text.x = element_blank()),
  oxy_panel(FALSE) + labs(x = "hours after one 10 mg dose") + theme(axis.title.x = el(16)),
  ncol = 1, heights = c(1, 1.2), align = "v")
save_fig(p_oxy, "l8_oxycontin_axes.png", 450, 320)

#----------------------------------------------------------------------------
sizes_file <- file.path(assets_dir, "l8_figure_sizes.csv")
write.csv(sizes, sizes_file, row.names = FALSE)
stopifnot(nrow(sizes) == 18)
message(sprintf("  oxycontin illustration: peak 10.6 at %.2f h, %.1f at 12 h, drop %d%%",
                oxy_peak_t, oxy_c12, oxy_drop))
message(sprintf("  frame example: census mean %.1f, sample mean %.1f, sampling error %.1f",
                mu_hours, xbar_hours, err_hours))

message("Wrote ", nrow(sizes), " figures and ", basename(sizes_file), " to ", assets_dir)
message("\nChecks (these are the numbers on the slides and in the Week 4 notes):")
message(sprintf("  population N = %d: %s", N,
                paste(sprintf("%s %d", YEARS, as.integer(table(pop$year))), collapse = ", ")))
message(sprintf("  SRS of n = %d picked by year: %s", n,
                paste(sprintf("%s %d", names(srs_by_year), as.integer(srs_by_year)), collapse = ", ")))
message(sprintf("  systematic: k = %d, random start = %d, positions %s ...", k, start,
                paste(head(sys_pos, 5), collapse = ", ")))
message(sprintf("  stratified allocation: %s", paste(alloc, collapse = ", ")))
message(sprintf("  cluster columns: %s;  two-stage columns: %s",
                paste(clusters, collapse = ", "), paste(clusters2, collapse = ", ")))
message(sprintf("  bias sim: Rec Center n=2000 mean %.3f sd %.4f | SRS n=200 mean %.3f sd %.4f",
                mean(big_biased), sd(big_biased), mean(small_srs), sd(small_srs)))
message(sprintf("  heights: N = 379 (F 262, M 117); n = 30 -> %.2f, %.2f -> %d, %d",
                h_raw[1], h_raw[2], h_alloc[1], h_alloc[2]))
for (i in seq_len(nrow(sizes))) {
  d <- dim(png::readPNG(file.path(assets_dir, sizes$file[i])))
  message(sprintf("  %-32s drawn at %3.0f x %3.0f pt  (%d x %d px)",
                  sizes$file[i], sizes$w_pt[i], sizes$h_pt[i], d[2], d[1]))
}

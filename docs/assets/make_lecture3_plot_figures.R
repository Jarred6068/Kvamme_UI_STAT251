#Replaces three figures in the Week 2 Lecture 3 deck that failed the accessibility
#sweep. The originals had no source in the repo, so they are rebuilt here from the
#data the slides actually describe.
#
#  oldfaithful_dotplot.png  slide 22. The original axis read "Waiting Time Until
#                           Eraption (Min)" - a typo baked into the picture.
#  mpg_dotplot_wide.png     slides 24 and 26, which share one media file. The
#                           original encoded cylinder count in red/green/blue
#                           ONLY. Red-green is the commonest colour-vision
#                           deficiency, so those two groups were indistinguishable
#                           for roughly one man in twelve. Cylinder count is now
#                           carried by SHAPE as well as colour, on the Okabe-Ito
#                           palette, which is designed to survive colour blindness.
#  teens_bar.png            slide 20, and
#  teens_pie.png            slide 20. The pie identified its slices through a
#                           colour legend alone; slices are now labelled in place,
#                           so nothing depends on telling the colours apart.
#
#IMPORTANT - each output's aspect ratio matches the picture frame it drops into.
#PowerPoint stretches whatever sits in the relationship to fill the frame, so a
#different aspect comes out visibly squashed. The frames are, in EMU:
#    slide 20 bar    5891026 x 3990975  -> 1.4761
#    slide 20 pie    5703693 x 4217903  -> 1.3523
#    slide 22        9865915 x 6681844  -> 1.4765
#    slide 24       10283269 x 6858000  -> 1.4995

suppressPackageStartupMessages(library(ggplot2))

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

W   <- 7      #inches; heights follow from the aspect ratios above
DPI <- 200

save_at <- function(plot, name, aspect) {
  ggsave(file.path(assets_dir, name), plot, width = W, height = W / aspect, dpi = DPI)
  message("  ", name, " (", sprintf("%.4f", aspect), ")")
}

#----------------------------------------------------------------------------
#Dot plots are drawn by hand rather than with geom_dotplot, because geom_dotplot
#will not take a shape aesthetic - and shape is the whole point of the fix.
stack_dots <- function(x, group = NULL, binwidth = 1) {
  b <- round(x / binwidth) * binwidth
  d <- data.frame(x = b, g = if (is.null(group)) factor(1) else factor(group))
  d <- d[order(d$x, d$g), ]
  d$y <- stats::ave(seq_len(nrow(d)), d$x, FUN = seq_along)
  d
}

#In a dot plot one dot stands for one observation, so the dots have to be as wide
#as one step along the x axis and stacked touching. Both follow from the panel
#size: the dot diameter is the panel width divided by the number of x steps, and
#the y axis is then scaled so one unit equals one dot.
dot_geometry <- function(x, width_in, height_in, pad_w = 1.1, pad_h = 1.0) {
  x_units <- diff(range(x)) + 2
  dot_in  <- (width_in - pad_w) / x_units
  list(size = dot_in * 25.4, y_max = (height_in - pad_h) / dot_in)
}

dot_theme <- function(base = 15) {
  theme_minimal(base_size = base) +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.line.x = element_line(colour = "black", linewidth = 0.6),
          axis.ticks.x = element_line(colour = "black"),
          legend.position = "top", legend.title = element_text(face = "bold"))
}

#----------------------------------------------------------------------------
#1. Old Faithful waiting times - same plot, spelling corrected

d <- stack_dots(faithful$waiting)
g <- dot_geometry(d$x, W, W / 1.4765)
p_faith <- ggplot(d, aes(x = x, y = y)) +
  geom_point(size = g$size, colour = "black") +
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  scale_y_continuous(limits = c(0.5, g$y_max), expand = c(0, 0)) +
  labs(x = "Waiting Time Until Eruption (min)") +
  dot_theme()
save_at(p_faith, "oldfaithful_dotplot.png", 1.4765)

#----------------------------------------------------------------------------
#2. MPG by cylinder count - colour AND shape

cars <- read.csv(file.path(data_dir, "carsdata.csv"), check.names = FALSE)
stopifnot(nrow(cars) == 32, all(c("mpg", "cyl") %in% names(cars)))

dm <- stack_dots(cars$mpg, cars$cyl)
#Okabe-Ito: blue, orange and bluish green stay distinct under every common form
#of colour blindness. Shapes repeat the same information.
CYL_COL <- c("4" = "#0072B2", "6" = "#E69F00", "8" = "#009E73")
CYL_PCH <- c("4" = 16, "6" = 17, "8" = 15)          #circle, triangle, square

mpg_plot <- function(base, aspect) {
  g <- dot_geometry(dm$x, W, W / aspect, pad_h = 1.5)   #1.5in: axis plus legend
  ggplot(dm, aes(x = x, y = y, colour = g, shape = g)) +
    geom_point(size = g$size) +
    scale_colour_manual("Number of Cylinders", values = CYL_COL) +
    scale_shape_manual("Number of Cylinders", values = CYL_PCH) +
    scale_x_continuous(breaks = seq(10, 35, 5)) +
    scale_y_continuous(limits = c(0.5, g$y_max), expand = c(0, 0)) +
    labs(x = "Miles Per Gallon (MPG)") +
    dot_theme(base) +
    guides(colour = guide_legend(override.aes = list(size = 4.5)))
}
save_at(mpg_plot(15, 1.4995), "mpg_dotplot_wide.png", 1.4995)

#----------------------------------------------------------------------------
#3. Teen survey bar and pie - nothing left depending on colour alone

teens <- data.frame(
  Response = factor(c("Never", "Rarely", "Sometimes", "Often"),
                    levels = c("Never", "Rarely", "Sometimes", "Often")),
  Proportion = c(0.39, 0.29, 0.24, 0.08))

#A single-hue sequential ramp: the categories are ordered, and lightness carries
#that order for anyone who cannot separate the hues.
REDS <- c("#FEE0D2", "#FC9272", "#EF3B2C", "#A50F15")

p_bar <- ggplot(teens, aes(x = Response, y = Proportion, fill = Response)) +
  geom_col(colour = "black", linewidth = 0.6, width = 0.75) +
  geom_text(aes(label = sprintf("%.2f", Proportion)), vjust = -0.45, size = 5) +
  scale_fill_manual(values = REDS, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Are You Losing Focus In Class By Checking Your Cell Phone?",
       x = "Response", y = "Relative Frequency") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 13))
save_at(p_bar, "teens_bar.png", 1.4761)

p_pie <- ggplot(teens, aes(x = "", y = Proportion, fill = Response)) +
  geom_col(colour = "black", linewidth = 0.6, width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(Response, "\n", sprintf("%.2f", Proportion)), colour = Response),
            position = position_stack(vjust = 0.5), size = 4.6) +
  scale_fill_manual(values = REDS, guide = "none") +
  scale_colour_manual(values = c(Never = "black", Rarely = "black",
                                 Sometimes = "white", Often = "white"),
                      guide = "none") +   #black on the dark slices is under contrast
  labs(title = "Response") +
  theme_void(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
save_at(p_pie, "teens_pie.png", 1.3523)

message("done")

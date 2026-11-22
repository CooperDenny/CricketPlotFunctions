##################################################################
# cricket_plot_functions.R
# R Project: CricketPlotFunctions
#
# Usage:
#   source("https://raw.githubusercontent.com/CooperDenny/CricketPlotFunctions/main/cricket_plot_functions.R")
#   cricket_field(straight_boundary = 65, square_boundary = 65)
#   cricket_pitch(protected_area = TRUE)
#   cricket_pitch_annotated(lengths = c(yorker = 2, full = 6, good = 8))
#   cricket_pitch_map(deliveries)
#   cricket_beehive()
#   cricket_wagon_wheel(shots)
#   cricket_scoring_zones(shots)
#   cricket_fielding_positions()
#   cricket_manhattan(overs); cricket_worm(overs)
#
# Coordinates are metres with the origin at the centre of the pitch; the pitch
# runs along the y axis with the batter's stumps at the top (+y) and the bowler at
# the bottom. A right-hander faces the off side with their left shoulder towards
# the bowler, so their off side is -x (the left of the plot, as on TV from behind
# the bowler). Every function returns a ggplot object.
##################################################################

#Load necessary libraries
library(ggplot2)    #Package for creating graphics


##################################################################
# Shared geometry
##################################################################

stumps_y   <- 10.06    #Half of the 22 yards (20.12m) between the stumps
popping_y  <- 8.84     #Popping crease, 1.22m (4ft) in front of the stumps
return_y   <- 11.28    #Return creases run 2.44m (8ft) behind the popping crease
inner_r    <- 27.43    #Radius of the fielding restriction ("30 yard") circle

#Crease markings at both ends: popping creases, bowling creases and return creases
crease_lines <- data.frame(
  x    = c(-2.1, -2.1, -1.32, -1.32, -1.32,  1.32, -1.32, 1.32),
  xend = c( 2.1,  2.1,  1.32,  1.32, -1.32,  1.32, -1.32, 1.32),
  y    = c(-popping_y, popping_y, -stumps_y, stumps_y, -popping_y, -popping_y, popping_y, popping_y),
  yend = c(-popping_y, popping_y, -stumps_y, stumps_y, -return_y, -return_y, return_y, return_y)
)

#Wide guidelines between the popping and bowling creases (blue)
wide_lines <- data.frame(
  x    = c(0.882, -0.882, 0.882, -0.882),
  xend = c(0.882, -0.882, 0.882, -0.882),
  y    = c(popping_y, popping_y, -popping_y, -popping_y),
  yend = c(stumps_y, stumps_y, -stumps_y, -stumps_y)
)

#Three stumps at each end (red)
stumps <- data.frame(
  x = rep(c(-0.125, 0, 0.125), times = 2),
  y = rep(c(-stumps_y, stumps_y), each = 3)
)

#Points around an ellipse centred on the origin
ellipse_path <- function(a, b, n = 361) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = a * cos(t), y = b * sin(t))
}

#The fielding restriction circle: semicircles of 30 yards radius centred on each
#set of stumps, joined by straight lines
inner_circle_path <- function(n = 100) {
  top    <- seq(0, pi, length.out = n)
  bottom <- seq(pi, 2 * pi, length.out = n)
  data.frame(
    x = c(inner_r * cos(top), inner_r * cos(bottom), inner_r),
    y = c(stumps_y + inner_r * sin(top), -stumps_y + inner_r * sin(bottom), stumps_y)
  )
}

#Layers that draw the creases, and optionally the wide guidelines and stumps
pitch_markings <- function(linewidth, wide_guides = TRUE, show_stumps = TRUE) {
  layers <- list(
    geom_segment(data = crease_lines, aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = linewidth, inherit.aes = FALSE)
  )
  if (wide_guides) {
    layers <- c(layers, geom_segment(data = wide_lines, aes(x = x, y = y, xend = xend, yend = yend),
                                     linewidth = linewidth, color = "#419CE1", inherit.aes = FALSE))
  }
  if (show_stumps) {
    layers <- c(layers, geom_point(data = stumps, aes(x = x, y = y), color = "red", size = 1,
                                   inherit.aes = FALSE))
  }
  layers
}

#Default length boundaries for a pace bowler, in metres from the batter's stumps:
#yorker 0-2m, full 2-6m, good 6-8m, short beyond 8m
default_lengths <- c(yorker = 2, full = 6, good = 8)

check_lengths <- function(lengths) {
  if (!is.numeric(lengths) || length(lengths) != 3 || any(is.na(lengths)) ||
      any(diff(c(0, lengths)) <= 0) || lengths[3] >= 2 * stumps_y) {
    stop("lengths must be three increasing distances (m) from the batter's stumps, ",
         "each less than ", 2 * stumps_y, ".", call. = FALSE)
  }
  invisible(TRUE)
}

#+1 when a right-hander's off side is -x, -1 for a left-hander (off side +x)
handed_sign <- function(handedness) {
  handedness <- match.arg(toupper(handedness), c("RHB", "LHB"))
  if (handedness == "RHB") 1 else -1
}

#Direction of a shot or fielder from the batter's stumps. Angles are degrees from
#straight down the ground (0), positive towards the off side: 90 is square on the
#off side, -90 square on the leg side, 180 straight behind the wicketkeeper.
shot_direction <- function(angle, handedness = "RHB") {
  rad <- angle * pi / 180
  data.frame(dx = -handed_sign(handedness) * sin(rad), dy = -cos(rad))
}

#Distance from the batter's stumps to the boundary along each direction. The
#boundary is the ellipse x^2/a^2 + y^2/b^2 = 1 and the stumps sit at (0, stumps_y).
boundary_distance <- function(dx, dy, straight_boundary, square_boundary) {
  a <- square_boundary
  b <- straight_boundary
  qa <- dx^2 / a^2 + dy^2 / b^2
  qb <- 2 * stumps_y * dy / b^2
  qc <- stumps_y^2 / b^2 - 1
  (-qb + sqrt(qb^2 - 4 * qa * qc)) / (2 * qa)
}

check_boundaries <- function(straight_boundary, square_boundary) {
  stopifnot(is.numeric(straight_boundary), straight_boundary > inner_r + stumps_y,
            is.numeric(square_boundary), square_boundary > inner_r)
}

#Text labels marking the off and leg sides at the batter's end
side_labels <- function(handedness, x, y, size = 3.2, colour = "grey30") {
  s <- handed_sign(handedness)
  annotate("text", x = c(-s * x, s * x), y = y, label = c("OFF", "LEG"), size = size,
           colour = colour, fontface = "bold")
}


##################################################################
# Plotting functions
##################################################################

#Define the function to plot the cricket field
cricket_field <- function(straight_boundary = 65, square_boundary = 65) {

  #This function, cricket_field, generates a graphical representation of an oval/circle cricket ground.
  #The field has specific markings such as the 30 yard circle, the pitch and the boundary line

  #The function takes two parameters:
  #- straight_boundary: The distance from the middle of the pitch to the boundary, along the line of the pitch (default is 65 metres).
  #- square_boundary: The distance from the middle of the pitch to the boundary, square of the pitch (default is 65 metres).

  check_boundaries(straight_boundary, square_boundary)

  ggplot() +

    #Plot the shape of the oval
    geom_polygon(data = ellipse_path(square_boundary + 5, straight_boundary + 5), aes(x, y), fill = "#65AF56") +

    #Plot the boundary line
    geom_path(data = ellipse_path(square_boundary, straight_boundary), aes(x, y), linewidth = 0.8, color = "white") +

    #Plot the 30 yard circle
    geom_path(data = inner_circle_path(), aes(x, y), linewidth = 0.8, linetype = "dotted", color = "white") +

    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -return_y, ymax = return_y,
             alpha = 1, fill = "#EDDDB1", color = "black", linewidth = 0.1) +

    #Plot the pitch markings
    pitch_markings(linewidth = 0.15, wide_guides = FALSE, show_stumps = FALSE) +

    #Remove background and set aspect ratio
    theme_void() +
    coord_fixed(ratio = 1)
}


cricket_pitch <- function(protected_area = FALSE){

  #This function, cricket_pitch, generates a graphical representation of a cricket pitch

  #- protected_area: shade the area bowlers must not run on (Law 41.14): 1ft (30.48cm)
  #  either side of the line joining the middle stumps, from 5ft (1.52m) in front of
  #  one popping crease to 5ft in front of the other.

  p <- ggplot() +

    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -return_y, ymax = return_y,
             alpha = 1, fill = "#EDDDB1")

  if (protected_area) {
    p <- p + annotate("rect", xmin = -0.3048, xmax = 0.3048,
                      ymin = -(popping_y - 1.524), ymax = popping_y - 1.524,
                      fill = "#C0392B", alpha = 0.25, colour = "#C0392B", linewidth = 0.4,
                      linetype = "dashed")
  }

  p +
    #Plot the creases, wide guidelines and stumps
    pitch_markings(linewidth = 1) +

    #Remove background and set aspect ratio
    theme_void() +
    coord_fixed(ratio = 1)
}




#Coloured length zones on the pitch, measured from the batter's stumps
length_zones <- function(lengths) {
  edges <- stumps_y - c(0, lengths, 2 * stumps_y)
  data.frame(
    zone = c("YORKER", "FULL", "GOOD", "SHORT"),
    ymax = edges[1:4],
    ymin = edges[2:5],
    fill = c("#F1F7A1", "#A1F7AB", "#F7A1A1", "#A1BEF7")
  )
}

cricket_pitch_annotated <- function(lengths = default_lengths, handedness = NULL){

  #This function, cricket_pitch_annotated, generates a graphical representation of an annotated cricket pitch
  #It includes coloured markings for different lengths, measured from the batter's stumps (top of the plot)

  #- lengths: where the yorker, full and good lengths end, in metres from the batter's
  #  stumps. The default c(yorker = 2, full = 6, good = 8) is the common pace-bowling
  #  convention; spinners' lengths are fuller, so pass your own breaks for them.
  #- handedness: "RHB" or "LHB" to label the off and leg sides, or NULL for no labels.

  check_lengths(lengths)
  zones <- length_zones(lengths)

  #Distance labels sit on the zone boundaries; zone names sit in the middle of each zone
  ticks <- data.frame(y = stumps_y - c(0, lengths, stumps_y),
                      label = c("STUMPS", paste0(format(lengths, trim = TRUE), "m"), "HALFWAY"))
  names_at <- data.frame(y = (zones$ymin + zones$ymax) / 2, label = zones$zone)
  names_at$y[4] <- (zones$ymax[4] + 0) / 2   #Centre the SHORT label between its start and halfway

  p <- ggplot() +

    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -return_y, ymax = return_y,
             alpha = 1, fill = "#EDDDB1") +

    #Fill in each length zone
    geom_rect(data = zones, aes(xmin = -1.83, xmax = 1.83, ymin = ymin, ymax = ymax),
              fill = zones$fill, inherit.aes = FALSE) +

    #Fill in area in line with the stumps (the 9 inch wicket) in grey
    annotate("rect", xmin = -0.1143, xmax = 0.1143,
             ymin = -stumps_y, ymax = stumps_y,
             alpha = 0.4, fill = "grey") +

    #Plot the creases, wide guidelines and stumps
    pitch_markings(linewidth = 1) +

    #Ticks and distances on the zone boundaries (left), zone names (right)
    geom_segment(data = ticks, aes(x = -2.15, xend = -1.83, y = y, yend = y), linewidth = 0.4,
                 inherit.aes = FALSE) +
    geom_text(data = ticks, aes(x = -2.3, y = y, label = label), hjust = 1, size = 3,
              inherit.aes = FALSE) +
    geom_text(data = names_at, aes(x = 2.05, y = y, label = label), hjust = 0, size = 2.8,
              colour = "grey35", fontface = "bold", inherit.aes = FALSE)

  if (!is.null(handedness)) {
    p <- p + side_labels(handedness, x = 1.1, y = return_y + 0.6, size = 2.8)
  }

  p +
    #Remove background, set aspect ratio, and widen the view so the labels aren't cut off
    theme_void() +
    coord_fixed(ratio = 1, xlim = c(-6, 6), ylim = c(-12, 12.5))
}


cricket_pitch_map <- function(data, length = "length", line = "line", colour = NULL,
                              lengths = default_lengths, handedness = "RHB") {

  #Where each delivery pitched, on the annotated pitch.
  #- length: column of distances (m) from the batter's stumps to where the ball pitched
  #- line:   column of distances (m) from the middle-stump line, positive towards the
  #          batter's off side (so the same data plots correctly for either hand)
  #- colour: optional column to fill the points by (e.g. outcome)

  needed <- c(length, line, colour)
  if (!all(needed %in% names(data))) {
    stop("data must contain the columns ", paste(needed, collapse = ", "), ".", call. = FALSE)
  }
  pts <- data.frame(x = -handed_sign(handedness) * data[[line]], y = stumps_y - data[[length]])
  if (!is.null(colour)) pts$group <- data[[colour]]

  p <- cricket_pitch_annotated(lengths, handedness = handedness)
  if (is.null(colour)) {
    p + geom_point(data = pts, aes(x, y), shape = 21, fill = "white", colour = "black",
                   size = 2.2, stroke = 0.5, inherit.aes = FALSE)
  } else {
    p + geom_point(data = pts, aes(x, y, fill = group), shape = 21, colour = "black",
                   size = 2.2, stroke = 0.5, inherit.aes = FALSE) +
      labs(fill = NULL) + theme(legend.position = "bottom")
  }
}


cricket_beehive <- function(handedness = "RHB") {

  #The batter's end as seen from behind the bowler: where deliveries passed the
  #stumps. x is metres from the middle stump (as in the other plots, so a right-hander's
  #off side is -x) and y is height above the ground in metres. Add points with
  #geom_point(aes(x = lateral, y = height)).

  #Law 8: stumps 71.12cm (28in) high, wicket 22.86cm (9in) wide, stumps up to 3.81cm
  #across; bails sit no more than 1.27cm (0.5in) above the stumps.
  stump_w <- 0.0381
  wicket  <- data.frame(
    xmin = c(-0.1143, -stump_w / 2, 0.1143 - stump_w),
    xmax = c(-0.1143 + stump_w, stump_w / 2, 0.1143),
    ymin = 0, ymax = 0.7112
  )
  bails <- data.frame(xmin = c(-0.1143, 0.002), xmax = c(-0.002, 0.1143),
                      ymin = 0.7112, ymax = 0.7239)
  #Where the wide guidelines and return creases meet the batter's popping crease
  ground_marks <- data.frame(x = c(-1.32, -0.882, 0.882, 1.32), colour = c("black", "#419CE1", "#419CE1", "black"))

  ggplot() +
    #Sky and pitch
    annotate("rect", xmin = -1.6, xmax = 1.6, ymin = 0, ymax = 2, fill = "#F4F7FB") +
    annotate("rect", xmin = -1.6, xmax = 1.6, ymin = -0.12, ymax = 0, fill = "#EDDDB1") +

    #The width of the wicket, extended upwards, so balls "hitting" are easy to read
    annotate("rect", xmin = -0.1143, xmax = 0.1143, ymin = 0, ymax = 2, fill = "grey", alpha = 0.2) +

    #Height guides every 0.5m
    geom_hline(yintercept = c(0.5, 1, 1.5), colour = "grey85", linewidth = 0.3) +
    annotate("text", x = -1.58, y = c(0.5, 1, 1.5), label = c("0.5m", "1.0m", "1.5m"),
             hjust = 0, vjust = -0.4, size = 2.6, colour = "grey50") +

    #Crease markings at ground level
    geom_segment(data = ground_marks, aes(x = x, xend = x, y = -0.12, yend = 0),
                 colour = ground_marks$colour, linewidth = 0.8, inherit.aes = FALSE) +

    #Stumps and bails
    geom_rect(data = wicket, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#F3E3C3", colour = "grey30", linewidth = 0.3, inherit.aes = FALSE) +
    geom_rect(data = bails, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#C8102E", inherit.aes = FALSE) +

    side_labels(handedness, x = 1.2, y = 1.88, size = 3) +

    theme_void() +
    coord_fixed(ratio = 1, xlim = c(-1.6, 1.6), ylim = c(-0.12, 2), expand = FALSE)
}


#Colours for runs off the bat
run_colours <- c(`1` = "white", `2` = "#FFD23F", `3` = "#FB8500", `4` = "#3A86FF",
                 `5` = "#8338EC", `6` = "#E63946")

cricket_wagon_wheel <- function(data, angle = "angle", distance = "distance", runs = "runs",
                                straight_boundary = 65, square_boundary = 65,
                                handedness = "RHB", show_dots = FALSE) {

  #Every scoring shot as a line from the batter's stumps.
  #- angle:    column of shot directions in degrees from straight down the ground,
  #            positive towards the off side (90 = square on the off side, -90 = square
  #            on the leg side, 180 = straight behind)
  #- distance: column of distances (m) the ball travelled from the batter's stumps.
  #            NA, or anything past the rope, is drawn to the boundary (use this for 4s and 6s)
  #- runs:     column of runs off the bat, which sets the colour

  check_boundaries(straight_boundary, square_boundary)
  needed <- c(angle, distance, runs)
  if (!all(needed %in% names(data))) {
    stop("data must contain the columns ", paste(needed, collapse = ", "), ".", call. = FALSE)
  }
  if (!show_dots) data <- data[data[[runs]] > 0, , drop = FALSE]

  dir   <- shot_direction(data[[angle]], handedness)
  reach <- boundary_distance(dir$dx, dir$dy, straight_boundary, square_boundary)
  d     <- data[[distance]]
  d     <- ifelse(is.na(d) | d > reach, reach, d)

  shots <- data.frame(x = 0, y = stumps_y, xend = dir$dx * d, yend = stumps_y + dir$dy * d,
                      runs = factor(data[[runs]], levels = sort(unique(c(0:6, data[[runs]])))))
  shots <- shots[order(data[[runs]]), ]   #Draw boundaries on top of singles

  totals <- tapply(data[[runs]], factor(data[[runs]]), sum)
  caption <- paste0(sum(data[[runs]]), " runs: ",
                    paste0(names(totals), "s ", tapply(data[[runs]], factor(data[[runs]]), length),
                           collapse = ", "))

  cricket_field(straight_boundary, square_boundary) +
    geom_segment(data = shots, aes(x = x, y = y, xend = xend, yend = yend, colour = runs),
                 linewidth = 0.7, lineend = "round", inherit.aes = FALSE) +
    scale_colour_manual(values = c(`0` = "grey40", run_colours), drop = TRUE, name = "Runs") +
    side_labels(handedness, x = square_boundary * 0.55, y = straight_boundary + 9) +
    labs(caption = caption) +
    theme(legend.position = "right", plot.caption = element_text(hjust = 0.5, colour = "grey30"),
          legend.key = element_rect(fill = "#65AF56", colour = NA))
}


cricket_scoring_zones <- function(data, angle = "angle", runs = "runs",
                                  straight_boundary = 65, square_boundary = 65,
                                  handedness = "RHB", colour = "#B2182B") {

  #Runs scored in each of eight 45 degree sectors around the batter, the usual
  #broadcast summary of a wagon wheel. Uses the same angle convention as
  #cricket_wagon_wheel().

  check_boundaries(straight_boundary, square_boundary)
  if (!all(c(angle, runs) %in% names(data))) {
    stop("data must contain the columns ", angle, " and ", runs, ".", call. = FALSE)
  }

  sectors <- data.frame(
    name  = c("Mid-off", "Cover", "Point", "Third man", "Fine leg", "Square leg", "Midwicket", "Mid-on"),
    start = c(0, 45, 90, 135, -180, -135, -90, -45)
  )
  a <- ((data[[angle]] + 180) %% 360) - 180   #Wrap angles into [-180, 180)
  idx <- findInterval(a, c(-180, -135, -90, -45, 0, 45, 90, 135, 180))
  order_in_breaks <- c("Fine leg", "Square leg", "Midwicket", "Mid-on", "Mid-off", "Cover", "Point", "Third man")
  sector_runs <- tapply(data[[runs]], factor(order_in_breaks[idx], levels = sectors$name), sum)
  sector_runs[is.na(sector_runs)] <- 0
  sectors$runs  <- as.numeric(sector_runs[sectors$name])
  sectors$share <- sectors$runs / max(1, sum(sectors$runs))

  wedges <- do.call(rbind, lapply(seq_len(nrow(sectors)), function(i) {
    th  <- seq(sectors$start[i], sectors$start[i] + 45, by = 1)
    dir <- shot_direction(th, handedness)
    r   <- boundary_distance(dir$dx, dir$dy, straight_boundary, square_boundary)
    data.frame(name = sectors$name[i], share = sectors$share[i],
               x = c(0, dir$dx * r, 0), y = c(stumps_y, stumps_y + dir$dy * r, stumps_y))
  }))

  mid <- shot_direction(sectors$start + 22.5, handedness)
  r   <- boundary_distance(mid$dx, mid$dy, straight_boundary, square_boundary)
  labels <- data.frame(x = mid$dx * r * 0.62, y = stumps_y + mid$dy * r * 0.62,
                       label = sprintf("%s\n%d (%.0f%%)", sectors$name, as.integer(sectors$runs),
                                       100 * sectors$share))

  cricket_field(straight_boundary, square_boundary) +
    geom_polygon(data = wedges, aes(x, y, group = name, fill = share), colour = "white",
                 linewidth = 0.4, alpha = 0.85, inherit.aes = FALSE) +
    scale_fill_gradient(low = "#FFF1E6", high = colour, limits = c(0, NA), guide = "none") +
    geom_label(data = labels, aes(x, y, label = label), size = 2.8, lineheight = 0.9,
               label.size = 0, fill = "white", alpha = 0.85, inherit.aes = FALSE) +
    side_labels(handedness, x = square_boundary * 0.55, y = straight_boundary + 9)
}


#Approximate positions of the named fielding positions. Angles use the wagon wheel
#convention (degrees from straight down the ground, positive to the off side).
#Close fielders are a distance in metres from the batter's stumps; outfielders
#(boundary = TRUE) stand at `distance` x the distance to the rope in that direction.
#Fielding positions are named regions rather than fixed points, so treat these as
#typical spots, not rules.
fielding_positions <- data.frame(
  position = c("Wicketkeeper", "Bowler", "First slip", "Second slip", "Third slip", "Gully",
               "Silly point", "Short leg", "Leg slip", "Point", "Backward point", "Cover",
               "Extra cover", "Mid-off", "Mid-on", "Midwicket", "Square leg", "Backward square leg",
               "Short fine leg", "Third man", "Deep point", "Deep cover", "Long off", "Long on",
               "Deep midwicket", "Deep square leg", "Fine leg"),
  angle    = c(180, 0, 162, 154, 146, 132, 100, -100, -165, 92, 108, 60, 40, 14, -14, -60, -92,
               -112, -150, 148, 95, 58, 10, -10, -58, -95, -162),
  distance = c(12, 22, 15, 16, 17, 15, 6, 6, 14, 24, 25, 26, 27, 26, 26, 26, 24, 23, 20,
               0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94),
  boundary = c(rep(FALSE, 19), rep(TRUE, 8)),
  #Which side of the point the label goes, so the slip cordon stays readable
  label    = c("below", "below", "above", "below", "left", "below", "left", "right", "right",
               rep("above", 18))
)

cricket_fielding_positions <- function(positions = c("Wicketkeeper", "Bowler", "First slip", "Point",
                                                     "Cover", "Mid-off", "Mid-on", "Midwicket",
                                                     "Square leg", "Third man", "Fine leg"),
                                       straight_boundary = 65, square_boundary = 65,
                                       handedness = "RHB", labels = TRUE) {

  #A field setting drawn on cricket_field(). Choose positions by name from
  #fielding_positions$position; pass positions = fielding_positions$position to see them all.

  check_boundaries(straight_boundary, square_boundary)
  unknown <- setdiff(positions, fielding_positions$position)
  if (length(unknown) > 0) {
    stop("Unknown fielding position(s): ", paste(unknown, collapse = ", "),
         ". See fielding_positions$position.", call. = FALSE)
  }
  f   <- fielding_positions[match(positions, fielding_positions$position), ]
  dir <- shot_direction(f$angle, handedness)
  r   <- ifelse(f$boundary,
                f$distance * boundary_distance(dir$dx, dir$dy, straight_boundary, square_boundary),
                f$distance)
  f$x <- dir$dx * r
  f$y <- stumps_y + dir$dy * r
  f$role <- ifelse(f$position %in% c("Wicketkeeper", "Bowler"), "Keeper / bowler", "Fielder")

  p <- cricket_field(straight_boundary, square_boundary) +
    geom_point(data = f, aes(x, y, fill = role), shape = 21, size = 3.2, colour = "black",
               stroke = 0.6, inherit.aes = FALSE) +
    scale_fill_manual(values = c(Fielder = "white", `Keeper / bowler` = "#FFD23F"), guide = "none") +
    side_labels(handedness, x = square_boundary * 0.55, y = straight_boundary + 9)

  if (labels) {
    #Labels on the off side of a left-hander mirror their side
    side <- f$label
    if (handed_sign(handedness) < 0) side <- c(left = "right", right = "left", above = "above",
                                               below = "below")[side]
    f$hjust <- c(above = 0.5, below = 0.5, left = 1.12, right = -0.12)[side]
    f$vjust <- c(above = -1.1, below = 2.1, left = 0.5, right = 0.5)[side]
    p <- p + geom_text(data = f, aes(x, y, label = position, hjust = hjust, vjust = vjust),
                       size = 2.5, colour = "grey10", inherit.aes = FALSE)
  }
  p
}


#Shared checks for the over-by-over charts
check_overs <- function(data, over, runs, wickets, team) {
  needed <- c(over, runs, if (!is.null(wickets)) wickets, if (!is.null(team)) team)
  if (!all(needed %in% names(data))) {
    stop("data must contain the columns ", paste(needed, collapse = ", "),
         " (one row per over, per team).", call. = FALSE)
  }
  d <- data.frame(over = data[[over]], runs = data[[runs]],
                  wickets = if (is.null(wickets)) 0 else data[[wickets]],
                  team = if (is.null(team)) "Innings" else as.character(data[[team]]))
  d[order(d$team, d$over), ]
}

#Points to mark each wicket: stacked above a bar, or on the worm line
wicket_marks <- function(d, height) {
  if (!any(d$wickets > 0)) return(NULL)
  w <- d[rep(seq_len(nrow(d)), d$wickets), ]
  w$k <- ave(w$over, w$team, w$over, FUN = seq_along)
  w$y <- height(w) + (w$k - 1) * 1.6
  w
}

team_colours <- c("#3A86FF", "#E63946", "#2A9D8F", "#FB8500")

cricket_manhattan <- function(data, over = "over", runs = "runs", wickets = "wickets",
                              team = NULL) {

  #Runs per over as bars, one bar per team side by side, with a dot above the bar for
  #each wicket that fell in the over. data has one row per over (per team).

  d <- check_overs(data, over, runs, wickets, team)
  teams <- unique(d$team)
  width <- 0.85 / length(teams)
  d$x <- d$over + (match(d$team, teams) - (length(teams) + 1) / 2) * width

  w <- wicket_marks(d, function(w) w$runs + 1.2)
  p <- ggplot(d, aes(x = x, y = runs, fill = team)) +
    geom_col(width = width * 0.92) +
    scale_fill_manual(values = team_colours, name = NULL) +
    scale_x_continuous(breaks = function(l) seq(0, ceiling(l[2]), by = if (l[2] > 25) 5 else 2),
                       expand = expansion(add = 0.6)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Over", y = "Runs") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          legend.position = if (length(teams) > 1) "top" else "none")
  if (!is.null(w)) {
    p <- p + geom_point(data = w, aes(x = x, y = y, colour = team), shape = 21, fill = "white",
                        size = 2.2, stroke = 1.1, show.legend = FALSE) +
      scale_colour_manual(values = team_colours)
  }
  p
}

cricket_worm <- function(data, over = "over", runs = "runs", wickets = "wickets", team = NULL) {

  #Cumulative runs against overs for each team, with a dot where each wicket fell.
  #data has one row per over (per team); runs are the runs scored in that over.

  d <- check_overs(data, over, runs, wickets, team)
  d$total <- ave(d$runs, d$team, FUN = cumsum)
  start <- do.call(rbind, lapply(unique(d$team), function(t) {
    data.frame(over = 0, runs = 0, wickets = 0, team = t, total = 0)
  }))
  line <- rbind(start, d)
  line <- line[order(line$team, line$over), ]

  w <- if (any(d$wickets > 0)) d[rep(seq_len(nrow(d)), d$wickets), ] else NULL
  p <- ggplot(line, aes(x = over, y = total, colour = team)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = team_colours, name = NULL) +
    scale_x_continuous(breaks = function(l) seq(0, ceiling(l[2]), by = if (l[2] > 25) 5 else 2)) +
    labs(x = "Over", y = "Runs") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = if (length(unique(d$team)) > 1) "top" else "none")
  if (!is.null(w)) {
    p <- p + geom_point(data = w, aes(x = over, y = total, colour = team), shape = 21,
                        fill = "white", size = 2.4, stroke = 1.1, show.legend = FALSE)
  }
  p
}

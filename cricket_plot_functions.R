##################################################################
# cricket_plot_functions.R
# R Project: CricketPlotFunctions
#
# Usage:
#   source("https://raw.githubusercontent.com/CooperDenny/CricketPlotFunctions/main/cricket_plot_functions.R")
#   cricket_field(straight_boundary = 65, square_boundary = 65)
#   cricket_pitch()
#   cricket_pitch_annotated()
#
# Coordinates are metres with the origin at the centre of the pitch; the pitch
# runs along the y axis. Every function returns a ggplot object.
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

  stopifnot(is.numeric(straight_boundary), straight_boundary > inner_r + stumps_y,
            is.numeric(square_boundary), square_boundary > inner_r)

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


cricket_pitch <- function(){

  #This function, cricket_pitch, generates a graphical representation of a cricket pitch

  ggplot() +

    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -return_y, ymax = return_y,
             alpha = 1, fill = "#EDDDB1") +

    #Plot the creases, wide guidelines and stumps
    pitch_markings(linewidth = 1) +

    #Remove background and set aspect ratio
    theme_void() +
    coord_fixed(ratio = 1)
}


cricket_pitch_annotated <- function(){

  #This function, cricket_pitch_annotated, generates a graphical representation of an annotated cricket pitch
  #It includes coloured markings for different lengths, measured from the batter's stumps (top of the plot)

  ggplot() +

    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -return_y, ymax = return_y,
             alpha = 1, fill = "#EDDDB1") +

    #Fill in the "yorker" length (0-2m from the stumps) in yellow
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = stumps_y - 2, ymax = stumps_y,
             alpha = 1, fill = "#F1F7A1") +

    #Fill in the "full" length (2-5m) in green
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = stumps_y - 5, ymax = stumps_y - 2,
             alpha = 1, fill = "#A1F7AB") +

    #Fill in the "good" length (5-7m) in red
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = stumps_y - 7, ymax = stumps_y - 5,
             alpha = 1, fill = "#F7A1A1") +

    #Fill in the short length (more than 7m) in blue
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -stumps_y, ymax = stumps_y - 7,
             alpha = 1, fill = "#A1BEF7") +

    #Fill in area in line with the stumps (the 9 inch wicket) in grey
    annotate("rect", xmin = -0.1143, xmax = 0.1143,
             ymin = -stumps_y, ymax = stumps_y,
             alpha = 0.4, fill = "grey") +

    #Plot the creases, wide guidelines and stumps
    pitch_markings(linewidth = 1) +

    #Add text labels at different lengths on the pitch
    annotate("text", x = -2, y = stumps_y - c(0, 2, 4, 6, 8, stumps_y),
             label = c("STUMPS", "2m", "4m", "6m", "8m", "HALFWAY"), hjust = 0.9, size = 3) +

    #Remove background, set aspect ratio, and widen the view so the labels aren't cut off
    theme_void() +
    coord_fixed(ratio = 1, xlim = c(-6, 6), ylim = c(-12, 12))
}

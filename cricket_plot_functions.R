##################################################################
# cricket_plot_functions.R
# R Project: CricketPlotFunctions
##################################################################

#Load necessary libraries
library(tidyverse)  #Collection of R packages for data manipulation and visualization
library(ggplot2)    #Package for creating graphics
library(ggforce)    #Package for additional ggplot2 functionalities




#Define the function to plot the cricket field
cricket_field <- function(straight_boundary = 65, square_boundary = 65) {
  
  #This function, cricket_field, generates a graphical representation of an oval/circle cricket ground.
  #The field has specific markings such as the 30 yard circle, the pitch and the boundary line
  
  #The function takes two parameters:
  #- straight_boundary: The vertical distance from the middle of the pitch to the boundary line (default is 65 metres).
  #- square_boundary: The vertical distance from the middle of the pitch to the boundary line (default is 65 metres).
  
  # Create a data frame with vales for the top and bottom arcs of the 30 yard circle
  angle_data <- data.frame(
    top_x = 27.4 * cos(seq(0, pi, length.out = 100)),
    top_y = 10.06 + 27.4 * sin(seq(0, pi, length.out = 100)),
    bottom_x = 27.4 * cos(seq(pi, 2*pi, length.out = 100)),
    bottom_y = -10.06 + 27.4 * sin(seq(pi, 2*pi, length.out = 100))
  )
  
  ggplot(angle_data, aes(x = top_x, y = top_y)) +
    
    #Plot the shape of the oval
    geom_ellipse(aes(x0 = 0, y0 = 0, a = square_boundary + 5, b = straight_boundary + 5, angle = 0), fill = "#65AF56") +
    
    #Plot the boundary line
    geom_ellipse(aes(x0 = 0, y0 = 0, a = square_boundary, b = straight_boundary, angle = 0), size = 0.8, color = "white") +
    
    #Plot the top arc of the 30 yard circle
    geom_path(size = 0.8, linetype = "dotted", color = "white") +
    
    #Plot the bottom arc of the 30 yard circle
    geom_path(aes(x = bottom_x, y = bottom_y), size = 0.8, linetype = "dotted", color = "white") +
    
    #Plot the left vertical line of the 30 yard circle
    geom_path(aes(x = seq(-27.4, -27.4, length.out = 100), y = seq(10.06, -10.06, length.out = 100)), size = 0.8, linetype = "dotted", color = "white") +
    
    #Plot the right vertical line of the 30 yard circle
    geom_path(aes(x = seq(27.4, 27.4, length.out = 100), y = seq(10.06, -10.06, length.out = 100)), size = 0.8, linetype = "dotted", color = "white") +
    
    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -11.28, ymax = 11.28,
             alpha = 1, fill = "#EDDDB1", color = "black", linewidth = 0.1) +
    
    #Plot the pitch markings
    geom_segment(aes(x = -2.1, y = -8.84, xend = 2.1, yend = -8.84), linewidth = 0.01) +
    geom_segment(aes(x = -2.1, y = 8.84, xend = 2.1, yend = 8.84), linewidth = 0.01) +
    geom_segment(aes(x = -1.32, y = -10.06, xend = 1.32, yend = -10.06), linewidth = 0.01) +
    geom_segment(aes(x = -1.32, y = 10.06, xend = 1.32, yend = 10.06), linewidth = 0.01) +
    geom_segment(aes(x = -1.32, y = -8.84, xend = -1.32, yend = -11.28), linewidth = 0.01) +
    geom_segment(aes(x = 1.32, y = -8.84, xend = 1.32, yend = -11.28), linewidth = 0.01) +
    geom_segment(aes(x = -1.32, y = 8.84, xend = -1.32, yend = 11.28), linewidth = 0.01) +
    geom_segment(aes(x = 1.32, y = 8.84, xend = 1.32, yend = 11.28), linewidth = 0.01) +
    
    #Remove background and set aspect ratio
    theme_void() + 
    coord_fixed(ratio = 1)
}




cricket_pitch <- function(){
  
  #This function, cricket_pitch, generates a graphical representation of a cricket pitch
  
  ggplot() +
    
    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -11.28, ymax = 11.28,
             alpha = 1, fill = "#EDDDB1") +
    
    #Plot the pitch markings
    geom_segment(aes(x = -2.1, y = -8.84, xend = 2.1, yend = -8.84), linewidth = 1) +
    geom_segment(aes(x = -2.1, y = 8.84, xend = 2.1, yend = 8.84), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = -10.06, xend = 1.32, yend = -10.06), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = 10.06, xend = 1.32, yend = 10.06), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = -8.84, xend = -1.32, yend = -11.28), linewidth = 1) +
    geom_segment(aes(x = 1.32, y = -8.84, xend = 1.32, yend = -11.28), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = 8.84, xend = -1.32, yend = 11.28), linewidth = 1) +
    geom_segment(aes(x = 1.32, y = 8.84, xend = 1.32, yend = 11.28), linewidth = 1) +
    geom_segment(aes(x = 0.882, y = 8.84, xend = 0.882, yend = 10.06), linewidth = 1, color = "#419CE1") +
    geom_segment(aes(x = -0.882, y = 8.84, xend = -0.882, yend = 10.06), linewidth = 1, color = "#419CE1") +
    geom_segment(aes(x = 0.882, y = -8.84, xend = 0.882, yend = -10.06), linewidth = 1, color = "#419CE1") +
    geom_segment(aes(x = -0.882, y = -8.84, xend = -0.882, yend = -10.06), linewidth = 1, color = "#419CE1") +
    
    #Plot the bottom set of stumps 
    geom_point(aes(x = -0.125, y = -10.06), color = "red", size = 1) +
    geom_point(aes(x = 0, y = -10.06), color = "red", size = 1) +
    geom_point(aes(x = 0.125, y = -10.06), color = "red", size = 1) +
    
    #Plot the top set of stumps
    geom_point(aes(x = -0.125, y = 10.06), color = "red", size = 1) +
    geom_point(aes(x = 0, y = 10.06), color = "red", size = 1) +
    geom_point(aes(x = 0.125, y = 10.06), color = "red", size = 1) +
    
    #Remove background and set aspect ratio
    theme_void() + 
    coord_fixed(ratio = 1)
}




cricket_pitch_annotated <- function(){
  
  #This function, cricket_pitch_annotated, generates a graphical representation of an annotated cricket pitch
  #It includes coloured markings for different lengths

  ggplot() +
    
    #Plot the rectangle shape of the pitch
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -11.28, ymax = 11.28,
             alpha = 1, fill = "#EDDDB1") +
    
    #Fill in the "yorker" length in yellow
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = 8.06, ymax = 10.06,
             alpha = 1, fill = "#F1F7A1") +
    
    #Fill in the "full" length in green
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = 5.06, ymax = 8.06,
             alpha = 1, fill = "#A1F7AB") +
    
    #Fill in the "good" length in red
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = 3.06, ymax = 5.06,
             alpha = 1, fill = "#F7A1A1") +
    
    #Fill in the short length in blue
    annotate("rect", xmin = -1.83, xmax = 1.83,
             ymin = -10.06, ymax = 3.06,
             alpha = 1, fill = "#A1BEF7") +
    
    #Fill in area in line with the stumps in grey
    annotate("rect", xmin = -0.1143, xmax = 0.1143,
             ymin = -10.06, ymax = 10.06,
             alpha = 0.4, fill = "grey") +
    
    #Plot the pitch markings
    geom_segment(aes(x = -2.1, y = -8.84, xend = 2.1, yend = -8.84), linewidth = 1) +
    geom_segment(aes(x = -2.1, y = 8.84, xend = 2.1, yend = 8.84), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = -10.06, xend = 1.32, yend = -10.06), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = 10.06, xend = 1.32, yend = 10.06), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = -8.84, xend = -1.32, yend = -11.28), linewidth = 1) +
    geom_segment(aes(x = 1.32, y = -8.84, xend = 1.32, yend = -11.28), linewidth = 1) +
    geom_segment(aes(x = -1.32, y = 8.84, xend = -1.32, yend = 11.28), linewidth = 1) +
    geom_segment(aes(x = 1.32, y = 8.84, xend = 1.32, yend = 11.28), linewidth = 1) +
    geom_segment(aes(x = 0.882, y = 8.84, xend = 0.882, yend = 10.06), linewidth = 1, color = "#419CE1") +
    geom_segment(aes(x = -0.882, y = 8.84, xend = -0.882, yend = 10.06), linewidth = 1, color = "#419CE1") +
    geom_segment(aes(x = 0.882, y = -8.84, xend = 0.882, yend = -10.06), linewidth = 1, color = "#419CE1") +
    geom_segment(aes(x = -0.882, y = -8.84, xend = -0.882, yend = -10.06), linewidth = 1, color = "#419CE1") +
    
    #Plot the bottom set of stumps 
    geom_point(aes(x = -0.125, y = -10.06), color = "red", size = 1) +
    geom_point(aes(x = 0, y = -10.06), color = "red", size = 1) +
    geom_point(aes(x = 0.125, y = -10.06), color = "red", size = 1) +
    
    #Plot the top set of stumps
    geom_point(aes(x = -0.125, y = 10.06), color = "red", size = 1) +
    geom_point(aes(x = 0, y = 10.06), color = "red", size = 1) +
    geom_point(aes(x = 0.125, y = 10.06), color = "red", size = 1) +
    
    #Add text labels at different lengths on the pitch
    geom_text(aes(x = -2, y = 10.06), label = "STUMPS", hjust = 0.9, size = 3) +
    geom_text(aes(x = -2, y = 8.06), label = "2m", hjust = 0.9, size = 3) +
    geom_text(aes(x = -2, y = 6.06), label = "4m", hjust = 0.9, size = 3) +
    geom_text(aes(x = -2, y = 4.06), label = "6m", hjust = 0.9, size = 3) +
    geom_text(aes(x = -2, y = 2.06), label = "8m", hjust = 0.9, size = 3) +
    geom_text(aes(x = -2, y = 0), label = "HALFWAY", hjust = 0.9, size = 3) +
    
    #Remove background and set aspect ratio
    theme_void() + 
    coord_fixed(ratio = 1) + 
    
    #Adjust the plot x and y limits so that it doesn't cut off the text
    xlim(-6,6) + 
    ylim(-12,12)
}

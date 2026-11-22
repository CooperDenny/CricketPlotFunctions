##################################################################
# examples.R
# Regenerates every image in images/. The ball-by-ball data is simulated
# (set.seed), so the plots show what each function draws, not a real innings.
#   Rscript examples.R
##################################################################

source("cricket_plot_functions.R")

save <- function(plot, file, width, height) {
  ggsave(file.path("images", file), plot, width = width, height = height, dpi = 150, bg = "white")
}

set.seed(2026)

##################################################################
# Grounds and pitches
##################################################################

save(cricket_field(straight_boundary = 60, square_boundary = 70), "cricket_field_1.png", 7, 6.2)
save(cricket_field(straight_boundary = 75, square_boundary = 65), "cricket_field_2.png", 6.2, 7)
save(cricket_pitch(), "cricket_pitch.png", 3, 7)
save(cricket_pitch(protected_area = TRUE), "cricket_pitch_protected_area.png", 3, 7)
save(cricket_pitch_annotated(handedness = "RHB"), "cricket_pitch_annotated.png", 4, 7)

##################################################################
# Pitch map and beehive: 120 simulated deliveries from a seam bowler
##################################################################

n <- 120
deliveries <- data.frame(
  length = pmin(pmax(rnorm(n, 6.8, 1.9), 0.3), 14),   #Metres from the batter's stumps
  line   = rnorm(n, 0.22, 0.28)                       #Metres from middle stump, + = off side
)
#Where the same deliveries passed the stumps: x is lateral position (m from middle
#stump, a right-hander's off side is -x) and y is height (m). Shorter balls bounce higher.
deliveries$lateral <- -deliveries$line + rnorm(n, 0, 0.06)
deliveries$height  <- pmax(0.05, 0.25 + 0.075 * deliveries$length + rnorm(n, 0, 0.12))
#Balls that would hit the stumps take most of the wickets in this simulation
hitting <- abs(deliveries$lateral) < 0.1143 & deliveries$height < 0.72
deliveries$outcome <- factor(ifelse(hitting & runif(n) < 0.4, "Wicket",
                             ifelse(runif(n) < 0.65, "Dot", "Runs")),
                             levels = c("Dot", "Runs", "Wicket"))
pitch_map <- cricket_pitch_map(deliveries, colour = "outcome") +
  scale_fill_manual(values = c(Dot = "white", Runs = "#3A86FF", Wicket = "#E63946"))
save(pitch_map, "cricket_pitch_map.png", 4.2, 7.4)

beehive <- cricket_beehive("RHB") +
  geom_point(data = deliveries, aes(x = lateral, y = height, fill = outcome), shape = 21,
             size = 2.2, colour = "black", stroke = 0.4) +
  scale_fill_manual(values = c(Dot = "white", Runs = "#3A86FF", Wicket = "#E63946"), name = NULL) +
  theme(legend.position = "bottom")
save(beehive, "cricket_beehive.png", 6, 4.6)

##################################################################
# Wagon wheel and scoring zones: 70 simulated scoring shots
##################################################################

m <- 70
shots <- data.frame(
  angle = c(rnorm(39, 55, 35), rnorm(m - 39, -70, 40)),   #Degrees, + = off side
  runs  = sample(c(1, 2, 3, 4, 6), m, replace = TRUE, prob = c(0.52, 0.18, 0.03, 0.2, 0.07))
)
shots$distance <- ifelse(shots$runs == 1, runif(m, 15, 35),
                  ifelse(shots$runs %in% c(2, 3), runif(m, 40, 58), NA))   #NA = to the rope
save(cricket_wagon_wheel(shots, straight_boundary = 68, square_boundary = 64),
     "cricket_wagon_wheel.png", 7, 7)
save(cricket_scoring_zones(shots, straight_boundary = 68, square_boundary = 64),
     "cricket_scoring_zones.png", 6.4, 7)

##################################################################
# Field settings
##################################################################

save(cricket_fielding_positions(), "cricket_fielding_positions.png", 6.4, 7)
save(cricket_fielding_positions(positions = fielding_positions$position),
     "cricket_fielding_positions_all.png", 7.4, 8)

##################################################################
# Manhattan and worm: two simulated T20 innings
##################################################################

innings <- function(team, boost) {
  over <- 1:20
  rate <- 7 + boost + ifelse(over <= 6, 1.2, 0) + ifelse(over >= 16, 3.5, 0)
  data.frame(team = team, over = over, runs = rpois(20, rate),
             wickets = rbinom(20, 2, ifelse(over >= 16, 0.18, 0.1)))
}
overs <- rbind(innings("Team A", 0.6), innings("Team B", 0))
save(cricket_manhattan(overs, team = "team"), "cricket_manhattan.png", 8, 4.2)
save(cricket_worm(overs, team = "team"), "cricket_worm.png", 8, 4.2)

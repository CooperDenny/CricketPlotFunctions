# CricketPlotFunctions
R functions that draw cricket grounds and pitches to scale with `ggplot2`, plus the standard analysis views on top of them: pitch maps, beehives, wagon wheels, scoring zones, field settings, Manhattans and worms.

## Usage

```r
source("https://raw.githubusercontent.com/CooperDenny/CricketPlotFunctions/main/cricket_plot_functions.R")

cricket_field(straight_boundary = 65, square_boundary = 65)  # boundary distances from the centre of the pitch (m)
cricket_pitch()
cricket_pitch_annotated()                                     # yorker / full / good / short lengths
```

Each function returns a ggplot object, so you can add your own layers in metres, with the origin at the centre of the pitch and the pitch running along the y axis. Requires `ggplot2` (3.4.0 or later) only.

### Orientation

The batter's stumps are at the top (+y) and the bowler at the bottom. A right-hander stands with their left shoulder to the bowler and faces the off side, so **a right-hander's off side is −x** (the left of the plot, as on TV from behind the bowler). Functions that depend on this take `handedness = "RHB"` or `"LHB"` and print OFF / LEG labels so the orientation is never ambiguous.

Shot and fielder directions use one angle convention everywhere: degrees from straight down the ground (0), **positive towards the off side**. 90 is square on the off side, −90 square on the leg side, 180 straight behind the wicketkeeper. Because the angle is relative to the off side, the same data plots correctly for either hand.

## Functions

| Function | What it draws |
|---|---|
| `cricket_field(straight_boundary, square_boundary)` | The ground: boundary, 30-yard circle and pitch |
| `cricket_pitch(protected_area = FALSE)` | The pitch with creases, wide guidelines and stumps. `protected_area = TRUE` shades the area bowlers must not run on (Law 41.14) |
| `cricket_pitch_annotated(lengths = c(yorker = 2, full = 6, good = 8), handedness = NULL)` | The pitch with length zones measured from the batter's stumps |
| `cricket_pitch_map(data, length, line, colour)` | Where each delivery pitched, on the annotated pitch |
| `cricket_beehive(handedness)` | The batter's end from behind the bowler, to plot where deliveries passed the stumps |
| `cricket_wagon_wheel(data, angle, distance, runs)` | Every scoring shot as a line from the batter, coloured by runs |
| `cricket_scoring_zones(data, angle, runs)` | Runs in eight 45° sectors around the batter |
| `cricket_fielding_positions(positions)` | A field setting by position name (see `fielding_positions$position`) |
| `cricket_manhattan(data, over, runs, wickets, team)` | Runs per over as bars, with wicket markers |
| `cricket_worm(data, over, runs, wickets, team)` | Cumulative runs by over, with wicket markers |

### Data each function expects

- **Pitch map**: one row per delivery. `length` is metres from the batter's stumps to where the ball pitched; `line` is metres from the middle-stump line, positive towards the off side.
- **Beehive**: add `geom_point(aes(x = lateral, y = height))`, where `lateral` is metres from the middle stump in plot coordinates (a right-hander's off side is −x) and `height` is metres above the ground.
- **Wagon wheel / scoring zones**: one row per shot with `angle` (degrees, convention above) and `runs`. The wagon wheel also takes `distance`, in metres from the stumps. Use `NA` for boundaries, and any distance past the rope is drawn to the rope.
- **Manhattan / worm**: one row per over (per team) with `over`, `runs` and `wickets`. From ball-by-ball data: `aggregate(cbind(runs, wickets) ~ team + over, balls, sum)`.

### Length zones

The default `lengths = c(yorker = 2, full = 6, good = 8)` is the common pace-bowling convention: yorker 0–2 m from the batter's stumps, full 2–6 m, good 6–8 m, short beyond 8 m. Earlier versions of this repo used 2/5/7 m and labelled 2/4/6/8 m, so the labels didn't line up with the zones. Labels now sit on the zone boundaries. Spinners' lengths are fuller, so pass your own breaks for them. To get the old zones back, use `cricket_pitch_annotated(lengths = c(2, 5, 7))`.

### Fielding positions

Fielding positions are named regions, not fixed points. `fielding_positions` holds typical spots: close fielders at a set distance from the batter's stumps, outfielders at 94% of the distance to the rope in their direction, so they follow the shape of the ground.

## Contents
- `cricket_plot_functions.R`: all functions and the shared geometry
- `examples.R`: regenerates every image in `images/` (the ball-by-ball data is simulated)
- `CricketPlotFunctions.Rproj`: R project file for the CricketPlotFunctions project.
- `README.md`: This file providing an overview of the repository.
- `images`: Folder containing images of the plots that the functions generate

## Example Plots

All match data in these examples is simulated; see `examples.R`.

`cricket_field(straight_boundary = 60, square_boundary = 70)`
![Cricket Field](images/cricket_field_1.png)

`cricket_field(straight_boundary = 75, square_boundary = 65)`
![Cricket Field](images/cricket_field_2.png)

`cricket_pitch()` and `cricket_pitch(protected_area = TRUE)`

<img src="images/cricket_pitch.png" width="200"> <img src="images/cricket_pitch_protected_area.png" width="200">

`cricket_pitch_annotated(handedness = "RHB")`
![Cricket Pitch Annotated](images/cricket_pitch_annotated.png)

`cricket_pitch_map(deliveries, colour = "outcome")`
![Pitch map](images/cricket_pitch_map.png)

`cricket_beehive("RHB") + geom_point(data = deliveries, aes(x = lateral, y = height, fill = outcome), shape = 21)`
![Beehive](images/cricket_beehive.png)

`cricket_wagon_wheel(shots, straight_boundary = 68, square_boundary = 64)`
![Wagon wheel](images/cricket_wagon_wheel.png)

`cricket_scoring_zones(shots, straight_boundary = 68, square_boundary = 64)`
![Scoring zones](images/cricket_scoring_zones.png)

`cricket_fielding_positions()`
![Field setting](images/cricket_fielding_positions.png)

`cricket_fielding_positions(positions = fielding_positions$position)`
![All fielding positions](images/cricket_fielding_positions_all.png)

`cricket_manhattan(overs, team = "team")`
![Manhattan](images/cricket_manhattan.png)

`cricket_worm(overs, team = "team")`
![Worm](images/cricket_worm.png)

## Gallery

### Right- and left-handers from the same data

The angle convention is relative to the off side, so one shot table plots correctly for either hand. Right-hander on the left, `handedness = "LHB"` on the right: the same shots, mirrored.

<img src="images/cricket_wagon_wheel.png" width="49%" alt="Wagon wheel, right-hander"> <img src="images/cricket_wagon_wheel_lhb.png" width="49%" alt="Wagon wheel, left-hander">

<img src="images/cricket_scoring_zones.png" width="49%" alt="Scoring zones, right-hander"> <img src="images/cricket_scoring_zones_lhb.png" width="49%" alt="Scoring zones, left-hander">

<img src="images/cricket_pitch_map.png" width="32%" alt="Pitch map, right-hander"> <img src="images/cricket_pitch_map_lhb.png" width="32%" alt="Pitch map, left-hander">

### Your own length breaks

The default pace breaks next to custom ones, `cricket_pitch_annotated(lengths = c(yorker = 1.5, full = 4, good = 6))`, e.g. for a spinner.

<img src="images/cricket_pitch_annotated.png" width="32%" alt="Default length zones"> <img src="images/cricket_pitch_annotated_custom.png" width="32%" alt="Custom length zones">

### Field settings

An attacking field with three slips and a gully, and a defensive field with six on the rope, on a ground wider square (72 m) than straight (66 m):

<img src="images/cricket_field_attacking.png" width="49%" alt="Attacking field"> <img src="images/cricket_field_defensive.png" width="49%" alt="Defensive field">

### One innings

Leave out `team` for a single innings: `cricket_manhattan(overs)` and `cricket_worm(overs)`

![Manhattan, one innings](images/cricket_manhattan_single.png)
![Worm, one innings](images/cricket_worm_single.png)

### Ground shapes

Round (62 m), longer than wide (75 × 65) and wider than long (60 × 70):

<img src="images/cricket_field_round.png" width="32%" alt="Round ground"> <img src="images/cricket_field_2.png" width="32%" alt="Long ground"> <img src="images/cricket_field_1.png" width="32%" alt="Wide ground">

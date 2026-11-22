# CricketPlotFunctions

R functions for plotting cricket venues using ggplot2.

## Functions (in `cricket_plot_functions.R`)

- `cricket_field(straight_boundary, square_boundary)` — Plots a cricket oval; `straight_boundary` is along the pitch (y), `square_boundary` across it (x)
- `cricket_pitch(protected_area = FALSE)` — Plots a standard cricket pitch; optionally shades the Law 41.14 protected area
- `cricket_pitch_annotated(lengths, handedness)` — Plots a cricket pitch with bowling length zones (yorker, full, good, short) measured from the batter's stumps; default breaks c(2, 6, 8) m
- `cricket_pitch_map()`, `cricket_beehive()` — where deliveries pitched / passed the stumps
- `cricket_wagon_wheel()`, `cricket_scoring_zones()` — shots radiating from the batter's stumps, clipped to the boundary ellipse
- `cricket_fielding_positions()` — field settings from the `fielding_positions` table
- `cricket_manhattan()`, `cricket_worm()` — over-by-over charts (one row per over per team)

## Conventions

- Batter's stumps at the top (+y), bowler at the bottom. A right-hander's off side is -x; `handed_sign()` mirrors for left-handers.
- Shot/fielder angles: degrees from straight down the ground, positive to the off side (`shot_direction()`); `boundary_distance()` solves the ray-ellipse intersection from the batter's stumps.
- `examples.R` regenerates every image in `images/` from simulated data (`Rscript examples.R`).

## Usage

Source the script directly from GitHub in other projects:

```r
source("https://raw.githubusercontent.com/CooperDenny/CricketPlotFunctions/main/cricket_plot_functions.R")
```

Or clone and source locally. Functions return ggplot2 objects and can be further customised with standard ggplot2 layers.

## Packages

`ggplot2` (>= 3.4.0) only. Shared geometry (`crease_lines`, `wide_lines`, `stumps`, `inner_circle_path()`, `shot_direction()`, `boundary_distance()`) and the `pitch_markings()` layer helper live at the top of the file; reuse them rather than repeating segments. Each marking is drawn once from its own data, never from a many-row base data frame (that overplots every layer).

## Example outputs

See the `images/` directory for example plots of each function.

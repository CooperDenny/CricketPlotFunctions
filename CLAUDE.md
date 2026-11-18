# CricketPlotFunctions

R functions for plotting cricket venues using ggplot2.

## Functions (in `cricket_plot_functions.R`)

- `cricket_field(straight_boundary, square_boundary)` — Plots a cricket oval; `straight_boundary` is along the pitch (y), `square_boundary` across it (x)
- `cricket_pitch()` — Plots a standard cricket pitch
- `cricket_pitch_annotated()` — Plots a cricket pitch with bowling length zones (yorker, full, good, short) measured from the batter's stumps

## Usage

Source the script directly from GitHub in other projects:

```r
source("https://raw.githubusercontent.com/CooperDenny/CricketPlotFunctions/main/cricket_plot_functions.R")
```

Or clone and source locally. Functions return ggplot2 objects and can be further customised with standard ggplot2 layers.

## Packages

`ggplot2` (>= 3.4.0) only. Shared geometry (`crease_lines`, `wide_lines`, `stumps`, `inner_circle_path()`) and the `pitch_markings()` layer helper live at the top of the file; reuse them rather than repeating segments. Each marking is drawn once from its own data, never from a many-row base data frame (that overplots every layer).

## Example outputs

See the `images/` directory for example plots of each function.

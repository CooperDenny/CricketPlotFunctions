# CricketPlotFunctions

R functions for plotting cricket venues using ggplot2.

## Functions (in `cricket_plot_functions.R`)

- `cricket_field(straight_boundary, square_boundary)` — Plots a cricket oval with configurable boundary dimensions
- `cricket_pitch()` — Plots a standard cricket pitch
- `cricket_pitch_annotated()` — Plots a cricket pitch with fielding position annotations

## Usage

Source the script directly from GitHub in other projects:

```r
source("https://raw.githubusercontent.com/CooperDenny/CricketPlotFunctions/main/cricket_plot_functions.R")
```

Or clone and source locally. Functions return ggplot2 objects and can be further customised with standard ggplot2 layers.

## Packages

`ggplot2`, `ggforce`

## Example outputs

See the `images/` directory for example plots of each function.

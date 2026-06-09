# Airbnb NYC Market Structure Analysis

This project is an interactive Shiny dashboard and data story that analyzes the NYC Airbnb market using Inside Airbnb listing and review data. It's available online here: https://nathnhr.shinyapps.io/airbnb_dashboard/

## Project Structure

- `app.R`: Main Shiny app (Data Story + Explore Data tabs).
- `data/nyc/listings.csv`: NYC Airbnb listings dataset.
- `data/nyc/reviews.csv`: NYC Airbnb reviews dataset.
- `story-figures/`: Exported figures used in the narrative and slides.
- `make_story_figures.R`: Script to generate static story figures.

## Research Focus

The app investigates:

1. Whether Airbnb activity is spatially concentrated in a small set of neighborhoods.
2. How prices vary by room type and geography.
3. How host structure (single-listing vs multi-listing hosts) relates to pricing.
4. How 2025 review activity is associated with price.

## Run Locally

### Prerequisites

- R (4.2+ recommended)
- R packages:
  - `shiny`
  - `plotly`
  - `dplyr`
  - `readr`
  - `scales`
  - `tidyr`

Install packages in R:

```r
install.packages(c("shiny", "plotly", "dplyr", "readr", "scales", "tidyr"))
```

### Start the app

From this project folder:

```bash
Rscript -e "shiny::runApp('app.R')"
```

Or in R/RStudio:

```r
shiny::runApp("app.R")
```

The app will open in your browser (usually `http://127.0.0.1:xxxx`).

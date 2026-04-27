# Dashboard Setup

## Likely platform
The course files strongly suggest this project should be built in **R Shiny**:
- the class workspace already contains a Shiny dashboard example at [app.R](/Users/austinmo/Library/CloudStorage/OneDrive-UniversityofVirginia/Data%20Science/DS%20labs/gapminder-dashboard/app.R)
- this machine already has `shiny`, `plotly`, `dplyr`, `readr`, and `scales` installed

I could not cleanly extract the assignment PDF text because the OneDrive-backed PDF kept timing out locally, so treat this as the best evidence-based assumption from the course repo.

## Files added
- [app.R](/Users/austinmo/Library/CloudStorage/OneDrive-UniversityofVirginia/Data%20Science/Project%203%20(final%20project)/app.R)

## Run the skeleton
From the project folder:

```bash
cd '/Users/austinmo/Library/CloudStorage/OneDrive-UniversityofVirginia/Data Science/Project 3 (final project)'
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

## Data wiring
If you have the real Inside Airbnb NYC files, place them here:

```text
data/nyc/listings.csv
data/nyc/reviews.csv
```

The app will use demo data until those files exist.

## What the skeleton already includes
- KPI row
- left filter sidebar
- concentration/map placeholder
- ranked neighborhood bar chart
- price distribution boxplot
- host structure stacked bar chart
- review activity timeline

## Next upgrades
1. Replace the placeholder scatter map with a real map using `leaflet` or `sf` + `plotly`.
2. Join `neighbourhoods.geojson` for true neighborhood geometry.
3. Add coordinated filtering from map clicks.
4. Add better host-level aggregation using `host_id` and `calculated_host_listings_count`.

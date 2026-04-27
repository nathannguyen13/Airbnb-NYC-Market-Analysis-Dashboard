library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(scales)
library(tidyr)

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- cmd_args[startsWith(cmd_args, file_arg)]
  
  if (length(script_path) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", script_path[1]))))
  }
  
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  
  getwd()
}

script_dir <- get_script_dir()
project_data_dir <- if (dir.exists(file.path(script_dir, "data", "nyc"))) {
  file.path(script_dir, "data", "nyc")
} else {
  "/Users/austinmo/Library/CloudStorage/OneDrive-UniversityofVirginia/Data Science/Project 3 (final project)/data/nyc"
}

summary_value <- function(value, prefix = "", suffix = "", digits = 0) {
  if (length(value) == 0 || all(is.na(value))) {
    return("N/A")
  }
  
  paste0(prefix, formatC(value, digits = digits, format = "f", big.mark = ","), suffix)
}

load_demo_listings <- function() {
  tibble::tribble(
    ~borough, ~neighborhood,         ~room_type,      ~host_type,       ~price, ~entire_home, ~listing_count, ~x,        ~y,
    "Manhattan", "Lower East Side",  "Entire home",   "Multi-listing",    245, TRUE,          1482,          -73.9857,  40.7185,
    "Brooklyn",  "Williamsburg",     "Entire home",   "Single listing",   210, TRUE,          1328,          -73.9571,  40.7081,
    "Manhattan", "Harlem",           "Private room",  "Single listing",   165, FALSE,         1214,          -73.9465,  40.8116,
    "Queens",    "Astoria",          "Private room",  "Single listing",   145, FALSE,         1096,          -73.9236,  40.7644,
    "Manhattan", "Hell's Kitchen",   "Entire home",   "Multi-listing",    230, TRUE,           983,          -73.9934,  40.7638,
    "Brooklyn",  "Bushwick",         "Private room",  "Single listing",   138, FALSE,          911,          -73.9212,  40.6958,
    "Queens",    "Long Island City", "Entire home",   "Multi-listing",    198, TRUE,           743,          -73.9496,  40.7447,
    "Bronx",     "Mott Haven",       "Private room",  "Single listing",   118, FALSE,          420,          -73.9207,  40.8091,
    "Staten Island", "St. George",   "Entire home",   "Single listing",   154, TRUE,           205,          -74.0776,  40.6437
  )
}

load_demo_reviews <- function() {
  dates <- seq.Date(as.Date("2018-01-01"), as.Date("2025-12-01"), by = "month")
  tibble::tibble(
    month = dates,
    review_count = round(seq(220, 690, length.out = length(dates)) + 40 * sin(seq_along(dates) / 4))
  )
}

first_existing_value <- function(data, candidates, default = NA_character_) {
  for (name in candidates) {
    if (name %in% names(data)) {
      return(data[[name]])
    }
  }
  
  rep(default, nrow(data))
}

load_airbnb_data <- function() {
  listings_path <- file.path(project_data_dir, "listings.csv")
  reviews_path <- file.path(project_data_dir, "reviews.csv")
  
  if (file.exists(listings_path)) {
    raw_listings <- readr::read_csv(listings_path, show_col_types = FALSE)
    
    borough_values <- first_existing_value(
      raw_listings,
      c("neighbourhood_group_cleansed", "neighbourhood_group"),
      "Unknown"
    )
    neighborhood_values <- first_existing_value(
      raw_listings,
      c("neighbourhood_cleansed", "neighbourhood"),
      "Unknown"
    )
    host_listing_count <- if ("calculated_host_listings_count" %in% names(raw_listings)) {
      raw_listings$calculated_host_listings_count
    } else {
      rep(1, nrow(raw_listings))
    }
    latitude_values <- if ("latitude" %in% names(raw_listings)) raw_listings$latitude else rep(NA_real_, nrow(raw_listings))
    longitude_values <- if ("longitude" %in% names(raw_listings)) raw_listings$longitude else rep(NA_real_, nrow(raw_listings))
    listing_id_values <- if ("id" %in% names(raw_listings)) raw_listings$id else seq_len(nrow(raw_listings))
    
    listings <- raw_listings %>%
      transmute(
        listing_id = listing_id_values,
        borough = borough_values,
        neighborhood = neighborhood_values,
        room_type = coalesce(room_type, "Unknown"),
        host_type = if_else(coalesce(host_listing_count, 1) > 1, "Multi-listing", "Single listing"),
        price = readr::parse_number(as.character(price)),
        entire_home = room_type %in% c("Entire home/apt", "Entire home"),
        listing_count = 1,
        x = longitude_values,
        y = latitude_values
      )
  } else {
    listings <- load_demo_listings() %>%
      mutate(listing_id = seq_len(n()),
             reviews_2025 = sample(0:30, n(), replace = TRUE))
  }
  
  if (file.exists(reviews_path)) {
    reviews_raw <- readr::read_csv(reviews_path, show_col_types = FALSE)
    date_col <- intersect(names(reviews_raw), c("date", "review_date"))
    review_listing_id_col <- intersect(names(reviews_raw), c("listing_id", "id"))
    
    if (length(date_col) == 1 && length(review_listing_id_col) == 1) {
      reviews <- reviews_raw %>%
        transmute(
          listing_id = .data[[review_listing_id_col]],
          review_date = as.Date(.data[[date_col]])
        ) %>%
        filter(!is.na(review_date)) %>%
        mutate(month = as.Date(paste0(substr(review_date, 1, 7), "-01")))
      
      reviews_2025 <- reviews %>%
        filter(
          review_date >= as.Date("2025-01-01"),
          review_date <= as.Date("2025-12-04")
        ) %>%
        count(listing_id, name = "reviews_2025")
      
      listings <- listings %>%
        left_join(reviews_2025, by = "listing_id") %>%
        mutate(reviews_2025 = coalesce(reviews_2025, 0L))
    } else {
      reviews <- load_demo_reviews()
      if (!"reviews_2025" %in% names(listings)) {
        listings <- listings %>% mutate(reviews_2025 = sample(0:30, n(), replace = TRUE))
      }
    }
  } else {
    reviews <- load_demo_reviews()
    if (!"reviews_2025" %in% names(listings)) {
      listings <- listings %>% mutate(reviews_2025 = sample(0:30, n(), replace = TRUE))
    }
  }
  
  list(listings = listings, reviews = reviews)
}

data_store <- load_airbnb_data()
listings_data <- data_store$listings
reviews_data <- data_store$reviews

price_max <- suppressWarnings(max(listings_data$price, na.rm = TRUE))
if (!is.finite(price_max)) {
  price_max <- 500
}
has_price_data <- any(!is.na(listings_data$price))
room_type_order <- c("Entire home/apt", "Private room", "Hotel room", "Shared room")

valid_story_boroughs <- listings_data %>%
  filter(!is.na(borough), borough != "Unknown")

story_borough_share <- valid_story_boroughs %>%
  count(borough, sort = TRUE) %>%
  slice_head(n = 2) %>%
  summarise(share = sum(n) / nrow(valid_story_boroughs)) %>%
  pull(share)

story_top_neighborhoods <- listings_data %>%
  filter(!is.na(neighborhood), neighborhood != "Unknown") %>%
  count(neighborhood, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(neighborhood)

story_price_stats <- listings_data %>%
  filter(
    !is.na(price),
    price > 0,
    room_type %in% c("Entire home/apt", "Private room")
  ) %>%
  group_by(room_type) %>%
  summarise(median_price = median(price, na.rm = TRUE), .groups = "drop")

story_entire_home_median <- story_price_stats$median_price[story_price_stats$room_type == "Entire home/apt"]
story_private_room_median <- story_price_stats$median_price[story_price_stats$room_type == "Private room"]
story_multi_share <- mean(listings_data$host_type == "Multi-listing", na.rm = TRUE)

borough_choices <- c("All NYC", sort(unique(listings_data$borough)))
room_type_choices <- c("All", sort(unique(listings_data$room_type)))
host_type_choices <- c("All hosts", "Single listing", "Multi-listing")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f6f1e8 0%, #efe5d8 100%);
        color: #182028;
        font-family: 'Avenir Next', 'Segoe UI', sans-serif;
      }
      .app-shell {
        margin-top: 20px;
        margin-bottom: 20px;
      }
      .nav-tabs {
        border-bottom: none;
        margin-bottom: 16px;
      }
      .nav-tabs > li > a {
        border-radius: 999px;
        border: 1px solid rgba(24, 32, 40, 0.12);
        background: rgba(255, 251, 245, 0.96);
        color: #364152;
        margin-right: 10px;
        font-weight: 700;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: #1f3140;
        color: #fff;
        border: 1px solid #1f3140;
      }
      .hero-card, .kpi-card, .sidebar-card, .content-card, .story-shell, .figure-card {
        border-radius: 20px;
        border: 1px solid rgba(24, 32, 40, 0.08);
        box-shadow: 0 10px 28px rgba(48, 36, 24, 0.08);
      }
      .hero-card {
        background: linear-gradient(145deg, #1f3140, #2e6485);
        color: #fff;
        padding: 20px 22px;
        min-height: 126px;
      }
      .hero-card h2 {
        margin-top: 8px;
        margin-bottom: 8px;
        font-family: Georgia, serif;
        font-size: 2rem;
        font-weight: 700;
      }
      .eyebrow {
        text-transform: uppercase;
        letter-spacing: 0.14em;
        font-size: 0.72rem;
        font-weight: 700;
        opacity: 0.78;
      }
      .hero-card p {
        margin-bottom: 0;
        opacity: 0.88;
      }
      .kpi-card {
        background: rgba(255, 251, 245, 0.94);
        padding: 16px 18px;
        min-height: 126px;
      }
      .kpi-label {
        font-size: 0.78rem;
        text-transform: uppercase;
        letter-spacing: 0.09em;
        font-weight: 700;
        color: #607080;
      }
      .kpi-value {
        font-size: 2rem;
        font-weight: 750;
        margin-top: 18px;
      }
      .kpi-subtext {
        margin-top: 10px;
        font-size: 0.9rem;
        color: #607080;
      }
      .sidebar-card {
        background: rgba(255, 252, 247, 0.95);
        padding: 18px;
        min-height: 760px;
      }
      .content-card {
        background: rgba(255, 252, 247, 0.95);
        padding: 18px;
        margin-bottom: 18px;
      }
      .story-shell {
        background: rgba(255, 252, 247, 0.97);
        max-width: 960px;
        margin: 0 auto;
        padding: 28px 34px;
      }
      .story-kicker {
        text-transform: uppercase;
        letter-spacing: 0.16em;
        font-size: 0.74rem;
        color: #607080;
        font-weight: 700;
      }
      .story-title {
        font-family: Georgia, serif;
        font-size: 2.4rem;
        line-height: 1.15;
        margin-top: 10px;
        margin-bottom: 12px;
      }
      .story-dek {
        font-size: 1.1rem;
        color: #4a5664;
        line-height: 1.6;
        margin-bottom: 20px;
      }
      .story-section {
        margin-top: 28px;
      }
      .story-section h3 {
        font-family: Georgia, serif;
        font-size: 1.6rem;
        margin-bottom: 10px;
      }
      .story-section p {
        font-size: 1.04rem;
        line-height: 1.7;
        margin-bottom: 14px;
      }
      .figure-card {
        background: rgba(246, 242, 236, 0.95);
        padding: 16px;
        margin: 18px 0 24px 0;
      }
      .figure-caption {
        font-size: 0.94rem;
        color: #607080;
        margin-top: 10px;
        line-height: 1.5;
      }
      .section-label {
        font-size: 0.78rem;
        text-transform: uppercase;
        letter-spacing: 0.12em;
        color: #607080;
        font-weight: 700;
        margin-bottom: 12px;
      }
      .chart-title {
        font-size: 1.35rem;
        font-weight: 700;
        margin-bottom: 2px;
      }
      .chart-subtitle {
        color: #607080;
        margin-bottom: 8px;
      }
      .control-label {
        font-weight: 700;
      }
      .shiny-input-container {
        margin-bottom: 14px;
      }
      .warning-box {
        margin-top: 14px;
        padding: 12px 14px;
        border-radius: 14px;
        background: rgba(199, 92, 60, 0.12);
        border: 1px solid rgba(199, 92, 60, 0.22);
        color: #7b3f2c;
        font-size: 0.92rem;
        line-height: 1.35;
      }
    "))
  ),
  div(
    class = "container-fluid app-shell",
    tabsetPanel(
      id = "main_tabs",
      tabPanel(
        "Data Story",
        div(
          class = "story-shell",
          div(class = "story-kicker", "DS2003 Data Story"),
          div(class = "story-title", "Inside Airbnb NYC Is Not One Uniform Market"),
          div(
            class = "story-dek",
            "One way to understand the Airbnb market is to look at how it operates in dense cities, where demand, space, and pricing all come together."
          ),
          div(
            class = "story-section",
            h3("A small set of neighborhoods drives visible activity"),
            p("New York City is a strong case for that kind of analysis. At first, Airbnb can look like one large citywide market, but the data suggests a more uneven structure. Activity is not spread evenly across the city. Instead, it is concentrated in a smaller set of neighborhoods, which raises a more specific question about who participates most heavily in the market and how that relates to pricing and demand. What looks like one platform is better understood as a set of smaller submarkets shaped by geography, room type, and host behavior. By looking at listings and review data together, it becomes easier to see these patterns. This analysis focuses on where activity is concentrated, how prices vary across neighborhoods and room types, how multi-listing hosts compare with single-listing hosts, and how 2025 review activity is associated with listing prices across different parts of the city. Together, these patterns point to a market that is uneven, layered, and shaped by several interacting factors."),
            p("Looking more closely at how listings are distributed across the city, a clear imbalance starts to appear. Manhattan and Brooklyn account for most listings in the cleaned dataset, while much of the remaining activity is spread across Queens, the Bronx, and Staten Island. This shows that Airbnb presence is not evenly distributed, but clustered in a limited set of neighborhoods. A small number of places, including Bedford-Stuyvesant, Midtown, and Hell's Kitchen, account for especially large numbers of listings. Rather than functioning as an evenly distributed citywide marketplace, Airbnb in New York City appears to operate through a smaller set of high-activity areas that dominate visible supply."),
            div(class = "figure-card", plotlyOutput("story_concentration_plot", height = "420px"), div(class = "figure-caption", "Figure 1. Ranked neighborhood counts show concentration in a smaller set of places."))
          ),
          div(
            class = "story-section",
            h3("Price differences reflect geography and room type"),
            p("When looking at pricing across the city, the differences become even more noticeable. Prices vary widely, and two factors drive much of that variation: where the listing is located and what type of space is being offered. Entire homes are consistently priced higher than private or shared rooms, especially in Manhattan and parts of Brooklyn. At the same time, listings in the outer boroughs tend to have lower prices and a tighter range overall. This shows that pricing is not just tied to the individual listing, but also to the local market surrounding it. Even within the same borough, there is still a wide spread in prices, which suggests that higher-end and more affordable segments often exist side by side."),
            div(class = "figure-card", plotlyOutput("story_price_plot", height = "440px"), div(class = "figure-caption", "Figure 2. Price distributions by room type show strong differences in market segments."))
          ),
          div(
            class = "story-section",
            h3("Host type explains part of the pricing pattern"),
            p("Looking beyond location and pricing, host behavior also helps explain how the market operates. Not all Airbnb hosts participate in the same way. When comparing single-listing hosts with those managing multiple properties, a clearer commercial pattern begins to emerge. Multi-listing hosts are especially common in the highest-activity parts of the city, and in some borough-room type comparisons they also post higher median prices. This suggests that part of the market operates less like occasional home sharing and more like a scaled short-term rental business. Those listings are often concentrated in areas that already have high supply, reinforcing the broader pattern of concentration."),
            div(class = "figure-card", plotlyOutput("story_host_price_plot", height = "420px"), div(class = "figure-caption", "Figure 3. Host-type price differences are compared within room types."))
          ),
          div(
            class = "story-section",
            h3("Review activity adds context, not causality"),
            p("Review activity offers another way to think about demand, although it should be treated as a proxy rather than a direct measure of bookings. When looking at reviews recorded during 2025, listings with the highest review counts tend to fall in the lower to middle price range rather than the top of the market. That pattern suggests that the most actively reviewed listings are often relatively affordable, while the most expensive listings do not necessarily generate the highest review volume. In that sense, the market appears split between listings that drive volume and listings that serve a smaller, more premium segment."),
            div(class = "figure-card", plotlyOutput("story_reviews_plot", height = "430px"), div(class = "figure-caption", "Figure 4. The review-price relationship is weak overall and should be read as association."))
          ),
          div(
            class = "story-section",
            h3("Conclusion"),
            p("Overall, Airbnb in New York City does not operate as one single, unified market. Instead, it functions as a set of uneven and spatially concentrated submarkets shaped by location, room type, host behavior, and price. A relatively small number of neighborhoods account for a large share of visible activity, and pricing reflects both geography and listing type. At the same time, some hosts operate at a larger scale, contributing to a more commercialized side of the platform. Looking at review activity also suggests that more affordable listings tend to drive more visible engagement. In the end, the Airbnb market in New York City mirrors the structure of the city itself: uneven, segmented, and strongly shaped by place.")
          )
        )
      ),
      tabPanel(
        "Explore Data",
        fluidRow(
          column(
            width = 4,
            div(
              class = "hero-card",
              div(class = "eyebrow", "DS2003 Interactive Exploration"),
              h2("Inside Airbnb NYC"),
              p("Use filters to test the story across boroughs, neighborhoods, room types, host types, and prices.")
            )
          ),
          column(width = 2, div(class = "kpi-card", div(class = "kpi-label", "Total Listings"), div(class = "kpi-value", textOutput("kpi_listings")), div(class = "kpi-subtext", "Current filtered view"))),
          column(width = 2, div(class = "kpi-card", div(class = "kpi-label", "Median Price"), div(class = "kpi-value", textOutput("kpi_price")), div(class = "kpi-subtext", "USD per night"))),
          column(width = 2, div(class = "kpi-card", div(class = "kpi-label", "Neighborhoods"), div(class = "kpi-value", textOutput("kpi_neighborhoods")), div(class = "kpi-subtext", "Visible in current view"))),
          column(width = 2, div(class = "kpi-card", div(class = "kpi-label", "Boroughs"), div(class = "kpi-value", textOutput("kpi_boroughs")), div(class = "kpi-subtext", "Represented after filtering")))
        ),
        fluidRow(
          column(
            width = 3,
            div(
              class = "sidebar-card",
              div(class = "section-label", "Filters"),
              selectInput("borough", "Borough", choices = borough_choices, selected = "All NYC"),
              uiOutput("neighborhood_ui"),
              selectInput("room_type", "Room Type", choices = room_type_choices, selected = "All"),
              uiOutput("price_filter_ui"),
              selectInput("host_type", "Host Type", choices = host_type_choices, selected = "All hosts")
            )
          ),
          column(
            width = 9,
            div(class = "content-card", div(class = "section-label", "Interactive Exploration"), div(class = "chart-subtitle", "The narrative is in a separate article-style tab. This tab is for open-ended exploration.")),
            fluidRow(
              column(width = 7, div(class = "content-card", div(class = "chart-title", "Neighborhood Concentration Map"), div(class = "chart-subtitle", "Real listing coordinates, aggregated to neighborhood centroids on an NYC basemap."), plotlyOutput("map_plot", height = "330px"))),
              column(width = 5, div(class = "content-card", div(class = "chart-title", "Top Neighborhoods"), div(class = "chart-subtitle", "Ranked by visible listing count."), plotlyOutput("ranked_bar", height = "330px")))
            ),
            fluidRow(column(width = 12, div(class = "content-card", div(class = "chart-title", "Price Distribution By Room Type"), div(class = "chart-subtitle", textOutput("price_plot_subtitle", container = span)), plotlyOutput("price_plot", height = "260px")))),
            fluidRow(
              column(
                width = 7,
                div(
                  class = "content-card",
                  div(class = "chart-title", "2025 Review Activity And Price"),
                  div(class = "chart-subtitle", "Each point is a listing. Trend line shows association, not causation. Colored by room type."),
                  div(
                    style = "display: flex; gap: 12px; margin-bottom: 10px;",
                    div(style = "flex: 1;", checkboxInput("log_reviews", "Log scale for reviews", value = TRUE)),
                    div(style = "flex: 1;", checkboxInput("show_trend", "Show trend line", value = TRUE))
                  ),
                  plotlyOutput("reviews_price_plot", height = "380px")
                )
              ),
              column(
                width = 5,
                div(
                  class = "content-card",
                  div(class = "chart-title", "Host Type And Price"),
                  div(class = "chart-subtitle", "Price distributions by host type, split by room type."),
                  div(style = "margin-bottom: 10px;", radioButtons("host_stat", label = NULL, choices = c("Show medians" = "median", "Show means" = "mean"), selected = "median", inline = TRUE)),
                  plotlyOutput("host_price_plot", height = "380px")
                )
              )
            )
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  story_data <- reactive({
    listings_data %>%
      filter(!is.na(neighborhood), !is.na(room_type), !is.na(host_type), !is.na(price), price > 0)
  })

  output$story_concentration_plot <- renderPlotly({
    plot_data <- story_data() %>%
      count(neighborhood, sort = TRUE) %>%
      slice_head(n = 12)

    plot_ly(
      data = plot_data,
      x = ~n,
      y = ~reorder(neighborhood, n),
      type = "bar",
      orientation = "h",
      marker = list(color = "#c75c3c"),
      hovertemplate = "<b>%{y}</b><br>Listings: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Listing count"),
        yaxis = list(title = ""),
        margin = list(l = 150, r = 10, t = 6, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$story_price_plot <- renderPlotly({
    plot_data <- story_data() %>%
      filter(room_type %in% room_type_order)

    price_cap <- unname(quantile(plot_data$price, 0.95, na.rm = TRUE))
    plot_data <- plot_data %>% filter(price <= price_cap)

    plot_ly(
      data = plot_data,
      x = ~price,
      y = ~room_type,
      color = ~room_type,
      colors = c("#2d698e", "#c75c3c", "#406b45", "#7c6c57"),
      type = "box",
      orientation = "h",
      boxpoints = FALSE,
      hoverinfo = "y+x",
      showlegend = FALSE
    ) %>%
      layout(
        xaxis = list(title = "Nightly price (USD)"),
        yaxis = list(title = ""),
        margin = list(l = 140, r = 10, t = 6, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$story_host_price_plot <- renderPlotly({
    plot_data <- story_data() %>%
      filter(room_type %in% c("Entire home/apt", "Private room")) %>%
      mutate(host_type = factor(host_type, levels = c("Single listing", "Multi-listing")))

    price_cap <- unname(quantile(plot_data$price, 0.95, na.rm = TRUE))
    plot_data <- plot_data %>% filter(price <= price_cap)

    plot_ly(
      data = plot_data,
      x = ~room_type,
      y = ~price,
      color = ~host_type,
      colors = c("Single listing" = "#2d698e", "Multi-listing" = "#c75c3c"),
      type = "box",
      boxpoints = FALSE,
      hoverinfo = "y+name"
    ) %>%
      layout(
        boxmode = "group",
        xaxis = list(title = ""),
        yaxis = list(title = "Nightly price (USD)"),
        margin = list(l = 60, r = 10, t = 6, b = 40),
        legend = list(orientation = "h", x = 0, y = 1.1),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$story_reviews_plot <- renderPlotly({
    plot_data <- story_data() %>%
      filter(!is.na(reviews_2025)) %>%
      filter(price <= quantile(price, 0.99, na.rm = TRUE))

    if (nrow(plot_data) > 6000) {
      plot_data <- plot_data %>% slice_sample(n = 6000)
    }

    fit_data <- plot_data %>% filter(reviews_2025 > 0)
    trend_line <- NULL
    if (nrow(fit_data) >= 10) {
      fit_data$x_fit <- log10(fit_data$reviews_2025)
      fit <- stats::lm(price ~ x_fit, data = fit_data)
      x_seq <- seq(min(fit_data$x_fit), max(fit_data$x_fit), length.out = 100)
      trend_line <- data.frame(
        reviews_2025 = 10 ^ x_seq,
        fitted = predict(fit, newdata = data.frame(x_fit = x_seq))
      )
    }

    p <- plot_ly(
      data = plot_data,
      x = ~reviews_2025,
      y = ~price,
      color = ~room_type,
      colors = c("#2d698e", "#c75c3c", "#406b45", "#7c6c57"),
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, opacity = 0.38, line = list(color = "white", width = 1)),
      text = ~paste0("<b>", neighborhood, "</b><br>Room type: ", room_type, "<br>Price: $", round(price), "<br>2025 reviews: ", reviews_2025),
      hoverinfo = "text"
    )

    if (!is.null(trend_line)) {
      p <- p %>% add_trace(
        data = trend_line,
        x = ~reviews_2025,
        y = ~fitted,
        type = "scatter",
        mode = "lines",
        line = list(color = "#000000", width = 3),
        name = "Trend",
        inherit = FALSE
      )
    }

    p %>%
      layout(
        xaxis = list(title = "Number of reviews in 2025 (log scale)", type = "log"),
        yaxis = list(title = "Nightly price (USD)"),
        margin = list(l = 60, r = 10, t = 6, b = 50),
        legend = list(orientation = "h", x = 0, y = 1.1),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$price_filter_ui <- renderUI({
    if (has_price_data) {
      return(
        sliderInput(
          "price_range",
          "Price Range",
          min = 0,
          max = max(500, ceiling(price_max / 50) * 50),
          value = c(0, min(450, max(500, ceiling(price_max / 50) * 50))),
          pre = "$"
        )
      )
    }
    
    div(
      class = "warning-box",
      strong("Price filter unavailable."),
      div("The current NYC listings snapshot has a blank price column, so price views are shown as a data limitation rather than a chart error.")
    )
  })
  
  output$neighborhood_ui <- renderUI({
    borough_filtered <- listings_data
    
    if (!is.null(input$borough) && input$borough != "All NYC") {
      borough_filtered <- borough_filtered %>% filter(borough == input$borough)
    }
    
    choices <- c("All neighborhoods", sort(unique(borough_filtered$neighborhood)))
    
    selectInput("neighborhood", "Neighborhood", choices = choices, selected = "All neighborhoods")
  })
  
  filtered_listings <- reactive({
    data <- listings_data
    
    if (any(!is.na(data$price)) && !is.null(input$price_range) && length(input$price_range) == 2) {
      data <- data %>%
        filter(!is.na(price), price >= input$price_range[1], price <= input$price_range[2])
    }
    
    if (input$borough != "All NYC") {
      data <- data %>% filter(borough == input$borough)
    }
    
    if (!is.null(input$neighborhood) && input$neighborhood != "All neighborhoods") {
      data <- data %>% filter(neighborhood == input$neighborhood)
    }
    
    if (input$room_type != "All") {
      data <- data %>% filter(room_type == input$room_type)
    }
    
    if (input$host_type != "All hosts") {
      data <- data %>% filter(host_type == input$host_type)
    }
    
    data
  })
  
  output$kpi_listings <- renderText({
    summary_value(nrow(filtered_listings()))
  })
  
  output$kpi_price <- renderText({
    prices <- filtered_listings()$price
    
    if (!any(!is.na(prices))) {
      return("Unavailable")
    }
    
    summary_value(stats::median(prices, na.rm = TRUE), prefix = "$")
  })
  
  output$price_plot_subtitle <- renderText({
    if (!has_price_data) {
      return("This snapshot's price field is blank, so the price panel documents the data gap for now.")
    }
    
    current_scope <- if (!is.null(input$neighborhood) && input$neighborhood != "All neighborhoods") {
      "Nightly price distributions for the current selection."
    } else if (!is.null(input$room_type) && input$room_type != "All") {
      "Nightly price distributions for the current selection."
    } else if (!is.null(input$borough) && input$borough != "All NYC") {
      paste0("Nightly price distributions for ", input$borough, " by room type.")
    } else {
      "Citywide nightly price distributions by room type."
    }
    
    paste(current_scope, "Display capped at the 95th percentile for readability.")
  })
  
  output$kpi_neighborhoods <- renderText({
    summary_value(dplyr::n_distinct(filtered_listings()$neighborhood))
  })
  
  output$kpi_boroughs <- renderText({
    summary_value(dplyr::n_distinct(filtered_listings()$borough))
  })
  
  output$map_plot <- renderPlotly({
    map_data <- filtered_listings() %>%
      filter(!is.na(x), !is.na(y)) %>%
      group_by(borough, neighborhood) %>%
      summarise(
        listing_count = n(),
        median_price = median(price, na.rm = TRUE),
        x = mean(x),
        y = mean(y),
        .groups = "drop"
      )
    
    if (nrow(map_data) == 0) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "No listings with coordinates match the current filters.",
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 15, color = "#607080")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(246, 242, 236, 0.8)"
          )
      )
    }
    
    map_center <- list(
      lon = mean(map_data$x, na.rm = TRUE),
      lat = mean(map_data$y, na.rm = TRUE)
    )
    
    plot_ly(
      data = map_data,
      type = "scattermapbox",
      lon = ~x,
      lat = ~y,
      mode = "markers",
      text = ~paste0(
        "<b>", neighborhood, "</b><br>",
        "Borough: ", borough, "<br>",
        "Listings: ", scales::comma(listing_count), "<br>",
        "Median price: ", ifelse(is.na(median_price), "N/A", paste0("$", round(median_price)))
      ),
      color = ~listing_count,
      colors = c("#f3d7c2", "#d78e6d", "#9d4d34"),
      size = ~listing_count,
      sizes = c(10, 30),
      marker = list(line = list(color = "white", width = 1.2), opacity = 0.82),
      hoverinfo = "text"
    ) %>%
      layout(
        mapbox = list(
          style = "carto-positron",
          center = map_center,
          zoom = 9.4
        ),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$ranked_bar <- renderPlotly({
    bar_data <- filtered_listings() %>%
      count(neighborhood, sort = TRUE) %>%
      slice_head(n = 8)
    
    plot_ly(
      data = bar_data,
      x = ~n,
      y = ~reorder(neighborhood, n),
      type = "bar",
      orientation = "h",
      marker = list(color = "#c75c3c"),
      hovertemplate = "<b>%{y}</b><br>Listings: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Listing count"),
        yaxis = list(title = ""),
        margin = list(l = 110, r = 10, t = 10, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$price_plot <- renderPlotly({
    price_data <- filtered_listings() %>% filter(!is.na(price))
    
    if (nrow(price_data) == 0) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "No price values match the current filters.",
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 15, color = "#607080")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(246, 242, 236, 0.8)"
          )
      )
    }
    
    price_cap <- unname(stats::quantile(price_data$price, 0.95, na.rm = TRUE, names = FALSE))
    if (!is.finite(price_cap)) {
      price_cap <- max(price_data$price, na.rm = TRUE)
    }
    
    display_price_data <- price_data %>%
      filter(price <= price_cap) %>%
      mutate(
        room_type = factor(
          room_type,
          levels = room_type_order[room_type_order %in% unique(room_type)]
        )
      ) %>%
      arrange(room_type)
    
    hover_stats <- display_price_data %>%
      group_by(room_type) %>%
      summarise(
        median_price = median(price, na.rm = TRUE),
        q1_price = unname(stats::quantile(price, 0.25, na.rm = TRUE, names = FALSE)),
        q3_price = unname(stats::quantile(price, 0.75, na.rm = TRUE, names = FALSE)),
        lower_whisker = boxplot.stats(price)$stats[1],
        upper_whisker = boxplot.stats(price)$stats[5],
        .groups = "drop"
      ) %>%
      mutate(
        hover_text = paste0(
          "<b>", room_type, "</b><br>",
          "Median: $", scales::comma(round(median_price)), "<br>",
          "Q1: $", scales::comma(round(q1_price)), "<br>",
          "Q3: $", scales::comma(round(q3_price)), "<br>",
          "Lower whisker: $", scales::comma(round(lower_whisker)), "<br>",
          "Upper whisker: $", scales::comma(round(upper_whisker))
        )
      )
    
    plot_ly() %>%
      add_trace(
        data = display_price_data,
        x = ~price,
        y = ~room_type,
        color = ~room_type,
        colors = c("#2d698e", "#c75c3c", "#406b45", "#7c6c57"),
        type = "box",
        orientation = "h",
        boxpoints = FALSE,
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      add_markers(
        data = hover_stats,
        x = ~median_price,
        y = ~room_type,
        marker = list(size = 18, opacity = 0),
        text = ~hover_text,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Nightly price (USD)", range = c(0, price_cap)),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(levels(display_price_data$room_type))),
        margin = list(l = 120, r = 10, t = 20, b = 40),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$host_price_plot <- renderPlotly({
    plot_data <- filtered_listings() %>%
      filter(!is.na(price), price > 0, !is.na(host_type), !is.na(room_type))
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "No listings match current filters.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 15, color = "#607080")
            ),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(246, 242, 236, 0.8)"
          )
      )
    }
    
    # Cap at 95th percentile for readability
    price_cap <- unname(stats::quantile(plot_data$price, 0.95, na.rm = TRUE))
    plot_data <- plot_data %>% filter(price <= price_cap)
    
    # Show all room types (gaps are OK where data is missing)
    valid_room_types <- unique(plot_data$room_type)
    
    plot_data <- plot_data %>%
      mutate(
        room_type = factor(room_type, levels = room_type_order[room_type_order %in% valid_room_types]),
        host_type = factor(host_type, levels = c("Single listing", "Multi-listing"))
      )
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "Not enough data to compare host types.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 15, color = "#607080")
            ),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(246, 242, 236, 0.8)"
          )
      )
    }
    
    # Compute stats for annotations
    stat_fn <- if (input$host_stat == "mean") mean else stats::median
    
    summary_stats <- plot_data %>%
      group_by(room_type, host_type) %>%
      summarise(
        stat = stat_fn(price, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    # Compute the premium (multi vs single) for each room type
    premium_text <- summary_stats %>%
      tidyr::pivot_wider(names_from = host_type, values_from = c(stat, n))
    
    # Only build labels if both host types have data
    if (all(c("stat_Multi-listing", "stat_Single listing") %in% names(premium_text))) {
      premium_text <- premium_text %>%
        mutate(
          diff = `stat_Multi-listing` - `stat_Single listing`,
          pct = diff / `stat_Single listing` * 100,
          label = paste0(
            room_type, ": Multi-listing listings are priced ",
            ifelse(diff >= 0, "+", ""),
            "$", round(diff), " (", round(pct), "%) more"
          )
        ) %>%
        filter(!is.na(diff))
    } else {
      premium_text <- premium_text %>% mutate(label = character(0)) %>% slice(0)
    }
    
    plot_ly(
      data = plot_data,
      x = ~room_type,
      y = ~price,
      color = ~host_type,
      colors = c("Single listing" = "#2d698e", "Multi-listing" = "#c75c3c"),
      type = "box",
      boxpoints = FALSE,
      hoverinfo = "y+name"
    ) %>%
      layout(
        boxmode = "group",
        xaxis = list(title = ""),
        yaxis = list(title = "Nightly price (USD)", range = c(0, price_cap)),
        legend = list(
          orientation = "h",
          x = 0, y = 1.2,
          xanchor = "left",
          yanchor = "bottom"
        ),
        margin = list(l = 60, r = 10, t = 85, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)",
        annotations = if (nrow(premium_text) > 0) {
          lapply(seq_len(nrow(premium_text)), function(i) {
            list(
              text = premium_text$label[i],
              x = 0,
              y = 1.0 - (i - 1) * 0.07,
              xref = "paper",
              yref = "paper",
              xanchor = "left",
              showarrow = FALSE,
              font = list(size = 11, color = "#4a5664")
            )
          })
        } else {
          list()
        }
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$reviews_price_plot <- renderPlotly({
    scatter_data <- filtered_listings() %>%
      filter(!is.na(price), !is.na(reviews_2025), price > 0)
    
    if (nrow(scatter_data) == 0) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "No listings with both price and review data match current filters.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 15, color = "#607080")
            ),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(246, 242, 236, 0.8)"
          )
      )
    }
    
    price_cap <- unname(stats::quantile(scatter_data$price, 0.99, na.rm = TRUE))
    scatter_data <- scatter_data %>% filter(price <= price_cap)
    
    if (nrow(scatter_data) > 5000) {
      scatter_data <- scatter_data %>% slice_sample(n = 5000)
    }
    
    # Compute the trend line BEFORE building the plot
    trend_line <- NULL
    min_points <- 4
    if (isTRUE(input$show_trend) && nrow(scatter_data) >= min_points) {
      trend_data <- scatter_data %>%
        filter(reviews_2025 > 0)
      
      if (nrow(trend_data) >= min_points) {
        if (isTRUE(input$log_reviews)) {
          trend_data$x_fit <- log10(trend_data$reviews_2025)
        } else {
          trend_data$x_fit <- trend_data$reviews_2025
        }
        
        # Use linear fit for small samples; LOESS for larger
        loess_fit <- tryCatch(
          if (nrow(trend_data) < 10) {
            stats::lm(price ~ x_fit, data = trend_data)
          } else {
            stats::loess(price ~ x_fit, data = trend_data, span = 0.75)
          },
          error = function(e) NULL
        )
        
        if (!is.null(loess_fit)) {
          x_range <- range(trend_data$x_fit, na.rm = TRUE)
          x_seq <- seq(x_range[1], x_range[2], length.out = 100)
          preds <- predict(loess_fit, newdata = data.frame(x_fit = x_seq))
          
          trend_line <- data.frame(
            reviews_2025 = if (isTRUE(input$log_reviews)) 10 ^ x_seq else x_seq,
            fitted = preds
          )
          trend_line <- trend_line[!is.na(trend_line$fitted), ]
        }
      }
    }
    
    # Adjust marker appearance based on dataset size
    marker_opacity <- if (nrow(scatter_data) > 500) 0.4 else 0.9
    marker_size <- if (nrow(scatter_data) > 500) 6 else 10
    
    # Build scatter plot
    p <- plot_ly() %>%
      add_markers(
        data = scatter_data,
        x = ~reviews_2025,
        y = ~price,
        color = ~room_type,
        colors = c("#2d698e", "#c75c3c", "#406b45", "#7c6c57"),
        marker = list(size = marker_size, opacity = marker_opacity, line = list(color = "white", width = 1)),
        text = ~paste0(
          "<b>", neighborhood, "</b><br>",
          "Room type: ", room_type, "<br>",
          "Price: $", round(price), "<br>",
          "2025 reviews: ", reviews_2025
        ),
        hoverinfo = "text"
      )
    
    # Add trend line on top
    if (!is.null(trend_line) && nrow(trend_line) > 0) {
      p <- p %>% add_trace(
        x = trend_line$reviews_2025,
        y = trend_line$fitted,
        type = "scatter",
        mode = "lines",
        line = list(color = "#000000", width = 4),
        name = "Overall trend",
        showlegend = TRUE,
        hoverinfo = "skip",
        inherit = FALSE
      )
    }
    
    xaxis_config <- if (isTRUE(input$log_reviews)) {
      list(title = "Number of reviews in 2025 (log scale)", type = "log")
    } else {
      list(title = "Number of reviews in 2025")
    }
    
    p %>%
      layout(
        xaxis = xaxis_config,
        yaxis = list(title = "Nightly price (USD)", range = c(0, price_cap)),
        margin = list(l = 60, r = 10, t = 80, b = 50),
        legend = list(
          orientation = "h",
          x = 0, y = 1.2,
          xanchor = "left",
          yanchor = "bottom"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(246, 242, 236, 0.8)"
      ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)

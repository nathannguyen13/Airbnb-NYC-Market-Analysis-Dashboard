library(dplyr)
library(ggplot2)
library(readr)
library(scales)

project_dir <- "/Users/austinmo/Library/CloudStorage/OneDrive-UniversityofVirginia/Data Science/Project 3 (final project)"
data_dir <- file.path(project_dir, "data", "nyc")
output_dir <- file.path(project_dir, "story-figures")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

listings <- read_csv(file.path(data_dir, "listings.csv"), show_col_types = FALSE) %>%
  transmute(
    listing_id = id,
    borough = neighbourhood_group,
    neighborhood = neighbourhood,
    room_type = room_type,
    price = parse_number(as.character(price)),
    host_type = if_else(coalesce(calculated_host_listings_count, 1) > 1, "Multi-listing", "Single listing")
  ) %>%
  filter(
    !is.na(borough),
    !is.na(neighborhood),
    !is.na(room_type),
    !is.na(price),
    price > 0
  )

reviews_2025 <- read_csv(file.path(data_dir, "reviews.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2025-01-01"), date <= as.Date("2025-12-04")) %>%
  count(listing_id, name = "reviews_2025")

borough_levels <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
room_levels <- c("Entire home/apt", "Private room", "Shared room")

price_cap <- quantile(listings$price, 0.99, na.rm = TRUE)

plot_listings <- listings %>%
  filter(
    borough %in% borough_levels,
    room_type %in% room_levels,
    price <= price_cap
  ) %>%
  mutate(
    borough = factor(borough, levels = borough_levels),
    room_type = factor(room_type, levels = room_levels)
  )

theme_story <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 11, color = "#4A5568"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(12, 16, 12, 12)
    )
}

price_boxplot <- ggplot(
  plot_listings,
  aes(x = borough, y = price, fill = room_type)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    outlier.alpha = 0.08
  ) +
  scale_fill_manual(
    values = c(
      "Entire home/apt" = "#D1495B",
      "Private room" = "#2E86AB",
      "Shared room" = "#7A9E7E"
    )
  ) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Listing Prices Vary By Borough And Room Type",
    subtitle = "Prices shown below the 99th percentile to keep the comparison readable",
    x = NULL,
    y = "Nightly price",
    fill = "Room type"
  ) +
  theme_story()

ggsave(
  filename = file.path(output_dir, "price_by_borough_roomtype.png"),
  plot = price_boxplot,
  width = 11,
  height = 6.5,
  dpi = 220
)

host_price_summary <- plot_listings %>%
  filter(room_type %in% c("Entire home/apt", "Private room")) %>%
  group_by(borough, room_type, host_type) %>%
  summarise(median_price = median(price), .groups = "drop")

host_bar <- ggplot(
  host_price_summary,
  aes(x = borough, y = median_price, fill = host_type)
) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  facet_wrap(~ room_type, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Single listing" = "#8D99AE",
      "Multi-listing" = "#D1495B"
    )
  ) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Median Prices By Host Type",
    subtitle = "Comparison shown within each borough for the two dominant room types",
    x = NULL,
    y = "Median nightly price",
    fill = "Host type"
  ) +
  theme_story()

ggsave(
  filename = file.path(output_dir, "host_type_price_comparison.png"),
  plot = host_bar,
  width = 11,
  height = 6.5,
  dpi = 220
)

review_scatter_data <- plot_listings %>%
  left_join(reviews_2025, by = "listing_id") %>%
  mutate(reviews_2025 = coalesce(reviews_2025, 0L)) %>%
  filter(room_type %in% c("Entire home/apt", "Private room")) %>%
  mutate(room_type = factor(room_type, levels = c("Entire home/apt", "Private room")))

review_scatter <- ggplot(
  review_scatter_data,
  aes(x = reviews_2025, y = price, color = room_type)
) +
  geom_point(alpha = 0.18, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_color_manual(
    values = c(
      "Entire home/apt" = "#D1495B",
      "Private room" = "#2E86AB"
    )
  ) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "2025 Reviews And Price",
    subtitle = "Most high-review listings sit in the low-to-mid price range",
    x = "Number of reviews in 2025",
    y = "Nightly price",
    color = "Room type"
  ) +
  theme_story()

ggsave(
  filename = file.path(output_dir, "reviews_vs_price_2025.png"),
  plot = review_scatter,
  width = 11,
  height = 6.5,
  dpi = 220
)

cat("Saved figures to:", output_dir, "\n")

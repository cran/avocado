## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig_width = 8
)

## ----setup--------------------------------------------------------------------
library(avocado)
library(dplyr)
library(ggplot2)

data('hass')

dplyr::glimpse(hass)

## ----fig.height=5, fig.width=8------------------------------------------------
hass %>%
  filter(location == 'Chicago') %>%
  ggplot(aes(x = week_ending)) +
  geom_line(aes(y = avg_price_nonorg, color = 'Non Organic')) +
  geom_line(aes(y = avg_price_org, color = 'Organic')) +
  scale_color_manual(name = 'Type', values = c('Non Organic' = 'steelblue', "Organic" = 'darkgreen')) +
  labs(x = 'Week Ending', y = 'Average Selling Price (US$)', title = 'Average Selling Price Over Time - Chicago', caption = "Source: Hass Avocado Board\nNot adjusted for inflation") +
  theme(plot.background = element_rect(fill = "grey20"),
    plot.title = element_text(color = "#FFFFFF"),
    axis.title = element_text(color = "#FFFFFF"),
    axis.text.x = element_text(color = 'grey50', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'grey50'),
    plot.caption = element_text(color = 'grey75'),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey50", size = 0.2),
    panel.grid.minor = element_line(color = "grey50", size = 0.2),
    legend.background = element_rect(fill = 'grey20'),
    legend.key = element_rect(fill = 'grey20'),
    legend.title = element_text(color = 'grey75'),
    legend.text = element_text(color = 'grey75'),
    legend.position = c(0.815, 0.85)
    )

## ----fig.height=5, fig.width=8------------------------------------------------

hass %>%
  filter(region == 'Great Lakes' & location != 'Chicago') %>%
  mutate(
    year = lubridate::year(week_ending)
  ) %>%
  group_by(
    year, location
  ) %>%
  summarize(
    total_revenue = sum((avg_price_nonorg * (plu4046 + plu4225 + plu4770 + small_nonorg_bag + large_nonorg_bag + xlarge_nonorg_bag)) + (avg_price_org * (plu94046 + plu94225 + plu94770 + small_org_bag + large_org_bag + xlarge_org_bag))) / 1000000,
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = year, y = total_revenue, group = location)) +
  geom_bar(aes(fill = location), color = 'white', stat = 'identity', position = 'dodge') +
  labs(x = "Year", y = 'Total Revenue (US$)', fill = 'Location', title = 'Total Revenue within Great Lakes Region', caption = 'Souce: Hass Avocado Board\nNot adjusted for inflation') +
  theme(plot.background = element_rect(fill = "grey20"),
  plot.title = element_text(color = "#FFFFFF"),
  axis.title = element_text(color = "#FFFFFF"),
  axis.text.x = element_text(color = 'grey50', angle = 45, hjust = 1),
  axis.text.y = element_text(color = 'grey50'),
  plot.caption = element_text(color = 'grey75'),
  panel.background = element_blank(),
  panel.grid.major = element_line(color = "grey50", size = 0.2),
  panel.grid.minor = element_line(color = "grey50", size = 0.2),
  legend.background = element_rect(fill = 'grey20'),
  legend.key = element_rect(fill = 'grey20'),
  legend.title = element_text(color = 'grey75'),
  legend.text = element_text(color = 'grey75')
  )

## ----fig.height=5, fig.width=8------------------------------------------------
hass %>%
  filter(region == 'Great Lakes' & location != 'Chicago') %>%
  mutate(
    month = lubridate::month(week_ending, label = T, abbr = F),
    year = lubridate::year(week_ending)
  ) %>%
  group_by(year, month, location) %>%
  summarize(
    total_revenue = sum((avg_price_nonorg * (plu4046 + plu4225 + plu4770 + small_nonorg_bag + large_nonorg_bag + xlarge_nonorg_bag)) + (avg_price_org * (plu94046 + plu94225 + plu94770 + small_org_bag + large_org_bag + xlarge_org_bag))) / 1000000,
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = month, y = total_revenue, group = location, color = location)) +
  geom_line() +
  facet_wrap(.~year, scales = 'free') +
  labs(fill = 'Location', x = 'Month', y = 'Total Revenue (US$)', caption = 'Source: Hass Avocado Board\nNot adjusted for inflation', title = 'Total Revenue per Year by Month - Great Lakes Region') +
  theme(plot.background = element_rect(fill = "grey20"),
        plot.title = element_text(color = "#FFFFFF"),
        axis.title = element_text(color = "#FFFFFF"),
        axis.text.x = element_text(color = 'grey50', angle = 45, hjust = 1),
        axis.text.y = element_text(color = 'grey50'),
        plot.caption = element_text(color = 'grey75'),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey50", size = 0.2),
        panel.grid.minor = element_line(color = "grey50", size = 0.2),
        legend.background = element_rect(fill = 'grey20'),
        legend.key = element_rect(fill = 'grey20'),
        legend.title = element_text(color = 'grey75'),
        legend.text = element_text(color = 'grey75'),
        strip.background = element_rect(fill = 'grey50'),
        strip.text = element_text(color = 'black')
  )


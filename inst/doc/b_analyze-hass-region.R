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

data('hass_region')

dplyr::glimpse(hass_region)

## -----------------------------------------------------------------------------
hass_region %>%
  mutate(
    year = lubridate::year(week_ending)
  ) %>%
  filter(year == 2019) %>%
  group_by(year, region) %>%
  summarize(
    total_revenue = sum((avg_price_nonorg)*(plu4046 + plu4225 + plu4770 + small_nonorg_bag + large_nonorg_bag + xlarge_nonorg_bag) + (avg_price_org)*(plu94046 + plu94225 + plu94770 + small_org_bag + large_org_bag + xlarge_org_bag)),
    .groups = 'drop'
  ) %>%
  slice(which.max(total_revenue))

## ----fig.height=5, fig.width=8------------------------------------------------
hass_region %>%
  mutate(
    year = lubridate::year(week_ending)
  ) %>%
  filter(year == 2019) %>%
  group_by(week_ending, region) %>%
  summarize(
    total_revenue = sum((avg_price_nonorg)*(plu4046 + plu4225 + plu4770 + small_nonorg_bag + large_nonorg_bag + xlarge_nonorg_bag) + (avg_price_org)*(plu94046 + plu94225 + plu94770 + small_org_bag + large_org_bag + xlarge_org_bag)),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = week_ending)) +
  geom_line(aes(y = total_revenue, color = region)) +
  labs(x = 'Month/Year', y = 'Revenue (US$)', title = 'Total Revenue for 2019 Across Regions', caption = 'Source: Hass Avocado Board\nNot adjusted for inflation') +
  scale_color_manual(name = 'Region', values = c('California' = 'orange', 'Great Lakes' = 'blue', 'Midsouth' = 'yellow', 'Northeast' = 'steelblue', 'Plains' = 'darkgreen', 'South Central' = 'red', 'Southeast' = 'magenta', 'West' = 'darkgray')) +
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
hass_region %>%
  group_by(region) %>%
  summarize(
    total_revenue = sum((avg_price_nonorg)*(plu4046 + plu4225 + plu4770 + small_nonorg_bag + large_nonorg_bag + xlarge_nonorg_bag) + (avg_price_org)*(plu94046 + plu94225 + plu94770 + small_org_bag + large_org_bag + xlarge_org_bag)),
    .groups = 'drop'
  ) %>%
  arrange(desc(region)) %>%
  mutate(
    prop = round(total_revenue / sum(total_revenue) * 100),
    ypos = cumsum(prop) - (0.5*prop),
    txt = paste0(region, ': ', prop,'%')
  ) %>%
  ggplot(aes(x = "", y = prop, fill = region)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  coord_polar(theta = 'y') +
  theme_void() +
  ggrepel::geom_label_repel(aes(y = ypos, label = txt), show.legend = FALSE, color = 'black', size = 3, nudge_x = 1) +
  labs(title = 'Revenue Proportion by Region', caption = 'Source: Hass Avocado Board') +
  theme(
    # plot.background = element_rect(fill = "grey20"),
    plot.title = element_text(color = "#000000"),
    plot.caption = element_text(color = 'grey75'),
    panel.background = element_blank(),
    legend.position = 'none'
    )




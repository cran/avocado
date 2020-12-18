## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(avocado)
library(dplyr)
library(ggplot2)

data('hass_usa')

dplyr::glimpse(hass_usa)

## ----fig.height=5, fig.width=8------------------------------------------------
hass_usa %>%
  ggplot(aes(x = week_ending)) +
  geom_line(aes(y = avg_price_nonorg, color = 'Non Organic')) +
  geom_line(aes(y = avg_price_org, color = 'Organic')) +
  scale_color_manual(values = c('steelblue','forestgreen')) +
  labs(x = 'Year', y = 'Average Price per Pound in US$', color = 'Type', title = 'Average Selling Price by Week', caption = 'Not adjusted for inflation\nSource: Hass Avocado Board') +
  ylim(min = 0, max = 3.0) +
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
hass_usa %>%
  mutate(
    nonorg_rev = (plu4046+plu4225+plu4770+small_nonorg_bag+large_nonorg_bag+xlarge_nonorg_bag) * avg_price_nonorg
  ) %>%
  ggplot(aes(x = week_ending)) +
  geom_line(aes(y = nonorg_rev/1000000), color = 'steelblue') +
  labs(x = 'Year', y = 'Revenue (Millions US$)', color = 'Type', title = 'Non Organic Avocado Sales Revenue', caption = 'Not adjusted for inflation\nSource: Hass Avocado Board') +
  theme(plot.background = element_rect(fill = "grey20"),
        plot.title = element_text(color = "#FFFFFF"),
        axis.title = element_text(color = "#FFFFFF"),
        axis.text.x = element_text(color = 'grey50'),
        axis.text.y = element_text(color = 'grey50'),
        plot.caption = element_text(color = 'grey75'),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey50", size = 0.2),
        panel.grid.minor = element_line(color = "grey50", size = 0.2),
        legend.background = element_rect(fill = 'grey20'),
        legend.key = element_rect(fill = 'grey20'),
        legend.title = element_text(color = 'grey75'),
        legend.text = element_text(color = 'grey75'),
        # legend.position = c(0.815, 0.2)
        legend.position = 'none'
  )

## ----fig.height=5, fig.width=8------------------------------------------------
hass_usa %>%
  mutate(
    nonorg_rev = (plu4046+plu4225+plu4770+small_nonorg_bag+large_nonorg_bag+xlarge_nonorg_bag) * avg_price_nonorg
  ) %>%
  select(week_ending, nonorg_rev) %>%
  filter(week_ending > lubridate::ymd('2017-09-10') & week_ending < lubridate::ymd('2017-10-15')) %>%
  ggplot(aes(x = week_ending, y = nonorg_rev)) +
  geom_line(color = 'steelblue') +
  labs(x = 'Year', y = 'Revenue', color = 'Type', title = 'Non Organic Avocado Sales Revenue', caption = 'Not adjusted for inflation\nSource: Hass Avocado Board') +
  theme(plot.background = element_rect(fill = "grey20"),
    plot.title = element_text(color = "#FFFFFF"),
    axis.title = element_text(color = "#FFFFFF"),
    axis.text.x = element_text(color = 'grey50'),
    axis.text.y = element_text(color = 'grey50'),
    plot.caption = element_text(color = 'grey75'),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey50", size = 0.2),
    panel.grid.minor = element_line(color = "grey50", size = 0.2),
    legend.background = element_rect(fill = 'grey20'),
    legend.key = element_rect(fill = 'grey20'),
    legend.title = element_text(color = 'grey75'),
    legend.text = element_text(color = 'grey75'),
    legend.position = 'none'
  )


## ----include = FALSE----------------------------------------------------------
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

hass_usa |> 
  ggplot(aes(x = week_ending)) +
  geom_line(aes(y = avg_selling_price, color = as.factor(type))) +
  scale_color_manual(labels = c('Conventional','Organic'), values = c('steelblue','forestgreen')) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(
    x = 'Year',
    y = 'Average Selling Price per Unit (US$)',
    title = 'Fluctuation of Average Selling Price', 
    caption = 'Not adjusted for inflation\nSource: Hass Avocado Board',
    color = ''
  ) +
  ylim(min = 0, max = 3.0) +
  theme(
    plot.background = element_rect(fill = "grey20"),
    plot.title = element_text(color = "#FFFFFF"),
    axis.title = element_text(color = "#FFFFFF"),
    axis.text.x = element_text(color = 'grey50', angle = 45, hjust = 1),
    axis.text.y = element_text(color = 'grey50'),
    plot.caption = element_text(color = 'grey75'),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey50", linewidth = 0.2),
    panel.grid.minor = element_line(color = "grey50", linewidth = 0.2),
    legend.background = element_rect(fill = 'grey20'),
    legend.key = element_rect(fill = 'grey20'),
    legend.title = element_text(color = 'grey75'),
    legend.text = element_text(color = 'grey75'),
    legend.position = 'inside',
    legend.position.inside = c(0.85, 0.85)
  )



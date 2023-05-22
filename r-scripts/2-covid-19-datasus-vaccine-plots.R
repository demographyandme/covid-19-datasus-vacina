# introduction ####
# R code for the paper Age reporting for the oldest old in the Brazilian COVID-19
# vaccination database: what can we learn from it?

# To avoid scientific notation on data frames
options(scipen = 100)

# Load libraries ####
library(patchwork)
library(ggrepel)
library(ggtext)
# library(ggforce)
# library(ggdist)

library(viridis)
library(systemfonts)

library(styler)
library(here)
library(glue)

library(tidyverse)

# theme and other options ####
theme_set(
  theme_minimal(base_size = 14, base_family = "Arial") +
    theme(
      title = element_markdown(lineheight = 0.9),
      line = element_line(linewidth = 0.50),
      plot.margin = margin(t = 20, r = 20, b = 10, l = 20), # top, right, bottom, left
      # plot.background = element_rect(fill = "#FFF1E0", color = "#FFF1E0"),
      plot.title.position = "plot", # left-aligned title
      plot.title = element_markdown(size = 20, color = "grey10"),
      plot.subtitle = element_markdown(size = 18, color = "grey20"),
      plot.caption = element_markdown(size = 14, color = "grey20", margin = margin(t = 10), hjust = 1.0),
      plot.caption.position = "plot", # caption aligned to plot
      legend.position = "top",
      legend.key.width = unit(4.0, "lines"),
      legend.key.height = unit(0.5, "lines"),
      legend.title = element_markdown(size = 16, color = "grey10", vjust = 0.5, hjust = 0.5),
      legend.text = element_markdown(size = 14, color = "grey20"),
      axis.title = element_markdown(color = "grey10"),
      axis.text = element_markdown(size = 14, color = "grey20"),
      axis.text.x = element_markdown(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title.x = element_markdown(vjust = -1, margin = margin(t = 6)),
      axis.title.y = element_markdown(vjust = -1, margin = margin(r = 12)),
      axis.ticks = element_line(size = 0.25, color = "grey80"),
      axis.ticks.length = unit(0.5, "lines"),
      panel.grid = element_line(size = 0.25, color = "grey80"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.25),
      # panel.grid.major.x = element_blank(),
      panel.spacing = unit(4, "mm"),
      strip.text = element_text(size = 14, color = "grey10")
    )
)

# Color Scales ####
ff_scale_color_d <- scale_color_viridis(discrete = TRUE, option = "B", end = 0.80)
ff_scale_color_c <- scale_color_viridis(discrete = FALSE, option = "B", end = 0.80)
ff_scale_fill_d <- scale_fill_viridis(discrete = TRUE, option = "B", end = 0.80)
ff_scale_fill_c <- scale_fill_viridis(discrete = FALSE, option = "B", end = 0.80)

# Reverse color scales ####
ff_scale_color_d_reverse <- scale_color_viridis(discrete = TRUE, option = "B", guide = guide_legend(reverse = TRUE), direction = -1, end = 0.80)
ff_scale_color_c_reverse <- scale_color_viridis(discrete = FALSE, option = "B", guide = guide_colorbar(reverse = TRUE), direction = -1, end = 0.80)
ff_scale_fill_d_reverse <- scale_fill_viridis(discrete = TRUE, option = "B", guide = guide_legend(reverse = TRUE), direction = -1, end = 0.80)
ff_scale_fill_c_reverse <- scale_fill_viridis(discrete = FALSE, option = "B", guide = guide_colorbar(reverse = TRUE), direction = -1, end = 0.80)

# Functions ####
# to save plots in PDF format
ff_ggsave_pdf <- function(filename, plot = last_plot(), device = cairo_pdf, path = here("output"),
                          scale = 1, width = NA, height = NA, limitsize = TRUE, ...) {
  ggsave(
    filename = filename, device = device, path = path,
    scale = scale, width = width, height = height, limitsize = limitsize, ...
  )
}

# function for ggsave as png with agg_png
ff_ggsave_png <- function(filename, plot = last_plot(), device = ragg::agg_png, path = here("output"),
                          scale = 1, width = NA, height = NA, dpi = 600, limitsize = TRUE,
                          bg = "white", bitsize = 16, ...) {
  ggsave(
    filename = filename, device = device, path = path,
    scale = scale, width = width, height = height, dpi = dpi, limitsize = limitsize,
    bg = bg, bitsize = bitsize, ...
  )
}

# 9.0 Plots ####
## 9.1.A p1_vaccination_by_day ####
first_date <- as_date("2021-01-01")
last_date <- as_date("2022-03-31")

plot_vaccination_max <-
  vaccination_by_day %>%
  group_by(residence_region_name, vaccination_date) %>%
  summarise(vaccination_total = sum(vaccination_total)) %>%
  filter(vaccination_total == max(vaccination_total)) %>%
  ungroup()

plot_vaccination_mean <-
  vaccination_by_day %>%
  filter(vaccination_date >= as_date("2022-03-01")) %>%
  group_by(residence_region_name) %>%
  summarise(, vaccination_date = mean(vaccination_date), vaccination_total = mean(vaccination_total)) %>%
  ungroup()

p1_vaccination_by_day <-
  vaccination_by_day %>%
  ggplot(mapping = aes(x = as_date(vaccination_date))) +
  geom_line(aes(y = vaccination_total, color = sex_name), alpha = 0.8, linewidth = 0.7) +
  geom_text_repel(
    data = (plot_vaccination_max),
    aes(
      y = vaccination_total,
      label = glue("Maximum (both sexes): \n {scales::comma(vaccination_total)}"), lineheight = 0.9
    ),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 5.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 999, point.padding = 0.25, box.padding = 1.00,
    segment.curvature = -0.25, segment.ncp = 2, segment.angle = +25,
    xlim = c(NA, as_date("2021-01-01")), ylim = c(4.65, NA), direction = "both"
  ) +
  geom_text_repel(
    data = (plot_vaccination_mean),
    aes(
      y = vaccination_total,
      label = glue("Mean, \n last 14 days (both sexes): \n {scales::comma(vaccination_total, accuracy = 1.0)}"), lineheight = 0.9
    ),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 5.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 999, point.padding = 0.25, box.padding = 1.00,
    segment.curvature = -0.25, segment.ncp = 2, segment.angle = +25,
    xlim = c(NA, NA), ylim = c(2.5, NA), direction = "both"
  ) +
  coord_cartesian(xlim = c(), ylim = c(1, 10000000), expand = FALSE, clip = "off") +
  scale_x_date(limits = c(first_date, last_date), labels = scales::date_format("%b %Y"), date_breaks = "1 month") +
  scale_y_log10(labels = scales::comma_format(accuracy = 1.0)) +
  labs(
    x = "Date",
    y = "Daily vaccination (log10)",
    color = "Sex"
  ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(
    axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust = 1.0),
  ) +
  ff_scale_color_d +
  facet_wrap(~residence_region_name, labeller = label_wrap_gen(12))

## 9.1.B p1_vaccination_by_day_acum_log ####
plot_vaccination_max_cum <-
  vaccination_by_day %>%
  group_by(residence_region_name, vaccination_date) %>%
  summarise(vaccination_accumulated = sum(vaccination_accumulated)) %>%
  filter(vaccination_accumulated == max(vaccination_accumulated)) %>%
  ungroup()

p1_vaccination_by_day_acum_log <-
  vaccination_by_day %>%
  ggplot(mapping = aes(x = as_date(vaccination_date))) +
  geom_line(aes(y = vaccination_accumulated, color = sex_name), alpha = 0.8, size = 0.7) +
  geom_text_repel(
    data = (plot_vaccination_max_cum),
    aes(y = vaccination_accumulated, label = glue("Maximum (both sexes): \n {scales::comma(vaccination_accumulated)}"), lineheight = 0.9),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 5.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 999, point.padding = 0.25, box.padding = 1.00,
    segment.curvature = +0.25, segment.ncp = 2, segment.angle = +25,
    xlim = c(NA, NA), ylim = c(2.5, 3.4), direction = "both"
  ) +
  coord_cartesian(xlim = c(), ylim = c(1, 10000000), expand = FALSE, clip = "off") +
  scale_x_date(limits = c(first_date, last_date), labels = scales::date_format("%b %Y"), date_breaks = "1 month") +
  scale_y_log10(labels = scales::comma_format(accuracy = 1.0)) +
  labs(
    x = "Date",
    y = "Population vaccinated (log10)",
    color = "Sex"
  ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(
    axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust = 1.0),
  ) +
  ff_scale_color_d +
  facet_wrap(~residence_region_name, labeller = label_wrap_gen(12))

## 9.1.A.B wrap_plots p1_vaccination_by_day and p1_vaccination_by_day_acum_log ####
patchwork::wrap_plots(
  p1_vaccination_by_day,
  p1_vaccination_by_day_acum_log,
  ncol = 1, nrow = 2
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a")
# save
ff_ggsave_pdf("p1_vaccination_by_day_acum_log.pdf", scale = 0.80, width = 16.00, height = 20.00)
ff_ggsave_png("p1_vaccination_by_day_acum_log.png", scale = 0.80, width = 16.00, height = 20.00)

## 9.2 p1_vaccination_accu_ratio_dumbbell ####
vaccination_population_age_groups %>%
  filter(age_groups %in% c("80-84", "85-89", "90+")) %>%
  mutate(
    age_groups =
      case_when(
        age_groups == "80-84" ~ "80\u201384", # en-dash
        age_groups == "85-89" ~ "85\u201389", # en-dash
        TRUE ~ age_groups
      )
  ) %>%
  ggplot(mapping = aes(x = reorder(glue("{residence_region_name}, {age_groups}"), vaccination_accu_ratio))) +
  geom_line(
    aes(y = vaccination_accu_ratio, group = glue("{residence_region_name}{age_groups}")),
    color = "grey20", lty = 1, size = 0.25
  ) +
  geom_point(aes(
    y = vaccination_accu_ratio, color = sex_name,
    size = vaccination_total
  ), alpha = 0.8) +
  geom_hline(yintercept = 1.0, color = "grey20", lty = 1, size = 0.25) +
  coord_flip(clip = "off", expand = TRUE, ylim = c(0.8, 1.3)) +
  scale_size_binned_area(
    labels = scales::comma_format(accuracy = 1.0),
    breaks = c(25000, 50000, 100000, 250000, 500000, 1000000, 2000000)
  ) +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.1), breaks = seq(0.8, 1.3, 0.1)) +
  labs(
    x = "Region, age group",
    y = "Vaccination prevalence",
    color = "Sex",
    size = "Size of respective population vaccinated with first or single dose"
  ) +
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1, override.aes = list(size = 4)),
    size = guide_bins(title.position = "top", title.hjust = 0.5, keywidth = unit(4.0, "lines"))
  ) +
  ff_scale_color_d
# save
ff_ggsave_pdf("p1_vaccination_accu_ratio_dumbbell.pdf", scale = 0.80, width = 16.00, height = 10.00)
ff_ggsave_png("p1_vaccination_accu_ratio_dumbbell.png", scale = 0.80, width = 16.00, height = 10.00)

## 9.4 p1_vaccination_90_80 ####
vaccination_100_90_80 %>%
  ggplot(mapping = aes(x = population_90_80)) +
  geom_point(aes(y = vaccination_90_80, color = residence_region_name, size = vaccination_80_plus), alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0.0, color = "grey20", lty = 1, size = 0.25) +
  geom_text_repel(
    seed = 15, force = 1.5, max.iter = 5000,
    aes(y = vaccination_90_80, label = residence_region_name, lineheight = 0.9),
    family = "Arial", fontface = "italic", size = 5.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.25, box.padding = 0.60,
    segment.curvature = -0.25, segment.ncp = 2, segment.angle = 25,
    xlim = c(0.10, 0.225), ylim = c(0.10, 0.225), direction = "both"
  ) +
  coord_cartesian(xlim = c(0.11, 0.23), ylim = c(0.11, 0.23), expand = FALSE, clip = "off") +
  scale_size_binned_area(
    labels = scales::comma_format(accuracy = 1.0),
    breaks = c(25000, 50000, 100000, 250000, 500000, 1000000, 2000000)
  ) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 0.01), breaks = seq(0.11, 0.23, 0.01)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01), breaks = seq(0.11, 0.23, 0.01)) +
  labs(
    x = "IBGE projected population 90+/80+",
    y = "Population 90+/80+ vaccinated with first dose",
    color = "Region",
    size = "Size of population 80+ vaccinated with first or single dose"
  ) +
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1, override.aes = list(size = 4)),
    size = guide_bins(title.position = "top", title.hjust = 0.5, keywidth = unit(4.0, "lines"))
  ) +
  ff_scale_color_d +
  theme(
    legend.box = "horizontal",
    panel.spacing = unit(9, "mm"),
    axis.text = element_markdown(size = 11)
  ) +
  facet_wrap(~sex_name, labeller = label_wrap_gen(12))
# save
ff_ggsave_pdf("p1_vaccination_90_80.pdf", scale = 0.80, width = 16.00, height = 10.00)
ff_ggsave_png("p1_vaccination_90_80.png", scale = 0.80, width = 16.00, height = 10.00)

## 9.5.A p1_100_90_80_swe_brasil_regions ####
p1_100_90_80_swe_brasil_regions <-
  ggplot() +
  geom_point(
    data = hmd_100_90_80_SWE,
    aes(x = population_90_80, y = population_100_90 * population_90_80, color = country_name, size = population_80_plus), alpha = 0.8
  ) +
  geom_point(
    data = (vaccination_100_90_80 %>% rename(sex = sex_name)),
    aes(x = vaccination_90_80, y = vaccination_100_90 * vaccination_90_80, color = "Brazil Vaccinated (*)", size = vaccination_80_plus),
    alpha = 0.8
  ) +
  geom_text_repel(
    data = (hmd_100_90_80_SWE %>% group_by(sex) %>%
      filter(year == min(year)) %>% ungroup()),
    aes(x = population_90_80, y = population_100_90 * population_90_80, label = glue("{country_name}, {year}"), lineheight = 0.9),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 4.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.10, box.padding = 0.50,
    segment.curvature = 0.25, segment.ncp = 2, segment.angle = -25,
    xlim = c(NA, NA), ylim = c(0.0040, NA), direction = "both"
  ) +
  geom_text_repel(
    data = (hmd_100_90_80_SWE %>% group_by(sex) %>%
      filter(year == max(year)) %>% ungroup()),
    aes(x = population_90_80, y = population_100_90 * population_90_80, label = glue("{country_name}, {year}"), lineheight = 0.9),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 4.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.10, box.padding = 0.25,
    segment.curvature = -0.25, segment.ncp = 2, segment.angle = -25,
    xlim = c(0.16, NA), ylim = c(NA, 0.002), direction = "both"
  ) +
  geom_text_repel(
    data = (vaccination_100_90_80 %>% rename(sex = sex_name)),
    aes(x = vaccination_90_80, y = vaccination_100_90 * vaccination_90_80, label = residence_region_name, lineheight = 0.9),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 4.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.25, box.padding = 1.00,
    segment.curvature = -0.25, segment.ncp = 2, segment.angle = -25,
    xlim = c(NA, NA), ylim = c(NA, NA), direction = "both"
  ) +
  coord_cartesian(xlim = c(0.07, 0.23), ylim = c(0.0, 0.024), expand = FALSE, clip = "off") +
  scale_size_binned_area(
    labels = scales::comma_format(accuracy = 1.0),
    breaks = c(10000, 25000, 50000, 75000, 100000, 250000, 500000, 1000000, 2000000)
  ) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 0.01), breaks = seq(0.07, 0.23, 0.01)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.001), breaks = seq(0.00, 0.024, 0.002)) +
  labs(
    x = "Population 90+/80+",
    y = "Population 100+/80+",
    color = "Country",
    size = "Size of population 80+"
  ) +
  guides(
    color = guide_legend(
      title.position = "top", title.hjust = 0.5, order = 1,
      override.aes = list(size = 4), keywidth = unit(1.0, "lines")
    ),
    size = guide_bins(title.position = "top", title.hjust = 0.5, keywidth = unit(4.0, "lines"))
  ) +
  theme(
    axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(10, "mm")
  ) +
  ff_scale_color_d +
  facet_grid(. ~ sex, labeller = label_wrap_gen(12))

## 9.5.B p1_life_expectancy_90_80 ####
p1_life_expectancy_90_80 <-
  ggplot() +
  geom_point(
    data = hmd_100_90_80_SWE,
    aes(y = ex_50, x = population_90_80, color = country_name, size = population_80_plus),
    alpha = 0.8
  ) +
  geom_point(
    data = (vaccination_100_90_80 %>% rename(sex = sex_name)),
    aes(y = ex_50, x = vaccination_90_80, color = "Brazil Vaccinated (*)", size = vaccination_80_plus),
    alpha = 0.8
  ) +
  geom_text_repel(
    data = (hmd_100_90_80_SWE %>% group_by(sex) %>%
      filter(year == min(year)) %>% ungroup()),
    aes(y = ex_50, x = population_90_80, label = glue("{country_name}, {year}"), lineheight = 0.9),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 4.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.10, box.padding = 1.00,
    segment.curvature = +0.25, segment.ncp = 2, segment.angle = -25,
    xlim = c(NA, NA), ylim = c(30, 31), direction = "both"
  ) +
  geom_text_repel(
    data = (hmd_100_90_80_SWE %>% group_by(sex) %>%
      filter(year == max(year)) %>% ungroup()),
    aes(y = ex_50, x = population_90_80, label = glue("{country_name}, {year}"), lineheight = 0.9),
    seed = 15, force = 1.5, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 4.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.10, box.padding = 0.25,
    segment.curvature = -0.25, segment.ncp = 2, segment.angle = 45,
    xlim = c(NA, NA), ylim = c(36, NA), direction = "both"
  ) +
  geom_text_repel(
    data = (vaccination_100_90_80 %>% rename(sex = sex_name)),
    aes(y = ex_50, x = vaccination_90_80, label = residence_region_name, lineheight = 0.9),
    seed = 15, force = 2.0, max.iter = 5000,
    family = "Arial", fontface = "plain", size = 4.0, hjust = 0.50, vjust = 0.50,
    color = "grey20", segment.linetype = "solid",
    segment.size = 0.25, min.segment.length = 0, point.padding = 0.25, box.padding = 2.00,
    segment.curvature = +0.25, segment.ncp = 2, segment.angle = -25,
    xlim = c(NA, NA), ylim = c(NA, NA), direction = "both"
  ) +
  coord_cartesian(ylim = c(25.0, 37.0), xlim = c(0.07, 0.23), expand = FALSE, clip = "off") +
  scale_size_binned_area(
    labels = scales::comma_format(accuracy = 1.0),
    breaks = c(10000, 25000, 50000, 75000, 100000, 250000, 500000, 1000000, 2000000)
  ) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1.00), breaks = seq(25.0, 37.0, 1.0)) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 0.01), breaks = seq(0.07, 0.23, 0.01)) +
  labs(
    y = "e(50)",
    x = "Population 90+/80+",
    color = "Country",
    size = "Size of population 80+",
    caption = "(*) e(50) from IBGE (2018)"
  ) +
  guides(
    color = guide_legend(
      title.position = "top", title.hjust = 0.5, order = 1,
      override.aes = list(size = 4), keywidth = unit(1.0, "lines")
    ),
    size = guide_bins(title.position = "top", title.hjust = 0.5, keywidth = unit(4.0, "lines"))
  ) +
  theme(
    axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(10, "mm")
  ) +
  ff_scale_color_d +
  facet_wrap(~sex, labeller = label_wrap_gen(12))

## 9.5.A.B wrap_plots p1_100_90_80_swechejpn_brasil_regions and  p1_life_expectancy_90_80####
patchwork::wrap_plots(
  p1_100_90_80_swe_brasil_regions,
  p1_life_expectancy_90_80,
  nrow = 2
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a")
# save
ff_ggsave_pdf("p1_100_90_80_life_expectancy_90_80.pdf", scale = 0.80, width = 18.00, height = 18.00)
ff_ggsave_png("p1_100_90_80_life_expectancy_90_80.png", scale = 0.80, width = 18.00, height = 18.00)

## 9.6.0 p1_hmd_SWE_sex_ratio_1992_2019 ####
p1_hmd_SWE_sex_ratio_1992_2019 <-
  hmd_population_SWE_sex_ratio %>%
  filter(year == 2019) %>%
  ggplot(mapping = aes(x = age)) +
  geom_line(aes(y = sex_ratio, group = year), alpha = 0.8, size = 0.7)

## 9.7 p1_vaccination_sex_ratio ####
# p1_vaccination_sex_ratio
ggplot(mapping = aes(x = age_calculated)) +
  geom_line(
    data = layer_data(p1_hmd_SWE_sex_ratio_1992_2019),
    aes(x = x, y = y, group = group, color = "Sweden 2019"), alpha = 0.8, size = 0.50
  ) +
  geom_line(
    data = vaccination_single_age_sex_ratio,
    aes(y = sex_ratio, color = "Brazil first or single dose of vaccination"), alpha = 0.8, size = 0.7
  ) +
  geom_line(
    data = subset(br_censuses_ipums_1960_2010_sex_ratio, year %in% c(2010)),
    aes(y = sex_ratio, color = "Brazil 2010 Census"), alpha = 0.8, size = 0.7
  ) +
  coord_cartesian(xlim = c(), ylim = c(0.0, 1.5), expand = FALSE) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1.0), breaks = seq(80, 120, 5)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01), breaks = seq(0, 1.5, 0.25)) +
  labs(
    x = "Single age",
    y = "Sex ratio",
    color = ""
  ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(
    panel.spacing = unit(8, "mm")
  ) +
  scale_color_manual(values = c("#A41034", "#0E4D92", "grey40")) +
  facet_wrap(~residence_region_name, labeller = label_wrap_gen(12))
# save
ff_ggsave_pdf("p1_vaccination_sex_ratio.pdf", scale = 0.80, width = 16.00, height = 10.00)
ff_ggsave_png("p1_vaccination_sex_ratio.png", scale = 0.80, width = 16.00, height = 10.00)

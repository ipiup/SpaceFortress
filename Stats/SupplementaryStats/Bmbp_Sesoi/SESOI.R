#SESOI 
devtools::install_github("mladenjovanovic/bmbstats")
require(bmbstats)

require(tidyverse)
require(cowplot)
require(ggridges)
require(kableExtra)

user_black <- "#000000"
user_blue <- "#5DA5DA"
user_red <- "#F15854"
user_grey <- "#4D4D4D"
user_green <- "#60BD68"
user_orange <- "#FAA43A"
user_pink <- "#F17CB0"
user_purple <- "#B276B2"
user_yellow <- "#DECF3F"

user_black_transparent <- "#0000006A"
user_blue_transparent <- "#5DA5DA6A"
user_red_transparent <- "#F158546A"
user_grey_transparent <- "#4D4D4D6A"
user_green_transparent <- "#60BD686A"
user_orange_transparent <- "#FAA43A6A"
user_pink_transparent <- "#F17CB06A"
user_purple_transparent <- "#B276B26A"
user_yellow_transparent <- "#DECF3F6A"
equivalent_color <- user_grey
equivalent_color_transparent <- user_grey_transparent
higher_color_transparent <- user_red_transparent
lower_color_transparent <- user_green_transparent
change_color <- user_black

data("bench_press_data")
typical_error <- 2.5
SESOI_lower <- -5
SESOI_upper <- 5


bench_press_data <- bench_press_data %>%
  mutate(
    Upper = Change + 2.09 * sqrt(2) * typical_error,
    Lower = Change - 2.09 * sqrt(2) * typical_error,
    `higher %` = round(100 * (1 - pt((SESOI_upper - Change) / (sqrt(2) * typical_error), df = 19)), 0),
    `lower %` = round(100 * pt((SESOI_lower - Change) / (sqrt(2) * typical_error), df = 19), 0),
    `equivalent %` = round(100 - (`higher %` + `lower %`), 0),
    label = paste("  [", `lower %`, "/", `equivalent %`, "/", `higher %`, "]", sep = "")
  )

bench_press_data$Athlete <- reorder(bench_press_data$Athlete, (bench_press_data$Change))
measurement_error_data <- data.frame(
  Athlete = numeric(0),
  Change = numeric(0),
  sample = numeric(0)
)

for (i in seq(1, nrow(bench_press_data))) {
  measurement_error_data <- rbind(
    measurement_error_data,
    data.frame(
      Athlete = bench_press_data$Athlete[i],
      Change = bench_press_data$Change[i],
      sample = perfect_rnorm(
        n = 100,
        mean = bench_press_data$Change[i],
        sd = typical_error * sqrt(2)
      )
    )
  )
}
gg_meas_err <- ggplot(measurement_error_data, aes(y = Athlete, x = sample)) +
  theme_cowplot(8) +
  annotate("rect", xmin = SESOI_lower, xmax = SESOI_upper, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = equivalent_color) +
  geom_vline(xintercept = 0, color = "dark grey") +
  geom_density_ridges_gradient(
    aes(
      fill = ifelse(..x.. > SESOI_upper, "Higher", ifelse(..x.. < SESOI_lower, "Lower", "Equivalent")),
      group = Athlete
    ),
    color = NA, vline_color = "white",
    scale = 2
  ) +
  ylab(NULL) +
  xlab("Change in Bench Press 1RM (kg)") +
  theme(legend.position = "none") +
  xlim(-30, 40) +
  scale_fill_manual(values = c(user_grey_transparent, user_green_transparent, user_red_transparent)) +
  scale_discrete_manual("point_color", values = c(user_green_transparent, user_red_transparent, user_grey_transparent))



gg_individual <- ggplot(bench_press_data, aes(y = Athlete, x = Change)) +
  theme_cowplot(8) +
  annotate("rect", xmin = SESOI_lower, xmax = SESOI_upper, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = equivalent_color) +
  geom_vline(xintercept = 0, color = "dark grey") +
  geom_errorbarh(aes(xmax = Upper, xmin = Lower), height = 0) +
  geom_point(color = change_color) +
  geom_text(aes(y = Athlete, x = Upper, label = label), hjust = "left", size = 2) +
  ylab(NULL) +
  xlab("Change in Bench Press 1RM (kg)") +
  theme(legend.position = "none") +
  xlim(-30, 50)

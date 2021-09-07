# BEWARE! nay take 15+ minutes!

library(ShinyItemAnalysis)
library(tidyverse)
library(gganimate)
library(magick) # note you have to install ImageMagick to your computer and make sure it is visible for R, i.e. on PATH (Sys.getenv("path"))
library(here)
library(svglite)

slide_asp_ratio <- 4 / 3
base_width <- 7


range <- 2:72
k_max <- 72
dir <- "top"

facet_width <- base_width
facet_height <- (base_width / slide_asp_ratio) / 2 # half of the slide (plus little more)
dpi <- 150


if (!file.exists(here("sampleRcode/IRRanimation/anim_data.rds"))) {
  # estimate all possible top-restricted subsets
  all_top_restricted <- map_dfr(
    2:72,
    ~ ICCrestricted(
      Data = AIBS,
      case = "ID",
      var = "Score",
      rank = "ScoreRankAdj",
      sel = .x,
      nsim = 100
    )
  )

  # save for later
  all_top_restricted %>% write_rds(here("sampleRcode/IRRanimation/anim_data.rds"))
} else {
  all_top_restricted <- read_rds(here("sampleRcode/IRRanimation/anim_data.rds"))
}

# catterpillar plot - prepare data for each frame ----------------
a_data <- map_dfr(
  range,
  ~ AIBS %>%
    mutate(
      frame = .x,
      hl = case_when(
        dir == "top" & ScoreRankAdj <= .x ~ "sol",
        dir == "bottom" & ScoreRankAdj > (k_max - .x) ~ "sol",
        TRUE ~ "alp"
      ) %>% factor(levels = c("alp", "sol"))
    )
)

# plot
a <- a_data %>%
  ggplot(aes(
    x = ScoreRankAdj,
    y = Score,
    group = ID,
    alpha = hl
  )) +
  geom_line(col = "gray") +
  geom_point(shape = 1, size = 1.5) +
  stat_summary(
    fun = mean,
    fun.args = list(na.rm = TRUE),
    geom = "point",
    col = "red",
    shape = 5,
    size = 2.5,
    stroke = .35
  ) +
  scale_alpha_discrete(range = c(.33, 1), drop = FALSE) +
  coord_cartesian(ylim = c(1, 5)) +
  scale_y_continuous(
    breaks = 1:5,
    labels = function(x) scales::number(x, .01)
  ) +
  scale_x_continuous(breaks = seq(1, 72, 10)) +
  labs(x = "Proposal rank", y = "Overall scientific merit score") +
  theme_app(base_size = 11) +
  transition_manual(frame %>% as.factor() %>% fct_rev()) # reverse

# render
a_gif <- animate(a, fps = 5, end_pause = 2, width = facet_width, height = facet_height, units = "in", res = dpi)

# show
# a_gif


# reliability plot - prepare data -----------------
b_data <- map_dfr(
  range,
  ~ all_top_restricted %>%
    mutate(
      frame = .x,
      hl = case_when(
        n_sel == .x ~ "black",
        n_sel > .x ~ "grey75",
        TRUE ~ "white"
      )
    )
)

# plot
b <- b_data %>%
  ggplot(aes(
    prop_sel,
    ICC1,
    ymin = ICC1_LCI,
    ymax = ICC1_UCI,
    col = hl
  )) + # TODO general
  geom_linerange() + # separate as plotly messes up otherwise
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_identity() +
  labs(
    x = paste0("Proportion of ", dir, " proposals"),
    y = "Inter-rater reliability"
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # TODO general
  theme_app(base_size = 11) +
  transition_manual(frame %>% as.factor() %>% fct_rev()) # reverse

b_gif <- animate(b, fps = 5, end_pause = 2, width = facet_width, height = facet_height, units = "in", res = dpi)

# b_gif


# join
a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

n_frames_a <- image_info(a_mgif) %>% nrow()
n_frames_b <- image_info(b_mgif) %>% nrow()

n_frames_a == n_frames_b

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for (i in 2:n_frames_a) {
  cat(i, "of", n_frames_a, "frames combined.\r")
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

new_gif %>% image_write("sampleRcode/IRRanimation/IRR1_smaller.gif")


# PNG ---------------------------------------------------------------------

a_png <- animate(a,
  fps = 5, end_pause = 0, units = "in", res = dpi, width = facet_width, height = facet_height,
  device = "png", renderer = file_renderer(here("sampleRcode/IRRanimation/irr1_png"), prefix = "irr1_upper_", overwrite = TRUE)
)

b_png <- animate(b,
  fps = 5, end_pause = 0, units = "in", res = dpi, width = facet_width, height = facet_height,
  device = "png",
  renderer = file_renderer(here("sampleRcode/IRRanimation/irr1_png"), prefix = "irr1_lower_", overwrite = TRUE)
)


for (i in seq_along(a_png)) {
  cat(i, "of", length(a_png), "frames combined.\r")
  combined <- image_append(c(image_read(a_png[i]), image_read(b_png[i])), stack = TRUE)
  combined %>% image_write(here(
    "sampleRcode/IRRanimation/irr1_png/combined",
    paste0("irr1-", i, ".png")
  ))
}


# svg stab ----------------------------------------------------------------
a_svg <- animate(a,
  fps = 5, end_pause = 0, width = facet_width, height = facet_height,
  device = "svg", renderer = file_renderer(here("sampleRcode/IRRanimation/irr1_svg/upper"), prefix = "irr1_upper_", overwrite = TRUE)
)

b_svg <- animate(b,
  fps = 5, end_pause = 0, width = facet_width, height = facet_height,
  device = "svg",
  renderer = file_renderer(here("sampleRcode/IRRanimation/irr1_svg/lower"), prefix = "irr1_lower_", overwrite = TRUE)
)


# combine in python
# svg_merge.py

# convert to pdf
here("sampleRcode/IRRanimation/irr1_svg/combined/") %>%
  list.files(pattern = "\\.svg$", full.names = TRUE) %>%
  walk(~ rsvg::rsvg_pdf(.x, file = str_replace(.x, "\\.svg", "\\.pdf")))

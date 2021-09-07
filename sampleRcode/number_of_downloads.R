library(tidyverse)
library(cranlogs)
library(ggtext)

d <- cran_downloads("ShinyItemAnalysis",
  from = "2016-07-04",
  to = "last-day"
) %>% mutate(cumsum = cumsum(count))


d %>%
  ggplot(aes(date, cumsum, col = package)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::number) +
  labs(y = "cumulative count") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("sampleRcode/down_num.pdf", height = unit(5.28, "in"), width = unit( 8.25, "in"))


d %>%
  ggplot(aes(date, cumsum, col = package)) +
  geom_line(size = 1) +
  scale_y_log10(labels = scales::number) +
  labs(y = "cumulative count [log<sub>10</sub>]") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.y = element_markdown()
  )

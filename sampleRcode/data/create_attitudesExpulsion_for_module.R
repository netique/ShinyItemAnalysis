library(tidyverse)
library(here)
library(ShinyItemAnalysis)

d <- AttitudesExpulsion

# MACRO attitudes
macro_pre <- c(
  "PreMacro_01", "PreMacro_02", "PreMacro_03", "PreMacro_04", "PreMacro_05",
  "PreMacro_06", "PreMacro_07"
) %>% set_names(paste0("macro_pre_", 1:7))
macro_post <- c(
  "PostMacro_01", "PostMacro_02", "PostMacro_03", "PostMacro_04", "PostMacro_05",
  "PostMacro_06", "PostMacro_07"
) %>% set_names(paste0("macro_post_", 1:7))
macro_del <- c(
  "DelMacro_01", "DelMacro_02", "DelMacro_03", "DelMacro_04", "DelMacro_05",
  "DelMacro_06", "DelMacro_07"
) %>% set_names(paste0("macro_del_", 1:7))

# MICRO attitudes
micro_pre <- c(
  "PreMicro_01", "PreMicro_02", "PreMicro_03", "PreMicro_04",
  "PreMicro_05", "PreMicro_06", "PreMicro_07", "PreMicro_08",
  "PreMicro_09", "PreMicro_10"
) %>% set_names(paste0("micro_pre_", 1:10))
micro_post <- c(
  "PostMicro_01", "PostMicro_02", "PostMicro_03", "PostMicro_04",
  "PostMicro_05", "PostMicro_06", "PostMicro_07", "PostMicro_08",
  "PostMicro_09", "PostMicro_10"
) %>% set_names(paste0("micro_post_", 1:10))
micro_del <- c(
  "DelMicro_01", "DelMicro_02", "DelMicro_03", "DelMicro_04",
  "DelMicro_05", "DelMicro_06", "DelMicro_07", "DelMicro_08",
  "DelMicro_09", "DelMicro_10"
) %>% set_names(paste0("micro_del_", 1:10))


ae <- d %>%
  select(all_of(c(
    "Group",
    micro_pre, "PreMicro", micro_post, "PostMicro", micro_del, "DelMicro",
    macro_pre, "PreMacro", macro_post, "PostMacro", macro_del, "DelMacro"
  ))) %>%
  mutate(group = as.numeric(Group) - 1, .before = 1) %>%
  select(-Group) %>%
  rename(
    # group = Group,
    micro_pre_ts = PreMicro,
    micro_post_ts = PostMicro,
    micro_del_ts = DelMicro,
    macro_pre_ts = PreMacro,
    macro_post_ts = PostMacro,
    macro_del_ts = DelMacro
  ) #%>%
  # mutate(group = fct_recode(group, "control" = "C", "experimental" = "E")  )

ae %>%
  write_rds(here("inst/shiny-examples/ShinyItemAnalysis-module-DIFCordinal/attitudes_expulsion.rds"))


# testing -----------------------------------------------------------------
#
# library(difNLR)
#
# dif_micro_post <- difORD(ae %>% select(micro_post_1:micro_post_10), ae$group, 0, "cumulative", type = "udif", match = ae$micro_pre_ts, p.adjust.method = "BH")
#
# dif_micro_post %>% plot(item = 10, plot.type = "cumulative")
#
#
# dif_micro_post <- difORD(ae %>% select(micro_del_1:micro_del_10), ae$group, 0, "cumulative", match = ae$micro_pre_ts)
#
# dif_micro_post %>% plot(item = 6, plot.type = "cumulative")

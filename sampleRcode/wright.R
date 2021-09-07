
# -------------------------------------------------------------------------

library(ShinyItemAnalysis)
library(tidyverse)
library(mirt)

data(HCI)

# fit Rasch model with the mirt package
fit <- mirt(dataMedical[,1:100], model = 1, itemtype = "Rasch")
# factor scores
theta <- as.vector(fscores(fit))
# difficulty estimates using IRT parametrization
b <- coef(fit, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

ggWrightMap(theta, b)

theta %>% enframe %>%
  mutate(bin = cut_number(value, 6)) %>%
  ggplot(aes(theta)) +
  geom_histogram(binwidth = .2)

# -------------------------------------------------------------------------


b %>% enframe() %>% ggplot(aes(value, label = name)) +
  # geom_histogram() +
  # geom_text(stat = "bin",nudge_y = 7, aes(label = "asd")) +
  geom_dotplot(binwidth = .5, dotsize = .1, method = "histodot")


b %>% enframe() %>%
  ggplot(aes(x =value, label = name)) +
  geom_point(aes(col = ..count..), stat = "bin", position = "stack", show.legend = F)
  # geom_histogram(aes(group = name)) +
  # stat_bin(geom = "text", aes(label = 1:100), position = position_stack(vjust = .5))



b %>% enframe() %>% mutate(row = row_number()) %>% ggplot() +
  geom_bar(aes( value), stat = "bin", binwidth = .5)

b %>% enframe() %>% mutate(row = row_number()) %>% ggplot() +
  geom_point(aes(value), stat = "bin")


b_data <- (b %>%
   enframe %>%
   ggplot(aes(value)) +
   geom_dotplot(method = "histodot", binwidth = .5) ) %>%
  ggplot_build() %>%
  pluck("data", 1) %>% mutate(type = "b")


tibble(name = NA, value = theta, type = "theta") %>%
  bind_rows(
    b %>% enframe() %>% mutate(type = "b", name = row_number())
  ) %>%
  mutate(bin =  value) %>%
  ggplot() +
  # geom_histogram(aes(y = bin), data = . %>% filter(type == "theta"), binwidth = .5)+
  geom_bar(aes(y = bin), data = . %>% filter(type == "theta"), stat = "bin", binwidth = .5) +
  # geom_label(aes(row, label = name), . %>% filter(type == "b") %>% mutate(row = 1)) +
  geom_text(aes(y = x, x = 1- stackpos/ max(stackpos), label = 1:100), b_data) +
    # scale_y_binned(breaks = scales::breaks_width(.5)) +
  scale_x_reverse() +
  scale_y_continuous(breaks = scales::breaks_width(1))+
  facet_wrap(~type %>% fct_rev, scales = "free_x") +theme_minimal()

p %>% ggplot_build() %>% ggplot_gtable()

b %>% enframe %>% ggplot(aes(y =value)) + geom_point(stat="bin")



p %>% ggplot_build() %>% pluck("data")


WrightMap::wrightMap(theta, b)
# -------------------------------------------------------------------------

geom_textplot <- function(mapping = NULL, data = NULL,
                         position = "identity",
                         ...,
                         binwidth = NULL,
                         binaxis = "x",
                         method = "histodot",
                         binpositions = "bygroup",
                         stackdir = "up",
                         stackratio = 1,
                         dotsize = 1,
                         stackgroups = FALSE,
                         origin = NULL,
                         right = TRUE,
                         width = 0.9,
                         drop = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  # If identical(position, "stack") or position is position_stack(), tell them
  # to use stackgroups=TRUE instead. Need to use identical() instead of ==,
  # because == will fail if object is position_stack() or position_dodge()
  if (!is.null(position) &&
      (identical(position, "stack") || (inherits(position, "PositionStack"))))
    message("position=\"stack\" doesn't work properly with geom_dotplot. Use stackgroups=TRUE instead.")

  if (stackgroups && method == "dotdensity" && binpositions == "bygroup")
    message('geom_dotplot called with stackgroups=TRUE and method="dotdensity". You probably want to set binpositions="all"')

  layer(
    data = data,
    mapping = mapping,
    stat = StatBindot,
    geom = GeomTextPlot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # Need to make sure that the binaxis goes to both the stat and the geom
    params = list(
      binaxis = binaxis,
      binwidth = binwidth,
      binpositions = binpositions,
      method = method,
      origin = origin,
      right = right,
      width = width,
      drop = drop,
      stackdir = stackdir,
      stackratio = stackratio,
      dotsize = dotsize,
      stackgroups = stackgroups,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextPlot <- ggproto("GeomTextPlot", Geom,
                       required_aes = c("x", "y", "label"),
                       non_missing_aes = c("size", "shape"),

                       default_aes = aes(colour = "black", fill = "black", alpha = NA, stroke = 1, linetype = "solid"),

                       setup_data = function(data, params) {
                         data$width <- data$width %||%
                           params$width %||% (resolution(data$x, FALSE) * 0.9)

                         # Set up the stacking function and range
                         if (is.null(params$stackdir) || params$stackdir == "up") {
                           stackdots <- function(a)  a - .5
                           stackaxismin <- 0
                           stackaxismax <- 1
                         } else if (params$stackdir == "down") {
                           stackdots <- function(a) -a + .5
                           stackaxismin <- -1
                           stackaxismax <- 0
                         } else if (params$stackdir == "center") {
                           stackdots <- function(a)  a - 1 - max(a - 1) / 2
                           stackaxismin <- -.5
                           stackaxismax <- .5
                         } else if (params$stackdir == "centerwhole") {
                           stackdots <- function(a)  a - 1 - floor(max(a - 1) / 2)
                           stackaxismin <- -.5
                           stackaxismax <- .5
                         }


                         # Fill the bins: at a given x (or y), if count=3, make 3 entries at that x
                         data <- data[rep(1:nrow(data), data$count), ]

                         # Next part will set the position of each dot within each stack
                         # If stackgroups=TRUE, split only on x (or y) and panel; if not stacking, also split by group
                         plyvars <- params$binaxis %||% "x"
                         plyvars <- c(plyvars, "PANEL")
                         if (is.null(params$stackgroups) || !params$stackgroups)
                           plyvars <- c(plyvars, "group")

                         # Within each x, or x+group, set countidx=1,2,3, and set stackpos according to stack function
                         data <- dapply(data, plyvars, function(xx) {
                           xx$countidx <- 1:nrow(xx)
                           xx$stackpos <- stackdots(xx$countidx)
                           xx
                         })


                         # Set the bounding boxes for the dots
                         if (is.null(params$binaxis) || params$binaxis == "x") {
                           # ymin, ymax, xmin, and xmax define the bounding rectangle for each stack
                           # Can't do bounding box per dot, because y position isn't real.
                           # After position code is rewritten, each dot should have its own bounding box.
                           data$xmin <- data$x - data$binwidth / 2
                           data$xmax <- data$x + data$binwidth / 2
                           data$ymin <- stackaxismin
                           data$ymax <- stackaxismax
                           data$y    <- 0

                         } else if (params$binaxis == "y") {
                           # ymin, ymax, xmin, and xmax define the bounding rectangle for each stack
                           # Can't do bounding box per dot, because x position isn't real.
                           # xmin and xmax aren't really the x bounds, because of the odd way the grob
                           # works. They're just set to the standard x +- width/2 so that dot clusters
                           # can be dodged like other geoms.
                           # After position code is rewritten, each dot should have its own bounding box.
                           data <- dapply(data, c("group", "PANEL"), transform,
                                          ymin = min(y) - binwidth[1] / 2,
                                          ymax = max(y) + binwidth[1] / 2)

                           data$xmin <- data$x + data$width * stackaxismin
                           data$xmax <- data$x + data$width * stackaxismax
                           # Unlike with y above, don't change x because it will cause problems with dodging
                         }
                         data
                       },


                       draw_group = function(data, panel_params, coord, na.rm = FALSE,
                                             binaxis = "x", stackdir = "up", stackratio = 1,
                                             dotsize = 1, stackgroups = FALSE) {

                         if (!coord$is_linear()) {
                           warn("geom_dotplot does not work properly with non-linear coordinates.")
                         }

                         tdata <- coord$transform(data, panel_params)

                         # Swap axes if using coord_flip
                         if (inherits(coord, "CoordFlip"))
                           binaxis <- ifelse(binaxis == "x", "y", "x")

                         if (binaxis == "x") {
                           stackaxis = "y"
                           dotdianpc <- dotsize * tdata$binwidth[1] / (max(panel_params$x.range) - min(panel_params$x.range))

                         } else if (binaxis == "y") {
                           stackaxis = "x"
                           dotdianpc <- dotsize * tdata$binwidth[1] / (max(panel_params$y.range) - min(panel_params$y.range))
                         }

                         ggname("geom_textplot",
                                textstackGrob(stackaxis = stackaxis, x = tdata$x, y = tdata$y, dotdia = dotdianpc,
                                             stackposition = tdata$stackpos, stackratio = stackratio,
                                             default.units = "npc",
                                             gp = gpar(col = alpha(tdata$colour, tdata$alpha),
                                                       fill = alpha(tdata$fill, tdata$alpha),
                                                       lwd = tdata$stroke, lty = tdata$linetype))
                         )
                       },

                       draw_key = draw_key_text
)


textstackGrob <- function(
  x = unit(0.5, "npc"),     # x pos of the dotstack's origin
  y = unit(0.5, "npc"),     # y pos of the dotstack's origin
  stackaxis = "y",
  dotdia = unit(1, "npc"),  # Dot diameter in the non-stack axis, should be in npc
  stackposition = 0,        # Position of each dot in the stack, relative to origin
  stackratio = 1,           # Stacking height of dots (.75 means 25% dot overlap)
  default.units = "npc", name = NULL, gp = gpar(), vp = NULL)
{
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(dotdia))
    dotdia <- unit(dotdia, default.units)
  if (!is_npc(dotdia))
    warn("Unit type of dotdia should be 'npc'")

  grob(x = x, y = y, stackaxis = stackaxis, dotdia = dotdia,
       stackposition = stackposition, stackratio = stackratio,
       name = name, gp = gp, vp = vp, cl = "textstackGrob")
}
# Only cross-version reliable way to check the unit of a unit object
is_npc <- function(x) isTRUE(grepl('^[^+^-^\\*]*[^s]npc$', as.character(x)))

#' @export
makeContext.textstackGrob <- function(x, recording = TRUE) {
  # Need absolute coordinates because when using npc coords with circleGrob,
  # the radius is in the _smaller_ of the two axes. We need the radius
  # to instead be defined in terms of the non-stack axis.
  xmm <- convertX(x$x, "mm", valueOnly = TRUE)
  ymm <- convertY(x$y, "mm", valueOnly = TRUE)

  if (x$stackaxis == "x") {
    dotdiamm <- convertY(x$dotdia, "mm", valueOnly = TRUE)
    xpos <- xmm + dotdiamm * (x$stackposition * x$stackratio + (1 - x$stackratio) / 2)
    ypos <- ymm
  } else if (x$stackaxis == "y") {
    dotdiamm <- convertX(x$dotdia, "mm", valueOnly = TRUE)
    xpos <- xmm
    ypos <- ymm + dotdiamm * (x$stackposition * x$stackratio + (1 - x$stackratio) / 2)
  }

  textGrob(
    x = xpos, y = ypos, r = dotdiamm / 2, default.units = "mm",
    name = x$name, gp = x$gp, vp = x$vp
  )
}


# -------------------------------------------------------------------------

# ymm + dotdiamm * (x$stackposition * x$stackratio + (1 - x$stackratio) / 2)


(b %>%
  enframe() %>%
  # mutate(row = dplyr::row_number(), bin = cut_width(b, .5, boundary = 1)) %>%
  ggplot(aes(value)) +
  geom_dotplot(method = "histodot", binwidth = .5 ) ) %>%
  ggplot_build() %>%
  pluck("data", 1) %>%
  ggplot(aes(x, y = stackpos/ max(stackpos))) +
  geom_text(aes(label = 1:100))

plt + coord_flip()

b %>% enframe %>%
  mutate(row = dplyr::row_number(), bin = cut_width(b, .5, boundary = 1)) %>%
  ggplot() +
  stat_(aes(b), label = "safd", geom = "label")




# -------------------------------------------------------------------------


library(grid)

geom_mypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyPoint, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomMyPoint <- ggproto("GeomMyPoint", Geom,
                       required_aes = c("x", "y", "label"),
                       default_aes = aes(shape = 1),
                       draw_key = draw_key_text,
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)

                         ## Let's print out the structure of the 'coords' object
                         str(coords)

                         ## Construct a grid grob
                         textGrob(
                           coords$label,
                           x = coords$x,
                           y = coords$y
                         )



                       })

b %>% enframe %>% mutate(row = row_number()) %>%
  ggplot(aes(row, value, label = name)) + geom_mypoint()



# new safer approach using flatten item labs ------------------------------

b %>%
  enframe %>%
  mutate(idx = row_number(), bin = cut_width(value, .5)) %>%
  group_by(bin) %>%
  summarise(idx = str_flatten(idx, collapse = " | "),
            name = str_flatten(name, collapse = " | ")) %>%
  ggplot(aes(label = name, bin, x = 0)) +
  geom_text(hjust = 0)



tibble(theta) %>%
  mutate(idx = row_number(), bin = cut_width(theta, .5)) %>%
  ggplot(aes(bin)) +
  geom_bar(width = 1)

difficulties <- b %>%
  enframe() %>%
  mutate(type = "b", idx = row_number())

abilities <- tibble(name = NA, value = theta, type = "theta")

bind_rows(abilities, difficulties) %>%
  mutate(bin = cut_width(value, .5)) %>%
  ggplot(aes(y = bin)) +
  geom_bar(aes(x = -1 * ..count..),
    position = position_nudge(x = 10), width = 1,
    data = . %>% filter(type == "theta") %>% mutate(bin = bin %>% as.numeric())
  ) +
  geom_text(aes(x = 0, label = name),
    hjust = 0, position = position_nudge(x = -10),
    data = . %>% filter(type == "b") %>% group_by(bin) %>% summarise(name = str_flatten(idx, " | ")) %>% mutate(type = "b") %>% mutate(bin = bin %>% as.numeric())
  ) +
  scale_x_reverse() +
  coord_cartesian(xlim = c(-500, 500))
# facet_grid(~type %>% fct_rev, scales = "free_y")


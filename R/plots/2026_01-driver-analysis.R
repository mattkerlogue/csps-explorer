library(ggplot2)

# ee framework ------

framework_df <- tibble::tribble(
  ~y , ~rect_x , ~text_x , ~hjust , ~width , ~height , ~class   , ~content                                ,
   1 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "My work"                               ,
   2 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "Organisational objectives and purpose" ,
   3 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "My manager"                            ,
   4 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "My team"                               ,
   5 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "Learning and development"              ,
   6 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "Inclusion and fair treatment"          ,
   7 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "Resources and workload"                ,
   8 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "Pay and benefits"                      ,
   9 , -2.25   , -0.2    , 1      , 4.5    , 0.75    , "driver" , "Leadership and managing change"        ,
   5 ,  2      ,  2      , 0.5    , 2      , 2       , "ee"     , "Employee\nengagement"                  ,
   2 ,  5      ,  5      , 0.5    , 2      , 2       , "out"    , "Organisational\nperformance"           ,
   8 ,  5      ,  5      , 0.5    , 2      , 2       , "out"    , "Personal\nwellbeing"
)

framework_arrows <- tibble::tribble(
  ~x  , ~y , ~group , ~class   ,
  0   ,  1 , "d1"   , "driver" ,
  0.9 ,  5 , "d1"   , "driver" ,
  0   ,  2 , "d2"   , "driver" ,
  0.9 ,  5 , "d2"   , "driver" ,
  0   ,  3 , "d3"   , "driver" ,
  0.9 ,  5 , "d3"   , "driver" ,
  0   ,  4 , "d4"   , "driver" ,
  0.9 ,  5 , "d4"   , "driver" ,
  0   ,  5 , "d5"   , "driver" ,
  0.9 ,  5 , "d5"   , "driver" ,
  0   ,  6 , "d6"   , "driver" ,
  0.9 ,  5 , "d6"   , "driver" ,
  0   ,  7 , "d7"   , "driver" ,
  0.9 ,  5 , "d7"   , "driver" ,
  0   ,  8 , "d8"   , "driver" ,
  0.9 ,  5 , "d8"   , "driver" ,
  0   ,  9 , "d9"   , "driver" ,
  0.9 ,  5 , "d9"   , "driver" ,
  3   ,  5 , "op"   , "ee"     ,
  3.9 ,  2 , "op"   , "ee"     ,
  3   ,  5 , "pw"   , "ee"     ,
  3.9 ,  8 , "pw"   , "ee"
)

framework_title_labels <- tibble::tribble(
  ~text_x , ~hjust , ~content                                                            ,
  -4.25   , 0      , "By taking action to\nimprove our people's\nexperiences of work..." ,
   1      , 0.5    , "...we increase levels\nof employee\nengagement..."                 ,
   5.75   , 1      , "...which raises\nperformance and\nenhances wellbeing."
)

framework_title_shapes <- tibble::tribble(
  ~x    , ~y , ~shape_id     ,
  -4.5  , -2 , "left_arrow"  ,
  -1.5  , -2 , "left_arrow"  ,
  -1    , -1 , "left_arrow"  ,
  -1.5  ,  0 , "left_arrow"  ,
  -4.5  ,  0 , "left_arrow"  ,
  -0.75 , -2 , "mid_arrow"   ,
   2.25 , -2 , "mid_arrow"   ,
   2.75 , -1 , "mid_arrow"   ,
   2.25 ,  0 , "mid_arrow"   ,
  -0.75 ,  0 , "mid_arrow"   ,
  -0.25 , -1 , "mid_arrow"   ,
   6    , -2 , "right_arrow" ,
   6    ,  0 , "right_arrow" ,
   3    ,  0 , "right_arrow" ,
   3.5  , -1 , "right_arrow" ,
   3    , -2 , "right_arrow"
)


framework_plot <- ggplot(framework_df, aes(y = y)) +
  geom_polygon(
    data = framework_title_shapes,
    mapping = aes(x = x, y = y, group = shape_id),
    fill = "#e5e0df",
    colour = NA
  ) +
  geom_rect(aes(x = rect_x, width = width, height = height, fill = class)) +
  geom_text(
    data = framework_title_labels,
    mapping = aes(x = text_x, y = -1, label = content, hjust = hjust),
    family = "IBM Plex Sans",
    size = 4,
    color = "#393939"
  ) +
  geom_step(
    data = framework_arrows,
    mapping = aes(x = x, y = y, group = group, color = class),
    direction = "mid",
    arrow = arrow(length = unit(12, "pt")),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "driver" = "#ffafd2",
      "ee" = "#d02670",
      "out" = "#740937"
    )
  ) +
  scale_fill_manual(
    values = c(
      "driver" = "#ffafd2",
      "ee" = "#d02670",
      "out" = "#740937"
    )
  ) +
  ggnewscale::new_scale_colour() +
  geom_text(
    aes(x = text_x, hjust = hjust, label = content, color = class),
    family = "IBM Plex Sans",
    size = 4,
  ) +
  scale_colour_manual(
    values = c(
      "driver" = "#393939",
      "ee" = "#ffffff",
      "out" = "#ffffff"
    )
  ) +
  scale_y_reverse(
    limits = c(10, -2),
    breaks = seq(-2, 9, 1)
  ) +
  scale_x_continuous(
    breaks = seq(-5, 5, 1)
  ) +
  # theme_minimal() +
  theme_void() +
  theme(
    legend.position = "none"
  )

inlinesvg::svg_plot(
  framework_plot,
  path = "partials/plots/ee_framework.svg",
  width = 800,
  height = 500,
  web_fonts = svglite::fonts_as_import("IBM Plex Sans")
)

# theme vs factor scores ------

ts_fa_circles <- tibble::tribble(
  ~x , ~y , ~class          , ~content                                  ,
   1 ,  1 , "theme scores"  , "My\nwork"                                ,
   2 ,  1 , "theme scores"  , "Organisational\nobjectives\nand purpose" ,
   3 ,  1 , "theme scores"  , "My\nmanager"                             ,
   1 ,  2 , "theme scores"  , "My\nteam"                                ,
   2 ,  2 , "theme scores"  , "Learning and\ndevelopment"               ,
   3 ,  2 , "theme scores"  , "Inclusion\nand fair\ntreatment"          ,
   1 ,  3 , "theme scores"  , "Resources\nand workload"                 ,
   2 ,  3 , "theme scores"  , "Pay\nbenefits"                           ,
   3 ,  3 , "theme scores"  , "Leadership\nand managing\nchange"        ,
   1 ,  1 , "factor scores" , "My\nwork"                                ,
   2 ,  1 , "factor scores" , "Organisational\nobjectives\nand purpose" ,
   3 ,  1 , "factor scores" , "My\nmanager"                             ,
   1 ,  2 , "factor scores" , "My\nteam"                                ,
   2 ,  2 , "factor scores" , "Learning and\ndevelopment"               ,
   3 ,  2 , "factor scores" , "Inclusion\nand fair\ntreatment"          ,
   1 ,  3 , "factor scores" , "Resources\nand workload"                 ,
   2 ,  3 , "factor scores" , "Pay\nbenefits"                           ,
   3 ,  3 , "factor scores" , "Leadership\nand managing\nchange"
) |>
  dplyr::mutate(
    class = factor(class, levels = c("theme scores", "factor scores"))
  )

ts_fa_plot <- ggplot(ts_fa_circles, aes(x = x, y = y)) +
  geom_point(
    aes(colour = content, alpha = class),
    stroke = 0,
    size = 30,
    alpha = 0.35
  ) +
  geom_point(
    aes(colour = content, size = class),
    stroke = 0,
    alpha = 0.3
  ) +
  geom_text(
    aes(label = content),
    size = 3,
    family = "IBM Plex Sans",
    colour = "#393939"
  ) +
  scale_size_manual(
    values = c(
      "theme scores" = 55,
      "factor scores" = 30
    )
  ) +
  # scale_alpha_manual(
  #   values = c(
  #     "theme scores" = 0.4,
  #     "factor scores" = 0.6
  #   )
  # ) +
  scale_y_reverse(
    limits = c(4, 0),
    oob = scales::oob_keep,
    expand = expansion()
  ) +
  scale_x_continuous(
    limits = c(0, 4),
    oob = scales::oob_keep,
    expand = expansion()
  ) +
  coord_fixed() +
  facet_wrap(
    vars(class),
    nrow = 1,
    labeller = as_labeller(stringr::str_to_title)
  ) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    text = element_text(colour = "#393939"),
    strip.text = element_text(size = 11),
    legend.position = "none"
  )

inlinesvg::svg_plot(
  ts_fa_plot,
  path = "partials/plots/ts_fa_compare.svg",
  width = 850,
  height = 425,
  web_fonts = svglite::fonts_as_import("IBM Plex Sans")
)

drivers_framework <- tibble::tribble(
  ~y  , ~rect_x , ~text_x , ~hjust , ~width , ~height , ~class   , ~content                                ,
  1   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "Leadership and managing change"        ,
  2   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "My work"                               ,
  3   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "My manager"                            ,
  4   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "Inclusion and fair treatment"          ,
  5   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "My team"                               ,
  6   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "Learning and development"              ,
  7   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "Resources and workload"                ,
  8   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "Organisational objectives and purpose" ,
  9   , -2.5    , -0.2    , 1      , 5      , 0.75    , "theme"  , "Pay and benefits"                      ,
  1.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Leadership"                            ,
  2.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Work"                                  ,
  3.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Management"                            ,
  4.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Teamwork"                              ,
  5.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Career"                                ,
  6.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Workload"                              ,
  7.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Objectives"                            ,
  8.5 ,  3.5    ,  2.7    , 0      , 2      , 0.75    , "factor" , "Pay"                                   ,
  5   ,  7.75   ,  7.75   , 0.5    , 2.5    , 2.5     , "ee"     , "Employee\nengagement"
)

factor_arrows <- tibble::tribble(
  ~x  , ~y   , ~xend , ~yend , ~size , ~group     , ~class  ,
  0.1 , 0.9  , 2.4   , 1.35  , 0.8   , "lc-f2_le" , "theme" ,
  0.1 , 3.85 , 2.4   , 1.5   , 0.4   , "if-f2_le" , "theme" ,
  0.1 , 6.9  , 2.4   , 1.65  , 0.4   , "rw-f2_le" , "theme" ,
  0.1 , 7.9  , 2.4   , 1.75  , 0.4   , "op-f2_le" , "theme" ,
  0.1 , 1.9  , 2.4   , 2.35  , 0.7   , "mw-f3_wk" , "theme" ,
  0.1 , 3.95 , 2.4   , 2.5   , 0.4   , "if-f3_wk" , "theme" ,
  0.1 , 5.85 , 2.4   , 2.65  , 0.4   , "ld-f3_wk" , "theme" ,
  0.1 , 1.1  , 2.4   , 3.25  , 0.4   , "lc-f1_mg" , "theme" ,
  0.1 , 2.1  , 2.4   , 3.35  , 0.5   , "mw-f1_mg" , "theme" ,
  0.1 , 3    , 2.4   , 3.45  , 0.8   , "lm-f1_mg" , "theme" ,
  0.1 , 4.05 , 2.4   , 3.55  , 0.7   , "if-f1_mg" , "theme" ,
  0.1 , 4.9  , 2.4   , 3.65  , 0.7   , "mt-f1_mg" , "theme" ,
  0.1 , 5.95 , 2.4   , 3.75  , 0.4   , "ld-f1_mg" , "theme" ,
  0.1 , 5.1  , 2.4   , 4.5   , 0.5   , "mt-f8_tw" , "theme" ,
  0.1 , 6.15 , 2.4   , 5.5   , 0.6   , "ld-f7_cr" , "theme" ,
  0.1 , 4.15 , 2.4   , 6.35  , 0.4   , "if-f4_wl" , "theme" ,
  0.1 , 6.05 , 2.4   , 6.5   , 0.4   , "ld-f4_wl" , "theme" ,
  0.1 , 7.1  , 2.4   , 6.65  , 0.7   , "rw-f4_wl" , "theme" ,
  0.1 , 8.1  , 2.4   , 7.5   , 0.8   , "op-f6_ob" , "theme" ,
  0.1 , 9    , 2.4   , 8.5   , 1     , "pb-f5_py" , "theme"
) |>
  dplyr::arrange(size)

driver_arrows <- tibble::tribble(
  ~x  , ~y  , ~xend , ~yend , ~size , ~group     , ~class   ,
  4.5 , 1.5 , 6.4   , 3.9   , 0.52  , "f2_le-ee" , "factor" ,
  4.5 , 2.5 , 6.4   , 4.21  , 0.49  , "f3_wk-ee" , "factor" ,
  4.5 , 3.5 , 6.4   , 4.53  , 0.34  , "f1_mg-ee" , "factor" ,
  4.5 , 4.5 , 6.4   , 4.84  , 0.08  , "f8_tw-ee" , "factor" ,
  4.5 , 5.5 , 6.4   , 5.16  , 0.15  , "f7_cr-ee" , "factor" ,
  4.5 , 6.5 , 6.4   , 5.47  , 0.26  , "f4_wl-ee" , "factor" ,
  4.5 , 7.5 , 6.4   , 5.79  , 0.24  , "f6_ob-ee" , "factor" ,
  4.5 , 8.5 , 6.4   , 6.1   , 0.16  , "f9_py-ee" , "factor" ,
) |>
  dplyr::arrange(size)

drivers_framework_plot <- ggplot(drivers_framework, aes(y = y)) +
  geom_rect(aes(x = rect_x, width = width, height = height, fill = class)) +
  geom_segment(
    data = factor_arrows,
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      group = group,
      linewidth = size,
      color = paste(class, size > 0.5, sep = "_")
    ),
    linejoin = "mitre",
    arrow = arrow(length = unit(6, "pt"), ends = "both", type = "closed")
  ) +
  scale_linewidth(
    range = c(0.2, 2.25),
    guide = guide_none()
  ) +
  scale_colour_manual(
    values = c(
      "theme_TRUE" = "#ffafd2",
      "theme_FALSE" = "#a8a8a8",
      "factor" = "#82cfff"
    ),
    breaks = c("theme_TRUE", "theme_FALSE", "factor"),
    labels = c(
      "theme_TRUE" = "Dominant relationships between\nthemes and drivers",
      "theme_FALSE" = "Other important relationships\nbetween themes and drivers",
      "factor" = "Impact of drivers\non employee engagement"
    ),
    guide = guide_legend(title = "arrows", position = "bottom", order = 2)
  ) +
  scale_fill_manual(
    values = c(
      "theme" = "#ffafd2",
      "factor" = "#82cfff",
      "ee" = "#d02670"
    ),
    breaks = c("theme", "factor", "ee"),
    labels = c(
      "theme" = "Survey themes",
      "factor" = "Engagement drivers",
      "ee" = "Engagement index"
    ),
    guide = guide_legend(title = "blocks", position = "bottom", order = 1)
  ) +
  ggnewscale::new_scale("linewidth") +
  geom_segment(
    data = driver_arrows,
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      group = group,
      linewidth = size,
      colour = class
    ),
    linejoin = "mitre",
    lineend = "round",
    arrow = arrow(length = unit(6, "pt"), type = "closed")
  ) +
  scale_linewidth(
    range = c(0.75, 2.5),
    guide = guide_none()
  ) +
  ggnewscale::new_scale_colour() +
  geom_text(
    aes(x = text_x, hjust = hjust, label = content, color = class),
    family = "IBM Plex Sans",
    size = 4,
  ) +
  scale_colour_manual(
    values = c(
      "theme" = "#393939",
      "factor" = "#393939",
      "ee" = "#ffffff"
    ),
    guide = guide_none()
  ) +
  scale_y_reverse(
    breaks = seq(10, 0, -0.5),
    expand = expansion(add = 0.25)
  ) +
  scale_x_continuous(
    breaks = seq(-5, 7, 0.5),
    expand = expansion(add = 0.25)
  ) +
  # theme_minimal() +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    plot.margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt"),
    legend.text = element_text(
      colour = "#393939",
      size = 10,
      margin = margin(l = 3, r = 6, unit = "pt")
    ),
    legend.title = element_blank(),
    legend.spacing.y = unit(12, "pt"),
    legend.key.spacing = unit(6, "pt"),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.box.margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt")
  )

inlinesvg::svg_plot(
  drivers_framework_plot,
  path = "partials/plots/ee_drivers_framework.svg",
  width = 900,
  height = 600,
  web_fonts = svglite::fonts_as_import("IBM Plex Sans")
)

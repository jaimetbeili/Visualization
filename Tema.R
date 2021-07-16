library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

p <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_boxplot(size = 5) +
  labs(title = "Grafica Chingarretina", subtitle = "Colores horribles",
       x = "X", y = "Y", tag = "A)", caption = "Realmente terrible") +
  facet_grid(x~y)
p

#PANEL BACKGROUND CHANGES
p + theme(panel.background = element_rect(fill = "mediumseagreen",
                                   color = "black", size = 3, linetype = 3))

#MAJOR GRID LINES CHANGES
p + theme(panel.grid.major = element_line(color = "black", size = 1.25,
                                    linetype = "solid", lineend = "round"))

#MINOR GRID LINE CHANGES          
p + theme(panel.grid.minor = element_line(color = "red", size = 1,
                                          linetype = "dashed", lineend = "butt"))
#AXIS LINE CHANGES
p + theme(axis.line = element_line(size = 3.25, linetype = "solid",
                                   colour = "yellow"))
#PLOT AND/OR LEGEND BACKGROUND CHANGES (ONLY FILL COLOR)
p + theme(plot.background = element_rect(fill = "navyblue"))
p + theme(legend.background = element_rect(fill = "navyblue"))
p + theme(plot.background = element_rect(fill = "navyblue"),
          legend.background = element_rect(fill = "navyblue"))

#PANEL BG + GRID LN + AXIS LN + PLOT BG + LEGND BG CHANGES
p + theme(panel.background = element_rect(fill = "mediumseagreen",
                                   color = "black", size = 3, linetype = 3),
         panel.grid.major = element_line(color = "black", size = 1.25,
                                         linetype = "solid", lineend = "round"),
         panel.grid.minor = element_line(color = "red", size = 1,
                                         linetype = "dashed", lineend = "butt"),
         axis.line = element_line(size = 3.25, linetype = "solid",
                                         colour = "yellow"),
         plot.background = element_rect(fill = "navyblue"),
         legend.background = element_rect(fill = "navyblue"))

#TO HIDE THE PANEL BORDER AND/OR THE GRID LINES
p + theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

grays <- c("gray30", "gray40", "gray50", "gray60", "gray70", "gray80", "gray90", "gray100")

#CREATING AN UGLY THEME
ugly_theme <- theme_gray() +
  theme(
#General
    text = element_text(
      family = "serif"),
    plot.background = element_rect(
      fill = "navyblue"),
    plot.title = element_text(
      color = "red", size = 25, face = "bold"),
    plot.subtitle = element_text(
      color = "pink", size = 15, face = "bold"),
    plot.tag = element_text(
      color = "brown", size = 18, face = "bold"),
    plot.caption = element_text(
      color = "blue", size = 10, face = "italic"),
#Panel
    panel.background = element_rect(
      fill = "mediumseagreen", color = "black", size = 3, linetype = 3),
    panel.grid.major = element_line(
      color = "black", size = 1.25, linetype = "solid", lineend = "round"),
    panel.grid.minor = element_line(
      color = "red3", size = 1, linetype = "dashed", lineend = "butt"),
#Axis
    axis.line = element_line(
      size = 3.25, linetype = "solid", colour = "yellow"),
    axis.ticks = element_line(
      color = "purple", size = 3),
    axis.ticks.length.x = unit(
      -.25, "cm"),
    axis.ticks.length.y = unit(
      .25,"cm"),
    axis.title.x = element_text(
      color = "orange", size = 12),
    axis.title.y = element_text(
      color = "salmon", size = 12),
    axis.text.x = element_text(
      margin = margin(t = .5, unit = "cm"), color = "salmon", size = 10),
    axis.text.y = element_text(
      margin = margin(t = .5, unit = "cm"), color = "orange", size = 10),
#Legend
    legend.background = element_rect(
      fill = "navyblue"),
    legend.justification = "right", legend.position = "top",
    legend.margin = margin (1, 1, 1, 1),
    legend.box.background = element_rect(
      color = "palevioletred", size = 4),
    legend.box.margin = margin(2, 2, 2, 2),
    legend.key = element_rect(
      color = "white"),
    legend.text = element_text(
      size = 10, color = "magenta"),
    legend.title = element_text(
      size = 12, color = "green", face = "bold"),
#Strip
    strip.text = element_text(
      face = "bold", size = 15),
    strip.text.x = element_text(
      color = "indianred3"),
    strip.text.y = element_text(
      color = "lightblue"),
    strip.background.x = element_rect(
      color = "black", fill = "lightblue"),
    strip.background.y = element_rect(
      color = "black", fill = "indianred3"), 
        )

p + ugly_theme

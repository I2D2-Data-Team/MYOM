# library(shiny)
# library(shinyBS)
# library(shinythemes)
# library(shinydashboardPlus)
# library(data.table)
library(tidyverse)
# library(sf)

# df <- fread("data/sample_data.csv") %>% tibble()
# 
# a <-
#   df %>% mutate(
#     across(.cols = c(integer, nominal, `dichotomous number`, `dichotomous character`), 
#            .fns = as.factor)
#   )
# 
# levels(a$integer)
# levels(a$`dichotomous number`)
# levels(a$`dichotomous character`)



color_palette <-
  list(
    yellow_grn = c("#fff999", "#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"),
    yellow_red = RColorBrewer::brewer.pal(7, "YlOrRd"),
    purple_red = RColorBrewer::brewer.pal(7, "RdPu"),
    blu_purple = RColorBrewer::brewer.pal(7, "BuPu"),
    yellow_blu = c(RColorBrewer::brewer.pal(11, "Spectral")[6:11], "darkslateblue"),
    yellow_pur = viridis::magma(18)[seq(17, 5, -2)],
    viridius = viridis::viridis(13)[seq(13, 1, -2)],
    i2d2 = c("#252C6A", "#0097CE", "#66c0e1", "#76777A", "#c299cc", "#863399", "#CF202E")
  )

col_names <- 
  tibble("yellow_grn" = "Yellow to Green",
         "yellow_red" = "Yellow to Red",
         "purple_red" = "Purple to Red",
         "blu_purple" = "Blue to Purple",
         "yellow_blu" = "Yellow to Blue",
         "yellow_pur" = "Yellow to Purple",
         "viridius" = "Viridius Colors",
         "i2d2" = "I2D2")

library(unikn)
unikn::seecol(color_palette, 
              col_brd = "white", lwd_brd = 8,
              title = "Maping Color Palettes", 
              pal_names = col_names[names(color_palette)],
              grid = FALSE)




hcl_col_names <- 
  c("Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", 
    "Purple-Green", "Purple-Brown", "Green-Brown", "Blue-Yellow 2", 
    "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta", 
    "Tropic", "Broc", "Cork", "Vik", "Berlin", "Lisbon", "Tofino") 

unikn::seecol(list(hcl.colors(7, "Berlin"),
                   hcl.colors(7, "Broc"),
                   hcl.colors(7, "Lisbon"),
                   hcl.colors(7, "Tofino"),
                   hcl.colors(7, "Tropic"),
                   hcl.colors(7, "Vik"),
                   hcl.colors(7, "Fall")), 
              col_brd = "white", lwd_brd = 8,
              main = "Maping Color Palettes",
              pal_names = c("Berlin", "Fall"), 
              grid = FALSE)


library(tidyverse)
library(pals)


for (i in 1:length(color_palette)) print(color_palette[[i]])
pals::pal.bands(c(for (i in 1:length(color_palette)) print(color_palette[[i]])))





my_colors <-
  col_names %>%
  gather(name, key) %>% 
  left_join(as.data.frame(color_palette) %>% gather(key, value)) %>%
  group_by(name, key) %>%
  nest() 

labs <- my_colors$name
pals::pal.bands(
  unlist(my_colors[1,3]),
  unlist(my_colors[2,3]),
  unlist(my_colors[3,3]),
  unlist(my_colors[4,3]),
  unlist(my_colors[5,3]),
  unlist(my_colors[6,3]),
  unlist(my_colors[7,3]),
  unlist(my_colors[8,3]),
  labels = labs, border = "black",
  gap = 0.2,
  show.names = FALSE
)

barplot(rep(1, 7), col= color_palette[[1]])




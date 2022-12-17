library(tidyverse)
library(unikn)

# Set of color palettes
color_palette <-
  list(
    yellow_blu = c(RColorBrewer::brewer.pal(11, "Spectral")[6:11], "darkslateblue"),
    yellow_grn = c("#fff999", "#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"),
    yellow_red = RColorBrewer::brewer.pal(7, "YlOrRd"),
    # yellow_pur = viridis::magma(18)[seq(17, 5, -2)],
    yellow_pur = viridis::magma(20)[c(20, seq(17, 6, -2))],
    red_purple = RColorBrewer::brewer.pal(7, "RdPu"),
    blu_purple = RColorBrewer::brewer.pal(7, "BuPu"),
    viridius = viridis::viridis(13)[seq(13, 1, -2)],
    i2d2 = c("#252C6A", "#0097CE", "#66c0e1", "#76777A", "#c299cc", "#863399", "#CF202E")
  )

# Names of color palettes
col_names <- 
  tibble(
    "yellow_blu" = "Yellow to Blue",
    "yellow_grn" = "Yellow to Green",
    "yellow_red" = "Yellow to Red",
    "yellow_pur" = "Yellow to Purple",
    "blu_purple" = "Blue to Purple",
    "red_purple" = "Red to Purple",
    "viridius" = "Viridius",
    "i2d2" = "I2D2")

# Plot color palettes
png("figs/color_palette.png", width = 600, height = 800, res = 96)
unikn::seecol(color_palette, 
              col_brd = "white", lwd_brd = 8,
              title = "Mapping Color Palettes", 
              pal_names = col_names[names(color_palette)],
              grid = FALSE)
dev.off()



# Plot Palette Using ggplot2 ----------------------------------------------

color_palette <-
  list(
    `Yellow to Blue` = c(RColorBrewer::brewer.pal(11, "Spectral")[6:11], "darkslateblue"),
    `Yellow to Green` = c("#fff999", "#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"),
    `Yellow to Red` = RColorBrewer::brewer.pal(7, "YlOrRd"),
    `Yellow to Purple` = viridis::magma(20)[c(20, seq(17, 6, -2))],
    `Red to Purple` = RColorBrewer::brewer.pal(7, "RdPu"),
    `Blue to Purple` = RColorBrewer::brewer.pal(7, "BuPu"),
    `Viridius` = viridis::viridis(13)[seq(13, 1, -2)],
    `I2D2` = c("#252C6A", "#0097CE", "#66c0e1", "#76777A", "#c299cc", "#863399", "#CF202E")
  )

data <- 
  tibble(Value = 1,
         Order = rep(1:7, 8),
         Palette = rep(names(color_palette), each = 7),
         Color = flatten_chr(color_palette))

data %>%
  mutate(Palette = 
           factor(Palette,
                  levels = c(
                    "I2D2",
                    "Blue to Purple",
                    "Red to Purple",
                    "Viridius",
                    "Yellow to Blue",
                    "Yellow to Green",
                    "Yellow to Purple",
                    "Yellow to Red"
                    ))) %>%
  ggplot(aes(x = Order, y = Value)) +
  geom_col(aes(fill = I(Color))) +
  facet_grid(fct_rev(Palette) ~ .) +
  theme_void() +
  theme(strip.placement = 'inside',
        text = element_text(size = 18, hjust = 0))

ggsave("figs/color_palettes.png", width = 6, height = 5, dpi = 80)



# Other color palettes to consider
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
              main = "Mapping Color Palettes",
              pal_names = c("Berlin", "Fall"), 
              grid = FALSE)


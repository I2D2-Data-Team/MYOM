library(markdown)
library(shiny)
library(shinyBS)
library(shinythemes)
library(data.table)
library(tidyverse)
library(sf)
library(DT)

not_sel <- "Not Selected"

color_palette <-
  list(
    yellow_grn = c("#fff999", "#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"),
    yellow_red = RColorBrewer::brewer.pal(7, "YlOrRd"),
    # yellow_pur = viridis::magma(18)[seq(17, 5, -2)],
    yellow_pur = viridis::magma(20)[c(20, seq(17, 6, -2))],
    red_purple = RColorBrewer::brewer.pal(7, "RdPu"),
    blu_purple = RColorBrewer::brewer.pal(7, "BuPu"),
    yellow_blu = c(RColorBrewer::brewer.pal(11, "Spectral")[6:11], "darkslateblue"),
    viridius = viridis::viridis(13)[seq(13, 1, -2)],
    i2d2 = c("#252C6A", "#0097CE", "#66c0e1", "#76777A", "#c299cc", "#863399", "#CF202E")
  )



# READ DATA ---------------------------------------------------------------

# Read sf data for mapping State of Iowa
ia_state_map <- read_rds("data/IA-state-map.rds")

# Read sf data for mapping Counties of Iowa
ia_county_map <- read_rds("data/IA-county-map.rds")

# Read list with rectified names of Iowa counties
ia_counties <- read_csv("data/IA-county-names-standard.csv")


# UI - ABOUT -------------------------------------------------------

about_page <- tabPanel(
  title = "About",
  fluidRow(
    column(width = 8, 
           offset = 2,
           includeMarkdown("README.md")
    )
  ),
  br(),
  tags$sup("Created with R Shiny, 2022"),
  br()
)


# UI - SAMPLE DATA -------------------------------------------------

sample_data_page <- tabPanel(
  title = "Sample Data",
  fluidRow(
    column(width = 8, 
           offset = 2,
           includeMarkdown("SAMPLE_DATA.md")
    )
  ),
  br(),
  tags$sup("Created with R Shiny, 2022"),
  br()
)


# UI - MAIN --------------------------------------------------------

main_page <- tabPanel(
  title = "Map",
  titlePanel("Make Your Own Map"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs", 
      width = 3,
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      selectInput("var_county", "Select County Names of FIPS Codes", choices = c(not_sel)),
      selectInput("var_value", "Select Variable to Plot", choices = c(not_sel)),
      selectInput("cut_value", "Split Data into Bins",
                  choices = c("None", "Interval", "Number")),
      conditionalPanel(
        condition = "input.cut_value != 'None'",
        sliderInput("cut_n", "Number of Bins", 1, 7, 4, ticks = FALSE)
        ),
      br(),
      actionButton("plot_button", "Plot Map", icon = icon("play")),
      downloadButton("download_button", "Download Map", icon = icon("download"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          column(width = 8, 
                 plotOutput("plot_map")
                 ),
          column(width = 4, 
                 # uiOutput("update_button")
                 bsCollapse(id = "collapseExample", 
                            open = "Plot Title", 
                            bsCollapsePanel("Plot Title",
                                            textInput("plot_title", "Title", placeholder = "Enter Title of the Map"),
                                            textInput("plot_subtitle", "Subtitle", placeholder = "Enter Subtitle of the Map"),
                                            style = "primary"),
                            bsCollapsePanel("Legend", 
                                            textInput("legend_title", "Legend Name", placeholder = "Enter Name of the Variable"),
                                            selectInput("legend_position", "Legend Position",
                                                        choices = c("none", "bottom", "left", "right", "top"), 
                                                        selected = "bottom"),
                                            "To remove the legend from the plot select 'none' in the drop-down list", 
                                            checkboxInput("hide_missing", label = "Hide Missing Label from Legend", value = FALSE),
                                            style = "primary"),
                            bsCollapsePanel("County Labels", 
                                            checkboxInput("show_county_name", label = "Show County Names", value = TRUE),
                                            checkboxInput("show_other_name", label = "Show Other Label", value = FALSE),
                                            conditionalPanel(
                                              condition = "input.show_other_name == true",
                                              selectInput("var_label", "Select Labels", choices = c(not_sel))
                                              ),
                                            style = "primary"),
                            bsCollapsePanel("Colors", 
                                            selectInput("fill_palette", "County Colors",
                                                        choices = c("Yellow to Blue" = "yellow_blu",
                                                                    "Yellow to Green" = "yellow_grn",
                                                                    "Yellow to Red" = "yellow_red",
                                                                    "Yellow to Purple" = "yellow_pur",
                                                                    "Red to Purple" = "red_purple",
                                                                    "Blue to Purple" = "blu_purple",
                                                                    "Viridius" = "viridius",
                                                                    "I2D2" = "i2d2"), 
                                                        selected = "yellow_blu"), 
                                            checkboxInput("reverse_palette", label = "Reverse Colors", value = FALSE),
                                            conditionalPanel(
                                              condition = "input.hide_missing == false",
                                              selectInput("fill_missing", "Missing County Color",
                                                          choices = c("White" = "gray100",
                                                                      "Grey (Light)" = "gray80",
                                                                      "Grey" = "gray50",
                                                                      "Grey (Dark)" = "gray30",
                                                                      "Black" = "gray0",
                                                                      "Azure" = "azure1",
                                                                      "Khaki" = "khaki",
                                                                      "Silk" = "cornsilk1",
                                                                      "Slate Gray" = "slategray1"), 
                                                          selected = "gray80"),
                                            ),
                                            style = "primary"),
                            bsCollapsePanel("Fonts", 
                                            selectInput("font_type", "Font Type",
                                                        choices = c("Arial" = "sans",
                                                                    "Courier" = "mono",
                                                                    "Times New Roman" = "serif"), 
                                                        selected = "serif"),
                                            
                                            selectInput("font_color_county", "County Label Font Color",
                                                        choices = c("Black" = "gray0",
                                                                    "Grey" = "gray50",
                                                                    "White" = "gray100"), 
                                                        selected = "gray0"),
                                            sliderInput("font_title_size", "Change Title Font Size", -5, 5, 0, ticks = FALSE),
                                            sliderInput("font_subtitle_size", "Change Subtitle Font Size", -5, 5, 0, ticks = FALSE),
                                            sliderInput("font_legend_title_size", "Change Legend Title Font Size", -4, 4, 0, ticks = FALSE),
                                            sliderInput("font_legend_text_size", "Change Legend Text Font Size", -4, 4, 0, ticks = FALSE),
                                            
                                            style = "primary")
                            ),
                 br()
                 )
        ),
        tabPanel(
          title = "Data",
          fluidRow(
            column(width = 12,
                   # strong("Plotted Data"),
                   DT::dataTableOutput("data_table"))#,
            # column(width = 6,
            #        strong("Data Not Plotted"),
            #        textOutput("CHECK1"),
            #        textOutput("CHECK2")
            #        # DT::dataTableOutput("data_table_not")
            #        )
          )
        )
      )
    )
  )
)


# FUNCTIONS ---------------------------------------------------------------

# Standardize uploaded table after selecting variables
create_var_table <- function(data_input, var_county, var_value, var_label = not_sel) {
  if (var_label == not_sel) {
    var_label <- var_value
  }
  if (is.numeric(data_input %>% pull(var_county))) {
    data_input %>%
      select(fips = var_county, value = var_value, label = var_label) %>%
      right_join(ia_counties, by = "fips") %>%
      select(fips, county = county_name, value, label) 
  } else {
    data_input %>%
      select(county = var_county, value = var_value, label = var_label) %>%
      mutate(county_CAP = str_remove_all(county, "[:punct:]"),
             county_CAP = str_squish(str_to_upper(county_CAP)),
             county_CAP = str_remove_all(county_CAP, "\\s")) %>%
      right_join(ia_counties, by = "county_CAP") %>%
      select(fips, county, value, label) 
  }
}


# Transform plotting variable into factor
char_into_factor <- function(data_table) {
  # character value into factor
  if (is.character(data_table$value)) data_table$value <- factor(data_table$value, exclude = c("", " ", NA))
  return(data_table)
}

dichotomous_into_factor <- function(data_table) {
  # dichotomous value into factor
  if (is.integer(data_table$value) && 
      length(unique(data_table$value[!is.na(data_table$value)])) < 3) 
    data_table$value <- factor(data_table$value, exclude = c("", " ", NA))
  return(data_table)
}

value_into_factor <- function(data_table) {
  data_table %>%
    char_into_factor() %>%
    dichotomous_into_factor()
}


# Create bins in not a factor
cut_values <- function(data_table, cut = "None", cut_n = 4) {
  switch (cut,
          None  = data_table,
          Interval = mutate(data_table, value = cut_interval(value, cut_n)),
          Number = mutate(data_table, value = cut_number(value, cut_n)),
          Width = mutate(data_table, value = cut_width(value, cut_n))
  )
}


make_map <- function(data_plot, 
                     plot_title = NULL,
                     plot_subtitle = NULL,
                     legend_title = NULL,
                     legend_position = "bottom",
                     fill_palette = "default",
                     reverse_palette = FALSE,
                     fill_missing = "default",
                     hide_missing = FALSE,
                     show_county_name = TRUE,
                     show_other_name = FALSE,
                     font_type,
                     font_title_size,
                     font_subtitle_size,
                     font_legend_title_size,
                     font_legend_text_size,
                     font_color_county,
                     ...){
  
  # do not show plot title if it is blank
  if (str_length(plot_title) == 0 | plot_title == "") {
    plot_title <- NULL
  }
  
  # do not show plot subtitle if it is blank
  if (str_length(plot_subtitle) == 0 | plot_subtitle == "") {
    plot_subtitle <- NULL
  }
  
  # do not show plot legend name if it is blank
  if (str_length(legend_title) == 0 | legend_title == "") {
    legend_title <- NULL
  }
  
  # reorient legend text according to its position
  if (legend_position %in% c("bottom", "top")) {
    legend_direction = "horizontal"
  } else {
    legend_direction = "vertical"
  }
  
  
  my_map <-
    ia_county_map %>% 
    left_join(data_plot, by = c("fips")) %>%
    ggplot() +
    geom_sf(aes(fill = value), size = 0.2) +
    geom_sf(data = ia_state_map, fill = NA, size = 0.8, color = "black") +
    labs(title = plot_title,
         subtitle = plot_subtitle, 
         fill = legend_title) +
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 18, lineheight = 1, family = font_type),
          legend.position = legend_position,
          legend.direction = legend_direction,
          # legend.spacing = unit(2, "mm"),
          # legend.key.size = unit(1, "mm"),
          plot.title = element_text(size = (32 + 2 * font_title_size), hjust = 0.5),
          plot.subtitle = element_text(size = (24 + 2 * font_subtitle_size), hjust = 0.5),
          legend.title = element_text(size = (18 + 2 * font_legend_title_size)),
          legend.text = element_text(size = (18 + 2 * font_legend_text_size)),
          plot.caption = element_text(family = "mono", size = 6, hjust = 0, lineheight = 0.3)) +
    guides(fill = guide_legend(byrow = TRUE))
  
  # display labels on the counties
  if (show_county_name && !show_other_name) {
    my_map <- my_map +
      geom_text(aes(long, lat, label = str_wrap(county_name, 9)),  color = font_color_county, size = 2)
  } else if (show_county_name && show_other_name) {
    my_map <- my_map +
      geom_text(aes(long, lat, label = county_name),  color = font_color_county, size = 2, nudge_y = -0.045) +
      geom_text(aes(long, lat, label = str_wrap(label, 9)),  color = font_color_county, size = 3, nudge_y = 0.045)
  } else if (!show_county_name && show_other_name) {
    my_map <- my_map +
      geom_text(aes(long, lat, label = str_wrap(label, 9)),  color = font_color_county, size = 3)
  } else if (!show_county_name && !show_other_name) {
    my_map <- my_map
  }
  
  # change color palette
  if ((fill_palette != "default" || fill_missing != "default") && !is.factor(data_plot$value)) {
    # reverse colors
    if (reverse_palette) {
      low_color = color_palette[[fill_palette]][7]
      high_color = color_palette[[fill_palette]][1]
    } else {
      low_color = color_palette[[fill_palette]][1]
      high_color = color_palette[[fill_palette]][7]
    }
    if (hide_missing) {
      fill_missing <- "white"
    }
    my_map <- my_map +
      scale_fill_gradient(low = low_color,
                          high = high_color,
                          na.value = fill_missing)
  }
  
  # change color palette
  if ((fill_palette != "default" || fill_missing != "default") && is.factor(data_plot$value)) {
    num_levels <- length(levels(data_plot$value))
    if (num_levels < 3) {
      my_colors <- c(1, 7)
    } else if (num_levels == 3) {
      my_colors <- c(1, 4, 7)
    } else {
      my_colors <- c(1:num_levels)
    }
    # reverse colors
    if (reverse_palette) {
      my_colors <- rev(my_colors)
    }
    my_map <- my_map +
      scale_fill_manual(values = color_palette[[fill_palette]][my_colors],
                        # prevents assigning wrong colors if bin is missing SEE teen_birth 2017
                        drop = FALSE,
                        na.value = fill_missing,
                        na.translate = !hide_missing)
  }
  
  return(my_map)
}


# UI --------------------------------------------------------------

ui <- navbarPage(
  title = "Data Visualization",
  # theme = shinytheme('united'),
  # theme = shinytheme('sandstone'),
  theme = shinytheme('yeti'),
  main_page,
  about_page,
  sample_data_page,
  position = c("fixed-top"),
  tags$head(
    tags$style(HTML("
      body {
        padding-top: 50px;
      }
      h2 {
        font-weight: bold;
        margin-top: 50px;
      }
      h3 {
        color: #008CBA;
        font-weight: bold;
        margin-top: 40px;
      }
                    "))
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  # data_input <- reactive(data)
  
  observeEvent(data_input(),{
    choices <- c(not_sel, names(data_input()))
    updateSelectInput(inputId = "var_county", choices = choices)
    updateSelectInput(inputId = "var_value", choices = choices)
    updateSelectInput(inputId = "var_label", choices = choices)
  })

  var_county <- eventReactive(input$plot_button,input$var_county)
  var_value <- eventReactive(input$plot_button,input$var_value)
  var_label <- eventReactive(input$plot_button,input$var_label)
  
  cut_value <- reactive(input$cut_value)
  cut_n <- reactive(input$cut_n)
  
  # fix county names
  data_plot <- reactive({
    df <- create_var_table(data_input(), var_county(), var_value(), var_label()) %>%
      value_into_factor()
    # split into bins only if numeric
    if (!is.factor(df$value)) {
      df <- df %>% cut_values(cut_value(), cut_n())
    }
    return(df)
  })
  
  # plot map
  plot_map <- eventReactive(input$plot_button,{
    make_map(data_plot(), 
             input$plot_title,
             input$plot_subtitle,
             input$legend_title,
             input$legend_position,
             input$fill_palette,
             input$reverse_palette,
             input$fill_missing,
             input$hide_missing,
             input$show_county_name,
             input$show_other_name,
             input$font_type,
             input$font_title_size,
             input$font_subtitle_size,
             input$font_legend_title_size,
             input$font_legend_text_size,
             input$font_color_county
             )
  })

  output$plot_map <- renderPlot(plot_map())
  

  output$download_button <- downloadHandler(
    filename = "your_map.png",
    content = function(file){
      ggsave(file, 
             plot = plot_map() + 
               labs(caption = paste("Downloaded on", format(Sys.Date(), "%B %d, %Y"))), 
             width = 10, height = 6, dpi = 320
             )
    }
  )
  
  # show table
  # output$data_table <- renderDataTable(data_plot())
  output$data_table <- renderDataTable(data_input())
  

  
  # SHOW NEW BOTTUN
  observeEvent(input$plot_button, {
    output$update_button <- renderUI({
      actionButton("update_button", label = "Update Map", icon = icon("globe"))
    })
  })
  
  output$CHECK1 <- renderText(input$fill_palette)
  output$CHECK2 <- renderText(color_palette[[input$fill_palette]])
  
  
}

shinyApp(ui = ui, server = server)

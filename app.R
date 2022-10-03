library(shiny)
library(shinyBS)
library(shinythemes)
library(shinydashboardPlus)
library(data.table)
library(tidyverse)
library(sf)
library(DT)

not_sel <- "Not Selected"

color_palette <-
  list(
    yellow_grn = c("#fff999", "#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"),
    yellow_red = RColorBrewer::brewer.pal(7, "YlOrRd"),
    purple_red = RColorBrewer::brewer.pal(7, "RdPu"),
    blu_purple = RColorBrewer::brewer.pal(7, "BuPu"),
    yellow_blu = c(RColorBrewer::brewer.pal(11, "Spectral")[6:11], "#darkslateblue"),
    yellow_pur = viridis::magma(18)[seq(17, 5, -2)],
    viridius = viridis::viridis(13)[seq(13, 1, -2)]
  )

my_font <- "serif"


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
  titlePanel("About"),
  "Created with R Shiny",
  br(),
  "2021 April"
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
        sliderInput("cut_n", "Number of Bins", 1, 7, 1, ticks = FALSE)
        ),
      selectInput("var_label", "Select Labels", choices = c(not_sel)),
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
                                            "To remove the legend from the plot select 'none' in the ",
                                            "drop-down list", style = "primary"),
                            bsCollapsePanel("Colors", 
                                            selectInput("fill_palette", "County Colors",
                                                        choices = c("Default" = "default",
                                                                    "Yellow to Green" = "yellow_grn",
                                                                    "Yellow to Red" = "yellow_red",
                                                                    "Purple to Red" = "purple_red",
                                                                    "Blue to Purple" = "blu_purple",
                                                                    "Yellow to Blue" = "yellow_blu",
                                                                    "Yellow to Purple" = "yellow_pur",
                                                                    "Viridius Colors" = "viridius"), 
                                                        selected = "default"), style = "primary")
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
  if (is.character(data_table$value)) data_table$value <- as.factor(data_table$value)
  return(data_table)
}

dichotomous_into_factor <- function(data_table) {
  # dichotomous value into factor
  if (is.integer(data_table$value) && 
      length(!is.na(unique(data_table$value))) < 3) 
    data_table$value <- as.factor(data_table$value)
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



# my_map <- make_map(data_plot(), var_county(), var_value(), var_label())

make_map <- function(data_plot, 
                     plot_title = NULL,
                     plot_subtitle = NULL,
                     legend_title = NULL,
                     legend_position = "bottom",
                     fill_palette = "default",
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
    mutate(county_name =
             ifelse(county_name == "Des Moines", "Des\nMoines", county_name)) %>%
    ggplot() +
    geom_sf(aes(fill = value), size = 0.2) +
    geom_sf(data = ia_state_map, fill = NA, size = 0.8, color = "black") +
    geom_text(aes(long, lat, label = county_name),  color = "black", size = 2) +
    labs(title = plot_title,
         subtitle = plot_subtitle, 
         fill = legend_title) +
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 18, lineheight = 1, family = my_font),
          legend.position = legend_position,
          legend.direction = legend_direction,
          # legend.spacing = unit(2, "mm"),
          # legend.key.size = unit(1, "mm"),
          plot.title = element_text(size = 32, hjust = 0.5),
          plot.subtitle = element_text(size = 24, hjust = 0.5),
          plot.caption = element_text(size = 9, hjust = 0, lineheight = 0.3)) +
    guides(fill = guide_legend(byrow = TRUE))
  
  # change color palette
  if (fill_palette != "default") {
    my_map <- my_map +
      scale_fill_manual(values = color_palette[[fill_palette]],
                        drop = FALSE,  # prevents assigning wrong colors if bin is missing SEE teen_birth 2017
                        na.value = "gray")
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
  about_page
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
    # my_map <- make_map(data_plot(), var_county(), var_value(), var_label())
    make_map(data_plot(), 
             input$plot_title,
             input$plot_subtitle,
             input$legend_title,
             input$legend_position,
             input$fill_palette)
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

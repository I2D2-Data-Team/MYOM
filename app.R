library(shiny)
library(shinythemes)
library(shinydashboardPlus)
library(data.table)
library(tidyverse)
library(sf)

not_sel <- "Not Selected"



# READ DATA ---------------------------------------------------------------

# Read sf data for mapping State of Iowa
ia_state_map <- read_rds("data/IA-state-map.rds")

# Read sf data for mapping Counties of Iowa
ia_county_map <- read_rds("data/IA-county-map.rds")

# Read list with rectified names of Iowa counties
ia_counties <- read_csv("data/IA-county-names-standard.csv")

# data <- fread("TEMP.csv")


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
          title = "Map",
          column(width = 8, 
                 plotOutput("plot_map")
                 ),
          column(width = 4, 
                 textInput("plot_title", "Title", placeholder = "Enter Title of the Map"),
                 textInput("plot_subtitle", "Subtitle", placeholder = "Enter Subtitle of the Map"),
                 # uiOutput("update_button")
                 selectInput("legend_position", "change position of the legend",
                             choices = c("none", "bottom", "left", "right", "top"), 
                             selected = "bottom"),
                 br()
                 )
        ),
        tabPanel(
          title = "Data",
          fluidRow(
            column(width = 12, strong("Plotted Data"))
          ),
          fluidRow(
            column(width = 12, tableOutput("summary_table"))
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

# Create bins
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
                     legend_position = "bottom",
                     ...){
  
  # do not show plot title if it is blank
  if (str_length(plot_title) == 0 | plot_title == "") {
    plot_title <- NULL
  }
  
  # do not show plot subtitle if it is blank
  if (str_length(plot_subtitle) == 0 | plot_subtitle == "") {
    plot_subtitle <- NULL
  }
  
  # reorient legend text according to its position
  if (legend_position %in% c("bottom", "top")) {
    legend_direction = "horizontal"
  } else {
    legend_direction = "vertical"
  }
  
  ia_county_map %>% 
    left_join(data_plot, by = c("fips")) %>%
    mutate(county_name = 
             ifelse(county_name == "Des Moines", "Des\nMoines", county_name)) %>%
    ggplot() +
    geom_sf(aes(fill = value), size = 0.2) +
    geom_sf(data = ia_state_map, fill = NA, size = 0.8, color = "black") +
    geom_text(aes(long, lat, label = county_name),  color = "black",
              family = "asap_cond", lineheight = .4, size = 3) +
    labs(title = plot_title,
         subtitle = plot_subtitle, 
         fill = NULL) +
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          text = element_text(family = "ubuntu", size = 18, lineheight = 1),
          legend.position = legend_position,
          legend.direction = legend_direction,
          legend.spacing = unit(2, "mm"),
          legend.key.size = unit(1.5, 'lines'),
          plot.title = element_text(size = 32, hjust = 0.5),
          plot.subtitle = element_text(size = 24, hjust = 0.5),
          plot.caption = element_text(size = 9, hjust = 0, lineheight = 0.3)) +
    guides(fill = guide_legend(byrow = TRUE))
}


# UI --------------------------------------------------------------

ui <- navbarPage(
  title = "Data Visualization",
  theme = shinytheme('united'),
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
    create_var_table(data_input(), var_county(), var_value(), var_label()) %>%
      cut_values(cut_value(), cut_n())
  })

  
  # plot map
  
  plot_map <- eventReactive(input$plot_button,{
    # my_map <- make_map(data_plot(), var_county(), var_value(), var_label())
    make_map(data_plot(), 
             input$plot_title,
             input$plot_subtitle,
             input$legend_position)
  })

  output$plot_map <- renderPlot(plot_map())
  

  output$download_button <- downloadHandler(
    filename = "your_map.png",
    content = function(file){
      ggsave(file, 
             plot = plot_map() + 
               labs(caption = paste("Downloaded on", format(Sys.Date(), "%B %d, %Y"))), 
             width = 12, height = 8)
    }
  )
  
  

  
  # SHOW NEW BOTTUN
  observeEvent(input$plot_button, {
    output$update_button <- renderUI({
      actionButton("update_button", label = "Update Map", icon = icon("globe"))
    })
  })
  
  # SHOW table
  summary_table <- eventReactive(input$plot_button,{
    head(data_plot(), 20) 
  })
  
  output$summary_table <- renderTable(summary_table())
  
}

shinyApp(ui = ui, server = server)

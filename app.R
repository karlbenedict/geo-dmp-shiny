# app.R
# Shiny application for capturing and displaying identified connections between
# different data management, sharing, and preservation principles. This
# application builds upon work initiated within the Group on Earth Observations
# Data Management Group, Data Management and Sharing Principles Subgroup

# Libraries ####################################################################
library(shiny)
library(shinyjs)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(igraph)
library(uuid)

# Setup ########################################################################
principles_labels <- c(
  "FAIR", 
  "GEO Data Sharing", 
  "GEO Data Management"
)
principles_names <- c(
  "fair",
  "geo_ds",
  "geo_dm"
)
names(principles_names) <- principles_labels
principles_files <- c(
  "source-data/fair",
  "source-data/geo_ds",
  "source-data/geo_dm"
  )
names(principles_files) <- principles_names


# Functions ####################################################################
get_principles <- function(source_files) {
  dt <- tibble(
    name = character(),
    label = character(),
    item_name = character(),
    item_label = character(),
    item_description = character()
  )
  for (file_path in source_files) {
    print(file_path)
    principles <- fromJSON(file_path)
    str(principles)
    dt <- principles %>% 
      as_tibble() %>% 
      unnest(items) %>% 
      select(name, label, item_name, item_label, item_description) %>% 
      rbind(dt)
      
    #str(temp_df)
  }
  str(dt)
  return(dt)
}

get_edges <- function(data_dir) {
  # generate an empty tibble to return until the retrieval function is written
  dt <- tibble(
    session_id = character(),
    source = character(),
    target = character()
  )
  
  return(dt)
}

save_edge <- function(outdir, source, target, session_id){
  data <- tibble(
    source = source,
    target = target,
    session_id = session_id
  )
  fileName = sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  write.csv(
    x = data,
    file = file.path(outdir, fileName),
    row.names = FALSE,
    quote = TRUE
  )
}

get_frameworks <- function(input_nodes) {
  dt <- input_nodes %>% 
    distinct(name, .keep_all = TRUE) %>% 
    select(name, label)
}

principle_subset <- function(nodes, framework_name) {
  dt <- nodes %>% 
    filter(name == framework_name) %>% 
    select(item_name, item_label, item_description)
  return(dt)
}



# Application Code #############################################################
shinyApp(
ui = fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("demo_mode", "Demo Mode", value = TRUE),
      selectInput("source", 
                  label = h3("Source Principles Framework"), 
                  choices = get_frameworks(get_principles(principles_files))['name'], 
                  selected = 1),
    ),
    mainPanel(
      plotOutput('connections'),
      verbatimTextOutput("dataDirectory", placeholder = TRUE)
    ),
  )
),

server = function(input, output, session) {
  data_directory <- reactiveVal("demo_data")
  session_id <- reactiveVal(UUIDgenerate())
  nodes <- reactiveVal(get_principles(principles_files))
  #frameworks <- reactiveVal(get_frameworks(nodes()))
  edges <- reactiveVal(get_edges(data_directory))
  output$dataDirectory <- renderText({paste("Data Directory: ", data_directory(), sep = "")})
  #observe({
  #  updateSelectInput(session,
  #                    "source",
  #                    label = h3("Source Principles Framework"),
  #                    
  #  )
  #})
  observeEvent(input$demo_mode, {
    if (input$demo_mode) {
      data_directory("demo_data")
    } else {
      data_directory("data")
    }
  })
}
)

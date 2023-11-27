# app.R
# Shiny application for capturing and displaying identified connections between
# different data management, sharing, and preservation principles. This
# application builds upon work initiated within the Group on Earth Observations
# Data Management Group, Data Management and Sharing Principles Subgroup

# Libraries
library(shiny)
library(jsonlite)
library(tidyverse)
library(tidyr)

# Setup 
principles_labels <- c(
  "FAIR", 
  "GEO Data Sharing", 
  "GEO Data Management"
)
principles_names <- c(
  "fair",
  "geo-ds",
  "geo-dm"
)
names(principles_names) <- principles_labels
principles_files <- c(
  "source-data/fair.json",
  "source-data/geo-ds.json",
  "source-data/geo-dm.json"
)
names(principles_files) <- principles_names


# Functions
get_principles <- function(source_files) {
  dt <- tibble(
    name = character(),
    label = character(),
    description = character()
  )
  for (file_path in source_files) {
    #print(file_path)
    principles <- tibble(package = fromJSON(file_path))
    
    
    
    dt <- bind_rows(dt, unnest_wider(principles, items))
  }
  return(dt)
}
out <- get_principles(principles_files)

# Application Code
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
    )
  ),
  mainPanel(
    plotOutput('connections')
  )
)

server <- function(input, output) {
  
}
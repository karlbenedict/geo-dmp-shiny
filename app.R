# app.R
# Shiny application for capturing and displaying identified connections between
# different data management, sharing, and preservation principles. This
# application builds upon work initiated within the Group on Earth Observations
# Data Management Group, Data Management and Sharing Principles Subgroup

# Libraries
library(shiny)

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
  "source-data/geo-ds",
  "source-data/geo-dm"
)
names(principles_files) <- principles_names


# Functions


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
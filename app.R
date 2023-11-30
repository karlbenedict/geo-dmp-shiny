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
library(ggraph)
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

values <- reactiveValues()
values$responses = 0


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
    #print(file_path)
    principles <- fromJSON(file_path)
    #str(principles)
    dt <- principles %>% 
      as_tibble() %>% 
      unnest(items) %>% 
      select(name, label, item_name, item_label, item_description) %>% 
      rbind(dt)
      
    #str(temp_df)
  }
  #str(dt)
  dt <- dt %>% 
    mutate(id = 1:n()) %>% 
    select(id, everything())
  #print(dt)
  return(dt)
}

save_edge <- function(outdir, source, target, relationship, session_id){
  data <- tibble(
    source_name = source,
    target_name = target,
    relationship = relationship,
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

get_framework_choices <- function(p_files) {
  dt <- get_frameworks(get_principles(p_files))
  choices <- dt %>% 
    select(label, name) %>% 
    deframe()
  #print(choices)
  return(choices)
}

get_principle_choices <-  function(f_nodes, f_framework) {
  dt <- f_nodes %>% 
    filter(name == f_framework)
  #print(dt)
  choices <- dt %>% 
    mutate(compound_label = paste(item_label, item_description, sep = ": ")) %>% 
    select(compound_label, item_name) %>% 
    deframe()
  #print(choices)
  return(choices)
}

load_edges <- function(outdir) {
  files <- list.files(outdir, pattern = "\\.csv$", full.names = TRUE)
  #print(files)
  if (length(files) > 0){
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  } else {
    data <- tibble(
      source_name = NA,
      target_name = NA,
      relationship = NA,
      session_id = NA
    )
    data
  }
}



# Application Code #############################################################
shinyApp(
# UI CODE ######################################################################
ui = fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("demo_mode", "Demo Mode", value = TRUE),
      checkboxInput("limit_to_principle_pair", "Limit Graph to Principle Frameworks Pair", value = TRUE),
      tags$hr(),
      tags$h3("Principle Frameworks"),
      tags$p("Select the source and target principle frameworkd that you want to identify the connections between"),
      selectInput("source", 
                  label = "Source", 
                  choices = get_framework_choices(principles_files), 
                  selected = 1),
      selectInput("target", 
                  label = "Target", 
                  choices = get_framework_choices(principles_files), 
                  selected = 2),
      tags$h3("Linked principles"),
      tags$p("Select the pair of principles from the source and target frameworks that you want to define the relationship between. Then characterize the relationship between the paired source and target principles along a continuum between opposing and supporting. "),
      selectInput("source_principle", 
                  label = "Source Principle", 
                  choices = "", 
                  selected = 1),
      selectInput("target_principle", 
                  label = "Target Principle", 
                  choices = "", 
                  selected = 1),
      radioButtons("relationship", 
                   label = "Relationship between source and target principles", 
                   choices = list(
                     "Opposes" = -1,
                     "No relationship" = 0,
                     "Supports" = 1
                    ),
                   selected = 0
                   ),
      actionButton("submit",
                   label = "Submit Relationship"),
      tags$hr(style = "border: 5px solid black"),
      tags$h4("Relationships entered in this session"),
      tableOutput("myEdgesTable"),
      downloadButton("downloadMyEdges", "Download my entered relationships"),
      tags$h4("All relationships"),
      tableOutput("edgesTable")
    ),
    mainPanel(
      plotOutput('connections'),
      tableOutput("principle_descriptions"),
      #verbatimTextOutput("dataDirectory", placeholder = TRUE),
      #verbatimTextOutput("sourceFramework", placeholder = TRUE),
      #verbatimTextOutput("targetFramework", placeholder = TRUE)
    ),
  )
),

# SERVER CODE ##################################################################
server = function(input, output, session) {
  data_directory <- reactiveVal("demo_data")              # the location where saved relationship files will be saved. toggled by the demo_mode checkbox input 
  session_id <- reactiveVal(UUIDgenerate())               # a UUID assigned to the user session that is included in each created relationship
  nodes <- reactiveVal(get_principles(principles_files))  # these are fixed and based on the content of the principles JSON files
  # initiaize the tibble containing the relationships added during the current session. updated each time a new relationship is created 
  my_data <- reactiveVal(
    tibble(
      source = NA,
      target = NA,
      source_name = NA,
      source_label = NA,
      source_framework = NA,
      target_name = NA,
      target_label = NA,
      target_framework = NA,
      relationship = NA,
      session_id = NA
    )
  )
  # initialize edges tibble that will then be updated with the following reativePoll
  edges <- reactiveVal( )
  data <- reactivePoll(1000,
                       session,
                       checkFunc = function () {
                         files <- list.files(data_directory(), pattern = "\\.csv$", full.names = TRUE)
                         #print(paste(length(files),data_directory(),sep="_"))
                         paste(length(files),data_directory(),sep="_")
                       },
                       valueFunc = function () {
                         #print("A new file was added")
                         files <- list.files(data_directory(), pattern = "\\.csv$", full.names = TRUE)
                         dt <- load_edges(data_directory()) %>% 
                           left_join(nodes(), relationship = "many-to-one", by = join_by(source_name == item_name)) %>% 
                           mutate(source = id,
                                  source_label = item_label,
                                  source_framework = name) %>% 
                           select(source, source_name, source_label, source_framework, target_name, relationship, session_id) %>% 
                           left_join(nodes(), relationship = "many-to-one", by = join_by(target_name == item_name)) %>% 
                           mutate(target = id,
                                  target_label = item_label,
                                  target_framework = name) %>% 
                           select(source, target, source_name, source_label, source_framework, target_name, target_label, target_framework, relationship, session_id)
                         my_data(dt %>% filter(session_id == session_id()))
                         edge_data <- dt %>% 
                           group_by(source, target, source_name, source_label, source_framework, target_name, target_label, target_framework) %>% 
                           summarise(mean_relationship = mean(relationship),
                                     n = n(),
                                     .groups = "keep") %>% 
                           ungroup()
                         edges(edge_data %>% 
                                 select(source, target, mean_relationship, n, source_name, target_name, source_framework, target_framework))
                         #print(edges())
                         edge_data
                       }
    )
  
  #frameworks <- reactiveVal(get_frameworks(nodes()))
  output$dataDirectory <- renderText({paste("Data Directory: ", data_directory(), sep = "")})
  output$sourceFramework <- renderText({paste("Source Framework: ", input$source, sep = "")})
  output$targetFramework <- renderText({paste("Target Framework: ", input$target, sep = "")})
  
  output$edgesTable <- renderTable((data() %>% 
                                     mutate(Source = paste(source, source_label, sep=": "),
                                            Target = paste(target, target_label, sep=": "),
                                            `Avg Relationship Score` = mean_relationship) %>% 
                                     select(Source, Target, `Avg Relationship Score`, n)
                                     ))
 
   output$myEdgesTable <- renderTable(my_data() %>% 
                                       mutate(Source = paste(source, source_label, sep=": "),
                                              Target = paste(target, target_label, sep=": "),
                                              Relationship = relationship) %>% 
                                       select(Source, Target, Relationship))
  
  output$connections <- renderPlot({
    print(input$source)
    print(input$target)
    print("printing plot_edges")
    print(edges())
    if (input$limit_to_principle_pair) {
      plot_edges <- edges() %>% 
        filter(source_framework == input$source & target_framework == input$target)
      plot_nodes <- nodes() %>% 
        filter(name == input$source | name == input$target)
    } else {
      plot_edges <- edges()
      plot_nodes <- nodes()
    }
    print("printing plot_edges")
    print(plot_edges)
    print("printing plot_nodes")
    print(plot_nodes)
    net <- graph_from_data_frame(d=plot_edges, v=plot_nodes, directed=T)
    set_graph_style(plot_margin = margin(1,1,1,1))
    ggraph(net, layout = 'linear', circular = TRUE) +
      geom_edge_fan(aes(color = mean_relationship,
                        width = n),
                     arrow = arrow(length = unit(3, 'mm'), type = 'closed'),
                     end_cap = circle(9, 'mm')) +
      scale_edge_width(range = c(0.25,2),
                       breaks = c(1,max(plot_edges$n))) +
      scale_edge_color_gradient2(low="red", 
                                 mid="lightgray", 
                                 high="blue", 
                                 midpoint=0.0, 
                                 limits=c(-1, 1),
                                 breaks=c(-1,0,1),
                                 labels=c("Opposes", "No Relationship", "Supports"),
                                 name="Type of Relationship") +
      geom_node_label(aes(label = item_label, color = label), repel = FALSE, size = 4) +
      labs(color='Principle Framework')
  })
  
  output$principle_descriptions <- renderTable(
    nodes() %>% 
      filter(name == input$source | name == input$target) %>% 
      select(item_label, item_description) %>% 
      rename(Principle = item_label,
             Description = item_description)
  )
  
  observeEvent(input$source, {
    f_nodes <- get_principles(principles_files)
    #print(f_nodes)
    f_choices <-  get_principle_choices(f_nodes, input$source)
    #print(f_choices)
    updateSelectInput(session,
                      "source_principle",
                      label = "Source Principle",
                      choices = f_choices
                      
    )
  })
  
  observeEvent(input$target, {
    f_nodes <- get_principles(principles_files)
    #print(f_nodes)
    f_choices <-  get_principle_choices(f_nodes, input$target)
    #print(f_choices)
    updateSelectInput(session,
                      "target_principle",
                      label = "Target Principle",
                      choices = f_choices
                      
    )
  })
  
  observeEvent(input$submit, {
    save_edge(
      data_directory(),
      input$source_principle,
      input$target_principle,
      input$relationship,
      session_id()
      )
  })
  
  observeEvent(input$demo_mode, {
    if (input$demo_mode) {
      data_directory("demo_data")
    } else {
      data_directory("data")
    }
  })
  
  output$downloadMyEdges <- downloadHandler(
    filename = function(){sprintf("export_data_%s_%s.csv", as.integer(Sys.time()), session_id())},
    content = function(fname){
      dt <- my_data() %>% 
        select(source_name, target_name, relationship, session_id)
      write.csv(dt, fname)
    }
  )
}
)

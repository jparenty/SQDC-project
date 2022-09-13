library(leaflet)
library(data.table)
library(gtools)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(hrbrthemes)
library(stringr)
library(shiny)
library(readr)
library(shinyWidgets)
library(DT)


if (file.exists("appdatacache.Rdata")) {
  load(file="appdatacache.Rdata")
} else {
  
  #disable scientific notation
  #options(scipen=999)
  
  sales_data_path = "../../intermediate/inventory/store_inventory_by_day_by_product/"
  stores_sale_path <- list.files(path = sales_data_path, pattern = ".csv$")
  stores_sale_path <- mixedsort(stores_sale_path)
  
  products = read_csv("../../intermediate/products.csv")
  products$name_bis = ifelse(products$strain == "Melange", products$name, products$strain)
  products <- products %>% relocate(name_bis, .before = name)
  
  stores = read_csv("../../intermediate/stores.csv")
  names(stores)[1] <- "index"
  #remove online store
  stores <- stores[-76, ]
  
  
  sales_data <- list()
  
  stores_avg_total_sale <- list()
  stores_avg_sales_per_product_day <- list()
  stores_total_sale <- list()
  
  for (i in seq_along(stores_sale_path)){
    sales_data[[i]] <- read_csv(paste0(sales_data_path, stores_sale_path[i]))
    sales_data[[i]]$X <- NULL
    data <- sales_data[[i]]
    #data$last_updated <- as.Date(data$last_updated)
    store_day_product_avg_sale = aggregate(data$`sales (in unit)`, by = list(data$last_updated), mean)
    
    store_day_total_sale = aggregate(data$`sales (in unit)`, by = list(data$last_updated), sum)
    store_avg_total_sale = mean(store_day_total_sale$x)
    store_total_sale = sum(store_day_total_sale$x)
  
    stores_avg_sales_per_product_day[[i]] = store_day_product_avg_sale 
    stores_avg_total_sale[[i]] = store_avg_total_sale
    stores_total_sale[[i]] = store_total_sale
  
  }
  
  stores_avg_total_sale = unlist(stores_avg_total_sale, use.names = FALSE)
  
  store_index <- gsub("_inventory_by_day_by_product.csv$", "", stores_sale_path)
  store_index <- store_index %>% str_replace("store", "")
  store_name <- stores$name
  
  store_index <- paste(store_index, store_name)
  
  save.image(file="appdatacache.Rdata")
  
}




topWellPanelStyle <-
  "display: flex;
  justify-content: space-around;
  align-items: center;
  background-color: white;
  border-radius: 5px;
  box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
  "

rightPanelStyle <-
  "display: flex;
  flex-direction: column;
  height: 400px;
  justify-content: center;"

borderStyle <-
  "padding: 5px;
  background-color: ghostwhite;
  margin-top: 3%;
  border-radius: 5px;
  box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;"

sidePanelBorder <-
  "padding: 0px 10px 0px 10px;
  background-color: ghostwhite;
  border-radius: 5px;
  box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;"

clusterPanel <-
  "background-color: ghostwhite;
  padding: 2%;
  border-radius: 5px;
  box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;"

clusterDetails <-
  "background-color: white;
  border-radius: 5px;
  box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
  padding: 3%;"

sliderStyle <- 
  "display: flex;
  justify-content: space-evenly;
  align-items: center"

sliderborder <- 
  "border-right: 1px solid grey;
  padding-right: 5px;
  margin-right: 5px;"


ui <- dashboardPage( skin = "green",
  dashboardHeader(title = "Clusters Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Map input", tabName = "map", icon = icon("map")),
      menuItem(text= "Clusters Analysis", tabName = "analysis", icon = icon("bong"))
    )
  ),
  dashboardBody(
    
    chooseSliderSkin("Flat"),
    #setSliderColor(c("#458B00", "#458B00", "#458B00", "#458B00"), c(1, 2, 3, 4)),
    setSliderColor(c("green", "green"), c(1, 2)),
    
    tabItems(
      tabItem('map',
              tags$head(
                # Note the wrapping of the string in HTML()
                tags$style(HTML(
                  "
                  .shiny-text-output {
                    font-weight: bold;
                    background-color: ghostwhite;
                    padding: 5px;
                    border-radius: 5px;
                    display: inline;
                    color: #458B00;
                  }
                  h1, h2, h3 {
                    font-weight: bold;
                  }
                  h3, h1 {
                      margin: 20px 0px 20px 0px;
                  }
                  #zone {
                      box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
                      border-radius: 5px;
                  }
                  #clear_selection{
                    margin-bottom: 5%;
                  }
                  #apply_selection {
                    margin-bottom: 5%;
                  }
                  #group1_size1 {
                    margin-right: 3%;
                  }
                  #group2_size {
                    color: red;
                  }
                  #group2_size1 {
                    margin-right: 3%;
                  }
                  .row {
                    margin: 0px;
                  }
                  "
                ))
              ),
              
        fluidRow(
          column(10,
                 leafletOutput("zone"),
          ),
          column(2, style = "padding-left: 2px;" ,
                 div(style = sidePanelBorder,  
                   div(style = rightPanelStyle, 
                      h3("Create Groups", style = "margin-top: 0px;"),
                     
                      selectInput("cluster_selection",
                                 label = "Choose a group to edit",
                                 choices = c("Group 1", "Group 2")
                      ),
                      
                      p("Create a custom group by clicking on stores markers, or choose a pre-set group: ", style = "color: grey; font-style: italic, margin-bottom: 25px;"),
                     
                      selectInput("preset_clusters",
                                  label = "Chose a pre-set group",
                                  choices = c("No Preset", "Montreal Stores", "Quebec Stores", "Border Stores")
                      ),
                      hr(style = "border-top: 1px solid black; color: grey; width: 100%; margin-top: -5px;"),
                      actionButton("clear_selection", "Clear store selection"),
                      actionButton("apply_selection", "Apply store selection"),
                   )
                 )
                 
          )
        ),
        fluidRow(
          column(6,
                 div(style = borderStyle,
                     div(style = topWellPanelStyle, 
                        actionButton("clear_cluster1", "Clear Group 1"),
                        h3("Group 1", style = "color: green; margin: 20px 0px 20px 0px;"),
                        textOutput("group1_size")
                     ),
                     h5("", style="height: 14.55px;"),
                     dataTableOutput("selected_stores")
                 )
          ),
          column(6,
                 div(style = borderStyle,
                   div(style = topWellPanelStyle, 
                       actionButton("clear_cluster2", "Clear Group 2"),
                       h3("Group 2", style = "color: red; margin: 20px 0px 20px 0px;"),
                       textOutput("group2_size")
                   ),
                   h5("Remaining stores by default", style = "color: grey; font-style: italic"),
                   dataTableOutput("complement_stores")
                 )
          )
        )
        
      ),#tabItem
      
      tabItem("analysis",
        
              fluidRow(
                column(6, 
                    div(style = clusterPanel, 
                       div( 
                           h1("Group 1", style = "color: green; margin-right: auto; margin-left: auto; text-align: center"),
                       ),
                       br(),
                       #cluster 1 details
                       div(style = clusterDetails,
                          h3("Group Details"),
                          fluidRow(
                            h4("Group Pre-set: "),
                            textOutput("cluster1_preset")
                          ),
                          fluidRow(
                            h4("Number of stores: "),
                            textOutput("group1_size1")
                          ),
                          fluidRow(
                            h4("Group Total Sales: "),
                            textOutput("cluster1_total_sales")
                          ),
                          fluidRow(
                            h4("Top Store: "),
                            textOutput("cluster1_top_store")
                          )
                       ),
                       br(),
                       
                       #cluster 1 popular products
                       div(style = clusterDetails, 
                         h3("Most Popular Products"),
                         dataTableOutput("cluster1_pop_prdcts")
                       ),
                       br(),

                       #cluster 1 popular producer
                       div(style = clusterDetails, 
                         div(style = sliderStyle, 
                             h2("Producers\' sales distribution"),
                             div(style = sliderborder, 
                               sliderInput("cluster1_n_producers", label = 'Show top n producers',
                                           min = 0, max = 40, value = 5),
                             ),
                             checkboxInput("pop_producer_percentage_1", "Show data as percentage", value = FALSE)
                         ),
                         plotOutput('cluster1_pop_producer')
                       ),
                       
                       br(),
                       
                       #h2("Ask for inputs", style="color : red;")
                       
                    )
                ),
                div(style = ""),
                column(6, 
                    div(style = clusterPanel, 
                       div( 
                           h1("Group 2", style = "color: red; margin-right: auto; margin-left: auto; text-align: center;"),
                       ),
                       br(),
                       #cluster 2 details
                       div(style = clusterDetails,
                           h3("Group Details"),
                           fluidRow(
                             h4("Group Preset: "),
                             textOutput("cluster2_preset")
                           ),
                           fluidRow(
                              h4("Number of stores: "),
                              textOutput("group2_size1")
                           ),
                           fluidRow(
                             h4("Group Total Sales: "),
                             textOutput("cluster2_total_sales")
                           ),
                           fluidRow(
                             h4("Top Store: "),
                             textOutput("cluster2_top_store")
                           )
                       ),                       
                       br(),
                       
                       #cluster 2 pop products
                       div(style = clusterDetails, 
                           h3("Most Popular Products"),
                           dataTableOutput("cluster2_pop_prdcts")
                       ),
                       br(),

                       #cluster 2 pop producer
                       div(style = clusterDetails, 
                         div(style = sliderStyle, 
                             h2("Producers\' sales distribution"),
                             div(style = sliderborder, 
                               sliderInput("cluster2_n_producers", label = 'Show top n producers',
                                           min = 0, max = 40, value = 5),
                             ),
                             checkboxInput("pop_producer_percentage_2", "Show data as percentage", value = FALSE)
                         ),
                         plotOutput('cluster2_pop_producer')
                       ),
                       
                       br(),
                       
                       #h2("Ask for inputs", style="color: red;")
                    )
                       
                )
              )
              
      ) #tabItem
    )#tabItems
    
  )
)


server <- function(input, output, session) {
  
  #click_count <- 0
  #type <- 0
  #group1 are stores selected by user
  group1 <- as.list(NULL)
  #group2 are all remaining stores
  group2 <- as.list(NULL)
  
  #cluster by store index !NOT store ID! ex: store index 68 has id 2195 
  montreal_group <- c(8, 10, 11, 12, 14, 49, 56, 63, 70, 75)
  quebec_group <- c(31, 32, 33, 34, 35, 52, 61, 62, 66, 69)
  border_group <- c(1, 2, 4, 36, 47, 53, 58, 71, 72, 74, 77)
  
  #query the selected group of stores for Analysis tab
  Query <- reactiveValues(g1 = NULL, g2 = NULL, index_g1 = NULL, index_g2 = NULL, g1Type = "Custom" , g2Type = "Custom")
  
  producer_1_ylim <- 0
  producer_2_ylim <- 0
  
  #render base map
  output$zone <- renderLeaflet({
    greenLeafIcon <- icons(
      iconUrl = ifelse(stores$index %in% group1,
                       "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                       ifelse(stores$index %in% group2,
                              "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                              "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                       )
      ),
      iconWidth = 25, iconHeight = 41,
    )
    
    leaflet(data = stores) %>% addTiles() %>%
      addMarkers( lng = ~latitude, lat = ~longitude, layerId = ~index, label = ~name, icon = greenLeafIcon)
  })
  
  #clear store selection
  observeEvent(input$clear_selection, {
    
    print("Clearing selected stores...")
    group1 <<- as.list(NULL)
    group2 <<- as.list(NULL)
    
    #update clusters table
    output$selected_stores <- renderDataTable({
      datatable(stores[stores$index %in% group1,][,c(2,4,9,14)],  options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })
    output$complement_stores <- renderDataTable({
      datatable(stores[stores$index %in% group2, ][,c(2,4,9,14)], options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })
    
    #update clusters size
    output$group1_size <- renderText({
      paste("Size: ", length(group1))
    })
    output$group2_size <- renderText({
      paste("Size: ", length(group2))  
    })
    
    output$zone <- renderLeaflet({
      greenLeafIcon <- icons(
        iconUrl = ifelse(stores$index %in% group1,
                         "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                         ifelse(stores$index %in% group2,
                                "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                                "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                         )
        ),
        iconWidth = 25, iconHeight = 41,
      )
      
      leaflet(data = stores) %>% addTiles() %>%
        addMarkers( lng = ~latitude, lat = ~longitude, layerId = ~index, label = ~name, icon = greenLeafIcon)
    })
  })
  
  observeEvent(input$clear_cluster1, {
    #clear list
    group1 <<- as.list(NULL)
    #update table
    output$selected_stores <- renderDataTable({
      datatable(stores[stores$index %in% group1,][,c(2,4,9,14)],  options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })
    #update size indicator
    output$group1_size <- renderText({
      paste("Size: ", length(group1))
    })
    #update map
    output$zone <- renderLeaflet({
      greenLeafIcon <- icons(
        iconUrl = ifelse(stores$index %in% group1,
                         "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                         ifelse(stores$index %in% group2,
                                "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                                "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                         )
        ),
        iconWidth = 25, iconHeight = 41,
      )
      
      leaflet(data = stores) %>% addTiles() %>%
        addMarkers( lng = ~latitude, lat = ~longitude, layerId = ~index, label = ~name, icon = greenLeafIcon)
    })
    
  })
  
  observeEvent(input$clear_cluster2, {
    group2 <<- as.list(NULL)
    output$complement_stores <- renderDataTable({
      datatable(stores[stores$index %in% group2, ][,c(2,4,9,14)], options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })
    output$group2_size <- renderText({
      paste("Size: ", length(group2))  
    })
    output$zone <- renderLeaflet({
      greenLeafIcon <- icons(
        iconUrl = ifelse(stores$index %in% group1,
                         "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                         ifelse(stores$index %in% group2,
                                "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                                "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                         )
        ),
        iconWidth = 25, iconHeight = 41,
      )
      
      leaflet(data = stores) %>% addTiles() %>%
        addMarkers( lng = ~latitude, lat = ~longitude, layerId = ~index, label = ~name, icon = greenLeafIcon)
    })
  })
  
  #handle store selection when user click on a map marker
  observeEvent(input$zone_marker_click, {
    
    cluster_selected <- input$cluster_selection
    print(paste("Cluster selected: ", cluster_selected))
    
    click <- input$zone_marker_click
    #print(click$id)
    
    
    if (cluster_selected == "Group 1") {
        if (click$id %in% group2) {
          group2 <<- group2[group2 != click$id]
        }
        if (click$id %in% group1) {
          group1 <<- group1[group1 != click$id]
        } 
        else {
          group1 <<- append(group1, click$id)
        }
    }
    
    if (cluster_selected == "Group 2") {
          if (click$id %in% group1) {
            group1 <<- group1[group1 != click$id]
          } 
          if (click$id %in% group2) {
            group2 <<- group2[group2 != click$id]
          } 
          else {
            group2 <<- append(group2, click$id)
          }
    }
    
    #update selected stores table
    output$selected_stores <- renderDataTable({
      group1_stores <- stores[stores$index %in% group1,][,c(2,4,9,14)]
      column_name = c("Store ID", "Name", "Area", "Products Offered")
      colnames(group1_stores) <- column_name
      datatable(group1_stores,  options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })
    
    #update remaining stores tables
    output$complement_stores <- renderDataTable({
      group2_stores <- stores[stores$index %in% group2,][,c(2,4,9,14)]
      column_name = c("Store ID", "Name", "Area", "Products Offered")
      colnames(group2_stores) <- column_name
      datatable(group2_stores, options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })
    
    #change selected store's marker to green
    output$zone <- renderLeaflet({
      
      greenLeafIcon <- icons(
        iconUrl = ifelse(stores$index %in% group1,
                         "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                         ifelse(stores$index %in% group2,
                                "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                                "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                                )
        ),
        iconWidth = 25, iconHeight = 41,
      )
      
      leaflet(data = stores) %>% addTiles() %>%
        addMarkers( lng = ~latitude, lat = ~longitude, label = ~name, layerId = ~index, icon = greenLeafIcon)
    })
    
    #update clusters size
    output$group1_size <- renderText({
      paste("Size: ", length(group1))
    })
    output$group2_size <- renderText({
      paste("Size: ", length(group2))  
    })
    
    
  })
  
  #handle predefined clusters selection
  observeEvent(input$preset_clusters, {
    
    print("Preset clusters input: ")
    print(input$cluster_selection)

    cluster_selected <- input$cluster_selection
    preset_cluster <- switch(input$preset_clusters, "No Preset" = "", "Montreal Stores" = montreal_group, "Quebec Stores" = quebec_group, "Border Stores" = border_group)
    preset_cluster_name <- switch(input$preset_clusters, "No Preset" = "Custom", "Montreal Stores" = "Montreal", "Quebec Stores" = "Quebec", "Border Stores" = "Border")
    
    group_clusters <- unlist(c(montreal_group, border_group, quebec_group), use.names = FALSE)
    preset_cluster <- unlist(preset_cluster, use.names = FALSE)
    
    print("Preset Cluster: ")
    print(input$preset_clusters)
    print(preset_cluster)
    
    
    
    if (cluster_selected == "Group 1") {
      
      Query$g1Type <<- preset_cluster_name
      
      group1 <<- group1[!group1 %in% group_clusters]
      
      
      if (!preset_cluster == "") {
    
        group2 <<- group2[!group2 %in% preset_cluster]
        
        group1 <<- append(group1, preset_cluster)
        
      }
    }
    
    if (cluster_selected == "Group 2") {
      
      Query$g2Type <<- preset_cluster_name
      
      group2 <<- group2[!group2 %in% group_clusters]
      
      if (preset_cluster != "") {
        
        group1 <<- group1[!group1 %in% preset_cluster]
        
        group2 <<- append(group2, preset_cluster)
        
      } 
    }
    
    #selected stores list
    group1 <<- unlist(group1, use.names = FALSE)
    #remaining stores list
    group2 <<- unlist(group2, use.names = FALSE)
    
    output$zone <- renderLeaflet({
      
      greenLeafIcon <- icons(
        iconUrl = ifelse(stores$index %in% group1,
                         "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                         ifelse(stores$index %in% group2,
                                "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                                "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                         )
        ),
        iconWidth = 25, iconHeight = 41,
      )
      
      leaflet(data = stores) %>% addTiles() %>%
        addMarkers( lng = ~latitude, lat = ~longitude, label = ~name, layerId = ~index, icon = greenLeafIcon)
    })
    
  
    #update selected stores table
    output$selected_stores <- renderDataTable({
      group1_stores <- stores[stores$index %in% group1,][,c(2,4,9,14)]
      column_name = c("Store ID", "Name", "Area", "Products Offered" )
      colnames(group1_stores) <- column_name
      datatable(group1_stores, options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))    
    })

    #update remaining stores tables
    output$complement_stores <- renderDataTable({
      group2_stores <- stores[stores$index %in% group2,][,c(2,4,9,14)]
      column_name = c("Store ID", "Name", "Area", "Products Offered")
      colnames(group2_stores) <- column_name
      datatable(group2_stores, options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
    })

    output$group1_size <- renderText({
      paste("Size: ", length(group1))
    })

    output$group2_size <- renderText({
      paste("Size: ", length(group2))
    })
    
  })


  ################# ANALYSIS TAB ################
  
  #Query <- reactiveValues(g1 = NULL, g2 = NULL, index_g1 = NULL, index_g2 = NULL, g1Type = NULL, g2Type = NULL)
  
  
  observeEvent(input$apply_selection, {
    
    #2nd duplicate text output for analysis dashboard tab
    output$group1_size1 <- renderText({
      length(group1)
    })
    output$group2_size1 <- renderText({
      length(group2)  
    })
    
    #if length group2 == 0, set group2 to remaining stores
    if (length(group2) == 0) {
      group2 <- setdiff(stores$index, group1)
      
      #update cluster 2 data table
      output$complement_stores <- renderDataTable({
        group2_stores <- stores[stores$index %in% group2,][,c(2,4,9,14)]
        column_name = c("Store ID", "Name", "Area", "Products Offered")
        colnames(group2_stores) <- column_name
        datatable(group2_stores, options = list(searching = FALSE, pageLength = -1,lengthMenu = FALSE, scrollX = T, dom = 't'))
      })
      #update cluster 2 size
      output$group2_size <- renderText({
        paste("Size: ", length(group2))  
      })
      
      Query$g2Type <<- "Custom"
      
      #update map
      output$zone <- renderLeaflet({
        
        greenLeafIcon <- icons(
          iconUrl = ifelse(stores$index %in% group1,
                           "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                           ifelse(stores$index %in% group2,
                                  "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                                  "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon.png"
                           )
          ),
          iconWidth = 25, iconHeight = 41,
        )
        
        leaflet(data = stores) %>% addTiles() %>%
          addMarkers( lng = ~latitude, lat = ~longitude, label = ~name, layerId = ~index, icon = greenLeafIcon)
      })
      
    }
  
    # print("Applying selection")
    # 
    # 
    # #selected stores list
    # group1 <- unlist(group1, use.names = FALSE)
    # #remaining stores list
    # group2 <- unlist(group2, use.names = FALSE)
    # 
    # print("GROUP1")
    # print(group1)
    # 
    # print("GROUP2")
    # print(group2)
    # 
    # Query$index_g1 <- group1
    # Query$index_g2 <- group2
    # 
    # Query$g1 <- sales_data[group1]
    # Query$g2 <- sales_data[group2]
    
  })
  
  observeEvent(input$apply_selection, {
    
    print("Applying selection")
    #selected stores list
    group1 <<- unlist(group1, use.names = FALSE)
    #remaining stores list
    group2 <<- unlist(group2, use.names = FALSE)
    
    if (length(group2) == 0) {
      group2 <<- setdiff(stores$index, group1)
    }  
    

    print("GROUP1")
    print(group1)

    print("GROUP2")
    print(group2)

    Query$index_g1 <- group1
    Query$index_g2 <- group2

    Query$g1 <- sales_data[group1]
    Query$g2 <- sales_data[group2]
    
  })
  
  cluster1_joined <- reactive({
    #group1 stores as list
    print("Concatenating group 1 sales...")
    stores_group1 <- Query$g1
    #stores_group1 <- sales_data[montreal_group]
    sales_gp1 <- rbindlist(stores_group1)
    sales_gp1 <- aggregate(sales_gp1$`sales (in unit)`, by=list(sales_gp1$variant_id), FUN=sum)
    
    print("Group 1 sales concatenated")
    #DEBUG
    # <- sales_data[montreal_group]
    #sales_gp1 <- rbindlist(stores_group1)
    #sales_gp1 <- aggregate(sales_gp1$`sales (in unit)`, by=list(sales_gp1$variant_id), FUN=sum)
    
    # tryCatch( { sales_gp1 <- aggregate(sales_gp1$`sales (in unit)`, by=list(sales_gp1$variant_id), FUN=sum)
    #             print("cluster 1 data aggregated") }
    #           , error = function(e) {print("Unable to aggregate")})
    
    sales_gp1
  })
  
  cluster2_joined <- reactive({
    #group1 stores as list
    print("Concatenating group 2 sales...")
    stores_group2 <- Query$g2
    ##try doing outer join by = c(last_updated, variant_id)
    sales_gp2 <- rbindlist(stores_group2)
    sales_gp2 <- aggregate(sales_gp2$`sales (in unit)`, by=list(sales_gp2$variant_id), FUN=sum)
    
    print("Group 2 sales concatenated")
    
    # tryCatch( { sales_gp2 <- aggregate(sales_gp2$`sales (in unit)`, by=list(sales_gp2$variant_id), FUN=sum) 
    #             print("cluster 2 data aggregated")}
    #           , error = function(e) {print("Unable to aggregate")})
    
    sales_gp2
  })
  
  producer <- reactive({
    
    print("START producer reactive")
    
    stores_sales_1 <- cluster1_joined()
    stores_sales_2 <- cluster2_joined()
    
    print(paste("Sales 1 size: ", nrow(stores_sales_1) ))
    print(paste("Sales 2 size: ", nrow(stores_sales_2) ))
    
    #DEBUG
    #stores_sales_1 <- sales_data[montreal_group]
    #stores_sales_1 <- rbindlist(stores_sales_1)
    #stores_sales_1 <- aggregate(stores_sales_1$`sales (in unit)`, by=list(stores_sales_1$variant_id), FUN=sum)
    ## END DEBUG
    
    #stores_sales <- aggregate(stores_sales$`sales (in unit)`, by=list(stores_sales$variant_id), FUN=sum)
    names(stores_sales_1)[1] <- "variant_id"
    names(stores_sales_2)[1] <- "variant_id"
    
    
    # producer_popularity1 <- merge(x = producer_popularity1, y = products[,c("variant_id", "producer")], by = "variant_id")
    # 
    # producer_popularity1 <- aggregate(producer_popularity1$`sales (in unit)`, by=list(producer_popularity1$producer), FUN=sum)
    # producer_popularity1 <- producer_popularity1[order(-producer_popularity1$x),]
    
    
    store_sales_merge_1 <- merge(x = stores_sales_1, y = products[,c("variant_id", "producer")], by = "variant_id")
    store_sales_merge_2 <- merge(x = stores_sales_2, y = products[,c("variant_id", "producer")], by = "variant_id")
    
    producer_popularity_1 <- aggregate(store_sales_merge_1$x, by=list(store_sales_merge_1$producer), FUN=sum)
    producer_popularity_1 <- producer_popularity_1[order(-producer_popularity_1$x),]
    
    producer_popularity_2 <- aggregate(store_sales_merge_2$x, by=list(store_sales_merge_2$producer), FUN=sum)
    producer_popularity_2 <- producer_popularity_2[order(-producer_popularity_2$x),]
    
    y_lim_1 = producer_popularity_1[1,2]
    y_lim_2 = producer_popularity_2[1,2]
      
    y_lim = max(y_lim_1, y_lim_2)
    
    print(paste("Popular producer 1 size: ", nrow(producer_popularity_1) ))
    print(paste("Popular producer 2 size: ", nrow(producer_popularity_2) ))
    
    
    print("END producer reactive")
    
    
    list(
        producer_pop_1 = producer_popularity_1,
        producer_pop_2 = producer_popularity_2,
        y_lim = y_lim
      )
    
  })
  
  
  output$cluster1_preset <- renderText({
    Query$g1Type
  })
  
  output$cluster2_preset <- renderText({
    Query$g2Type
  })
  
  output$cluster1_top_store <- renderText({
    stores_group1 <- Query$index_g1
    #stores_group1 <- montreal_group
    print(stores_group1)
    
    gp1_avg_sales <- stores_avg_total_sale[stores_group1]
    max_sales_store <- stores_group1[which.max(gp1_avg_sales)]
    max_sales_store_dtls <- stores[stores$index == max_sales_store,]
    
    paste("Store ID: ", max_sales_store_dtls$store_id, " - Name: ", max_sales_store_dtls$name, " - Average Daily Sales in unit: ", format(floor(max(gp1_avg_sales)), nsmall=0, big.mark=","))
    
  })
  
  output$cluster2_top_store <- renderText({
      stores_group2 <- Query$index_g2
      gp2_avg_sales <- stores_avg_total_sale[stores_group2]
      max_sales_store <- stores_group2[which.max(gp2_avg_sales)]
      max_sales_store_dtls <- stores[stores$index == max_sales_store,]
      
      paste("Store ID: ", max_sales_store_dtls$store_id, " - Name: ", max_sales_store_dtls$name, " - Average Daily Sales in unit: ", format(floor(max(gp2_avg_sales)), nsmall=0, big.mark=","))
            
  })
  
  output$cluster1_total_sales <- renderText({
      stores_group1 <- Query$index_g1
      #stores_group1 <- montreal_group
      g1_total_sales <- stores_total_sale[stores_group1]
      g1_total_sales <- unlist(g1_total_sales, use.names = FALSE)
      g1_total_sales <- sum(g1_total_sales)
      
      paste(format(g1_total_sales, nsmall=0, big.mark=","), "Units")
      
  })
  
  output$cluster2_total_sales <- renderText({
      stores_group2 <- Query$index_g2
      g2_total_sales <- stores_total_sale[stores_group2]
      g2_total_sales <- unlist(g2_total_sales, use.names = FALSE)
      g2_total_sales <- sum(g2_total_sales)
      
      paste(format(g2_total_sales, nsmall=0, big.mark=","), "Units")
  })
  
  #most popular products cluster 1
  output$cluster1_pop_prdcts <- renderDataTable({
        
        print("START renderDataTable 1")
    
    
        sales_gp1 <- cluster1_joined()
        
        #DEBUGG
        #sales_gp1 <- sales_data[[1]]
        #sales_gp1 <- rbindlist(sales_gp1)
        #sales_gp1 <- aggregate(sales_gp1$`sales (in unit)`, by=list(sales_gp1$variant_id), FUN=sum)
        
        ###END DEBUG
        
        #group sales of group1 by variant_id --> total sales for each products
        #sales_gp1 <- aggregate(sales_gp1$`sales (in unit)`, by=list(sales_gp1$variant_id), FUN=sum)
        sales_gp1 <- sales_gp1 %>% mutate(sales_percentage = (x/sum(x)*100))
        sales_gp1$sales_percentage <- round(sales_gp1$sales_percentage, 2) 
        sales_gp1$sales_percentage <- lapply(sales_gp1$sales_percentage, paste0, '%')
        
        row.names(sales_gp1) <- NULL
        
        print(paste("DEBUG: rednerDataTable stores 1 size", nrow(sales_gp1) ))
        names(sales_gp1)[1] <- "variant_id"

        #excluding products from category "Papiers, cones et embouts"
        #excl_prdcts <- c(100000000201, 100000000402, 100000000403, 100000000404, 100030062948, 107717012149, 177170121590)
        #sales_gp1 <- sales_gp1[!(sales_gp1$variant_id %in% excl_prdcts), ]
    
        #sales_gp1 <- sales_gp1[order(-sales_gp1$n), ][1:5, ]
    
        #merge products with products information
        products_merge <- merge(sales_gp1,products, by = "variant_id")
        products_merge <- products_merge[order(-products_merge$x), ]
        #keep columns
        keep = c(1, 2, 3, 6, 7, 8, 11, 12)
        products_merge <- products_merge[keep]
        
        column_name = c("Variant ID", "Unit Sold", "Sales Percentage", "Producer", "Brand", "Name", "Weight", "Category")
        colnames(products_merge) <- column_name
        
        products_merge$`Unit Sold` <- lapply(products_merge$`Unit Sold`, function(x) format(x, nsmall=0, big.mark=",")) 
        #reset dataframe index
        row.names(products_merge) <- NULL
    
        print("END renderDataTable 1")
        

        datatable(products_merge, filter="bottom", options = list(searching = TRUE, pageLength = 5, lengthMenu = list(c(5, 10, 15), c("5", "10", "15")), scrollX = T))
    
  })

  
    
  output$cluster2_pop_prdcts <- renderDataTable({
    
    print("START renderDataTable 2")
    
    sales_gp2 <- cluster2_joined()
    #group sales of group1 by variant_id --> total sales for each products
    #sales_gp2 <- aggregate(sales_gp2$`sales (in unit)`, by=list(sales_gp2$variant_id), FUN=sum)
    sales_gp2 <- sales_gp2 %>% mutate(sales_percentage = (x/sum(x)*100))
    sales_gp2$sales_percentage <- round(sales_gp2$sales_percentage, 2) 
    sales_gp2$sales_percentage <- lapply(sales_gp2$sales_percentage, paste0, '%')
    
    
    print(paste("DEBUG: rednerDataTable stores 2 size", nrow(sales_gp2) ))
    
    
    names(sales_gp2)[1] <- "variant_id"
    #excluding products from category "Papiers, cones et embouts"
    #excl_prdcts <- c(100000000201, 100000000402, 100000000403, 100000000404, 100030062948, 107717012149, 177170121590)
    #sales_gp2 <- sales_gp2[!(sales_gp2$variant_id %in% excl_prdcts), ]
    
    #sales_gp2 <- sales_gp2[order(-sales_gp2$n), ][1:5, ]
    
    #merge top 5 products with products information
    products_merge <- merge(x = sales_gp2, y = products, by = "variant_id")
    products_merge <- products_merge[order(-products_merge$x), ]
    #keep columns
    keep = c(1, 2, 3, 6, 7, 8, 11, 12)
    products_merge <- products_merge[keep]
    
    column_name = c("Variant ID", "Unit Sold", "Sales Percentage", "Producer", "Brand", "Name", "Weight", "Category")
    colnames(products_merge) <- column_name
    
    products_merge$`Unit Sold` <- lapply(products_merge$`Unit Sold`, function(x) format(x, nsmall=0, big.mark=",")) 
    #reset dataframe index
    row.names(products_merge) <- NULL
    
    print("END renderDataTable 2")
    
    
    datatable(products_merge, filter="bottom", options = list(searching = TRUE, pageLength = 5, lengthMenu = list(c(5, 10, 15), c("5", "10", "15")), scrollX = T))
      
  })
  
  #dynamically update slider to show top n producers 
  # !!WARNING!! increase dynamic map refresh time by A LOT
  # observe({
  #   stores_sales1 <- cluster1_joined()
  #   stores_sales2 <- cluster2_joined()
  #   
  #   tryCatch( { 
  #     stores_sales1 <- aggregate(stores_sales1$`sales (in unit)`, by=list(stores_sales1$variant_id), FUN=sum)
  #     stores_sales2 <- aggregate(stores_sales2$`sales (in unit)`, by=list(stores_sales2$variant_id), FUN=sum)
  #     names(stores_sales1)[1] <- "variant_id"
  #     names(stores_sales2)[1] <- "variant_id"
  #     }
  #     , error = function(e) {print("Unable to rename")}
  #   )
  #   
  #   
  #   #DEBUG
  #   # print(paste("DEBUG: observer stores1 size", length(stores_sales1) ))
  #   # print(paste("DEBUG: observer stores2 size", length(stores_sales2) ))
  #   # 
  #   # stores_sales1 <- sales_data[montreal_group]
  #   # stores_sales1 <- rbindlist(stores_sales1)
  #   # 
  #   # stores_sales1 <- aggregate(stores_sales1$`sales (in unit)`, by=list(stores_sales1$variant_id), FUN=sum)
  #   
  #   #stores_sales1 <- aggregate(stores_sales1$`sales (in unit)`, by=list(stores_sales1$variant_id), FUN=sum)
  #   #stores_sales2 <- aggregate(stores_sales2$`sales (in unit)`, by=list(stores_sales2$variant_id), FUN=sum)
  #   
  #   
  #   gp1_stores_unique_products = unique(stores_sales1$variant_id)
  #   gp1_stores_products = products[products$variant_id %in% gp1_stores_unique_products,]
  #   gp1_producers = unique(gp1_stores_products$producer)
  #   
  #   updateSliderInput(session, "cluster1_n_producers",
  #                     label = 'Show top n producers',
  #                     min = 0,
  #                     max = length(gp1_producers),
  #                     value = 10
  #   )
  #   
  #   gp2_stores_unique_products = unique(stores_sales2$variant_id)
  #   gp2_stores_products = products[products$variant_id %in% gp2_stores_unique_products,]
  #   gp2_producers = unique(gp2_stores_products$producer)
  #   
  #   updateSliderInput(session, "cluster2_n_producers",
  #                     label = 'Show top n producers',
  #                     min = 0,
  #                     max = length(gp2_producers),
  #                     value = 10
  #   )
  # })
  
  output$cluster1_pop_producer <- renderPlot({
    
    print("START Producer 1")
    
    producer_popularity <- producer()$producer_pop_1
    y_lim <- producer()$y_lim
    
    print(paste("Popular producer size 1", nrow(producer_popularity)))

    #DEBUG
    #producer_popularity1 <- sales_data[[14]]
    #producer_popularity1 <- rbindlist(producer_popularity1)
    #producer_popularity1 <- aggregate(producer_popularity1$`sales (in unit)`, by=list(producer_popularity1$producer), FUN=sum)
    
    #names(producer_popularity1)[3] <- "variant_id"

    #producer_popularity1 <- merge(x = producer_popularity1, y = products[,c("variant_id", "producer")], by = "variant_id")

    #producer_popularity1 <- aggregate(producer_popularity1$`sales (in unit)`, by=list(producer_popularity1$producer), FUN=sum)
    #producer_popularity1 <- producer_popularity1[order(-producer_popularity1$x),]
    
    #y_lim = 1000000
    ## END DEBUG
    
    show_percentage <- input$pop_producer_percentage_1
    show_n_producer <- input$cluster1_n_producers
    
    producer_popularity <- producer_popularity %>% mutate(sales_percentage = (x/sum(x)*100))
    producer_popularity$sales_percentage <- round(producer_popularity$sales_percentage, 2) 
    producer_popularity <- producer_popularity[1:show_n_producer, ]
    
    #$sales_percentage <- lapply(producer_popularity$sales_percentage, paste0, '%')
    
    names(producer_popularity)[1] <- "Producer"
    names(producer_popularity)[2] <- "Unit_Sold"
    #producer_popularity$Unit_Sold <- lapply(producer_popularity$Unit_Sold, function(x) format(x, nsmall=0, big.mark=",")) 
    
    
    bar_chart_position <- producer_popularity$Producer
    
    print("END Producer 1")
    
    
    if (show_percentage) {
        ggplot(producer_popularity, aes(x=Producer, y=sales_percentage)) +
          geom_bar(stat="identity", fill="chartreuse4") +
          geom_text(aes(label=paste(sales_percentage,"%")), vjust=-0.3, size=3.5)+
          labs(title=paste0('Percentage sales in unit for each producer')) +
          xlab("Producers") +
          ylab("Percentage of Sales") +
          scale_x_discrete(limits = bar_chart_position) +
          ylim(0, 50) +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=60, hjust=1, size=13)) 
    } else {
      ggplot(producer_popularity, aes(x=Producer, y=Unit_Sold)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=format(Unit_Sold, nsmall=0, big.mark=",")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Total sales in unit for each producer')) +
        xlab("Producers") +
        ylab("Unit Sold") +
        scale_x_discrete(limits = bar_chart_position) +
        ylim(0, y_lim) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1, size=13)) 
    }
    
  })
  
  output$cluster2_pop_producer <- renderPlot({
    
    print("START Producer 2")
    
    
    producer <- producer()
    producer_popularity <- producer$producer_pop_2
    y_lim <- producer$y_lim
    
    print(paste("Popular producer size 2", nrow(producer_popularity)))
    
    #stores_sales <- cluster2_joined()
    
    show_percentage <- input$pop_producer_percentage_2
    
    #stores_sales <- aggregate(stores_sales$`sales (in unit)`, by=list(stores_sales$variant_id), FUN=sum)

    show_n_producer <- input$cluster2_n_producers
    
    producer_popularity <- producer_popularity %>% mutate(sales_percentage = (x/sum(x)*100))
    producer_popularity$sales_percentage <- round(producer_popularity$sales_percentage, 2) 
    
    producer_popularity <- producer_popularity[1:show_n_producer, ]
    
    #producer_popularity$sales_percentage <- lapply(producer_popularity$sales_percentage, paste0, '%')
    
    
    names(producer_popularity)[1] <- "Producer"
    names(producer_popularity)[2] <- "Unit_Sold"
    #producer_popularity$Unit_Sold <- lapply(producer_popularity$Unit_Sold, function(x) format(x, nsmall=0, big.mark=",")) 
    
    
    bar_chart_position <- producer_popularity$Producer
    
    print("END Producer 2")
    
    
    if (show_percentage) {
      ggplot(producer_popularity, aes(x=Producer, y=sales_percentage)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=paste(sales_percentage,"%")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Percentage sales in unit for each producer')) +
        xlab("Producers") +
        ylab("Percentage of Sales") +
        scale_x_discrete(limits = bar_chart_position) +
        ylim(0, 50) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1, size=13)) 
    } else {
      ggplot(producer_popularity, aes(x=Producer, y=Unit_Sold)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=format(Unit_Sold, nsmall=0, big.mark=",")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Total sales in unit for each producer')) +
        xlab("Producers") +
        ylab("Unit Sold") +
        scale_x_discrete(limits = bar_chart_position) +
        ylim(0, y_lim) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1, size=13)) 
    }
    
  })

  

}


# Run the application 
shinyApp(ui = ui, server = server)




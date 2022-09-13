#
# author: Jean Parenty
#
# NOTE 
# store with id 22,, don't get graph updated probably because observer failed to get list of products
#

library(htmlwidgets)
library(leaflet)
library(gtools)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(tidyquant)
library(hrbrthemes)
library(stringr)
library(readr)
library(hash)
library(scales)
library(DT)
library(shinyWidgets)

options(scipen=999)


if (file.exists("appdatacache.Rdata")) {
  load(file="appdatacache.Rdata")
#if no cache file of sales data, will read sales data from SQDC_cleam_data dir
} else {
  
  ###loading data
  #get sales dfs path
  data_path = "../../SQDC_clean_data/inventory/store_inventory_by_day_by_product/"
  sale_path <- list.files(path = data_path, pattern = ".csv$")
  sale_path <- mixedsort(sale_path)
  
  products = read_csv("../../SQDC_clean_data/products.csv")
  products$name_bis = ifelse(products$strain == "Melange", products$name, products$strain)
  products <- products %>% relocate(name_bis, .before = name)
  stores = read_csv("../../SQDC_clean_data/stores.csv")
  names(stores)[1] <- "id"
  
  #create a list of sales dfs of length 78
  sales_data <- list()
  avg_sales <- list()
  stores_total_sales <- list()
  for (i in seq_along(sale_path)){
    sales_data[[i]] <- read_csv(paste0(data_path, sale_path[i]))
    data <- sales_data[[i]]
    #data$last_updated <- as.Date(data$last_updated)
    
    data_price <- merge(x=data, y = products[,c("display_price", "variant_id")], by="variant_id")
    data_price$`sales (in dol)`  <- data_price$display_price * data_price$`sales (in unit)`
    total_sales = sum(data_price$`sales (in dol)`)
      
       
    store_avg_sale = aggregate(data$`sales (in unit)`, by = list(data$last_updated), mean)
    #store_avg_sale$Group.1 <- as.Date(store_avg_sale$Group.1)
    
    avg_sales[[i]] = store_avg_sale
    stores_total_sales[[i]] = total_sales
  }
  
  avg_total_sales <- round(mean(unlist(stores_total_sales)),2)
  
  #store name
  store_index <- gsub("_inventory_by_day_by_product.csv$", "", sale_path)
  store_index <- store_index %>% str_replace("store", "")
  store_name <- stores$name
  
  store_index <- paste(store_index, store_name)
  
  products$id = seq(1, 961, 1)
  
  product_name <- data.frame(variant_id = products$variant_id)
  product_name$name  <- paste(products$id ,products$producer , "-", products$name_bis)
  
  save.image(file="appdatacache.Rdata")
}


#######################################


topWellPanelStyle <-
  "display: flex;
  justify-content: space-evenly;"

topWellPanelStyleSpaceBetween <-
  "display: flex;
  justify-content: space-between;
  align-items: center"

panelBackground <- 
  "background-color: ghostwhite;
    border-radius: 5px;
    box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
    width: inherit;
    //margin-left: -15px;
    //margin-right: -15px;
    padding: 15px;"

panelBackgroundMargin <- 
  "background-color: ghostwhite;
    border-radius: 5px;
    box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
    width: inherit;
    padding: 15px;
    margin: 0px;"

###building the app
ui <- dashboardPage( skin = "green",
    dashboardHeader(title = "SQDC - McGill"),
    dashboardSidebar(
      sidebarMenu(
          menuItem(text= "  Store Selection", tabName = "store_details", icon = icon("store")),
          menuItem(text = "Producers Sales", tabName = "store_producer", icon = icon("bong")),
          menuItem(text = "Products Catalog", tabName = "store_product", icon = icon("list-alt")),
          menuItem(text = "Products Sales", tabName = "store_comparator", icon = icon("filter"))
          
        )
    ),
    
    dashboardBody(
      
      chooseSliderSkin("Flat"),
      #setSliderColor(c("#458B00", "#458B00", "#458B00", "#458B00"), c(1, 2, 3, 4)),
      setSliderColor(c("green", "green", "green", "green"), c(1, 2, 3, 4)),
      
      
      tags$head( 
        tags$style(HTML(
          ".main-sidebar {
            font-size: 20px; 
          }
          .row {
            margin: 0px;
          }
          .well {
            margin-bottom: 0px;
          }"
          ))
      ),
      tags$head(
        tags$style(HTML("
                  .my_table .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
                    text-align: center !important;
                    padding: 8px;
                    line-height: 1.42857143;
                    vertical-align: top;
                    border-top: 2px solid #458B00; 
                  }
                  .table>thead>tr>th {
                    color: white;
                  }
                  .table>thead {
                    background: green;
                    
                  }
                  .table {
                    border-radius: 5px;
                    overflow: hidden;
                  }
                  "))
      ),
      
      tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML(
          ".h1, .h2, .h3, h1, h2, h3 {
                      margin-top: 10px;
                      margin-bottom: 10px;
                    }
                    .shiny-text-output {
                      font-weight: bold;
                      display: inline-block;
                      background-color: white;
                      padding: 5px;
                      border-radius: 5px;
                      color: green;
                    }
                    .store-selector .form-group:nth-child(1), .store-selector .form-group:nth-child(2) {
                      width: 30%;
                    }
                    
                    .producer-plot-header .form-group:nth-child(n) {
                      width: 43%;
                    }
                    
                    .plot-header {
                      display: flex;
                      align-items: center;
                      
                    }
                    
                    //.plot-header div.form-group:nth-child(1) {
                    //  margin-left: auto;
                    //}
                    
                    #product {
                      width: 100%;
                      background-color: ghostwhite;
                      border-radius: 5px;
                      margin-top: 2%;
                      padding: 0px;
                      box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
                    }
                    #product th {
                      text-align: center;
                    }
                    #save_plot {
                      color: white;
                      font-size: 16px;
                      font-weight: 600;
                      background-color: green;
                      box-shadow: rgba(0, 0, 0, 0.1) 0px 3px 8px;
                      margin: auto;
                    }
                    #save_plot:hover {
                      color: green;
                      background-color: white;
                      border-color: green;
                      text-decoration: none;
                    }
                    .infoBox {
                      background-color: white;
                      text-align: center;
                      padding: 15px;
                      border-radius: 5px;
                      margin: 12px;
                      box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
                    }
                    .infoBoxWidth {
                      background-color: white;
                      text-align: center;
                      padding: 15px;
                      border-radius: 5px;
                      margin: 12px;
                      box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
                      width: 50%;
                    }
                    
                    .infoBoxMaxWidth {
                      background-color: white;
                      text-align: center;
                      padding: 15px;
                      border-radius: 5px;
                      margin: 12px;
                      box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
                    }
                    
                    .infoBox h3, .infoBoxWidth h3, .infoBoxMaxWidth h3 {
                      font-weight: 500;
                      color: #0c3e00;
                    }
                    .infoBox .shiny-text-output, .infoBoxWidth .shiny-html-output, .infoBoxWidth .shiny-text-output {
                      font-size: 35px;
                    }
                    "
        ))
      ),
      tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML(
          ".row {
                      //display: -webkit-box;
                      //display: -webkit-flex;
                      //display: -ms-flexbox;
                      display:         flex;
                      //flex-wrap: wrap;
                    }
                    h2 {
                      color: green;
                      font-weight: bold;
                    }"
        ))
      ),
      tabItems(
        
         #tab item
        tabItem(tabName = 'store_details',
        
                sidebarLayout(
                    #store details - store information
                    sidebarPanel(
                      h2("Store Information"),
                      selectInput("st_dtls_store",
                          label = "Choose a store: ",
                          choices = store_index,
                          selected = store_index[1]
                      ),
                      h4("Store Name"),
                      textOutput('st_dtls_name'),
                      h4("Store Adress"),
                      textOutput('st_dtls_area'),
                      h4("Reviews"),
                      textOutput('st_dtls_review'),
                      h4("Location"),
                      leafletOutput("st_dtls_map")
                      
                      
                    ),
                    
                    mainPanel(class="main-panel", style = panelBackground, 
                      
                      fluidRow(
                        div(class = "infoBox",
                            h3("Unique products"),
                            textOutput('st_dtls_nmbr_prdcts'), 
                        ),
                        div(class = "infoBox",
                            h3("Most Popular Product"),
                            textOutput('st_dtls_most_popular_product'), 
                        ),
                        div(class = "infoBox",
                            h3("Most Popular Strain"),
                            textOutput('st_dtls_most_popular_strain'), 
                        ),
                        div(class = "infoBox",
                            h3("Most Popular Producer"),
                            textOutput('st_dtls_most_popular_producer'), 
                        ),
                        
                      ),
              
                      fluidRow(
                        div(class = "infoBoxWidth",
                            h3("Total Sales"),
                            textOutput('st_dtls_total_sales'), 
                        ),
                        div(class = "infoBoxWidth",
                            h3("Above/Under Average"),
                            htmlOutput('st_dtls_average_position'), 
                        ),
                        
                      ),
                      
                      div(class = "infoBoxMaxWidth",
                          h3("Store Daily Total Sales"),
                          plotOutput("st_dtls_plot_total_sales")
                      ),
                    )
                    
                ), #sidebarLayout
                
          
        ),  #tab item
        
        tabItem(tabName = "store_producer",
                
                div(style = panelBackgroundMargin, 
                  div(class="plot-header",  
                      h2("Producers\' sales distribution"),
                      div(style = "margin-left: auto;",
                      checkboxInput("producer_average", "Show values as percentage", value = FALSE),
                      ),
                      sliderInput("st_dtls_n_producers", label = 'Show top n producers',
                                  min = 0, max = 20, value = 5)
                      
                  ),
                  plotOutput('st_dtls_mst_pop_prod'),
                ),
                br(),
                
                div( style = panelBackgroundMargin,
                  fluidRow(
                    column(2, h2('Sales by producer' )),
                    column(6,
                           tags$div(class = "producer-plot-header", style = topWellPanelStyle, 
                               selectInput('st_dtls_producer', 
                                           label= 'Choose producers',
                                           choices = c()
                               ),
                               selectInput("st_dtls_category",
                                           label = "Choose categories",
                                           choices = c(),
                                           multiple=TRUE
                               )
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           fluidRow(
                             plotOutput('st_dtls_sales')
                           ),
                    ),
                    
                    column(6,
                           fluidRow(
                             plotOutput('st_dtls_producer_sales')
                           ),
                    )
                    
                  )
                )
                
                        
        ), #tabItem
        
        tabItem(tabName = 'store_product', 
                fluidPage(
                  div(style = panelBackground,
                      div(style = topWellPanelStyleSpaceBetween,
                          h2("Store's Products Strain and Category distribution: "),
                          checkboxInput("pop_str_cat_percentage", "Show values as percentage", value = FALSE),
                          sliderInput("st_dtls_n_categories", label = 'Show top n categories',
                                      min = 0, max = 20, value = 5)
                      ),
                      
                      br(),
                      
                      fluidRow(
                        column(6,
                               plotOutput('st_dtls_mst_pop_str'),                      
                               
                        ),
                        column(6,
                               plotOutput('st_dtls_mst_pop_cat')
                        )
                      ),
                  ),
                  
                  br(),
                  
                  div( style = panelBackground,
                       h2("Most Popular Products"),
                       dataTableOutput('st_dtls_mst_pop_prdct')
                  )
                  
                  
                ) #fluid page    
        ), #tab item
        
        tabItem(tabName = 'store_comparator',
                fluidPage(
                  
                  fluidRow(
                    column(style="padding-left: 0px; ", 6, 
                           wellPanel(style = "height: 100%; margin-bottom: 0px; box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;",
                                     h2("Sales by Store and by Product:"),
                                     helpText("Compare selected store product's sales with another store:"),
                                     
                                     div(class = "store-selector", style = topWellPanelStyle, 
                                         
                                         selectInput("store1", 
                                                     label = "Selected store: ",
                                                     choices = store_index,
                                                     selected = store_index[1]),
                                         
                                         selectInput("store2", 
                                                     label = "Choose a store: ",
                                                     choices = store_index,
                                                     selected = store_index[2]),
                                         
                                         #updated by the observer
                                         selectInput("product", 
                                                     label = "Choose a product",
                                                     choices = c()),
                                         
                                     )
                                     
                           )
                    ),
                    column(style="padding-right: 0px;", 6, 
                           wellPanel(style = "height: 100%; margin-bottom: 0px; box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;",
                                     div(style = topWellPanelStyle,
                                         column(4, style = "display: flex; flex-direction: column; align-self: baseline; border-right: 1px solid green;",
                                                checkboxInput("show_moving_average", "Show moving average", value = FALSE),
                                                
                                                sliderInput("moving_average",
                                                            "Moving Average:",
                                                            min = 2,
                                                            max = 20,
                                                            value=5, step=1),   
                                         ),
                                         column(4, style = "display: flex; flex-direction: column; align-self: baseline; ",
                                                
                                                checkboxInput("inventory_line", "Show product inventory", value = FALSE),
                                                
                                                checkboxInput("store_average", "Show store average", value = FALSE),
                                                
                                         ),
                                         column(4, style = "display: flex; flex-direction: column; border-left: 1px solid green; padding-left: 3%; ",
                                                
                                                sliderInput("date_slider",
                                                            "Dates:",
                                                            min = as.Date("2020-06-22","%Y-%m-%d"),
                                                            max = as.Date("2021-08-31","%Y-%m-%d"),
                                                            value=c(as.Date("2020-06-22"),as.Date("2021-08-31")),
                                                            timeFormat="%Y-%m-%d"),
                                                actionButton("save_plot", "Save plots")
                                         ),     
                                         
                                         
                                     )
                           )
                    )
                  ), #fluidRow
                  
                  fluidRow(style = "margin-top: 2%;
                            display: flex;
                            justify-content: space-around;
                            background-color: ghostwhite;
                            box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
                            padding: 15px;
                            border-radius: 5px;",
                           column( 6,
                                   fluidRow(
                                     plotOutput("sales_store1")
                                   ),
                           ),
                           
                           column( 6,
                                   fluidRow(
                                     plotOutput("sales_store2")
                                   ),
                           )
                           
                  ), #fluid row
                  
                  #dataTableOutput("product"),
                  tags$div(
                    class="my_table", # set to custom class
                    tableOutput("product"),
                  ),
                  
                ) #page
        ) #tab item
      ) #tab items
    ) #dashboard body
  ) #page

# Server logic ----
server <- function(input, output, session) {
  
  #####Store sales Page######
  previous_product <- ""
  #dynamically update selectInput widget for product depending on the store's unique product id 
  observe({
  
    store1 <- match(input$store1, store_index)
    store2 <- match(input$store2, store_index)
  
    print(paste("observer1", store1))
    print(paste("observer2", store2))
    
    
    data1 = sales_data[[store1]]
    data2 = sales_data[[store2]]
    
    #DEBUG
    #data1 = sales_data[[1]]
    #data2 = sales_data[[2]]
    #END DEBUG
    
    product_data1 = unique(data1$variant_id)

    product_data2 = unique(data2$variant_id)
    
    merge_product = Reduce(intersect, list(product_data1, product_data2))
    
    products_interect = product_name[product_name$variant_id %in% merge_product,]

    print(paste("Previous Product", previous_product))
    print(paste("Product intersect size", nrow(products_interect)))
    
    # Can also set the label and select items
    if (previous_product != "" && previous_product %in% products_interect$name) {
      updateSelectInput(session, "product",
                        label = paste0("Choose a product (", length(products_interect$name), " total products):"  ),
                        choices = products_interect$name,
                        selected = previous_product)
    } else {
      updateSelectInput(session, "product",
                        label = paste0("Choose a product (", length(products_interect$name), " total products):"  ),
                        choices = products_interect$name,
                        selected = products_interect$name[1])
    }
    
  })
  
  observe({

    previous_product <<- input$product 
    
  })
  
  output$sales_store1 <- renderPlot({
    
    print(paste("input", input$store1))
    print(paste("input", input$product))
    
    store <- match(input$store1, store_index)
    name_product <- input$product
    
    product <- product_name[product_name$name == name_product, ]$variant_id

    print(paste('store', store))
    
    #store <- 68
    #product <- 100000000201
    
    show_quantity <- input$inventory_line
    show_moving_average <- input$show_moving_average
    show_store_average <- input$store_average
    
    date_filter <- input$date_slider
    print(input$date_slider)
    print(input$date_slider[1])
    print(input$date_slider[2])
    
    data <- sales_data[[store]]
    #data$last_updated <- as.Date(data$last_updated)
    
    avg_sale = avg_sales[[store]]
  
    data <- data[data$variant_id == product, ]
    #data$last_updated <- as.Date(data$last_updated)
    
    avg_sale_keep = data$last_updated
    avg_sale_keep = avg_sale[avg_sale$Group.1 %in% avg_sale_keep, ]
    data$store_avg_sale = avg_sale_keep$x
    
    #get max and min sales for both shops to have same y lim on both plots
    data1 <- sales_data[[match(input$store2, store_index)]]
    data1 <- data1[data1$variant_id == product, ]$`sales (in unit)`
    
    max_data = max(data$`sales (in unit)`)
    max_data1 = max(data1)
    
    data$last_updated = as.Date(data$last_updated)
    
    # c(input$date_slider[1],input$date_slider[2])
    # c(as.Date("2020-06-22"),as.Date("2021-08-31"))
    
    #initiate plot with no lines
    sales_plot <- ggplot(data, aes(x=last_updated, y=`sales (in unit)`)) +
      ggtitle(paste0(input$store1, ", ", name_product, " Sales")) +
      xlab("") +
      ylab("sales in unit") +
      theme_ipsum() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(limits = as.Date(c(input$date_slider[1],input$date_slider[2]))
                   ,date_breaks = "1 month", date_labels = "%b-%y")
    
    if (show_quantity & show_moving_average) {
      sales_plot <- sales_plot + geom_line(aes(colour = "Product Sales"), linetype = "dotdash") + 
        geom_ma(aes( colour = "Moving Average"), ma_fun = SMA, n = input$moving_average, linetype="solid") +
        geom_line(aes(y = quantity, colour = "Product Inventory")) 
      
    } else if (show_quantity) {
      sales_plot <- sales_plot + geom_line(aes(colour = "Product Sales")) + 
        geom_line(aes(y = quantity, colour = "Product Inventory")) 
      
    } else if (show_moving_average) {
      sales_plot <- sales_plot + geom_line( aes(colour = "Product Sales"), linetype = "dotdash") + 
        geom_ma(aes(colour = "Moving Average"), ma_fun = SMA, n = input$moving_average, linetype="solid") 
      
    } else {
      sales_plot <- sales_plot + geom_line(aes(colour = "Product Sales")) 
    }
    
    #ylim for quantity are way higher than ylim for products sales
    if (!show_quantity) {
      sales_plot <- sales_plot + ylim(0, max(c(max_data, max_data1))) 
    }
    
    if (show_store_average) {
      sales_plot <- sales_plot + geom_line(aes(y = store_avg_sale, colour = "Store avg Sales"), linetype="solid") 
      
    }
    #add legend for all lines present in sales_plot
    sales_plot + scale_color_manual(name = "Legend", values = c("Product Sales" = "chartreuse4", "Store avg Sales" = "orange", "Moving Average" = "purple", "Product Inventory" = "red")) + 
      guides(fill = guide_legend(nrow = 1))+
      theme(legend.text=element_text(size=14)) +
      theme(legend.position="bottom") +
      theme(panel.grid.minor = element_blank(),
          panel.background = element_blank())

    # sales_plot + scale_color_manual(name = "Legend", values = c("Product Sales" = "chartreuse4", "Store avg Sales" = "orange", "Moving Average" = "purple", "Product Inventory" = "red")) + 
    #   guides(fill = guide_legend(nrow = 1))+
    #   theme(plot.margin=unit(c(1,1,4,0.5),"cm"))+
    #   theme(legend.position=c(0.85,-0.7))
    
    
  })
  
  output$sales_store2 <- renderPlot({
    
    print(paste("input", input$store2))
    print(paste("input", input$product))
    
    store <- match(input$store2, store_index)
    name_product <- input$product
    
    product <- product_name[product_name$name == name_product, ]$variant_id
    
    #store <- 1
    #product <- 826966011504
    
    show_quantity <- input$inventory_line
    show_moving_average <- input$show_moving_average
    show_store_average <- input$store_average
    
    date_filter <- input$date_slider
    print(input$date_slider)
    print(input$date_slider[1])
    print(input$date_slider[2])
    
    #get store1 data
    data <- sales_data[[store]]

    #get store1 avg sales
    avg_sale = avg_sales[[store]]
    
    #get store1 product data
    data <- data[data$variant_id == product, ]
    
    avg_sale_keep = data$last_updated
    avg_sale_keep = avg_sale[avg_sale$Group.1 %in% avg_sale_keep, ]
    data$store_avg_sale = avg_sale_keep$x
    
    #get max and min sales for both shops to have same y lim on both plots
    data1 <- sales_data[[match(input$store1, store_index)]]
    data1 <- data1[data1$variant_id == product, ]$`sales (in unit)`
    
    max_data = max(data$`sales (in unit)`)
    max_data1 = max(data1)
    
    data$last_updated = as.Date(data$last_updated)
    
    # c(input$date_slider[1],input$date_slider[2])
    # c(as.Date("2020-06-22"),as.Date("2021-08-31"))
    
    #initiate plot with no lines
    sales_plot <- ggplot(data, aes(x=last_updated, y=`sales (in unit)`)) +
      ggtitle(paste0(input$store2, ", ", name_product, " Sales")) +
      xlab("") +
      ylab("sales in unit") +
      theme_ipsum() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(limits = as.Date(c(input$date_slider[1],input$date_slider[2]))
                   ,date_breaks = "1 month", date_labels = "%b-%y")
    
    if (show_quantity & show_moving_average) {
      sales_plot <- sales_plot + geom_line(aes(colour = "Product Sales"), linetype = "dotdash") + 
        geom_ma(aes( colour = "Moving Average"), ma_fun = SMA, n = input$moving_average, linetype="solid") +
        geom_line(aes(y = quantity, colour = "Product Inventory")) 
      
    } else if (show_quantity) {
      sales_plot <- sales_plot + geom_line(aes(colour = "Product Sales")) + 
        geom_line(aes(y = quantity, colour = "Product Inventory")) 
      
    } else if (show_moving_average) {
      sales_plot <- sales_plot + geom_line( aes(colour = "Product Sales"), linetype = "dotdash") + 
        geom_ma(aes(colour = "Moving Average"), ma_fun = SMA, n = input$moving_average, linetype="solid") 
      
    } else {
      sales_plot <- sales_plot + geom_line(aes(colour = "Product Sales")) 
    }
    
    if (show_store_average) {
      sales_plot <- sales_plot + geom_line(aes(y = store_avg_sale, colour = "Store avg Sales"), linetype="solid") 
    }
    
    #ylim for quantity are way higher than ylim for products sales
    if (!show_quantity) {
      sales_plot <- sales_plot + ylim(0, max(c(max_data, max_data1))) 
    }
    #add legend for all lines present in sales_plot
    sales_plot + scale_color_manual(name = "Legend", values = c("Product Sales" = "chartreuse4", "Store avg Sales" = "orange", "Moving Average" = "purple", "Product Inventory" = "red")) + 
      guides(fill = guide_legend(nrow = 1))+
      theme(legend.text=element_text(size=14)) +
      theme(legend.position="bottom") +
      theme(panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
  })
  
  output$product <- renderTable({
    
    product <- input$product
    product <- product_name[product_name$name == product, ]$variant_id
    
    product_data = products[products$variant_id == product, ]
    
    #drop unnecessary columns 
    
    product_data = product_data[-c(1, 3, 7, 8, 13, 20, 21, 28)] 
    column_name = c("Product ID", "Producer", "Brand", "Name", "Weight", "Category", "Product Type", "Strength", "Display Price", "Price Per Gram", "CBD Min", "CBD Max", "THC Min", "THC Max", "Natural Aroma", "Terpenes", "Packaging", "Dominant Effect", "Positive Effect", "Negative Effect")
    colnames(product_data) <- column_name
    
    product_data
    
    #datatable(product_data, options = list(searching = FALSE, pageLength = 5,lengthMenu = FALSE, scrollX = T))

  })
  
  output$store1 <- renderDataTable({
    
    store <- match(input$store1, store_index)
    store_data <- stores[stores$id == store, ]
    
    #drop unnecessary columns 
    store_data = store_data[-c(1, 2, 5, 6, 7, 12, 17, 18)] 
    
    datatable(store_data, options = list(searching = FALSE, pageLength = 5,lengthMenu = FALSE, scrollX = T))
    
  })
  
  output$store2 <- renderDataTable({
    
    store <- match(input$store2, store_index)
    store_data <- stores[stores$id == store, ]
    
    #drop unnecessary columns 
    store_data = store_data[-c(1, 2, 5, 6, 7, 12, 17, 18)] 
    
    datatable(store_data, options = list(searching = FALSE, pageLength = 5,lengthMenu = FALSE, scrollX = T))
    
  })
  
  
  output$store1_map <- renderLeaflet({
    store <- match(input$store1, store_index)
    store_data <- stores[stores$id == store, ]

    lat = store_data$latitude
    long = store_data$longitude
    
    greenLeafIcon <- icons(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
      iconWidth = 25, iconHeight = 41
    )
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%  
      setView(lng = lat, lat = long,  zoom = 10) %>% 
      addMarkers(lng = lat, lat = long, popup=store_data$name, icon = greenLeafIcon)
    
  })
  
  output$store2_map <- renderLeaflet({
    store <- match(input$store2, store_index)
    store_data <- stores[store, ]
    
    lat = store_data$latitude
    long = store_data$longitude
    
    greenLeafIcon <- icons(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
      iconWidth = 25, iconHeight = 41
    )
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%  
      setView(lng = lat, lat = long,  zoom = 10) %>% 
      addMarkers(lng = lat, lat = long, popup=store_data$name, icon = greenLeafIcon)
    
  })
  
  observeEvent(input$save_plot, {
    
    store <- input$store1
    product <- input$product
    product <- product_name[product_name$name == product, ]$variant_id
    
    path = "./plots/"
    
    filename = paste0(store1, "_",product, "_sales.pdf")
    #save the latest rendered plot with ggplot (sales chart output$sales)
    ggsave(paste0(path, filename))
    
  })
  
  ######End of store sales######
  
  
  ######Beginning of Store details#####
  
  store_data <- reactive({
    store <- match(input$st_dtls_store, store_index)
    store <- sales_data[[store]]
    store
  })
  
  store_dtls <- reactive({
    #grab the store matching row from stores.csv ()
    store <- match(input$st_dtls_store, store_index)
    store_detail <- stores[store,]
    #store details row (address, review, location, ...)
    store_detail
  })
  
  store_products <- reactive({
    store_sales <- store_data()
    
    store_unique_products = unique(store_sales$variant_id)
    store_products = products[products$variant_id %in% store_unique_products,]
    store_products
    
  })
  
  observeEvent(input$st_dtls_store, {
    
    choice <- input$st_dtls_store
    
    store1 <- match(choice, store_index)
    
    
    updateSelectInput(session, "store1" ,
                      label = "Selected store: ",
                      choices = store_index,
                      selected = choice)
    
    if (store1 == 78) {
      updateSelectInput(session, "store2" ,
                        label = "Chose a store: ",
                        choices = store_index,
                        selected = store_index[store1-1])
    } else {
      updateSelectInput(session, "store2" ,
                        label = "Chose a store: ",
                        choices = store_index,
                        selected = store_index[store1+1])
    }
    
    
  })
  
  observe({
    store_products <- store_products()
    
    producer = unique(store_products$producer)
    
    updateSelectInput(session, "st_dtls_producer",
                      label = paste0("Choose a producer (", length(producer), " producers for this shop):"  ),
                      choices = producer,
                      selected = producer[1]
                      )
    
    updateSliderInput(session, "st_dtls_n_producers",
                      label = 'Show top n producers',
                      min = 0,
                      max = length(producer),
                      value = 10
    )
    
    categories = unique(store_products$category)
    
    updateSliderInput(session, "st_dtls_n_categories",
                      label = 'Show top n categories',
                      min = 0,
                      max = length(categories),
                      value = 5
    )
    
  })
  
  observe({
    producer <- input$st_dtls_producer
    
    store_products <- store_products()
    producer_products = store_products[store_products$producer == producer, ]
    categories = unique(producer_products$category)
    categories = categories[!is.na(categories)]
    
    updateSelectInput(session, "st_dtls_category",
                      label = paste0("Choose a category (",length(categories)," categories for selected producer): " ),
                      choices = categories, 
                      selected = categories[1])
                      
    
    
  })
  
  ##### INFO BOX OBSERVER FOR RENDERTEXT ######
  observe({
    store_data <- store_data()
    
    #DEBUG
    #store_data <- sales_data[[1]]
    #END DEBUG
    
    store <- store_dtls()
    total_sales <- 0
    
    output$st_dtls_nmbr_prdcts <- renderText({
      unique_product <- unique(store_data$variant_id)
      length(unique_product)
      
    })
    
    output$st_dtls_most_popular_product <- renderText({
      
      products_sales <- aggregate(store_data$`sales (in unit)`, by=list(store_data$variant_id), FUN=sum)
      products_sales <- products_sales[order(-products_sales$x),][1,]
      names(products_sales)[1] <- "variant_id"
      products_sales <- products_sales[1,]
      products_merge <- merge(x = products_sales, y = products, by = "variant_id")
      name <- products_merge$name_bis
      
      name
    })
    
    output$st_dtls_most_popular_strain <- renderText({
    
      products_sales_merge <- merge(x = store_data, y = products[,c("product_type", "variant_id")], by = "variant_id")
      products_sales_merge <- products_sales_merge[!is.na(products_sales_merge$product_type), ]
      
      strain_popularity <- aggregate(products_sales_merge$`sales (in unit)`, by=list(products_sales_merge$product_type), FUN=sum)
      strain_popularity <- strain_popularity[order(-strain_popularity$x),][1,]
      
      strain_popularity[1,1]
    })
    
    output$st_dtls_most_popular_producer <- renderText({
      
      producer_popularity <- merge(x = store_data, y = products[,c("variant_id", "producer")], by = "variant_id")
      
      producer_popularity <- aggregate(producer_popularity$`sales (in unit)`, by=list(producer_popularity$producer), FUN=sum)
      #producer_popularity <- producer_popularity[order(-producer_popularity_1$x),]
      producer_popularity <- producer_popularity[order(-producer_popularity$x),]
      
      producer_popularity[1,1]
    })
    
    output$st_dtls_total_sales <- renderText({
      
      store_id <- store[1,1][[1]]
      total_sales <<- stores_total_sales[[store_id]]
      paste0("$", format(total_sales, nsmall=2, big.mark=","))
 
    })
    
    output$st_dtls_average_position <- renderText({
      
      paste("hello input is","<font color=\"#FF0000\"><b>", input$n, "</b></font>")
      if (total_sales >= avg_total_sales) {
        paste("<font color=\"green\"><b>","Above","</b></font>")
      } else {
        paste("<font color=\"red\"><b>","Under","</b></font>")      }
      
    })
    
    output$st_dtls_plot_total_sales <- renderPlot({
      store_total_sales <- merge(x = store_data, y = products[,c("variant_id", "display_price")], by = "variant_id")
      store_total_sales$`sales (in dol)` <- store_total_sales$`sales (in unit)` * store_total_sales$display_price
      
      store_total_sales <- aggregate(store_total_sales$`sales (in dol)`, by=list(store_total_sales$last_updated), FUN=sum)
      
      ggplot(store_total_sales, aes(x = Group.1, y = x)) +
        xlab('Year-Month') + 
        ylab('Sales in dollars') + 
        geom_ma( ma_fun = SMA, n =5, linetype="solid", color = "chartreuse4") +
        theme_ipsum() +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(labels = comma, limits = c(0, 100000)) +
        scale_x_date(limits = as.Date(c(as.Date("2020-06-22"),as.Date("2021-08-31"))) ,date_breaks = "1 month", date_labels = "%b-%y") +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
    })
    
  })
  
  

  
  output$st_dtls_map <- renderLeaflet({
    store_dtls <- store_dtls()
    
    lat = store_dtls$latitude
    long = store_dtls$longitude
    
    greenLeafIcon <- icons(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
      iconWidth = 25, iconHeight = 41
    )
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%  
      setView(lat, long,  zoom = 10) %>% 
      addMarkers(lng = lat, lat = long, icon = greenLeafIcon)
    
  })
  
  output$st_dtls_name <- renderText({
    store_dtls <- store_dtls()
    store_dtls$name
  })
  
  output$st_dtls_area <- renderText({
    store_dtls <- store_dtls()
    area = store_dtls$area
    adress = store_dtls$address_street
    paste(adress, ", ", area)
  })
  
  output$st_dtls_review <- renderText({
    store_dtls <- store_dtls()
    store_review <- store_dtls$number_of_reviews
    store_avg_review <- store_dtls$average_review
    paste("Number of reviews: ", as.character(store_review), ", average review: ", as.character(store_avg_review))
  })
  
  
  output$st_dtls_mst_pop_prdct <- renderDataTable({
    store_sales <- store_data()
    
    #DEBUG
    #store_sales <- sales_data[[1]]
    #END DEBUG
    
    store_products <- aggregate(store_sales$`sales (in unit)`, by=list(store_sales$variant_id), FUN=sum)
    
    store_products <- store_products %>% mutate(sales_percentage = (x/sum(x)*100))
    store_products$sales_percentage <- round(store_products$sales_percentage, 2) 
    store_products$sales_percentage <- lapply(store_products$sales_percentage, paste0, '%')
    
    names(store_products)[1] = "variant_id"
    names(store_products)[2] = "Unit Sold"
  
    #merge top 5 products with products information
    products_merge <- merge(x = store_products, y = products, by = "variant_id")
    products_merge <- products_merge[order(-products_merge$`Unit Sold`), ]
    products_merge$`Unit Sold` <- lapply(products_merge$`Unit Sold`, function(x) format(x, nsmall=0, big.mark=",")) 
    #keep columns
    
    products_merge = products_merge[-c(4, 5, 9, 10, 14, 15, 22, 23, 30)] 
    column_name = c("Variant ID", "Unit Sold", "Sales Percentage", "Producer", "Brand", "Name", "Weight", "Category", "Product Type", "Display Price", "Price Per Gram", "CBD Min", "CBD Max", "THC Min", "THC Max", "Natural Aroma", "Terpenes", "Packaging", "Dominant Effect", "Positive Effect", "Negative Effect")
    colnames(products_merge) <- column_name
    
    #reset dataframe index
    row.names(products_merge) <- NULL
    
    datatable(products_merge, filter="bottom", options = list(searching = TRUE, pageLength = 5, lengthMenu = list(c(5, 10, 20) , c("5", "10", "20")), scrollX = T))
 
  })
  
  strain_cat_distribution <- reactive({
    store_sales <- store_data()
    
    print(paste("Strain_cat Reactive store sales size: ", nrow(store_sales)))
    
    str_store_sales_merge <- merge(x = store_sales, y = products[,c("product_type", "variant_id")], by = "variant_id")
    str_store_sales_merge <- str_store_sales_merge[str_store_sales_merge$product_type!="", ]
    
    strain_popularity <- aggregate(str_store_sales_merge$`sales (in unit)`, by=list(str_store_sales_merge$product_type), FUN=sum)
    strain_popularity <- strain_popularity %>% mutate(sales_percentage = (x/sum(x)*100))
    strain_popularity$sales_percentage <- round(strain_popularity$sales_percentage, 2) 
    
    cat_store_sales_merge <- merge(x = store_sales, y = products[,c("variant_id", "category")], by = "variant_id")
    
    category_popularity <- aggregate(cat_store_sales_merge$`sales (in unit)`, by=list(cat_store_sales_merge$category), FUN=sum)
    category_popularity <- category_popularity %>% mutate(sales_percentage = (x/sum(x)*100))
    category_popularity$sales_percentage <- round(category_popularity$sales_percentage, 2) 
    
    y_lim = max(max(strain_popularity$x), max(category_popularity$x))
    
    list(
      strain_popularity = strain_popularity,
      category_popularity = category_popularity,
      y_lim = y_lim
    )
  })
  
  output$st_dtls_mst_pop_str <- renderPlot({
    
    
    #DEBUG
    #store_sales <- sales_data[[1]]
    #END DEBUG

    strain_popularity <- strain_cat_distribution()$strain_popularity
    y_lim <- strain_cat_distribution()$y_lim
    
    show_percentage <- input$pop_str_cat_percentage
    
    #strain_popularity$sales_percentage <- lapply(strain_popularity$sales_percentage, paste0, '%')
    
    strain_popularity <- strain_popularity[order(-strain_popularity$x),]
    
    names(strain_popularity)[1] <- "strain"
    names(strain_popularity)[2] <- "unit_sold"
    
    bar_chart_position <- strain_popularity$strain
    
    
    if (show_percentage) {
      ggplot(strain_popularity, aes(x=strain, y=sales_percentage)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=paste0(sales_percentage, "%")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Sales by Product\'s Strain')) +
        xlab("Strain") +
        ylab("Unit Sold") +
        ylim(0, 100) +
        scale_x_discrete(limits = bar_chart_position) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=50, size=15, hjust = 1)) 
    } else {
      ggplot(strain_popularity, aes(x=strain, y=unit_sold)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=format(unit_sold, nsmall=0, big.mark=",")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Sales by Product\'s Strain')) +
        xlab("Strain") +
        ylab("Unit Sold") +
        ylim(0, y_lim) +
        scale_x_discrete(limits = bar_chart_position) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=50, size=15, hjust = 1)) 
    }
  })
  

  output$st_dtls_mst_pop_cat <- renderPlot({

    #DEBUG
    #store_sales <- sales_data[[1]]
    #END DEBUG
    
    show_n <- input$st_dtls_n_categories
    show_percentage <- input$pop_str_cat_percentage
    
    category_popularity <- strain_cat_distribution()$category_popularity
    y_lim <- strain_cat_distribution()$y_lim
    
    category_popularity <- category_popularity[order(-category_popularity$x),][1:show_n, ]
    
    names(category_popularity)[1] <- "category"
    names(category_popularity)[2] <- "unit_sold"
    bar_chart_position <- category_popularity$category
    
    if (show_percentage) {
      ggplot(category_popularity, aes(x=category, y=sales_percentage)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=paste0(sales_percentage, "%")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Sales Product\'s Category')) +
        xlab("Category") +
        ylab("Unit Sold") +
        ylim(0, 100) +
        scale_x_discrete(limits = bar_chart_position) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=30, hjust=1, size=13)) 
      
    } else {
      ggplot(category_popularity, aes(x=category, y=unit_sold)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=format(unit_sold, nsmall=0, big.mark=",")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Sales Product\'s Category')) +
        xlab("Category") +
        ylab("Unit Sold") +
        ylim(0, y_lim) +
        scale_x_discrete(limits = bar_chart_position) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=30, hjust=1, size=13)) 
    }
    
  })
  
  output$st_dtls_mst_pop_prod <- renderPlot({
    store_sales <- store_data()
    
    #DEBUG
    #store_sales <- sales_data[[14]]
    #
    
    show_percentage <- input$producer_average
    show_n <- input$st_dtls_n_producers
    
    producer_popularity <- merge(x = store_sales, y = products[,c("variant_id", "producer")], by = "variant_id")

    producer_popularity <- aggregate(producer_popularity$`sales (in unit)`, by=list(producer_popularity$producer), FUN=sum)
    #producer_popularity <- producer_popularity[order(-producer_popularity_1$x),]
    
    producer_popularity <- producer_popularity %>% mutate(sales_percentage = (x/sum(x)*100))
    producer_popularity$sales_percentage <- round(producer_popularity$sales_percentage, 2) 
    
    producer_popularity <- producer_popularity[order(-producer_popularity$x),][1:show_n, ]
    
    names(producer_popularity)[2] <- "unit_sold"
    names(producer_popularity)[1] <- "producer"
    
    bar_chart_position <- producer_popularity$producer
    
    if (show_percentage) {
      ggplot(producer_popularity, aes(x=producer, y=sales_percentage)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=paste0(sales_percentage, "%")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Sales in percentage for each producer')) +
        xlab("Category") +
        ylab("Unit Sold") +
        ylim(0, 100) +
        scale_x_discrete(limits = bar_chart_position) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=50, hjust=1, size=13)) 
    } else {
      
      ggplot(producer_popularity, aes(x=producer, y=unit_sold)) +
        geom_bar(stat="identity", fill="chartreuse4") +
        geom_text(aes(label=format(unit_sold, nsmall=0, big.mark=",")), vjust=-0.3, size=3.5)+
        labs(title=paste0('Sales in unit for each producer')) +
        xlab("Category") +
        ylab("Unit Sold") +
        ylim(0, max(producer_popularity$unit_sold)) +
        scale_x_discrete(limits = bar_chart_position) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=50, hjust=1, size=13)) 
      
    }
    
    
  })
  
  output$st_dtls_sales <- renderPlot({
    categories <- input$st_dtls_category
    
    store_sales <- store_data()
    
    store_sales <- sales_data[[1]]
    
    store_sales_products <- merge(x = store_sales, y = products[, c("variant_id", "category")], by = "variant_id")
    
    store_sales_products <- store_sales_products[store_sales_products$category %in% categories,]
    
    #groupby day and category
    store_sales_group_day_category <- aggregate(store_sales_products$`sales (in unit)`, by=list(store_sales_products$last_updated, store_sales_products$category), sum)
    store_sales_group_day_category <- store_sales_group_day_category[order(store_sales_group_day_category$Group.1),]
    
    store_sales_max <- store_sales_group_day_category[order(-store_sales_group_day_category$x),][1,]$x

    
    #format date
    store_sales_group_day_category$Group.1 = as.Date(store_sales_group_day_category$Group.1)
    
    ggplot(store_sales_group_day_category, aes(x = Group.1, y = x, color = Group.2, group = Group.2)) +
      xlab('Year-Month') + 
      ylab('Sales in Unit') + 
      labs(title=paste0('Store total sales in unit for each category')) +
      theme_ipsum() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(limits = as.Date(c(as.Date("2020-06-22"),as.Date("2021-08-31"))) ,date_breaks = "1 month", date_labels = "%b-%y") +
      geom_line() +
      ylim(0, store_sales_max) +
      labs(color = "Category")
    
  })
  
  output$st_dtls_producer_sales <- renderPlot({
    producer <- input$st_dtls_producer
    categories <- input$st_dtls_category
    
    store_sales <- store_data()
  
    store_sales_products <- merge(x = store_sales, y = products, by = "variant_id")
   
    #keep only given categories
    store_sales_products <- store_sales_products[store_sales_products$category %in% categories,]
    
    store_total_sales <- aggregate(store_sales_products$`sales (in unit)`, by=list(store_sales_products$last_updated, store_sales_products$category), sum)
    store_sales_max <- store_total_sales[order(-store_total_sales$x),][1,]$x
    

    #keep only given producer
    store_sales_products <- store_sales_products[store_sales_products$producer == producer,]
    
    #group all products by day
    store_sales_group_day_category <- aggregate(store_sales_products$`sales (in unit)`, by=list(store_sales_products$last_updated, store_sales_products$category), sum)
    #sort by date
    store_sales_group_day_category <- store_sales_group_day_category[order(store_sales_group_day_category$Group.1),]
    
    store_sales_group_day_category$Group.1 = as.Date(store_sales_group_day_category$Group.1)
    
    ggplot(store_sales_group_day_category, aes(x = Group.1, y = x, color = Group.2, group = Group.2)) +
      xlab('Year-Month') + 
      ylab('Sales in Unit') + 
      labs(title=paste0(producer, ' sales in unit for each category')) +
      theme_ipsum() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(limits = as.Date(c(as.Date("2020-06-22"),as.Date("2021-08-31"))) ,date_breaks = "1 month", date_labels = "%b-%y") +
      geom_line() +
      ylim(0, store_sales_max) +
      labs(color = "Category")
        
      
  })
 
}

# Run app ----
shinyApp(ui, server)




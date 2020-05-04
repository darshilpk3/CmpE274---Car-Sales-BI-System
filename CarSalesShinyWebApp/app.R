#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(tidyr)
library(tseries)
library(scales)
library(plotly)
library(treemap)
library(treemapify)
library(d3treeR)
library(RJDBC)
#setwd("C:/Users/darsh/OneDrive/Desktop/SJSU/Courses/274")




ui <- navbarPage("Car Sales BI System",
    tabPanel("Top Selling Models",
             fluidPage(
                 # Application title
                 titlePanel(
                     h1("Top Models",align="center")
                 ),
                 
                 hr(),
                 
                 fluidRow(
                     column(3, offset = 4,
                            wellPanel(
                                sliderInput("no_of_models",
                                            "Select the number of top models you want to see:",
                                            min = 1,
                                            max = 25,
                                            value = 10)
                            )
                     )
                 ),
                 br(),
                 fluidRow(
                     plotlyOutput("distPlot")
                 )
             )),
    tabPanel("Sales By Manufacturer",
             fluidPage(
                 titlePanel(
                     h1("Sales By Manufacturer",align="center")
                 ),
                 hr(),
                 fluidRow(
                     column(3, offset = 4,
                            wellPanel(
                                selectInput("car_make_sales_by_manufacturer", 
                                            "Select the Car Manufacturer for comparison between EVs vs Hybrid",
                                            choices=c("AUDI","BMW","CADILLAC","CHRYSLER","FIAT","FORD","HONDA","HYUNDAI",
                                                      "TESLA","CHEVROLET","JAGUAR","KIA","MERCEDES","MINI","MITSUBISHI",
                                                      "NISSAN","PORSCHE","SMART","VOLKSWAGEN","VOLVO","TOYOTA","VW","OTHER"))
                            )
                     )),
                 br(),
                 fluidRow(
                            plotlyOutput("carMakePlot")
                )
             )),
    tabPanel("Car Volume Sales",
             fluidPage(
                 titlePanel(
                     h1("Car Volume Sales",align="center")
                 ),
                 hr(),
                 fluidRow(
                     plotlyOutput("carVolumePlot")
                 )
             )),
    tabPanel("EVs vs Hybrid",
             fluidPage(
                 titlePanel(
                     h1("EVs vs Hybrid",align="center")
                 ),
                 hr(),
                 navlistPanel(
                     tabPanel("By commulative sales",
                              fluidPage(
                                titlePanel(
                                    h3("By Commulative Sales")
                                    ),
                                    br(),
                                    
                                    plotlyOutput("commulativeSales")
                                    
                                ) 
                              ),
                     tabPanel("By Total Sales through Tree Map",
                              fluidPage(
                                  titlePanel(
                                      h3("By Total Sales through Tree Map")
                                  ),
                                  br(),
                                  
                                  d3tree2Output("treeMapPlot")
                                  
                              ) 
                     ),
                     tabPanel("By manufacturer sales",
                              fluidPage(
                                  titlePanel(
                                      h3("By Car Manufacturer")
                                      ),
                                      br(),
                                      fluidRow(
                                          wellPanel(
                                              selectInput("car_make_ev_vs_hybrid", 
                                                          "Select the Car Manufacturer for comparison between EVs vs Hybrid",
                                                          choices=c("AUDI","BMW","CADILLAC","CHRYSLER","FIAT","FORD","HONDA","HYUNDAI",
                                                                    "TESLA","CHEVROLET","JAGUAR","KIA","MERCEDES","MINI","MITSUBISHI",
                                                                    "NISSAN","PORSCHE","SMART","VOLKSWAGEN","VOLVO","TOYOTA","VW","OTHER"))
                                          )             
                                      ),
                                      br(),
                                      
                                      plotlyOutput("manSales")
                                      
                                  ) 
                              ),
                     tabPanel("Compare Manufacturers",
                              fluidPage(
                                  titlePanel(
                                      h3("Compare Manufacturers")
                                  ),
                                  br(),
                                  fluidRow(
                                      column(9,
                                          checkboxGroupInput("car_make_ev_vs_hybrid_manufacturer_comparison", 
                                                      "Select the Car Manufacturer for comparison between EVs vs Hybrid",
                                                      c("AUDI"="AUDI","BMW"="BMW","CADILLAC"="CADILLAC","CHRYSLER"="CHRYSLER","FIAT"="FIAT",
                                                        "FORD"="FORD","HONDA"="HONDA","HYUNDAI"="HYUNDAI","TESLA"="TESLA","CHEVROLET"="CHEVROLET",
                                                        "JAGUAR"="JAGUAR","KIA"="KIA","MERCEDES"="MERCEDES","MINI"="MINI","MITSUBISHI"="MITSUBISHI","NISSAN"="NISSAN",
                                                        "PORSCHE"="PORSCHE","SMART"="SMART","VOLKSWAGEN"="VOLKSWAGEN","VOLVO"="VOLVO","TOYOTA"="TOYOTA","VW"="VW","OTHER"="OTHER"),
                                                      selected = c("AUDI","BMW"), inline = TRUE
                                          )
                                      ),
                                      column(3,
                                             selectInput(
                                                 "car_type_ev_vs_hybrid", 
                                                 "Select the Car Type for comparison between Manufacturers",
                                                 choices=c("Fully Electric","Hybrid")
                                             )
                                      )
                                  ),
                                  br(),
                                  plotlyOutput("compareSales")
                              ) 
                     )
                     )
                 )
             ),
    
    tabPanel("Market Share",
             fluidPage(
                 titlePanel(
                     h1("Market Shares",align="center")
                 ),
                 hr(),
                 navlistPanel(
                     tabPanel("By Total Cars Sold",
                              fluidPage(
                                  titlePanel(
                                      h3("By Total Cars Sold")
                                  ),
                                  br(),
                                  
                                  plotlyOutput("market_total")
                              ) 
                     ),
                     tabPanel("By Month",
                              fluidPage(
                                  titlePanel(
                                      h3("By Month")
                                  ),
                                  br(),
                                  fluidRow(
                                     column(5,
                                      sliderInput("sliderYear",
                                                  "Select the Year for which you want to see market share:",
                                                  min = 2011,
                                                  max = 2018,
                                                  value = 2013)
                                     ),
                                     column(5, offset=1,
                                            selectInput("selMonth", 
                                                        "Select the Month of Year",
                                                        choices=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
                                                                  "SEP","OCT","NOV","DEC")
                                                        )    
                                     )
                                  ),
                                  plotlyOutput("market_month")
                                  
                              ) 
                     )
                 )
             )
    ),
    tabPanel("Forecasting",
             fluidPage(
                 # Application title
                 titlePanel(
                     h1("Forecasting of Electric/Hybrid Sales",align="center")
                 ),
                 
                 hr(),
                 
                 fluidRow(
                     column(5, 
                            wellPanel(
                                sliderInput("forecast_years",
                                            "Select the number of years you want forecast for (from 2018):",
                                            min = 1,
                                            max = 15,
                                            value = 5)
                            )
                     ),
                     column(5, offset=1,
                            wellPanel(
                                selectInput(
                                    "forecast_car_type", 
                                    "Select the Car Type to Forecast Sales for",
                                    choices=c("Fully Electric","Hybrid")
                                )
                            )
                     )
                 ),
                 br(),
                 fluidRow(
                     plotOutput("forecastPlot")
                 ),
                 br(),
                 titlePanel(
                     h3("Forecasting Details")
                 ),
                 fluidRow(
                     plotlyOutput("forecastPlotly")
                 )
                 
             ))
    )
# Define UI for application that draws a histogram

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    setwd("C:/Users/darsh/OneDrive/Desktop/SJSU/Courses/274")
    #Connecting to Cassandra Database by creating a connection
    #drv <- JDBC("org.apache.cassandra.cql.jdbc.CassandraDriver",
                #list.files("C:/Program Files/apache-cassandra-3.11.6/lib/",pattern="jar$",full.names=T))
    #.jaddClassPath("C:/Program Files/apache-cassandra-3.11.6/externaljars")
    
    #casscon <- dbConnect(drv, "jdbc:cassandra://localhost:9160/cmpe274")
    
    #Reading Data from CSV
    #car_sales_csv <- read.csv('CarSales.csv')
    #names(car_sales_csv)
    
    #Data PreProcessing
    #######Removing unwanted Columns
    #car_sales_csv <- subset(car_sales_csv, select = c(CAR.MAKE,CAR.MODEL,CAR.MAKE.AND.MODEL,Year.Month,Sales.Tesla,Sales.Non.Tesla,Car.Type1,Month,Sales,Year,Year.Month..copy.))
    #######Adding a ID Column
    #car_sales_csv$id <- seq.int(nrow(car_sales_csv))
    #######Rearranging Columns
    #car_sales <- car_sales[c(12,1,3,2,7,8,11,9,6,5,10,11)]
    
    #Writing it to a cassandra table
    #dbWriteTable(casscon,"car_sales_data_2",car_sales)
    
    #Reading from Cassandra Table
    #car_sales <- dbGetQuery(casscon, "select * from car_sales_data_2")
    
    #Replacing NA with 0
    #car_sales[is.na(car_sales)] <- 
    car_sales <- read.csv('CarSales.csv')
    attach(car_sales)

    output$distPlot <- renderPlotly({
        top_models <- car_sales %>%
            group_by(CAR.MAKE.AND.MODEL) %>%
            summarise(Sales=sum(Sales)) %>%
            top_n(input$no_of_models,Sales) %>%
            arrange(desc(Sales))
        
        top_models_plot <- ggplot(top_models, aes(x = reorder(CAR.MAKE.AND.MODEL, Sales), y = Sales, fill=Sales)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_continuous(type = "viridis") +
            labs(x ="Car Make and Model", y = "Total Sales", title = paste("Top" ,input$no_of_models,"Selling Models(Year 2010-2019)")) +
            theme(panel.grid.major = element_blank())
        
        ggplotly(top_models_plot)
    })
    
    output$carMakePlot <- renderPlotly({
        
        car_make_comparison <- car_sales %>%
            filter(CAR.MAKE == input$car_make_sales_by_manufacturer) %>%
            group_by(CAR.MODEL,Car.Type1) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(desc(Sales))
            head(car_make_comparison)
        car_make_plot <- ggplot(car_make_comparison, aes(x = CAR.MODEL, y = Sales, fill=Car.Type1)) +
            geom_bar(stat = "identity") +
            labs(x ="Car Model", y = "Total Sales", title = paste("Models sold by" ,input$car_make," (Year 2010-2019)")) +
            theme(axis.text.x = element_text(colour="grey40", size=9, angle=90, hjust=.5, vjust=.5),
                  axis.text.y = element_text(colour="grey40", size=9),
                  strip.text = element_text(size=12),
                  text = element_text(size=12))
        
        ggplotly(car_make_plot)
    })

    
    output$carVolumePlot <- renderPlotly({
        
        car_sales_volume <- car_sales %>%
            group_by(CAR.MAKE.AND.MODEL) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(CAR.MAKE.AND.MODEL,Sales)
        
        
        vol_plot <- ggplot(car_sales_volume, aes(x = CAR.MAKE.AND.MODEL, y = Sales, fill=Sales))+ 
            geom_bar(stat="identity") + 
            scale_fill_continuous() +
            labs(x ="Car Make and Model", y = "Sales", title="Sales Volume by Car Model",
                 caption="Source: Alternative Fuels Data (https://afdc.energy.gov/data/10567)") +
            theme(axis.text.x = element_text(size = 8, angle=90, vjust=0.6), panel.grid.major = element_blank())
        ggplotly(vol_plot)
    })    
    
    output$commulativeSales <- renderPlotly({
        
        comparison_yearly <- car_sales %>%
            group_by(Year,Car.Type1) %>%
            summarise(Sales=sum(Sales)) %>%
            top_n(25,Sales) %>%
            arrange(Year,Car.Type1,desc(Sales))
        head(comparison_yearly)
        
        comparison_yearly_plot <- ggplot(comparison_yearly, aes(x = Year, y = Sales/1000, fill = Car.Type1)) +
            scale_x_continuous(breaks=function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
            geom_area() +
            labs(x ="Year", y = "Total Sales(in 1000s)", title = "Yearly Sales Comparison of EVs and Hybrid") +
            theme(panel.grid.major = element_blank())
        
        ggplotly(comparison_yearly_plot)
    })
    
    output$treeMapPlot <- renderD3tree2({
        
        treemap_df <- car_sales %>%
            group_by(Year,Car.Type1,CAR.MAKE,CAR.MAKE.AND.MODEL) %>%
            summarise(Sales=sum(Sales)) %>%
            top_n(25,Sales) %>%
            arrange(Year,Car.Type1,desc(Sales))
        
        
        treemap_df$year_and_cartype = paste(treemap_df$Car.Type1,treemap_df$Year)
        treemap_df$carmake_and_sales = paste(treemap_df$CAR.MAKE,treemap_df$Sales)

        p <- treemap(treemap_df,
                     index=c("year_and_cartype","CAR.MAKE"),
                     vSize="Sales",
                     type="index",
                     inflate.labels=F,
                     )  
        d3tree2(p,rootname = "Sales through Tree Map")
        
    })
    
    output$manSales <- renderPlotly({
        
        comparison_manly <- car_sales %>%
            filter(CAR.MAKE==input$car_make_ev_vs_hybrid) %>%
            group_by(Car.Type1) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(desc(Sales))
        head(comparison_manly)
        
        comparison_manly_plot <- ggplot(comparison_manly, aes(x = Car.Type1, y = Sales/1000, fill = Car.Type1)) +
            geom_bar(stat="identity") +
            labs(x ="Car Type", y = "Total Sales(in 1000s)", title = paste("Comparison of EVs and Hybrid For",input$car_make2)) +
            theme(panel.grid.major = element_blank())
        ggplotly(comparison_manly_plot)
    })
    
    output$compareSales <- renderPlotly({
        
        comparison_by_man <- car_sales %>%
            filter(CAR.MAKE %in% input$car_make_ev_vs_hybrid_manufacturer_comparison,Car.Type1==input$car_type_ev_vs_hybrid) %>%
            group_by(CAR.MAKE) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(desc(Sales))
        
        comparison_by_man_plot <- ggplot(comparison_by_man, aes(x = CAR.MAKE, y = Sales/1000, fill=CAR.MAKE)) +
            geom_bar(stat="identity") +
            labs(x ="Car Manufacturer", y = "Total Sales(in 1000s)", title = paste("Comparison of",input$car_type,"Sales")) +
            theme(panel.grid.major = element_blank())
        
        ggplotly(comparison_by_man_plot)
    })
    
    output$market_total <- renderPlotly({
        
        market_total_df <- car_sales %>%
            group_by(CAR.MAKE) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(desc(Sales))
        
        totalSales <- sum(car_sales$Sales)
        market_total_df <- market_total_df %>% 
            mutate(AllYears = "2010-2019")
        
        market_total_df_plot <- ggplot(market_total_df, aes(x = AllYears, y = (Sales/totalSales)*100, fill=CAR.MAKE)) +
            geom_bar(stat="identity") +
            labs(x ="Years", y = "Market Share(in %)", title = paste("Market Share By Total Cars Sold")) +
            theme(panel.grid.major = element_blank())
        
        ggplotly(market_total_df_plot)
    })
    
    
    output$market_month <- renderPlotly({
        
        market_month_df <- car_sales %>%
            filter(Year==input$sliderYear,Month==input$selMonth) %>%
            group_by(CAR.MAKE) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(desc(Sales))
            
        
        totalSales <- sum(market_month_df$Sales)
        
        #market_month_df <- market_month_df %>% 
        #    group_by(Year.Month) %>% 
        #    mutate(per=paste0(round(Sales/sum(Sales)*100, 2), "%")) %>% 
        #    ungroup
        
        #tail(market_month_df)
        
        #plot2 <- ggplot(market_month_df, aes(x = input$selMonth, y = reorder((Sales/totalSales)*100,(Sales/totalSales)*100), fill=CAR.MAKE)) +
        market_month_df_plot <- ggplot(market_month_df, aes(x = input$selMonth, y = (Sales/totalSales)*100, fill=CAR.MAKE,)) +
            geom_bar(stat = "identity") +
            labs(x ="Month", y = "Market Share(in %)", title = paste("Monthly Market Share")) +
            theme(panel.grid.major = element_blank())
        
        ggplotly(market_month_df_plot)
    })
    
    output$forecastPlot <- renderPlot({
        
        for_ts <- car_sales %>%
            filter(Car.Type1==input$forecast_car_type, Year<2019, Year>2010) %>%
            group_by(Year, Month) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(Month)
        Qty_ts <- ts(data=for_ts$Sales, start=2011, end=2019, freq=12)
        (fit <- arima(log(Qty_ts), c(1, 0, 0),seasonal = list(order = c(1, 1, 0), period = 12)))
        pred <- predict(fit, n.ahead = input$forecast_years*12)
        #ggplot2::autoplot(2.718^pred$pred, xlab="Year", ylab="Sales")
        ts.plot(Qty_ts,2.718^pred$pred, log = "y", lty = c(1,3),xlab="Year",ylab="Sales")
        
    })
    
    output$forecastPlotly <- renderPlotly({
        
        for_ts <- car_sales %>%
            filter(Car.Type1==input$forecast_car_type, Year<2019, Year>2010) %>%
            group_by(Year, Month) %>%
            summarise(Sales=sum(Sales)) %>%
            arrange(Month)
        Qty_ts <- ts(data=for_ts$Sales, start=2011, end=2019, freq=12)
        (fit <- arima(log(Qty_ts), c(1, 0, 0),seasonal = list(order = c(1, 1, 0), period = 12)))
        pred <- predict(fit, n.ahead = input$forecast_years*12)
        ggplot2::autoplot(2.718^pred$pred, xlab="Year", ylab="Sales")
        
    })
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)


#CREATE KEYSPACE cmpe274
#WITH REPLICATION = { 
#    'class' : 'NetworkTopologyStrategy', 
#    'datacenter1' : 1 
#} ;


#CREATE TABLE car_sales_data ("REC NO" int PRIMARY KEY,"CAR MAKE" text,"CAR MODEL" text,"CAR MAKE AND MODEL" text,"Year Month" text,"Sales Tesla" int,"Sales Non Tesla" int,"Car Type1" text,"Month" text,"Sales" int,"Year" int,"Month No" int);
# CREATE TABLE car_sales_data_2 ("id" int PRIMARY KEY,"CAR.MAKE" text,"CAR.MODEL" text,"CAR.MAKE.AND.MODEL" text,"Year.Month" text,"Sales.Tesla" int,"Sales.Non.Tesla" int,"Car.Type1" text,"Month" text,"Sales" int,"Year" int,"Year.Month..copy." int);
#COPY car_sales_data FROM 'C:\Users\darsh\OneDrive\Desktop\SJSU\Courses\274\CarSalesCleaned.csv' WITH HEADER=TRUE;

library(shiny)
library(magrittr)
library(dplyr)
library(tidyr)
library(scales)
library(data.table)
library(bigmemory)
library(googleVis)
library(highcharter)
library(highchartR)
library(plotly)
suppressPackageStartupMessages(library(googleVis))

shinyServer(function(input, output, session) {
  
  #increase the max file size to be 600MB
  options(shiny.maxRequestSize=600*1024^2)
  
  #upload the data
  get_uploaded_data <- reactive({
    if (is.null(input$data_csv) && is.null(input$data_rds)) {
      # User has not uploaded a file yet
      Dataset <- data.frame()
    } else if(!is.null(input$csv)) {
      Dataset <- read.csv(input$data_csv$datapath, #data.table::fread(), readr::readr(), read.csv
                          stringsAsFactors=FALSE, header = TRUE) # for sample.csv
      
#       Dataset <- read.csv(input$data_own$datapath, #data.table::fread(), readr::readr(), read.csv
#                           stringsAsFactors=FALSE, header = T, 
#                           fileEncoding="UCS-2LE", sep = "\t") #for the large accenture file, worked
#       

      Dataset[,"Paid.Fare"] <- as.numeric(gsub(",", "", Dataset$Paid.Fare))
      Dataset[,"Miles"] <- as.numeric(gsub(",", "", Dataset$Miles))
      Dataset[,"Subtrips"] <- as.numeric(gsub(",", "", Dataset$Subtrips))
      Dataset[,"Base.Fare"] <- as.numeric(gsub(",", "", Dataset$Base.Fare))
      Dataset[,"Tax"] <- as.numeric(gsub(",", "", Dataset$Tax))
    } else{
      Dataset <- readRDS(input$data_rds$datapath)
    }
    summary_filters$data <- Dataset
  })
  
  output$glimpse <- renderDataTable({ get_uploaded_data() })
  
#=====================SUMMARY TAB============================
  summary_by_cabin <- function(df){
    df  %>% 
      group_by(Cabin) %>% 
      summarize(Paid.Fare = round(sum(Paid.Fare), 0), 
                Subtrips = as.integer(sum(Subtrips)), 
                ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
                CPM = round(sum(Paid.Fare)/sum(Miles), 2), 
                Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)), 
                Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips))) %>% 
      arrange(desc(Paid.Fare))
  }
  
  summary_filters <<- reactiveValues(
    data = NULL,
    POS_region = NULL,
    POS_country = NULL,
    carrier_name = NULL,
    carrier_code = NULL,
    trip_type = NULL,
    month_year = NULL,
    bi_direct_region = NULL,
    fiscal_qtr = NULL,
    fiscal_year = NULL,
    bi_direct_country = NULL,
    market = NULL
  )
  
  observe({
    summary_filters$POS_region <<- input$summary_POS_region
    summary_filters$POS_country <<- input$summary_POS_country
    summary_filters$carrier_name <<- input$summary_carrier_name
    summary_filters$carrier_code <<- input$summary_carrier_code
    summary_filters$trip_type <<- input$summary_trip_type
    summary_filters$month_year <<- input$summary_month_year
    summary_filters$bi_direct_region <<- input$summary_bi_direct_region
    summary_filters$fiscal_qtr <<- input$summary_fiscal_qtr
    summary_filters$fiscal_year <<- input$summary_fiscal_year
    summary_filters$bi_direct_country <<- input$summary_bi_direct_country
    summary_filters$market <<- input$summary_market
  })
  
  observe({ 
    Dataset <- get_uploaded_data()
    updateSelectizeInput(session, "summary_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
    updateSelectizeInput(session, "summary_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
    updateSelectizeInput(session, "summary_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
    updateSelectizeInput(session, "summary_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
    updateSelectizeInput(session, "summary_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
    updateSelectizeInput(session, "summary_month_year", choices=unique(Dataset$Month.Year), server = TRUE)
    updateSelectizeInput(session, "summary_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
    updateSelectizeInput(session, "summary_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
    updateSelectizeInput(session, "summary_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
    updateSelectizeInput(session, "summary_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
    updateSelectizeInput(session, "summary_market", choices=unique(Dataset$Market), server = TRUE)
    })
  
  updateDF <-reactive({
    Dataset <- get_uploaded_data()
    if (all(is.null(c(summary_filters$POS_region, summary_filters$POS_country, summary_filters$carrier_name,
                      summary_filters$carrier_code,summary_filters$trip_type,summary_filters$month_year,
                      summary_filters$bi_direct_region,summary_filters$fiscal_qtr,summary_filters$fiscal_year,
                      summary_filters$bi_direct_country,summary_filters$market)))){
      #summary_by_cabin(Dataset)
      Dataset
    }
    filtered <- Dataset
    if (!is.null(summary_filters$POS_region)) {
      filtered <- filtered %>% filter(POS.Region == summary_filters$POS_region)
    }
    if (!is.null(summary_filters$POS_country)) {
      filtered <- filtered %>% filter(POS.Country == summary_filters$POS_country)
    }
    if (!is.null(summary_filters$carrier_code)) {
      filtered <- filtered %>% filter(Carrier.Code == summary_filters$carrier_code)
    }
    if (!is.null(summary_filters$carrier_name)) {
      filtered <- filtered %>% filter(Carrier.Name == summary_filters$carrier_name)
    }
    if (!is.null(summary_filters$trip_type)) {
      filtered <- filtered %>% filter(Trip.Type == summary_filters$trip_type)
    }
    if (!is.null(summary_filters$month_year)) {
      filtered <- filtered %>% filter(Month.Year == summary_filters$month_year)
    }
    if (!is.null(summary_filters$bi_direct_region)) {
      filtered <- filtered %>% filter(Bi.Directional.Region.Pair == summary_filters$bi_direct_region)
    }
    if (!is.null(summary_filters$bi_direct_country)) {
      filtered <- filtered %>% filter(Bi.Directional.Country.Pair == summary_filters$bi_country_region)
    }
    if (!is.null(summary_filters$fiscal_qtr)) {
      filtered <- filtered %>% filter(Fiscal.Qtr == summary_filters$fiscal_qtr)
    }
    if (!is.null(summary_filters$fiscal_year)) {
      filtered <- filtered %>% filter(Fiscal.Year == summary_filters$fiscal_year)
    }
    if (!is.null(summary_filters$market)) {
      filtered <- filtered %>% filter(Market == summary_filters$market)
    }
    #summary_by_cabin(filtered)
    filtered
  })
  
    output$summary <- renderTable( {summary_by_cabin(updateDF())} ) 

    #SANKEY DIAGRAM
  
    sankey <- reactive({
      df <- 
        updateDF() %>% 
        select(Paid.Fare,Cabin,Market) %>% 
        group_by( Cabin, Market)  %>% 
        summarise(Spend = sum(Paid.Fare)) %>% 
        arrange(desc(Spend)) %>% 
        top_n(n = 10, wt = Spend)
      
      colnames(df) <- c("From", "To", "Spend")
      df
    })
    
    output$sankey <- renderGvis(
        gvisSankey(sankey(), from="From", 
                   to="To", weight="Spend",
                   options=list(
                     height=900,
                     width= 450,
                     sankey="{link:{color:{fill:'lightblue'}}}"
                   ))
    ) 
    
    #PIE CHART
    pie <- reactive({
      df <- updateDF() %>%
       group_by(Cabin) %>% 
      summarise(Spend = sum(Paid.Fare))
      df
    })
    
#     output$pie <- renderHighchart(
#       highchart() %>% 
#         hc_title(text = "Total Spend") %>%
#         hc_chart(type = "pie") %>% 
#         hc_add_serie_labels_values(pie()$Cabin, pie()$Spend) 
#      # %>%  hc_add_theme(hc_theme_538())
#     )
    
    output$pie <- renderPlotly(
      p1 <- plot_ly(type="pie",values=pie()$Spend, height = 5,
                    labels=pie()$Cabin,textinfo="label", hoverinfo="all",
                    textposition="outside",showlegend=F
#                     ,marker=list(colors=c(
#                       "lightskyblue",
#                       "deepblue"))
                    ) %>%
        layout(autosize = T)
    )
    
 #====================POS TAB============================
    summary_by_POS <- function(df){
      df  %>% 
        group_by(POS.Region, POS.Country) %>% 
        summarize(Paid.Fare = round(sum(Paid.Fare), 0), 
                  Subtrips = as.integer(sum(Subtrips)), 
                  ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
                  CPM = round(sum(Paid.Fare)/sum(Miles), 2), 
                  Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)), 
                  Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips)))
    }
    
    POS_filters <<- reactiveValues(
      fiscal_year = NULL,
      fiscal_qtr = NULL,
      carrier_name = NULL,
      carrier_code = NULL,
      trip_type = NULL,
      bi_direct_region = NULL,
      bi_direct_country = NULL,
      market = NULL, 
      cabin = NULL
    )
    
    observe({
      POS_filters$fiscal_year <<- input$POS_fiscal_year
      POS_filters$fiscal_qtr <<- input$POS_fiscal_qtr
      POS_filters$carrier_name <<- input$POS_carrier_name
      POS_filters$carrier_code <<- input$POS_carrier_code
      POS_filters$trip_type <<- input$POS_trip_type
      POS_filters$bi_direct_region <<- input$POS_bi_direct_region
      POS_filters$bi_direct_country <<- input$POS_bi_direct_country
      POS_filters$market <<- input$POS_market
      POS_filters$cabin <<- input$POS_cabin
    })
    
    observe({ 
      Dataset <- get_uploaded_data()
      updateSelectizeInput(session, "POS_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
      updateSelectizeInput(session, "POS_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
      updateSelectizeInput(session, "POS_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
      updateSelectizeInput(session, "POS_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
      updateSelectizeInput(session, "POS_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
      updateSelectizeInput(session, "POS_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
      updateSelectizeInput(session, "POS_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
      updateSelectizeInput(session, "POS_market", choices=unique(Dataset$Market), server = TRUE)
      updateSelectizeInput(session, "POS_cabin", choices=unique(Dataset$Cabin), server = TRUE)
    })
    
    updateDF_POS <-reactive({
      Dataset <- get_uploaded_data()
      if (all(is.null(c(POS_filters$carrier_name,POS_filters$cabin,
                        POS_filters$carrier_code,POS_filters$trip_type,
                        POS_filters$bi_direct_region,POS_filters$fiscal_qtr,POS_filters$fiscal_year,
                        POS_filters$bi_direct_country,POS_filters$market)))){
        Dataset
      }
      filtered <- Dataset
      if (!is.null(POS_filters$carrier_code)) {
        filtered <- filtered %>% filter(Carrier.Code == POS_filters$carrier_code)
      }
      if (!is.null(POS_filters$carrier_name)) {
        filtered <- filtered %>% filter(Carrier.Name == POS_filters$carrier_name)
      }
      if (!is.null(POS_filters$trip_type)) {
        filtered <- filtered %>% filter(Trip.Type == POS_filters$trip_type)
      }
      if (!is.null(POS_filters$bi_direct_region)) {
        filtered <- filtered %>% filter(Bi.Directional.Region.Pair == POS_filters$bi_direct_region)
      }
      if (!is.null(POS_filters$bi_direct_country)) {
        filtered <- filtered %>% filter(Bi.Directional.Country.Pair == POS_filters$bi_country_region)
      }
      if (!is.null(POS_filters$fiscal_qtr)) {
        filtered <- filtered %>% filter(Fiscal.Qtr == POS_filters$fiscal_qtr)
      }
      if (!is.null(POS_filters$fiscal_year)) {
        filtered <- filtered %>% filter(Fiscal.Year == POS_filters$fiscal_year)
      }
      if (!is.null(POS_filters$market)) {
        filtered <- filtered %>% filter(Market == POS_filters$market)
      }
      if (!is.null(POS_filters$cabin)) {
        filtered <- filtered %>% filter(Cabin == POS_filters$cabin)
      }
      filtered
    }) 
    output$POS <- renderTable( { summary_by_POS(updateDF_POS())} )
 
    #==========dual axis graph==============
    pos_graph <- reactive({
      pos <- summary_by_POS(updateDF_POS()) %>% 
        group_by(POS.Region) %>% 
        summarize(Paid.Fare = round(sum(Paid.Fare), 0)) %>% 
        arrange(desc(Paid.Fare))
      pos
    })
    
    output$POS_dual <- renderHighchart(
      highchart() %>% 
        hc_title(text = "POS") %>% 
        hc_xAxis(categories = pos_graph()$POS.Region) %>% 
        hc_add_serie(name = "Paid.Fare", type = "bar",
                     data = pos_graph()$Paid.Fare)
    )


#=====================CARRIER TAB=========NEED TO WORK ON MAKING THE TABLE INTO A WIDE ONE
summary_by_carrier <- function(df){
  df  %>% 
    group_by(Fiscal.Year,Fiscal.Qtr, Carrier.Name ) %>% 
    summarize(Paid.Fare = round(sum(Paid.Fare), 0), 
              Subtrips = as.integer(sum(Subtrips)), 
              Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)), 
              Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips))) %>% 
    top_n(5, wt = Paid.Fare) %>% 
    arrange(desc(Paid.Fare))
}

carrier_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_code = NULL,
  trip_type = NULL,
  bi_direct_region = NULL,
  bi_direct_country = NULL,
  fiscal_year = NULL,
  fiscal_qtr = NULL,
  market = NULL,
  cabin = NULL
)

observe({
  carrier_filters$POS_region <<- input$carrier_POS_region
  carrier_filters$POS_country <<- input$carrier_POS_country
  carrier_filters$carrier_code <<- input$carrier_carrier_code
  carrier_filters$trip_type <<- input$carrier_trip_type
  carrier_filters$bi_direct_region <<- input$carrier_bi_direct_region
  carrier_filters$fiscal_qtr <<- input$carrier_fiscal_qtr
  carrier_filters$fiscal_year <<- input$carrier_fiscal_year
  carrier_filters$bi_direct_country <<- input$carrier_bi_direct_country
  carrier_filters$market <<- input$carrier_market
  carrier_filters$cabin <<- input$carrier_cabin
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "carrier_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "carrier_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "carrier_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "carrier_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
  updateSelectizeInput(session, "carrier_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "carrier_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "carrier_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "carrier_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "carrier_market", choices=unique(Dataset$Market), server = TRUE)
  updateSelectizeInput(session, "carrier_cabin", choices=unique(Dataset$Cabin), server = TRUE)
})

updateDF_carrier <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(carrier_filters$POS_region, carrier_filters$POS_country,
                    carrier_filters$carrier_code,carrier_filters$trip_type,carrier_filters$cabin,
                    carrier_filters$bi_direct_region,carrier_filters$fiscal_qtr,carrier_filters$fiscal_year,
                    carrier_filters$bi_direct_country,carrier_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(carrier_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == carrier_filters$POS_region)
  }
  if (!is.null(carrier_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == carrier_filters$POS_country)
  }
  if (!is.null(carrier_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == carrier_filters$carrier_code)
  }
  if (!is.null(carrier_filters$trip_type)) {
    filtered <- filtered %>% carrier_filter(Trip.Type == carrier_filters$trip_type)
  }
  if (!is.null(carrier_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == carrier_filters$bi_direct_region)
  }
  if (!is.null(carrier_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == carrier_filters$bi_country_region)
  }
  if (!is.null(carrier_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == carrier_filters$fiscal_qtr)
  }
  if (!is.null(carrier_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == carrier_filters$fiscal_year)
  }
  if (!is.null(carrier_filters$market)) {
    filtered <- filtered %>% filter(Market == carrier_filters$market)
  }
  if (!is.null(carrier_filters$cabin)) {
    filtered <- filtered %>% filter(Cabin == carrier_filters$cabin)
  }
  filtered
}) 
output$carrier <- renderTable( {summary_by_carrier(updateDF_carrier())} )

#=====================TOP10 Markets TAB============================
summary_by_top10 <- function(df){
  df  %>% 
    group_by(Bi.Directional.Region.Pair, Market) %>% 
    summarize(Paid.Fare = round(sum(Paid.Fare), 0), 
              Subtrips = as.integer(sum(Subtrips)), 
              ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
              CPM = round(sum(Paid.Fare)/sum(Miles), 2), 
              Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)), 
              Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips))) %>% 
    arrange(desc(Paid.Fare)) %>% 
    top_n(n = 10, wt = Paid.Fare)
}

top10_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  trip_type = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  cabin = NULL
)

observe({
  top10_filters$POS_region <<- input$top10_POS_region
  top10_filters$POS_country <<- input$top10_POS_country
  top10_filters$carrier_name <<- input$top10_carrier_name
  top10_filters$carrier_code <<- input$top10_carrier_code
  top10_filters$trip_type <<- input$top10_trip_type
  top10_filters$fiscal_qtr <<- input$top10_fiscal_qtr
  top10_filters$fiscal_year <<- input$top10_fiscal_year
  top10_filters$bi_direct_country <<- input$top10_bi_direct_country
  top10_filters$cabin <<- input$top10_cabin
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "top10_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "top10_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "top10_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "top10_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "top10_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
  updateSelectizeInput(session, "top10_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "top10_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "top10_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "top10_cabin", choices=unique(Dataset$Cabin), server = TRUE)
})

updateDF_top10 <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(top10_filters$POS_region, top10_filters$POS_country, top10_filters$carrier_name,
                    top10_filters$carrier_code,top10_filters$trip_type,
                    top10_filters$fiscal_qtr,top10_filters$fiscal_year,
                    top10_filters$bi_direct_country,top10_filters$cabin)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(top10_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == top10_filters$POS_region)
  }
  if (!is.null(top10_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == top10_filters$POS_country)
  }
  if (!is.null(top10_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == top10_filters$carrier_code)
  }
  if (!is.null(top10_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == top10_filters$carrier_name)
  }
  if (!is.null(top10_filters$trip_type)) {
    filtered <- filtered %>% top10_filter(Trip.Type == top10_filters$trip_type)
  }
  if (!is.null(top10_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == top10_filters$bi_country_region)
  }
  if (!is.null(top10_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == top10_filters$fiscal_qtr)
  }
  if (!is.null(top10_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == top10_filters$fiscal_year)
  }
  if (!is.null(top10_filters$cabin)) {
    filtered <- filtered %>% filter(Cabin == top10_filters$cabin)
  }
  filtered
}) 
output$top10 <- renderTable( {summary_by_top10(updateDF_top10())} ) 

#=====================OBT TAB============================
summary_by_OBT <- function(df){
  df  %>% 
    group_by(EBkd) %>% 
    summarize(Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips)),
              Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)),
              ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
              CPM = round(sum(Paid.Fare)/sum(Miles), 2)
              )
}

OBT_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  trip_type = NULL,
  bi_direct_region = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL, 
  EBkd = NULL
)

observe({
  OBT_filters$POS_region <<- input$OBT_POS_region
  OBT_filters$POS_country <<- input$OBT_POS_country
  OBT_filters$carrier_name <<- input$OBT_carrier_name
  OBT_filters$carrier_code <<- input$OBT_carrier_code
  OBT_filters$trip_type <<- input$OBT_trip_type
  OBT_filters$bi_direct_region <<- input$OBT_bi_direct_region
  OBT_filters$fiscal_qtr <<- input$OBT_fiscal_qtr
  OBT_filters$fiscal_year <<- input$OBT_fiscal_year
  OBT_filters$bi_direct_country <<- input$OBT_bi_direct_country
  OBT_filters$market <<- input$OBT_market
  OBT_filters$cabin <<- input$OBT_cabin
  OBT_filters$EBkd <<- input$OBT_EBkd
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "OBT_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "OBT_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "OBT_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "OBT_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "OBT_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
  updateSelectizeInput(session, "OBT_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "OBT_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "OBT_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "OBT_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "OBT_market", choices=unique(Dataset$Market), server = TRUE)
  updateSelectizeInput(session, "OBT_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "OBT_EBkd", choices=unique(Dataset$EBkd), server = TRUE)
})


updateDF_OBT <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(OBT_filters$POS_region, OBT_filters$POS_country, OBT_filters$carrier_name,
                    OBT_filters$carrier_code,OBT_filters$trip_type,OBT_filters$cabin, 
                    OBT_filters$EBkd,OBT_filters$bi_direct_region,OBT_filters$fiscal_qtr,
                    OBT_filters$fiscal_year,OBT_filters$bi_direct_country,
                    OBT_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(OBT_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == OBT_filters$POS_region)
  }
  if (!is.null(OBT_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == OBT_filters$POS_country)
  }
  if (!is.null(OBT_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == OBT_filters$carrier_code)
  }
  if (!is.null(OBT_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == OBT_filters$carrier_name)
  }
  if (!is.null(OBT_filters$trip_type)) {
    filtered <- filtered %>% OBT_filter(Trip.Type == OBT_filters$trip_type)
  }
  if (!is.null(OBT_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == OBT_filters$cabin)
  }
  if (!is.null(OBT_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == OBT_filters$bi_direct_region)
  }
  if (!is.null(OBT_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == OBT_filters$bi_country_region)
  }
  if (!is.null(OBT_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == OBT_filters$fiscal_qtr)
  }
  if (!is.null(OBT_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == OBT_filters$fiscal_year)
  }
  if (!is.null(OBT_filters$market)) {
    filtered <- filtered %>% filter(Market == OBT_filters$market)
  }
  if (!is.null(OBT_filters$EBkd)) {
    filtered <- filtered %>% filter(Market == OBT_filters$EBkd)
  }
  filtered
}) 
output$OBT <- renderTable( {summary_by_OBT(updateDF_OBT())} ) 

# ====== OBT dual axis chart =================================
obt_graph <- reactive({
  obt <- summary_by_OBT(updateDF_OBT()) %>% select(EBkd, Paid.Fare.Share, ASP)
  obt$Paid.Fare.Share <- gsub("%", "", obt$Paid.Fare.Share)
  obt$Paid.Fare.Share <- as.numeric(obt$Paid.Fare.Share)
  obt$Paid.Fare.Share <- round((obt$Paid.Fare.Share)/100, digits = 2)
  
  obt
})

output$OBT_dual <- renderHighchart(
  highchart() %>% 
    hc_title(text = "OBT") %>% 
    hc_xAxis(categories = obt_graph()$EBkd) %>% 
    hc_yAxis(  
      list(
        title = list(text = "Paid.Fare.Share"),
        align = "left",
        showFirstLabel = FALSE,
        showLastLabel = FALSE,
        labels = list(format = "{value}")
      ),
      list(
        title = list(text = "ASP"),
        align = "right",
        showFirstLabel = FALSE,
        showLastLabel = FALSE,
        labels = list(format = "{value}"),
        opposite = TRUE
      )
    ) %>% 
    hc_add_serie(name = "Paid.Fare.Share", type = "column",
                 data = obt_graph()$Paid.Fare.Share) %>% 
    hc_add_serie(name = "ASP", type = "spline",
                 data = obt_graph()$ASP, yAxis = 1)
)

#=====================ADV PURCH TAB============================
summary_by_adv_purch <- function(df){
  df  %>% 
    group_by(Advance.Purchase) %>% 
    summarize(Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips)),
              Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)),
              ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
              CPM = round(sum(Paid.Fare)/sum(Miles), 2)) %>% 
    arrange(desc(Subtrip.Share))
}

adv_purch_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  trip_type = NULL,
  bi_direct_region = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL,
  adv_purch = NULL
)

observe({
  adv_purch_filters$POS_region <<- input$adv_purch_POS_region
  adv_purch_filters$POS_country <<- input$adv_purch_POS_country
  adv_purch_filters$carrier_name <<- input$adv_purch_carrier_name
  adv_purch_filters$carrier_code <<- input$adv_purch_carrier_code
  adv_purch_filters$trip_type <<- input$adv_purch_trip_type
  adv_purch_filters$cabin <<- input$adv_purch_cabin
  adv_purch_filters$bi_direct_region <<- input$adv_purch_bi_direct_region
  adv_purch_filters$fiscal_qtr <<- input$adv_purch_fiscal_qtr
  adv_purch_filters$fiscal_year <<- input$adv_purch_fiscal_year
  adv_purch_filters$bi_direct_country <<- input$adv_purch_bi_direct_country
  adv_purch_filters$market <<- input$adv_purch_market
  adv_purch_filters$adv_purch <<- input$adv_purch_adv_purch
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "adv_purch_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "adv_purch_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "adv_purch_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "adv_purch_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "adv_purch_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
  updateSelectizeInput(session, "adv_purch_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "adv_purch_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "adv_purch_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "adv_purch_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "adv_purch_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "adv_purch_market", choices=unique(Dataset$Market), server = TRUE)
  updateSelectizeInput(session, "adv_purch_adv_purch", choices=unique(Dataset$Advance.Purchase), server = TRUE)
})

updateDF_adv_purch <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(adv_purch_filters$POS_region, adv_purch_filters$POS_country, adv_purch_filters$carrier_name,
                    adv_purch_filters$carrier_code,adv_purch_filters$trip_type,adv_purch_filters$cabin,
                    adv_purch_filters$bi_direct_region,adv_purch_filters$fiscal_qtr,adv_purch_filters$fiscal_year,
                    adv_purch_filters$bi_direct_country,adv_purch_filters$market,
                    adv_purch_filters$adv_purch)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(adv_purch_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == adv_purch_filters$POS_region)
  }
  if (!is.null(adv_purch_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == adv_purch_filters$POS_country)
  }
  if (!is.null(adv_purch_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == adv_purch_filters$carrier_code)
  }
  if (!is.null(adv_purch_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == adv_purch_filters$carrier_name)
  }
  if (!is.null(adv_purch_filters$trip_type)) {
    filtered <- filtered %>% adv_purch_filter(Trip.Type == adv_purch_filters$trip_type)
  }
  if (!is.null(adv_purch_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == adv_purch_filters$month_year)
  }
  if (!is.null(adv_purch_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == adv_purch_filters$bi_direct_region)
  }
  if (!is.null(adv_purch_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == adv_purch_filters$bi_country_region)
  }
  if (!is.null(adv_purch_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == adv_purch_filters$fiscal_qtr)
  }
  if (!is.null(adv_purch_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == adv_purch_filters$fiscal_year)
  }
  if (!is.null(adv_purch_filters$market)) {
    filtered <- filtered %>% filter(Market == adv_purch_filters$market)
  }
  if (!is.null(adv_purch_filters$adv_purch)) {
    filtered <- filtered %>% filter(Market == adv_purch_filters$adv_purch)
  }
  filtered
}) 
output$adv_purch <- renderTable( {summary_by_adv_purch(updateDF_adv_purch())} ) 


#=====================LCC TAB============================
summary_by_lcc <- function(df){
  df  %>% filter(!(is.null(LCC))) %>% 
    group_by(Fiscal.Year, Fiscal.Qtr) %>% 
    summarize(Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips)),
              Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)),
              ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
              CPM = round(sum(Paid.Fare)/sum(Miles), 2)) 
}

lcc_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  trip_type = NULL,
  bi_direct_region = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL
)

observe({
  lcc_filters$POS_region <<- input$lcc_POS_region
  lcc_filters$POS_country <<- input$lcc_POS_country
  lcc_filters$carrier_name <<- input$lcc_carrier_name
  lcc_filters$carrier_code <<- input$lcc_carrier_code
  lcc_filters$trip_type <<- input$lcc_trip_type
  lcc_filters$cabin <<- input$lcc_cabin
  lcc_filters$bi_direct_region <<- input$lcc_bi_direct_region
  lcc_filters$bi_direct_country <<- input$lcc_bi_direct_country
  lcc_filters$market <<- input$lcc_market
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "lcc_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "lcc_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "lcc_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "lcc_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "lcc_trip_type", choices=unique(Dataset$Trip.Type), server = TRUE)
  updateSelectizeInput(session, "lcc_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "lcc_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "lcc_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "lcc_market", choices=unique(Dataset$Market), server = TRUE)
})

updateDF_lcc <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(adv_purch_filters$POS_region, adv_purch_filters$POS_country, adv_purch_filters$carrier_name,
                    adv_purch_filters$carrier_code,adv_purch_filters$trip_type,adv_purch_filters$cabin,
                    adv_purch_filters$bi_direct_region,
                    adv_purch_filters$bi_direct_country,adv_purch_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(lcc_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == lcc_filters$POS_region)
  }
  if (!is.null(lcc_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == lcc_filters$POS_country)
  }
  if (!is.null(lcc_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == lcc_filters$carrier_code)
  }
  if (!is.null(lcc_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == lcc_filters$carrier_name)
  }
  if (!is.null(lcc_filters$trip_type)) {
    filtered <- filtered %>% lcc_filter(Trip.Type == lcc_filters$trip_type)
  }
  if (!is.null(lcc_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == lcc_filters$month_year)
  }
  if (!is.null(lcc_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == lcc_filters$bi_direct_region)
  }
  if (!is.null(lcc_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == lcc_filters$bi_country_region)
  }
  if (!is.null(lcc_filters$market)) {
    filtered <- filtered %>% filter(Market == lcc_filters$market)
  }
  filtered
})

output$lcc <- renderTable( {summary_by_lcc(updateDF_lcc())} ) 

#=====================RESTRICTED TAB============================
summary_by_restricted <- function(df){
  df  %>% 
    group_by(Trip.Type, Restrictions) %>% 
    summarize(Subtrip.Share = percent(sum(Subtrips)/ sum(df$Subtrips)),
              Paid.Fare.Share = percent(sum(Paid.Fare)/sum(df$Paid.Fare)),
              ASP = round(sum(Paid.Fare)/sum(Subtrips),0),
              CPM = round(sum(Paid.Fare)/sum(Miles), 2))  
}

restricted_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  bi_direct_region = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL
)

observe({
  restricted_filters$POS_region <<- input$restricted_POS_region
  restricted_filters$POS_country <<- input$restricted_POS_country
  restricted_filters$carrier_name <<- input$restricted_carrier_name
  restricted_filters$carrier_code <<- input$restricted_carrier_code
  restricted_filters$cabin <<- input$restricted_cabin
  restricted_filters$bi_direct_region <<- input$restricted_bi_direct_region
  restricted_filters$fiscal_qtr <<- input$restricted_fiscal_qtr
  restricted_filters$fiscal_year <<- input$restricted_fiscal_year
  restricted_filters$bi_direct_country <<- input$restricted_bi_direct_country
  restricted_filters$market <<- input$restricted_market
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "restricted_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "restricted_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "restricted_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "restricted_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "restricted_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "restricted_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "restricted_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "restricted_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "restricted_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "restricted_market", choices=unique(Dataset$Market), server = TRUE)
})

updateDF_restricted <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(restricted_filters$POS_region, restricted_filters$POS_country, restricted_filters$carrier_name,
                    restricted_filters$carrier_code,restricted_filters$cabin,
                    restricted_filters$bi_direct_region,restricted_filters$fiscal_qtr,restricted_filters$fiscal_year,
                    restricted_filters$bi_direct_country,restricted_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(restricted_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == restricted_filters$POS_region)
  }
  if (!is.null(restricted_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == restricted_filters$POS_country)
  }
  if (!is.null(restricted_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == restricted_filters$carrier_code)
  }
  if (!is.null(restricted_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == restricted_filters$carrier_name)
  }
  if (!is.null(restricted_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == restricted_filters$month_year)
  }
  if (!is.null(restricted_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == restricted_filters$bi_direct_region)
  }
  if (!is.null(restricted_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == restricted_filters$bi_country_region)
  }
  if (!is.null(restricted_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == restricted_filters$fiscal_qtr)
  }
  if (!is.null(restricted_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == restricted_filters$fiscal_year)
  }
  if (!is.null(restricted_filters$market)) {
    filtered <- filtered %>% filter(Market == restricted_filters$market)
  }
  filtered
}) 
output$restricted <- renderTable( {summary_by_restricted(updateDF_restricted())} ) 

#=====================PAID FARE TREND TAB============================
summary_by_paid_fare_trend <- function(df){
  df  %>% 
    group_by(Trip.Type) %>% 
    summarize(Paid.Fare.Total = sum(Paid.Fare))  
}

paid_fare_trend_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  bi_direct_region = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL
)

observe({
  paid_fare_trend_filters$POS_region <<- input$paid_fare_trend_POS_region
  paid_fare_trend_filters$POS_country <<- input$paid_fare_trend_POS_country
  paid_fare_trend_filters$carrier_name <<- input$paid_fare_trend_carrier_name
  paid_fare_trend_filters$carrier_code <<- input$paid_fare_trend_carrier_code
  paid_fare_trend_filters$cabin <<- input$paid_fare_trend_cabin
  paid_fare_trend_filters$bi_direct_region <<- input$paid_fare_trend_bi_direct_region
  paid_fare_trend_filters$fiscal_qtr <<- input$paid_fare_trend_fiscal_qtr
  paid_fare_trend_filters$fiscal_year <<- input$paid_fare_trend_fiscal_year
  paid_fare_trend_filters$bi_direct_country <<- input$paid_fare_trend_bi_direct_country
  paid_fare_trend_filters$market <<- input$paid_fare_trend_market
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "paid_fare_trend_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "paid_fare_trend_market", choices=unique(Dataset$Market), server = TRUE)
})

updateDF_paid_fare_trend <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(paid_fare_trend_filters$POS_region, paid_fare_trend_filters$POS_country, paid_fare_trend_filters$carrier_name,
                    paid_fare_trend_filters$carrier_code,paid_fare_trend_filters$cabin,
                    paid_fare_trend_filters$bi_direct_region,paid_fare_trend_filters$fiscal_qtr,paid_fare_trend_filters$fiscal_year,
                    paid_fare_trend_filters$bi_direct_country,paid_fare_trend_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(paid_fare_trend_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == paid_fare_trend_filters$POS_region)
  }
  if (!is.null(paid_fare_trend_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == paid_fare_trend_filters$POS_country)
  }
  if (!is.null(paid_fare_trend_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == paid_fare_trend_filters$carrier_code)
  }
  if (!is.null(paid_fare_trend_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == paid_fare_trend_filters$carrier_name)
  }
  if (!is.null(paid_fare_trend_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == paid_fare_trend_filters$month_year)
  }
  if (!is.null(paid_fare_trend_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == paid_fare_trend_filters$bi_direct_region)
  }
  if (!is.null(paid_fare_trend_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == paid_fare_trend_filters$bi_country_region)
  }
  if (!is.null(paid_fare_trend_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == paid_fare_trend_filters$fiscal_qtr)
  }
  if (!is.null(paid_fare_trend_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == paid_fare_trend_filters$fiscal_year)
  }
  if (!is.null(paid_fare_trend_filters$market)) {
    filtered <- filtered %>% filter(Market == paid_fare_trend_filters$market)
  }
  filtered
}) 
output$paid_fare_trend <- renderTable( {summary_by_paid_fare_trend(updateDF_paid_fare_trend())} ) 

#=============Paid Fare Trend graph==============
paid_fare_trend_graph <- reactive({
  paid <- get_uploaded_data() %>% 
    mutate(Year.Qtr = paste0(Fiscal.Year,"_", gsub("FY", "", Fiscal.Qtr))) %>% 
    group_by(Trip.Type,Year.Qtr) %>% 
    summarize(Paid.Fare.Total = sum(Paid.Fare)) %>% 
    spread(Trip.Type, Paid.Fare.Total)
  paid
})

output$paid_fare_trend_time <- renderHighchart(
  highchart() %>% 
    hc_title(text = "Paid Fare Trend") %>% 
    hc_xAxis(categories = paid_fare_trend_graph()$Year.Qtr) %>% 
    hc_yAxis(title = list(text = "Paid Fare")) %>% 
    hc_add_serie(name = "Domestic", data = paid_fare_trend_graph()$Domestic) %>% 
    hc_add_serie(name = "Continental", data = paid_fare_trend_graph()$Continental) %>% 
    hc_add_serie(name = "Intercontinental",
                 data = paid_fare_trend_graph()$Intercontinental)
)



#=====================ASP TREND TAB============================
summary_by_asp_trend <- function(df){
  df  %>% 
    group_by(Trip.Type) %>% 
    summarize(ASP = round(sum(Paid.Fare)/sum(Subtrips)))  
}

asp_trend_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  bi_direct_region = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL
)

observe({
  asp_trend_filters$POS_region <<- input$asp_trend_POS_region
  asp_trend_filters$POS_country <<- input$asp_trend_POS_country
  asp_trend_filters$carrier_name <<- input$asp_trend_carrier_name
  asp_trend_filters$carrier_code <<- input$asp_trend_carrier_code
  asp_trend_filters$cabin <<- input$asp_trend_cabin
  asp_trend_filters$bi_direct_region <<- input$asp_trend_bi_direct_region
  asp_trend_filters$fiscal_qtr <<- input$asp_trend_fiscal_qtr
  asp_trend_filters$fiscal_year <<- input$asp_trend_fiscal_year
  asp_trend_filters$bi_direct_country <<- input$asp_trend_bi_direct_country
  asp_trend_filters$market <<- input$asp_trend_market
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "asp_trend_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "asp_trend_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "asp_trend_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "asp_trend_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "asp_trend_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "asp_trend_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "asp_trend_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "asp_trend_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "asp_trend_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "asp_trend_market", choices=unique(Dataset$Market), server = TRUE)
})

updateDF_asp_trend <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(asp_trend_filters$POS_region, asp_trend_filters$POS_country, asp_trend_filters$carrier_name,
                    asp_trend_filters$carrier_code,asp_trend_filters$cabin,
                    asp_trend_filters$bi_direct_region,asp_trend_filters$fiscal_qtr,asp_trend_filters$fiscal_year,
                    asp_trend_filters$bi_direct_country,asp_trend_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(asp_trend_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == asp_trend_filters$POS_region)
  }
  if (!is.null(asp_trend_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == asp_trend_filters$POS_country)
  }
  if (!is.null(asp_trend_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == asp_trend_filters$carrier_code)
  }
  if (!is.null(asp_trend_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == asp_trend_filters$carrier_name)
  }
  if (!is.null(asp_trend_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == asp_trend_filters$month_year)
  }
  if (!is.null(asp_trend_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == asp_trend_filters$bi_direct_region)
  }
  if (!is.null(asp_trend_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == asp_trend_filters$bi_country_region)
  }
  if (!is.null(asp_trend_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == asp_trend_filters$fiscal_qtr)
  }
  if (!is.null(asp_trend_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == asp_trend_filters$fiscal_year)
  }
  if (!is.null(asp_trend_filters$market)) {
    filtered <- filtered %>% filter(Market == asp_trend_filters$market)
  }
  filtered
})

output$asp_trend <- renderTable( {summary_by_asp_trend(updateDF_asp_trend())} ) 

# =====asp_trend time series graph==========
asp_trend_graph <- reactive({
  asp <- get_uploaded_data() %>% mutate(Year.Month = paste0(Year,"_", Month...)) %>% 
    group_by(Trip.Type, Year.Month) %>% 
    summarize(ASP = round(sum(Paid.Fare)/sum(Subtrips))) %>% spread(Trip.Type, ASP)
  asp
})

output$asp_trend_time <- renderHighchart(
  highchart() %>% 
    hc_title(text = "ASP Trend") %>% 
    hc_xAxis(categories = asp_trend_graph()$Year.Month) %>% 
    hc_yAxis(title = list(text = "ASP")) %>% 
    hc_add_serie(name = "Domestic", data = asp_trend_graph()$Domestic) %>% 
    hc_add_serie(name = "Continental", data = asp_trend_graph()$Continental) %>% 
    hc_add_serie(name = "Intercontinental",
                 data = asp_trend_graph()$Intercontinental)
)

#=====================CPM TREND TAB============================
summary_by_cpm_trend <- function(df){
  df  %>% 
    group_by(Trip.Type) %>% 
    summarize(CPM = round(sum(Paid.Fare)/sum(Miles),2))  
}

cpm_trend_filters <<- reactiveValues(
  POS_region = NULL,
  POS_country = NULL,
  carrier_name = NULL,
  carrier_code = NULL,
  bi_direct_region = NULL,
  fiscal_qtr = NULL,
  fiscal_year = NULL,
  bi_direct_country = NULL,
  market = NULL,
  cabin = NULL
)

observe({
  cpm_trend_filters$POS_region <<- input$cpm_trend_POS_region
  cpm_trend_filters$POS_country <<- input$cpm_trend_POS_country
  cpm_trend_filters$carrier_name <<- input$cpm_trend_carrier_name
  cpm_trend_filters$carrier_code <<- input$cpm_trend_carrier_code
  cpm_trend_filters$cabin <<- input$cpm_trend_cabin
  cpm_trend_filters$bi_direct_region <<- input$cpm_trend_bi_direct_region
  cpm_trend_filters$fiscal_qtr <<- input$cpm_trend_fiscal_qtr
  cpm_trend_filters$fiscal_year <<- input$cpm_trend_fiscal_year
  cpm_trend_filters$bi_direct_country <<- input$cpm_trend_bi_direct_country
  cpm_trend_filters$market <<- input$cpm_trend_market
})

observe({ 
  Dataset <- get_uploaded_data()
  updateSelectizeInput(session, "cpm_trend_POS_region", choices=unique(Dataset$POS.Region), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_POS_country", choices=unique(Dataset$POS.Country), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_carrier_name", choices=unique(Dataset$Carrier.Name), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_carrier_code", choices=unique(Dataset$Carrier.Code), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_cabin", choices=unique(Dataset$Cabin), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_bi_direct_region", choices=unique(Dataset$Bi.Directional.Region.Pair), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_fiscal_qtr", choices=unique(Dataset$Fiscal.Qtr), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_fiscal_year", choices=unique(Dataset$Fiscal.Year), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_bi_direct_country", choices=unique(Dataset$Bi.Directional.Country.Pair), server = TRUE)
  updateSelectizeInput(session, "cpm_trend_market", choices=unique(Dataset$Market), server = TRUE)
})

updateDF_cpm_trend <-reactive({
  Dataset <- get_uploaded_data()
  if (all(is.null(c(cpm_trend_filters$POS_region, cpm_trend_filters$POS_country, cpm_trend_filters$carrier_name,
                    cpm_trend_filters$carrier_code,cpm_trend_filters$cabin,
                    cpm_trend_filters$bi_direct_region,cpm_trend_filters$fiscal_qtr,cpm_trend_filters$fiscal_year,
                    cpm_trend_filters$bi_direct_country,cpm_trend_filters$market)))){
    Dataset
  }
  filtered <- Dataset
  if (!is.null(cpm_trend_filters$POS_region)) {
    filtered <- filtered %>% filter(POS.Region == cpm_trend_filters$POS_region)
  }
  if (!is.null(cpm_trend_filters$POS_country)) {
    filtered <- filtered %>% filter(POS.Country == cpm_trend_filters$POS_country)
  }
  if (!is.null(cpm_trend_filters$carrier_code)) {
    filtered <- filtered %>% filter(Carrier.Code == cpm_trend_filters$carrier_code)
  }
  if (!is.null(cpm_trend_filters$carrier_name)) {
    filtered <- filtered %>% filter(Carrier.Name == cpm_trend_filters$carrier_name)
  }
  if (!is.null(cpm_trend_filters$cabin)) {
    filtered <- filtered %>% filter(Month.Year == cpm_trend_filters$month_year)
  }
  if (!is.null(cpm_trend_filters$bi_direct_region)) {
    filtered <- filtered %>% filter(Bi.Directional.Region.Pair == cpm_trend_filters$bi_direct_region)
  }
  if (!is.null(cpm_trend_filters$bi_direct_country)) {
    filtered <- filtered %>% filter(Bi.Directional.Country.Pair == cpm_trend_filters$bi_country_region)
  }
  if (!is.null(cpm_trend_filters$fiscal_qtr)) {
    filtered <- filtered %>% filter(Fiscal.Qtr == cpm_trend_filters$fiscal_qtr)
  }
  if (!is.null(cpm_trend_filters$fiscal_year)) {
    filtered <- filtered %>% filter(Fiscal.Year == cpm_trend_filters$fiscal_year)
  }
  if (!is.null(cpm_trend_filters$market)) {
    filtered <- filtered %>% filter(Market == cpm_trend_filters$market)
  }
  filtered
})

output$cpm_trend <- renderTable( {summary_by_cpm_trend(updateDF_cpm_trend())} ) 

# =====cpm trend time series graph==========
cpm_trend_graph <- reactive({
  cpm <- get_uploaded_data() %>% mutate(Year.Month = paste0(Year,"_", Month...)) %>% 
    group_by(Trip.Type, Year.Month) %>% 
    summarize(CPM = round(sum(Paid.Fare)/sum(Miles),2)) %>% spread(Trip.Type, CPM)
  cpm
})

output$cpm_trend_time <- renderHighchart(
  highchart() %>% 
    hc_title(text = "CPM Trend") %>% 
    hc_xAxis(categories = cpm_trend_graph()$Year.Month) %>% 
    hc_yAxis(title = list(text = "CPM")) %>% 
    hc_add_serie(name = "Domestic", data = cpm_trend_graph()$Domestic) %>% 
    hc_add_serie(name = "Continental", data = cpm_trend_graph()$Continental) %>% 
    hc_add_serie(name = "Intercontinental",
                 data = cpm_trend_graph()$Intercontinental)
)

})

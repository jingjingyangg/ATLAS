paid_fare_trend_tab <- tabPanel(
  "Paid Fare Trend",
  column(2, 
         wellPanel(
           selectizeInput(
             "paid_fare_trend_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput("paid_fare_trend_POS_region", "POS Region:", multiple = TRUE, 
                          options = list(placeholder = 'All'),
                          list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "paid_fare_trend_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ), 
           
           selectizeInput(
             "paid_fare_trend_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    column(6,
                           wellPanel(highchartOutput("paid_fare_trend_time"))
                    )
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("paid_fare_trend"))
           )
         )
  )
)
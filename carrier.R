carrier_tab <- tabPanel(
  "Carrier",
  column(2, 
         wellPanel(
           selectizeInput(
             "carrier_POS_region", "POS Region:", multiple = TRUE, 
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "carrier_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "carrier_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "carrier_trip_type", "Trip Type:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "carrier_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "carrier_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),

           selectizeInput(
             "carrier_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           
           selectizeInput(
             "carrier_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           
           selectizeInput(
             "carrier_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "carrier_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                           wellPanel(plotlyOutput("carrier_by_qtr")
                    )
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("carrier"))
           )
           
         )
  )
)
POS_tab <- tabPanel(
  "POS",
  column(2, 
         wellPanel(
           selectizeInput(
             "POS_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_trip_type", "Trip Type:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           
           selectizeInput(
             "POS_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "POS_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    column(6,
                           wellPanel(highchartOutput("POS_dual"))
                           )
                    ),
           tabPanel("Details", 
                    wellPanel(tableOutput("POS"))
           )
           
         )
  )
    
)
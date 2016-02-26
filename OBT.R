OBT_tab <- tabPanel(
  "OBT",
  column(2, 
         wellPanel(
           selectizeInput(
             "OBT_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_POS_region", "POS Region:", multiple = TRUE, 
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_bi-direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_EBkd", "EBkd:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_trip_type", "Trip Type:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
          
           selectizeInput(
             "OBT_bi-direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "OBT_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    column(6,
                           wellPanel(highchartOutput("OBT_dual"))
                    )
                    #                     ,
                    #                     column(6,
                    #                            wellPanel(plotlyOutput("pie"))
                    #                     )
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("OBT"))
           )
         )
  )
)
lcc_tab <- tabPanel(
  "Low Cost Carrier",
  column(2, 
         wellPanel(
           selectizeInput("lcc_POS_region", "POS Region:", multiple = TRUE, 
                          options = list(placeholder = 'All'),
                          list("bogus")
           ),
           
           selectizeInput(
             "lcc_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "lcc_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "lcc_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "lcc_trip_type", "Trip Type:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "lcc_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "lcc_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
          
           selectizeInput(
             "lcc_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ), 
           
           selectizeInput(
             "lcc_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    wellPanel()
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("lcc"))
           )
         )
  )
)
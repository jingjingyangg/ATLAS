restricted_tab <- tabPanel(
  "Resticted",
  column(2, 
         wellPanel(
           selectizeInput(
             "restricted_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "restricted_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput("restricted_POS_region", "POS Region:", multiple = TRUE, 
                          options = list(placeholder = 'All'),
                          list("bogus")
           ),
           
           selectizeInput(
             "restricted_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "restricted_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "restricted_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "restricted_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "restricted_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "restricted_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ), 
           
           selectizeInput(
             "restricted_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    column(6,
                      wellPanel(highchartOutput("restricted_dual_fare"))
                    ),
                    column(6,
                      wellPanel(highchartOutput("restricted_dual_asp"))
                    )
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("restricted"))
           )
         )
  )
)
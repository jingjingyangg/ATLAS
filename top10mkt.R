top10mkt_tab <- tabPanel(
  "Top 10 Markets",
  column(2, 
         wellPanel(
           
           selectizeInput(
             "top10_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_POS_region", "POS Region:", multiple = TRUE, 
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_trip_type", "Trip Type:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_bi-direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "top10_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    wellPanel(showOutput("top10_1", "polycharts")),
                    wellPanel(showOutput("top10_2", "nvd3"))
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("top10"))
           )
           
         )
  )
)
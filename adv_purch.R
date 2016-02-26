adv_purch_tab <- tabPanel(
  "Adv Purch",
  column(2, 
         wellPanel(
             selectizeInput(
               "adv_purch_fiscal_year", "Fiscal Year:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "adv_purch_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput("adv_purch_POS_region", "POS Region:", multiple = TRUE, 
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_trip_type", "Trip Type:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "adv_purch_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ), 
           
           selectizeInput(
             "adv_purch_adv_purch", "Advance Purchase:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ), 
           
           selectizeInput(
             "adv_purch_cabin", "Cabin:",multiple = TRUE,
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
                    wellPanel(tableOutput("adv_purch"))
           )
           
         )
  )
)
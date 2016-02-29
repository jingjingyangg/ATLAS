CPM_tab <- tabPanel(
  "CPM Trend",
  column(2, 
         wellPanel(
           selectizeInput(
             "cpm_trend_fiscal_year", "Fiscal Year:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput("cpm_trend_POS_region", "POS Region:", multiple = TRUE, 
                          options = list(placeholder = 'All'),
                          list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_POS_country", "POS Country:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_carrier_name", "Carrier Name:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_carrier_code", "Carrier Code:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ),
           
           selectizeInput(
             "cpm_trend_market", "Market:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           ), 
           
           selectizeInput(
             "cpm_trend_cabin", "Cabin:",multiple = TRUE,
             options = list(placeholder = 'All'),
             list("bogus")
           )
         )),
  
  column(10, 
         tabsetPanel(
           tabPanel("Charts", 
                    column(6,
                           wellPanel(highchartOutput("cpm_trend_time"))
                    )
           ),
           tabPanel("Details", 
                    wellPanel(tableOutput("cpm_trend"))
           )
         )
  )
)
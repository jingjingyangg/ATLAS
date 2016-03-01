summary_tab <- tabPanel(
  "Summary",
  column(2, 
         wellPanel(
           selectizeInput(
             "summary_POS_region", "POS Region:", multiple = TRUE, 
             options = list(placeholder = 'All'),
             list("bogus")
             ),
             
             selectizeInput(
               "summary_POS_country", "POS Country:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_carrier_name", "Carrier Name:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_carrier_code", "Carrier Code:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_trip_type", "Trip Type:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_month_year", "Month/Year:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_bi_direct_region", "Bi-Directional Region Pair:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_fiscal_qtr", "Fiscal Qtr:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_fiscal_year", "Fiscal Year:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_bi_direct_country", "Bi-Directional Country Pair:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             ),
             
             selectizeInput(
               "summary_market", "Market:",multiple = TRUE,
               options = list(placeholder = 'All'),
               list("bogus")
             )
           )),
         
         column(10, 
                tabsetPanel(
                  tabPanel("Charts", 
                           column(6,
                                  p("This is a relatively complicated graph, so might take a few seconds"),
                                  wellPanel(htmlOutput("sankey"))
                                  ),
#                                  wellPanel(highchartOutput("pie"
#                                                 #,lib="highcharts", showOutput)
#                                      )
                              column(6,
                                     wellPanel(plotlyOutput("pie"))
                                        )),

                  tabPanel("Details", 
                           wellPanel(tableOutput("summary"))
                           )
                           
                )
         )
)
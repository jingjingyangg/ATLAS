data_tab <- tabPanel(
  "Data",
  column(2, 
         wellPanel(fileInput('data_csv', 'Upload CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')
                      ),
                   
                   fileInput("data_rds", "or Upload RDS file",
                             accept = ".rds")
                   )
         
  ),
  column(10, 
         wellPanel(dataTableOutput("glimpse"))
  )
)

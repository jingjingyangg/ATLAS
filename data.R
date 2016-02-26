data_tab <- tabPanel(
  "Data",
  column(2, 
         wellPanel(fileInput('data_own', 'Upload CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')
                      )
                   )
         
  ),
  column(10, 
         wellPanel(dataTableOutput("glimpse"))
  )
)

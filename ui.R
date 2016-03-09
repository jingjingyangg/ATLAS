library(shinythemes)
library(plotly)
library(highcharter)
library(rCharts)
source("data.R")
source("summary.R")
source("adv_purch.R")
source("asp_trend.R")
source("carrier.R")
source("CPM.R")
source("lcc.R")
source("OBT.R")
source("paid_fare_trend.R")
source("POS.R")
source("restricted.R")
source("top10mkt.R")


shinyUI( 
  navbarPage( a(img(src="SG_Dual_Logo.jpg", height = "100%"), 
                href="http://www.cwt-solutions-group.com/cwtsg/"),
              theme = shinytheme("cerulean"),
              tabPanel("Air",
                       tabsetPanel(
                         data_tab,
                         summary_tab,
                         POS_tab,
                         carrier_tab,
                         top10mkt_tab,
                         OBT_tab,
                         adv_purch_tab,
                         lcc_tab,
                         restricted_tab,
                         paid_fare_trend_tab,
                         ASP_trend_tab, 
                         CPM_tab
                       )
              ),
              tabPanel("Hotel"),
              tabPanel("Ground")
  )
)

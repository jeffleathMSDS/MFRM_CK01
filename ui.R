## Jeff Leath
## Testing Shiny Dashboard

## load library
library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "MFRM Check Clearing"),
    dashboardSidebar(
      menuItem("Dashboard"),
      menuSubItem("DB for Finance"),
      menuSubItem("DB for Sales"),
      menuItem("Detailed Analysis"),
      menuItem("Raw Data")
    ),
    dashboardBody()
  )
)

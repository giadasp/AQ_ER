library(shinydashboard)
sidebar <- dashboardSidebar(
  sidebarMenuOutput("menu")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)
ui<-dashboardPage(
  dashboardHeader(title = "Rimini Dati di qualità dell'aria"),
  sidebar,
  dashboardBody()
)
# Design Page
dashboardPage(
  
  # Title
  dashboardHeader(title = "Crypto Twitter Feed"),
  
  # Side nav bar
  dashboardSidebar(
    
    selectInput("select", label = h3("Select box"), 
                
                choices = list("ALL" = "ALL","BTC" = "BTC", "ETH" = "ETH", "LTC" = "LTC", "XLM" = "XLM"), 
                selected = 1),
    
    # Side menu bar with raw data
    sidebarMenu(menuItem("Raw data", tabName = "rawdata"))
  ),
  
  # Main body of the UI
  dashboardBody(
    
    tabItems(
      
      tabItem("rawdata",
              
              # Program that shows the time
              h2(textOutput("currentTime")),
              
              # Table to be displayed
              dataTableOutput("rawtable")
      )
    )
  )
)

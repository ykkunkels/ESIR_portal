ui <- fluidPage(
  titlePanel("ESM Item Repository"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("search_label", "Search Label"),
      textInput("search_english", "Search English"),
      textInput("search_description", "Search Description"),
      textInput("search_citation", "Search Citation"),
      numericInput("search_beeps", "Search Beeps Per Day", value = NA, min = 0),
      
      tags$h4("Filter by Tags"),
      uiOutput("tag_buttons"),
      actionButton("clear_filters", "Clear Filters")
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)
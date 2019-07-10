
###################################
#### ESIR Portal in Shiny - UI ####
#### YKK - 13/05/2019          ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~####


## Load and / or Install required packages
if(!require('shiny')){install.packages('shiny', dep = TRUE)};library('shiny')
if(!require('shinyjs')){install.packages('shinyjs', dep = TRUE)};library('shinyjs')
if(!require('shinydashboard')){install.packages('shinydashboard', dep = TRUE)};library('shinydashboard')

# UI ----

ui <- dashboardPage(skin = "green",
                    
                    ## Header----
                    dashboardHeader(title = "ESM Item Repository (beta)", titleWidth = 350),
                    
                    
                    dashboardSidebar(width = 250,
                     sidebarMenu(menuItem("Menu"),
                                 menuItem("Home", tabName = "home_tab", icon = icon("file-text")),
                                 menuItem("Help", tabName = "help_tab", icon = icon("question")),
                                 # br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                                 # br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                                 div(htmlOutput("logo"), style="position: relative;")
                     ) 
                    ),
  
                   dashboardBody(

      
  tabItems(
    # First tab content
    tabItem(tabName = "home_tab",
         
    ## Left column - main panel   
    column(4,   
           
        ## USe: shinyjs
        useShinyjs(),
        
        ## Input: Search topic select
        selectInput("topic_select", "Select search topic:",
                    c("Item ID" = "item_ID",
                      "Dutch" = "label",
                      "English" = "english",
                      "Description" = "description",
                      "Dataset" = "dataset",
                      "Beeps per day" = "beeps_per_day",
                      "Population" = "population",
                      "Citation" = "citation",
                      "Contact" = "contact")),
  
        ## Input: Search query
        textAreaInput(inputId = "search_text",height = '40px', label = "Search the Repository",  
                      placeholder = "Enter search query here"),
        
        ## Input: Action button
        actionButton(inputId= "go", label = "Find relevant items"),
        
        ## Input: All button
        actionButton(inputId= "all", label = "Show all items"),
        
        ## Input: Reset button
        actionButton(inputId= "reset", label = "Clear")#,
      
      ), #closing left column  

      
      ## Right column - Main panel
      column(8, 
             
        ## Output: Text----
        # br(), br(),
               
        # verbatimTextOutput(outputId = "search_res", placeholder = T),
        # 
        # verbatimTextOutput(outputId = "output_found", placeholder = T),
        
        verbatimTextOutput(outputId = "item_selection", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_show", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_english", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_description", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_dataset", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_beeps_per_day", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_population", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_citation", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_contact", placeholder = FALSE),
      # ), # closing right colum
      
      br(), br(),
      htmlOutput(outputId = "warningtext"),
      
      
      ## Previous and next buttons
      # enable_buttonnav <- reactiveValues(ok = FALSE),
      
      fluidRow(
        column(width = 1, hidden(actionButton(inputId = "first", label = "<<<"))),
        column(width = 1, hidden(actionButton(inputId = "previous_10", label = "<<"))),
        column(width = 1, hidden(actionButton(inputId = "previous", label = "<"))),
        column(width = 1, hidden(verbatimTextOutput(outputId = "match_no"))),
        column(width = 1, hidden(actionButton(inputId = "nextb", label = ">"))),
        column(width = 1, hidden(actionButton(inputId = "nextb_10", label = ">>"))),
        column(width = 1, hidden(actionButton(inputId = "last", label = ">>>")))
      )
      
      ), # closing right colum
    
    h5("When insights and content from the repository are used, the repository should be cited as: Kirtley, O. J., Hiekkaranta, A. P., Kunkels, Y. K., Verhoeven, D., Van Nierop, M., & Myin-Germeys, I. (2019, April 2). The Experience Sampling Method (ESM) Item Repository. Retrieved from osf.io/kg376, DOI 10.17605/OSF.IO/KG376.", 
       style = "font-style: normal; letter-spacing: 0px; line-height: 10pt; position: fixed; bottom: 0; left: 100;")
    
    ), # closing tabItem()
    
    tabItem(tabName = "help_tab",
            
            h2("Welcome to the ESM Item Repository"),
            
            h4("We are Olivia Kirtley (KU Leuven), Anu Hiekkaranta (KU Leuven),", a("Yoram K. Kunkels", href = "http://www.ykkunkels.com/", target = "_blank"), "(University Medical Center Groningen), Martine van Nierop (KU Leuven), Davinia Verhoeven (KU Leuven), and Inez Myin-Germeys (KU Leuven) and we aim to support the further development of Experience Sampling Methodology (ESM) research by creating an open repository of existing ESM items", a("(https://osf.io/kg376/)", href = "https://osf.io/kg376/", target = "_blank"), ". To achieve this, we need your help in collecting as many items as possible!", 
                style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"),
            
            div(htmlOutput("flow"), style="height: auto; width: auto; position: fixed;"),
            
            h4("Please submit docs to: martine.vannierop@kuleuven.be", 
               style = "font-style: normal; letter-spacing: 1px; line-height: 26pt; position: fixed; bottom: 0; left: 100;")
            
            ) # closing tabItem()
    
            
    
    ) # closing tabItems()
      
    )
  )


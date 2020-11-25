
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
                    dashboardHeader(title = "ESM Item Repository", titleWidth = 350),
                    
                    
                    dashboardSidebar(width = 250,
                     sidebarMenu(menuItem("Menu"),
                                 menuItem("Home", tabName = "home_tab", icon = icon("file-text")),
                                 menuItem("Welcome", tabName = "welcome_tab", icon = icon("door-open")),
                                 menuItem("Acknowledgements", tabName = "acknowledgements_tab", icon = icon("book-reader")),
                                 menuItem("Feedback", tabName = "feedback_tab", icon = icon("comment")),
                                 div(htmlOutput("logo"), style="position: relative;"),
                                 h5("version 1.1.4", style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;
                                    position: fixed; bottom: 0; left: 100;")
                     ) 
                    ),
  
                   dashboardBody(

      
  tabItems(
    # First tab content
    tabItem(tabName = "home_tab",
         
    ## Left column - main panel   
    column(4,   
           
        ## Use: shinyjs
        useShinyjs(),
        
        ## Input: Search topic select
        selectInput("topic_select", "Select search topic:",
                    c("Item ID" = "item_ID",
                      "Original language" = "label",
                      "English" = "english",
                      "Description" = "description",
                      "Dataset" = "dataset",
                      "Beeps per day" = "beeps_per_day",
                      "Population" = "population",
                      "Citation" = "citation",
                      "Existing reference" = "existing_ref",
                      "Contact" = "contact")),
  
        ## Input: Search query
        textAreaInput(inputId = "search_text",height = '40px', label = "Enter search term",  
                      placeholder = "Enter search term here"),
        
        ## Input: Action button
        actionButton(inputId= "go", label = "Search items"),
        
        ## Input: All button
        actionButton(inputId= "all", label = "Show all items"),
        
        ## Input: Reset button
        actionButton(inputId= "reset", label = "Clear"),
        
        br(),
        
        h5("To download the complete dataset from the portal,", 
           style = "font-style: normal; letter-spacing: 0.5px; line-height: 15pt;"),
        h5("press the 'Show all items' button and then press 'Download'.", 
           style = "font-style: normal; letter-spacing: 0.5px; line-height: 15pt;"),
        
        
        br(), br(),
        
        ## Download text
        h4("Download your selection as .csv file"),
        
        ## Input: Download button for .CSV
        downloadButton("downloadData", "Download .CSV file"),
        
        br(), br(),
        
        ## Download text
        h4("Download your selection as Excel file"),
        
        ## Input: Download button for .XLSX
        downloadButton("downloadData_Excel", "Download Excel file")
        
        
        
      ), #closing left column  

      
      ## Right column - Main panel
      column(8, 
             
             ## Add Style tags
             tags$head(tags$style("#item_show{min-height: 50px; max-height: 50px; overflow-y:scroll; white-space: pre-wrap;}")), 
             tags$head(tags$style("#item_english{min-height: 50px; max-height: 50px; overflow-y:scroll; white-space: pre-wrap;}")), 
             tags$head(tags$style("#item_citation{min-height: 60px; max-height: 60px; overflow-y:scroll; white-space: pre-wrap;}")), 
             tags$head(tags$style("#existing_ref{min-height: 60px; max-height: 60px; overflow-y:scroll; white-space: pre-wrap;}")), 
        
        ## Output: Text----
        verbatimTextOutput(outputId = "item_selection", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_show", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_english", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_description", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_dataset", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_beeps_per_day", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_population", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_citation", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "existing_ref", placeholder = FALSE),
        
        verbatimTextOutput(outputId = "item_contact", placeholder = FALSE),
      

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
    
   
    ), # closing tabItem()
    
    tabItem(tabName = "welcome_tab",
            
            h2("Welcome to the ESM Item Repository"),

            h4("We are Olivia Kirtley (KU Leuven), Anu Hiekkaranta (KU Leuven),", a("Yoram K. Kunkels", href = "http://www.ykkunkels.com/", target = "_blank"), "(University Medical Center Groningen), Martine van Nierop (KU Leuven), Davinia Verhoeven (KU Leuven), and Inez Myin-Germeys (KU Leuven) and we aim to support the further development of Experience Sampling Methodology (ESM) research by creating an open repository of existing ESM items", a("(https://osf.io/kg376/)", href = "https://osf.io/kg376/", target = "_blank"), ". To achieve this, we need your help in collecting as many items as possible!", 
                style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"),
            
            div(htmlOutput("flow"), style="height: auto; width: auto; position: fixed;"),
            
            h4("Please submit docs to: martine.vannierop@kuleuven.be", 
               style = "font-style: normal; letter-spacing: 1px; line-height: 26pt; position: fixed; bottom: 0; left: 100;")
            
            ), # closing tabItem()
    

    
    tabItem(tabName = "acknowledgements_tab",
            
            h2("Acknowledgements and citation help"),
            
            br(),
            
            h4("When insights and content from the repository are used, the repository should be cited as: Kirtley, O. J., Hiekkaranta, A. P., Kunkels, Y. K., Verhoeven, D., Van Nierop, M., & Myin-Germeys, I. (2019, April 2). The Experience Sampling Method (ESM) Item Repository. Retrieved from osf.io/kg376, DOI 10.17605/OSF.IO/KG376.", 
               style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"),
            
            br(), br(),
            
            h4("Funding acknowledgements: Olivia Kirtley and Anu Hiekkaranta’s work on the project is supported by postdoctoral and PhD fellowships, respectively, from an FWO Odysseus grant to Inez Myin-Germeys (FWO GOF8416N). Yoram Kunkels’ work on this project is supported by the European Research Council (ERC-CoG-2015; TRANS-ID; No 681466 to Marieke Wichers).", 
               style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"),
            
            
    ), # closing tabItem()
    
    tabItem(tabName = "feedback_tab",
            
            h2("Feedback"),
            
            br(),
            
            h4("Please leave your feedback on how we could further improve the Repository", 
               style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"),
            
            br(),br(),
            
            ## Input: feedback
            textAreaInput(inputId = "feedback_text",height = '400px', width = '400px', label = "Please leave your feedback",  
                          placeholder = "Enter feedback here"),
            
            ## Input: Action button
            actionButton(inputId= "send", label = "Send feedback"),
            
            
            
    ) # closing tabItem()
            
    
    ), # closing tabItems()
  
  
  h5("We do not take responsibility for the quality of items within the repository. Inclusion of items within the repository does not indicate our endorsement of them.", 
     style = "font-style: normal; letter-spacing: 0px; line-height: 10pt; position: fixed; bottom: 0; left: 100;")
      
    )
  )


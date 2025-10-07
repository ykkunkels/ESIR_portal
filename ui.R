######################################
### ESIR Portal in Shiny           ###
### UI version 1.1.24              ###
### YKK - 07/10/2025               ###
### Changelog:                     ###
### > Removed banner               ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###


## Load and / or Install required packages----
if (!require("htmltools")) {
  install.packages("htmltools", dep = TRUE)
}
library("htmltools")

if (!require("sass")) {
  install.packages("sass", dep = TRUE)
}
library("sass")

if (!require("shiny")) {
  install.packages("shiny", dep = TRUE)
}
library("shiny")

if (!require("shinydashboard")) {
  install.packages("shinydashboard", dep = TRUE)
}
library("shinydashboard")

if (!require("shinyjs")) {
  install.packages("shinyjs", dep = TRUE)
}
library("shinyjs")

if (!require("xlsx")) {
  install.packages("xlsx", dep = TRUE)
}
library("xlsx")


# Temp libraries
# if (!require("styler")) {
#   install.packages("styler", dep = TRUE)
# }
# library("styler")
# 
# if (!require("devtools")) {
#   install.packages("devtools", dep = TRUE)
# }
# library("devtools")

# ## Check code style ----
# styler::style_dir()

# UI ----


ui <- dashboardPage(skin = "green",
                    
                    ## Header ----
                    dashboardHeader(title = "ESM Item Repository", titleWidth = 300),
                    
                    dashboardSidebar(width = 300,
                      sidebarMenu(menuItem("Menu"),
                                  menuItem("Home", tabName = "home_tab", icon = icon("file-text")),
                                  menuItem("Welcome", tabName = "welcome_tab", icon = icon("door-open")),
                                  menuItem("Blog", icon = icon("atlas"), href = "http://www.esmitemrepositoryinfo.com/"),
                                  menuItem("Acknowledgements", tabName = "acknowledgements_tab", icon = icon("book-reader")),
                                  menuItem("Citations", tabName = "citations_tab", icon = icon("quote-left")),
                                  uiOutput("logo"),
                                  h5("[version 1.1.22] We do not take responsibility for the", br(), 
                                     "quality of items within the repository. Inclusion of", br(), 
                                     "items within the repository does not indicate", br(), 
                                     "our endorsement of them. All items within the", br(), 
                                     "repository are subject to a Creative Commons", br(), 
                                     "Attribution Non-Commerical License (CC BY-NC).",
                                     style = "font-style: normal; font-size: 80%; color: #b5c8d4; letter-spacing: 0.2px; line-height: 10pt;
                                    position: relative; left: 18px;")
                                  
                      ) # closing sidebarMenu
                    ), # closing dashboardSidebar
  
                   ## Body ----
                   dashboardBody(
                     
                     tags$style(HTML("


                       .panel-container {
                           display: grid;
                           grid-template-rows: auto;
                           height: 100%; /* Ensure the container takes up full height */
                       }

                       .panel {
                           /* No specific styling needed for panels */
                       }
                       .column-class4, .column-class8 {
                   background-color: #ecf0f5; /* Set your desired color for columns*/
                       }

                      #home_tab_content {
                   background-color: #ecf0f5; /* Set your desired color for home_tab*/
               }
           ")),
    tabItems(
      ## Home tab
      tabItem(
        tabName = "home_tab",

        ## Left column - main panel----
        column(
          4,

          ## Use: shinyjs
          useShinyjs(),

          ## Flexbox container for dynamic position of objects
          div(
            style = "display: flex; flex-direction: column;",

            ## Input: Search topic select
            tags$div(
              style = "width: 100%;",
              selectInput(
                "topic_select", "Select search topic:",
                c(
                  "All" = "all",
                  "Item ID" = "item_ID",
                  "Original language" = "label",
                  "English" = "english",
                  "Description" = "description",
                  "Dataset" = "dataset",
                  "Beeps per day" = "beeps_per_day",
                  "Population" = "population",
                  "Citation" = "citation",
                  "Existing reference" = "existing_ref",
                  "Contact" = "contact"
                )
              ),
            ),
          ),

          ## Input: Search query
          tags$div(
            style = "width: 100%;",
            textAreaInput(
              inputId = "search_text", height = "auto", label = "Enter search term",
              placeholder = "Enter search term here"
            )
          ),
          tags$script(
            HTML(
              "
            $(document).keypress(function(event) {
              if (event.keyCode == 13) {
                event.preventDefault(); // Prevent default behavior of Enter key
                Shiny.setInputValue('go', 1, {priority: 'event'});
              }
            });
            "
            )
          ),



          ## Input: Action button
          actionButton(inputId = "go", label = "Search items"),

          ## Input: All button
          actionButton(inputId = "all", label = "Show all items"),

          ## Input: Reset button
          actionButton(inputId = "reset", label = "Clear"),
          br(),
          h5("This portal shows only a selection of the available item information. 
             Download the data via the download-buttons to get the full dataset.",
            style = "font-style: normal; letter-spacing: 0.5px; line-height: 15pt;"
          ),
          h5("To download the complete dataset from the portal, 
             press the 'Show all items' button hereabove and then press 'Download .CSV' 
             or 'Download Excel' herebelow.",
            style = "font-style: normal; letter-spacing: 0.5px; line-height: 15pt;"
          ),
          br(), br(),

          ## Input: Download button for .CSV
          h4("Download your selection as .csv file"),
          downloadButton("downloadData", "Download .CSV file"),
          br(), br(),

          ## Input: Download button for .XLSX
          h4("Download your selection as Excel file"),
          downloadButton("downloadData_Excel", "Download Excel file"),
          br(), br(),
        ), # closing left column


        ## Right column - Main panel----
        column(
          8,

          ## Add Style tags
          tags$head(tags$style("#item_show{min-height: auto; max-height: auto; overflow-y:scroll; white-space: pre-wrap;}")),
          tags$head(tags$style("#item_english{min-height: auto; max-height: auto; overflow-y:scroll; white-space: pre-wrap;}")),
          tags$head(tags$style("#item_description{min-height: auto; max-height: auto; overflow-y:scroll; white-space: pre-wrap;}")),
          tags$head(tags$style("#item_citation{min-height: auto; max-height: auto; overflow-y:scroll; white-space: pre-wrap;}")),
          tags$head(tags$style("#existing_ref{min-height: auto; max-height: auto; overflow-y:scroll; white-space: pre-wrap;}")),
          tags$head(tags$style("#item_contact{min-height: auto; max-height: auto; overflow-y:scroll; white-space: pre-wrap;}")),

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
          fluidRow(
            column(width = 1, hidden(actionButton(inputId = "first", label = "<<<"))),
            column(width = 1, hidden(actionButton(inputId = "previous_10", label = "<<"))),
            column(width = 1, hidden(actionButton(inputId = "previous", label = "<"))),
            column(width = 1, hidden(verbatimTextOutput(outputId = "match_no"))),
            column(width = 1, hidden(actionButton(inputId = "nextb", label = ">"))),
            column(width = 1, hidden(actionButton(inputId = "nextb_10", label = ">>"))),
            column(width = 1, hidden(actionButton(inputId = "last", label = ">>>")))
          ) # closing fluidRow
        ), # closing right colum
        
        # fluidRow(
        #   uiOutput("saa")
        # )
        
      ), # closing tabItem()


      # Welcome tab content----
      tabItem(
        tabName = "welcome_tab",
        h2("Welcome to the ESM Item Repository!"),
        h4("We are Olivia Kirtley (KU Leuven),", a("Yoram K. Kunkels", href = "http://www.ykkunkels.com/", target = "_blank"), "(Centraal Bureau voor de Statistiek), Gudrun Eisele (KU Leuven), Steffie Schoefs (KU Leuven), Nieke Vermaelen (KU Leuven), Laura Van Heck (KU Leuven), Milla Pihlajamäki (KU Leuven), Benjamin Kunc (KU Leuven), and Inez Myin-Germeys (KU Leuven). We aim to support the further development of Experience Sampling Methodology (ESM) research with an open repository of existing ESM items", a("(https://osf.io/kg376/)", href = "https://osf.io/kg376/", target = "_blank"), ".",
          style = "font-style: normal; letter-spacing: 1px; line-height: 125%;"
        ),
        h4("To achieve this, we need your help in collecting as many items as possible! Please submit completed documents to: submissions@esmitemrepository.com",
          style = "font-style: normal; letter-spacing: 1px; line-height: 125%;"
        ),
        uiOutput("flow"),
      ), # closing tabItem()


      # Acknowledgements tab content----
      tabItem(
        tabName = "acknowledgements_tab",
        h2("Acknowledgements and citation help"),
        br(),
        h4("When insights and content from the repository are used, the repository should be cited as: Kirtley, O. J., Eisele, G., Kunkels, Y. K., Hiekkaranta, A., Van Heck, L., Pihlajamäki, M. R., Kunc, B., Schoefs, S., Kemme, N., Biesemans, T., & Myin-Germeys, I. (2024). The Experience Sampling Method Item Repository", a("https://doi.org/10.17605/OSF.IO/KG376", href = "https://doi.org/10.17605/OSF.IO/KG376", target = "_blank"),
          style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"
        ),
        br(), br(),
        h4("Funding acknowledgements: The ESM Item Repository and those who are part of the ESM Item Repository team receive funding from the following sources: A KU Leuven C1 grant (C16/23/011) to Inez Myin-Germeys and Olivia Kirtley, a KU Leuven C+ grant (CPLUS/24/009) to Olivia Kirtley, an Research Foundation Flanders (FWO; G049023N) grant to Inez Myin-Germeys and Olivia Kirtley, and a Junior Postdoctoral Fellowship from Research Foundation Flanders (FWO; 1223725N) to Gudrun Eisele.",
          style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;"
        ),
      ), # closing tabItem()
      
      # Citations tab content----
      tabItem(
        tabName = "citations_tab",
        h2("Articles citing the ESM Item Repository"),
        
        dataTableOutput("df_citations")      # Display the DataTable

      ) # closing tabItem()
      
    ) # closing tabItems()
  ) # closing dashboardBody
) # closing dashboardPage

# to do
# make population + q type checkboxes
# make sure tags do not undo dt table choices
# make layout adapted to the screen size

# Modified ESM Item Repository Shiny app
# Change: make the tag box collapsible and default-collapsed using HTML <details>/<summary>

library(shiny)       # For building interactive web apps
library(DT)          # For interactive data tables
library(stringr)     # For string manipulation
library(openxlsx)    # For reading/writing Excel files
library(dplyr)       # For data manipulation
library(shinyjs)

# Define all possible tags for items in the repository
all_tags <- c(
  "activity",
  "anxiety",
  "appraisal",
  "arousal",
  "attention",
  "avoidance",
  "body image",
  "cognition",
  "coping",
  "depression",
  "emotion regulation",
  "event",
  "fatigue",
  "identity",
  "location",
  "methodological",
  "motivation",
  "negative affect",
  "physical health",
  "pleasure",
  "positive affect",
  "psychosis",
  "rumination",
  "self-esteem",
  "self-harm",
  "sleep",
  "social context",
  "social interaction",
  "social support",
  "stress",
  "substance use",
  "suicidality",
  "symptom",
  "worry"
)

# Assign default color to all tags
tag_colors <- setNames(rep("#5e7d6a", length(all_tags)), all_tags)

# -------------------------
# UI definition
# -------------------------
ui <- navbarPage(
  title = NULL,
  id = "main_tabs",
  collapsible = TRUE,
  fluid = TRUE,
  
  tabPanel(
    "Search the Repository",
    fluidPage(
      tags$head(
        tags$link(href = "https://fonts.googleapis.com/css2?family=Oswald:wght@500&display=swap", rel = "stylesheet"),
        tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel = "stylesheet"),
        tags$style(HTML(
          paste0(
            "
              /* Collapsible info box styling — open by default */
              details.collapsible-info-box {
              margin: 20px 0;
              background-color: #ffffff;
              border-left: 6px solid rgba(46, 125, 78, 0.85);
              padding: 0;
              border-radius: 6px;
              box-shadow: 0 2px 6px rgba(0,0,0,0.05);
              }

              /* Summary header (clickable) */
              details.collapsible-info-box > summary {
              cursor: pointer;
              padding: 12px 16px;
              font-weight: 600;
              font-size: 20px;
              background-color: rgba(84, 163, 110, 0.40);
              color: rgba(46, 125, 78, 0.85);
              border-radius: 6px;
              list-style: none;
              display: flex;
              align-items: center;
              gap: 12px;
              }

              /* Remove default disclosure triangle */
              details.collapsible-info-box summary::-webkit-details-marker { display: none; }

            /* Body and navbar styling */
            body {
            font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
            background-color: #f4f6f7;
            margin: 0;
            padding: 0;
            padding-top: 70px;
            }
            h1, h2, h3, h4, h5, summary {
            font-family: 'Inter', sans-serif;
            font-weight: 600;
            }
            .navbar { position: fixed; top: 0; width: 100%; z-index: 1000; font-size: 18px; color: black; }

            /* Title panel styling */
            .title-panel { background-color: rgba(84, 163, 110, 0.70); padding: 10px; color: white; display: flex; justify-content: center; width: 100vw; margin-left: calc(-50vw + 50%); box-sizing: border-box; margin-bottom: 10px; }
            .title-content { display: flex; align-items: center; gap: 20px; }
            .title-panel strong { font-family: 'Oswald', sans-serif; font-size: 60px; font-weight: 500; text-transform: uppercase; letter-spacing: 1.5px; white-space: nowrap; }
            .title-logo { height: 100px; object-fit: contain; }

            /* Info text box */
            .info-text { background-color: #ffffff; border-left: 6px solid #54a36e; padding: 12px; margin: 20px 0; font-size: 18px; color: #333; line-height: 1.6; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.05); }

            /* Badge/tag styling */
            .badge-tag { display: inline-block; padding: 6px 10px; margin: 4px 4px 4px 0; background-color: #2ecc71; color: white; border-radius: 20px; font-size: 16px; cursor: pointer; border: 2px solid transparent; }
            .badge-tag.selected { border: 3px solid darkorange !important; font-weight: 700; }

            /* Buttons styling */
            #reset_btn, #download_csv, #download_excel { font-size: 18px !important; }
            #reset_btn .btn, #download_csv .btn, #download_excel .btn { font-size: 18px !important; padding: 10px 20px !important; border-radius: 20px !important; }

            /* Table container styling */
            .table-container { background-color: white; border-radius: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); padding: 12px; margin-top: 8px; }

            /* DataTable styling */
            table.dataTable { border-radius: 10px; border-collapse: separate; width: 100% !important; margin-top: 8px; font-size: 13.5px; font-family: 'Inter', sans-serif; }
            table.dataTable th, table.dataTable td { padding: 8px; text-align: left;}
            table.dataTable th { background-color: #2c3e50; color: white; font-size: 12px; }
            table.dataTable tbody tr:hover { background-color: #f1f1f1; }
            .dataTables_length select, .dataTables_filter input { font-size: 12px; }
            .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter { margin-bottom: 10px; }
            .dataTables_paginate { display: flex; justify-content: flex-end; margin-top: 16px !important; gap: 5px; }
            .dataTables_info { font-size: 12px; margin-top: 20px !important; }
            .dataTables_paginate .paginate_button { background-color: #a2b9c1 !important; border-radius: 5px; color: white !important; padding: 8px 10px; font-size: 12px; }
            .dataTables_paginate .paginate_button.current { background-color: #7f8c8d !important; }

            /* Footer text */
            .footer-text { font-size: 10px; color: #777; text-align: center; margin-top: 15px; margin-bottom: 20px; line-height: 1.4; }

            /* Download citation button */
            #download_citation.light-download-btn { background-color: #f4f6f7 !important; color: #333 !important; font-size: 18px !important; padding: 10px 20px !important; border-radius: 20px !important; margin-top: 0px !important; margin-left: 0px !important; vertical-align: top; }

            /* Collapsible tag box (details/summary) styling — match original grey + font size */
            details.collapsible-tag-box {
            margin-bottom: 16px;
            background-color: #f0f0f0;
            border-radius: 10px;
            }
            details.collapsible-tag-box > summary {
            cursor: pointer;
            padding: 10px 12px;
            font-weight: 600;
            font-size: 18px;
            background-color: #f0f0f0;
            border-radius: 10px;
            list-style: none;
            }
            details.collapsible-tag-box[open] > summary {
            border-bottom-left-radius: 0;
            border-bottom-right-radius: 0;
            }
            details.collapsible-tag-box > div {
            padding: 10px 12px;
            background-color: #f0f0f0;
            font-size: 18px;
            line-height: 1.6;
            border-top: 1px solid #ddd;
            border-bottom-left-radius: 10px;
            border-bottom-right-radius: 10px;
            }
            /* Remove default disclosure triangle */
            summary::-webkit-details-marker {
            display: none;
            }
            .table-container .dataTables_wrapper {
              max-width: 1760px;
              margin: 0 auto;
            }
            /* Sidebar filter box */
            .filter-sidebar {
              background-color: #ffffff;
              border-right: 6px solid #f0f0f0;
              border-radius: 10px;
              box-shadow: 0 2px 6px rgba(0,0,0,0.05);
              padding: 0;
            }
            
            /* Sidebar header */
            .filter-header {
              padding: 10px 12px;
              font-weight: 600;
              font-size: 18px;
              background-color: #f0f0f0;
              border-top-left-radius: 10px;
              border-top-right-radius: 10px;
              list-style: none;
            }
            
            /* Sidebar body */
            .filter-body {
              padding: 16px;
            }
            .filter-body-small {
              font-weight: 400;
              font-size: 13px;
              display: block;
              margin-top: 3px;
              line-height: 1.4;
            }

            "
          )
        ))
      ),
      
      div(class = "title-panel", div(
        class = "title-content",
        tags$strong("Welcome to the ESM Item Repository!"),
        tags$img(src = "logo.png", class = "title-logo")
      )),
      
      # Collapsible info box — default open
      tags$details(
        class = "collapsible-info-box info-text",
        open = NA,
        tags$summary(tags$h4(
          strong("How do I use the ESM Item Repository?")
        )),
        # keep the original inner layout but wrapped inside the details
        div(
          style = "display: flex; flex-direction: column; padding: 12px;",
          div(
            style = "display: flex; gap: 20px; flex-wrap: wrap;",
            div(
              style = "flex: 3; min-width: 300px;",
              "This portal presents a selection of item information available for review. ",
              "It is designed to allow you to easily search, filter, and explore these items based on various columns provided below. ",
              tags$br(),
              "Note that this portal only shows a selection of the available item information. You can download the data via the buttons below for more information about the items.",
              tags$br(),
              "💡 Tip: You can now hover over any of the column titles in the table to see more information about what each column means️.",
              tags$br(),
              "⬇️ To download the complete dataset from the portal, press the 'Show all items (Clear search)' button below and then press 'Download .csv' or 'Download Excel (.xlsx)' below.",
              tags$br(),
              "⬇️ To download a subset of the dataset (e.g., after filtering it based on your desired characteristics), complete your search and press 'Download .csv' or 'Download Excel (.xlsx)' below."
            ),
            div(
              style = "flex: 2; min-width: 200px; display: flex; flex-direction: column; justify-content: space-between; background-color: #f0f0f0; padding: 12px; border-radius: 6px; font-size: 18px; line-height: 1.5;",
              div(
                tags$b(
                  "How do I refer to the ESM Item Repository in publications and other documents?"
                ),
                tags$br(),
                "If you use insights from the ESM Item Repository, please cite as: ",
                tags$br(),
                "Kirtley, O. J., Eisele, G., Kunkels, Y. K., Hiekkaranta, A., Van Heck, L., Pihlajamäki, M. R., Kunc, B., Schoefs, S., Kemme, N., Biesemans, T., & Myin-Germeys, I. (2024). The Experience Sampling Method Item Repository ",
                tags$a(
                  href = "https://doi.org/10.17605/OSF.IO/KG376",
                  target = "_blank",
                  "https://doi.org/10.17605/OSF.IO/KG376"
                ),
                ".",
                tags$br(),
                "Alternatively, you can download the citation in your preferred format using the button below."
              ),
              div(
                style = "display: flex; justify-content: flex-end; gap: 8px; margin-top: 6px;",
                selectInput(
                  inputId = "citation_format",
                  label = NULL,
                  choices = list(
                    "BibTeX (.bib)" = "citation_bibtex.bib",
                    "RIS (.ris)" = "citation_ris.ris",
                    "EndNote (.xml)" = "citation_endnote_xlm.xml",
                    "RefWorks (.txt)" = "citation_refworks.txt",
                    "CSV (.csv)" = "citation_csv.csv",
                    "Zotero RDF (.rdf)" = "citation_zotero_rdf.rdf"
                  ),
                  selected = "citation_bibtex.bib",
                  width = "150px"
                ),
                downloadButton(
                  outputId = "download_citation",
                  label = "Download citation",
                  class = "light-download-btn"
                )
              )
            )
          ),
          div(
            style = "display: flex; justify-content: flex-start; gap: 12px; margin-top: 6px;",
            actionButton("reset_btn", "🔄 Show all items (Clear search)", style = "background-color: #3498db; color: white; border: none;"),
            downloadButton("download_csv", "Download .csv", style = "background-color: #2ecc71; color: white; border: none;"),
            downloadButton("download_excel", "Download Excel (.xlsx)", style = "background-color: #1abc9c; color: white; border: none;")
          )
        )
      )
      ,
      
      fluidRow(column(
        12,
        div(
          class = "table-container",
          
          # Collapsible Tags explanation box (default collapsed)
          tags$details(
            class = "collapsible-tag-box",
            tags$summary(tags$h4(strong(
              "Click to filter by tags"
            ))),
            div(
              style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: flex-start; gap: 18px;",
              
              # Left explanatory text
              div(
                style = "flex: 1 1 40%; min-width: 300px; font-size: 18px; line-height: 1.6;",
                HTML(
                  "Each item in the repository has one or more tags that describe what it measures—for example, mood, stress, sleep, or social interaction.
       These tags help you quickly find items of interest.<br>
       <strong>Where the tags come from:</strong> Tags were created by analyzing item descriptions and then carefully reviewed by the ESM Item Repository team.
       Use the tags to explore items in the repository.<br>
       <strong>How to use the tags:</strong> Click on a tag to filter the table and see only items with that tag.
       You can select multiple tags at the same time; the filter will show items that have <em>any</em> of the selected tags (OR logic).<br>"
                )
              ),
              
              div(
                style = "
    width: 100%;
    display: flex;
    justify-content: flex-start;
    margin-bottom: 4px;
    padding: 0;
  ",
                radioButtons(
                  inputId = "tag_logic",
                  label = strong("Match items that have:"),
                  choices = c(
                    "Any selected tag (OR)" = "or",
                    "All selected tags (AND)" = "and"
                  ),
                  selected = "or",
                  inline = TRUE
                )
              )
              ,
              
              # Right tag selector
              div(style = "flex: 1 1 55%; min-width: 350px; display: flex; flex-wrap: wrap; justify-content: center; align-content: flex-start; gap: 10px; text-align: center;", uiOutput("tag_selector"))
            )
          ),
          
          div(style = "height: 2px;"),
          fluidRow(
            column(
              width = 3,
              
              div(
                class = "filter-sidebar",
                
                div(
                  class = "filter-header",
                  tags$h4("Filter items")
                ),
                
                div(
                  class = "filter-body",
                
                  textInput("filter_item_og", 
                            label = HTML("<strong>Item (original language)</strong>"),
                            placeholder = "Search original item wordings"),
                  
                  textInput("filter_item_english", 
                            label = HTML("<strong>Item (English)</strong><br>
                                         <span class='filter-body-small'>
                                         Note that this might be blank if English is the original language of the item.
                                         </span>"),
                            placeholder = "Search item wordings in English"),
                  
                  textInput("filter_desc", 
                            "Description",
                            placeholder = "Search item descriptions"),
                  
                  textInput("filter_dataset", 
                            "Dataset",
                            placeholder = "Search the name of a dataset"),
  
                  selectInput(
                    "filter_qtype",
                    label = HTML("Questionnaire type <br>
                                 <span class='filter-body-small'>
                                 Multiple choices are possible: items that correspond to one of the choices will be shown.
                                 </span>"),
                    choices = list("regular (i.e., the questionnaire shown at every signal-contingent beep)" = "regular",
                                "morning" = "morning",
                                "evening" = "evening",
                                "event" = "event"),
                    multiple = TRUE
                  ),
  
                  selectInput(
                    "filter_population",
                    label = HTML("Population<br>
                                 <span class='filter-body-small'>
                                 Multiple choices are possible: choose whether you want this to follow AND or OR logic below
                                 </span>"),
                    choices = c("children", "adolescents", "adults", "elderly", "general population", "outpatient", "inpatient"),
                    multiple = TRUE
                  ),
                  
                  radioButtons("filter_population_andor", 
                               label = NULL,
                               choices = c(
                                 "OR - The item should have been applied in at least one of the selected populations" = "or",
                                 "AND - The item should have been applied in all selected populations" = "and"),
                               selected = "or")
                )
              )
            ),
            
            column(
              width = 9,
              DTOutput("filtered_table")
            )
          )
        )
      )),
      
      div(
        class = "footer-text",
        "[version 1.1.22] We do not take responsibility for the quality of items within the repository. Inclusion of items within the repository does not indicate our endorsement of them. All items within the repository are subject to a Creative Commons Attribution Non-Commercial License (CC BY-NC)."
      )
    )
  ),
  
  tabPanel("About the Repository", fluidPage(
    div(class = "title-panel", div(
      class = "title-content",
      tags$strong("Welcome to the ESM Item Repository!"),
      tags$img(src = "logo.png", class = "title-logo")
    )),
    
    div(
      class = "info-text",
      style = "display: flex; gap: 16px; flex-wrap: wrap; padding: 10px;",
      
      div(
        style = "flex: 2; min-width: 300px; padding: 0 10px; border-right: 1px solid #ccc; box-sizing: border-box;",
        tags$b(style = "font-size: 18px; display: block; margin-bottom: 10px;", "Who are we? 👋"),
        tags$p(
          "We are Olivia Kirtley (KU Leuven), Yoram K. Kunkels (Centraal Bureau voor de Statistiek), Gudrun Eisele (KU Leuven), Steffie Schoefs (KU Leuven), Louise Bresseel (KU Leuven), Laura Van Heck (KU Leuven), Milla Pihlajamäki (KU Leuven), Lisa Peeters (KU Leuven), and Inez Myin-Germeys (KU Leuven)."
        ),
        tags$p(
          "We are an open science initiative supporting the development of Experience Sampling Methodology (ESM) research through an open repository of ESM items. To make this possible, we launched the ESM Item Repository in 2018 and opened our portal in October 2019. Since then, researchers from 11 countries have contributed over 3,300 items—and more are coming! 🚀"
        ),
        tags$p(
          "We aim to support the further development of ESM research with an open repository of existing ESM items:",
          tags$a(href = "https://osf.io/kg376/", target = "_blank", "https://osf.io/kg376/")
        ),
        tags$p(
          "💌 Want to contribute? Find more information about submission on our OSF page (",
          tags$a(href = "https://osf.io/kg376/", target = "_blank", "https://osf.io/kg376/"),
          ") and send your completed items to: ",
          tags$b("submissions@esmitemrepository.com")
        )
      ),
      
      div(
        style = "flex: 1; min-width: 250px; padding: 0 10px; box-sizing: border-box;",
        tags$b(style = "font-size: 18px; display: block; margin-bottom: 10px;", "Funding acknowledgements 💰"),
        tags$p("The ESM Item Repository and its team are funded by:"),
        tags$ul(
          tags$li(
            "A KU Leuven C1 grant (C16/23/011) to Inez Myin-Germeys and Olivia Kirtley"
          ),
          tags$li("A KU Leuven C+ grant (CPLUS/24/009) to Olivia Kirtley"),
          tags$li("A Research Foundation Flanders (FWO; G049023N) grant")
        )
      )
    ),
    
    div(
      class = "footer-text",
      "[version 1.1.22] We do not take responsibility for the quality of items within the repository. Inclusion of items within the repository does not indicate our endorsement of them. All items within the repository are subject to a Creative Commons Attribution Non-Commercial License (CC BY-NC)."
    )
  ))
)
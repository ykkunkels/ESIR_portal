library(shiny)
library(DT)
library(stringr)
library(openxlsx)
library(dplyr)

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

tag_colors <- setNames(
  c(
    "#8F7EE5",
    "#CC8E51",
    "#0F6B99",
    "#99540F",
    "#C3E57E",
    "#E57E7E",
    "#B22C2C",
    "#B26F2C",
    "#E5B17E",
    "#E5B17E",
    "#6B990F",
    "#C3E57E",
    "#B26F2C",
    "#CC8E51",
    "#6551CC",
    "#85B22C",
    "#990F0F",
    "#2C85B2",
    "#B22C2C",
    "#0F6B99",
    "#7EC3E5",
    "#422CB2",
    "#51A3CC",
    "#990F0F",
    "#CC5151",
    "#A3CC51",
    "#85B22C",
    "#CC5151",
    "#E57E7E",
    "#6B990F",
    "#A3CC51",
    "#BFB2FF",
    "#260F99",
    "#99540F"
  ),
  all_tags
)

ui <- navbarPage(
  title = NULL,
  # Removes navbar title
  id = "main_tabs",
  collapsible = TRUE,
  fluid = TRUE,
  
  tabPanel(
    "Search the Repository",
    fluidPage(
      tags$head(
        tags$link(href = "https://fonts.googleapis.com/css2?family=Oswald:wght@500&display=swap", rel = "stylesheet"),
        tags$style(HTML(
          paste0(
            "
      body {
        font-family: 'Arial', sans-serif;
        background-color: #f4f6f7;
        margin: 0; padding: 0;
      }
      .title-panel {
        background-color: rgba(84, 163, 110, 0.70);
        padding: 10px 10px;
        color: white;
        display: flex;
        align-items: center;
        justify-content: center;
        width: 100vw;
        margin-left: calc(-50vw + 50%);
        box-sizing: border-box;
        margin-bottom: 10px;
      }
      .title-content {
        display: flex;
        align-items: center;
        gap: 20px;
      }
      .title-panel strong {
        font-family: 'Oswald', sans-serif;
        font-size: 55px;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 1.5px;
        white-space: nowrap;
      }
      .title-logo {
        height: 200px;
        object-fit: contain;
      }
      .info-text {
        background-color: #ffffff;
        border-left: 6px solid #54a36e;
        padding: 20px;
        margin: 20px 0;
        font-size: 18px;
        color: #333333;
        line-height: 1.6;
        border-radius: 6px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .badge-tag {
        display: inline-block;
        padding: 6px 12px;
        margin: 4px 4px 4px 0;
        background-color: #2ecc71;
        color: white;
        border-radius: 20px;
        font-size: 16px;
        cursor: pointer;
        border: 2px solid transparent;
      }
      #reset_btn,
      #download_csv,
      #download_excel {
        font-size: 18px !important;
      }

      #reset_btn .btn,
      #download_csv .btn,
      #download_excel .btn {
        font-size: 18px !important;
        padding: 10px 20px !important;
        border-radius: 20px !important;
      }
      .badge-tag.selected {
        border: 2px solid black !important;
      }
      .table-container {
        background-color: white;
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.05);
        padding: 20px;
        margin-top: 20px;
      }
      table.dataTable {
        border-radius: 10px;
        border-collapse: separate;
        width: 100%;
        margin-top: 20px;
        font-size: 13px;
      }
      table.dataTable th, table.dataTable td {
        padding: 12px;
        text-align: left;
      }
      table.dataTable th {
        background-color: #2c3e50;
        color: white;
        font-size: 14px;
      }
      table.dataTable tbody tr:hover {
        background-color: #f1f1f1;
      }
      .dataTables_length select, .dataTables_filter input {
        font-size: 14px;
      }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter {
        margin-bottom: 10px;
      }
      .dataTables_paginate {
        display: flex;
        justify-content: flex-end;
        margin-top: 20px !important;
        gap: 5px;
      }
      .dataTables_info {
        font-size: 14px;
        margin-top: 20px !important;
      }
      .dataTables_paginate .paginate_button {
        background-color: #a2b9c1 !important;
        border-radius: 5px;
        color: white !important;
        padding: 8px 12px;
        font-size: 14px;
      }
      .dataTables_paginate .paginate_button.current {
        background-color: #7f8c8d !important;
      }
      .footer-text {
        font-size: 12px;
        color: #777;
        text-align: center;
        margin-top: 15px;
        margin-bottom: 20px;
        line-height: 1.4;
      }
      #download_citation.light-download-btn {
       background-color: #f4f6f7 !important;
       color: #333 !important;
       font-size: 18px !important;
       padding: 10px 20px !important;
       border-radius: 20px !important;
       margin-top: 0px !important;
       margin-left: 0px !important;
       vertical-align: top;
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
      
      div(
        class = "info-text",
        tags$b("How do I use the ESM Item Repository?"),
        tags$br(),
        "This portal presents a selection of item information available for review. ",
        "It is designed to allow you to easily search 🔍, filter 🧹, and explore 🧭 these items based on various columns provided below. ",
        "You can use the available search and filter features to refine your results and quickly find the items most relevant to your needs 🎯",
        "Note that this portal only shows a selection of the available item information. You can download the data via the buttons below for more information about the items.",
        tags$br(),
        "💡 Tip: You can now hover over any of the column titles in the table to see more information about what each column means️.",
        tags$br(),
        "⬇️ To download the complete dataset from the portal, press the 'Show all items (Clear search)' button below and then press 'Download .csv' or 'Download Excel (.xlsx)' below.",
        tags$br(),
        tags$span(
          HTML(
            "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;To download a subset of the dataset, complete your search and press 'Download .csv' or 'Download Excel (.xlsx)' below."
          )
        ),
        tags$br(),
        div(
          style = "background-color: #e0e0e0; padding: 20px; border-radius: 10px; margin-top: 20px;",
          tags$b(
            "How do I refer to the ESM Item Repository in publications and other documents?"
          ),
          tags$br(),
          
          div(
            style = "display: flex; align-items: flex-start; gap: 20px;",
            
            # Left: Citation text
            div(
              style = "flex: 0 0 85%; max-width: 85%; font-size: 18px; line-height: 1.5;",
              "If you use insights from the ESM Item Repository, please cite as: ",
              tags$br(),
              "Kirtley, O. J., Eisele, G., Kunkels, Y. K., Hiekkaranta, A., Van Heck, L., Pihlajamäki, M. R., Kunc, B., Schoefs, S., Kemme, N., Biesemans, T., & Myin-Germeys, I. (2024). The Experience Sampling Method Item Repository ",
              tags$a(
                href = "https://doi.org/10.17605/OSF.IO/KG376",
                target = "_blank",
                "https://doi.org/10.17605/OSF.IO/KG376"
              ),
              ". Alternatively, you can download the citation in your preferred format using the button on the right."
            ),
            
            # Right: Citation controls (dropdown and button)
            div(
              style = "flex: 1; display: flex; flex-direction: column; align-items: center; gap: 3px;",
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
                width = "220px"
              ),
              downloadButton(
                outputId = "download_citation",
                label = "Download citation",
                class = "light-download-btn"
              )
            )
          )
        )
        ,
        div(
          style = "display: flex; justify-content: space-between; align-items: flex-end; margin-top: 20px;",
          
          # Left: Reset and download buttons
          div(
            style = "display: flex; gap: 16px;",
            actionButton("reset_btn", "🔄 Show all items (Clear search)", style = "background-color: #3498db; color: white; border: none;"),
            downloadButton("download_csv", "Download .csv", style = "background-color: #2ecc71; color: white; border: none;"),
            downloadButton("download_excel", "Download Excel (.xlsx)", style = "background-color: #1abc9c; color: white; border: none;")
          ),
        )
        
        
      ),
      
      div(style = "margin-top: 20px;", tags$h4("Filter by tags"), uiOutput("tag_selector")),
      
      fluidRow(column(
        12,
        div(class = "table-container", DTOutput("filtered_table")),
        div(style = "height: 20px;")
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
      tags$b(style = "font-size: 18px; display: block; margin-top: 6px; margin-bottom: 10px;", "Who are we? 👋"),
      tags$p(
        "We are Olivia Kirtley (KU Leuven), Yoram K. Kunkels (Centraal Bureau voor de Statistiek), Gudrun Eisele (KU Leuven), Steffie Schoefs (KU Leuven), Nieke Vermaelen (KU Leuven), Laura Van Heck (KU Leuven), Milla Pihlajamäki (KU Leuven), Benjamin Kunc (KU Leuven), and Inez Myin-Germeys (KU Leuven)."
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
      ),
      
      tags$hr(),
      tags$b(style = "font-size: 18px; display: block; margin-top: 20px; margin-bottom: 10px;", "Funding acknowledgements 💰"),
      tags$p("The ESM Item Repository and its team are funded by:"),
      tags$ul(
        tags$li(
          "A KU Leuven C1 grant (C16/23/011) to Inez Myin-Germeys and Olivia Kirtley"
        ),
        tags$li("A KU Leuven C+ grant (CPLUS/24/009) to Olivia Kirtley"),
        tags$li("A Research Foundation Flanders (FWO; G049023N) grant")
      )
    ),
    div(
      class = "footer-text",
      "[version 1.1.22] We do not take responsibility for the quality of items within the repository. Inclusion of items within the repository does not indicate our endorsement of them. All items within the repository are subject to a Creative Commons Attribution Non-Commercial License (CC BY-NC)."
    )
  ))
  
)

server <- function(input, output, session) {
  df <- read.csv(url("https://osf.io/5ba2c/download"), stringsAsFactors = FALSE)[-(1:3), ]
  colnames(df) <- c(
    "item_ID",
    "label",
    "english",
    "description",
    "comment",
    "response_scale_discrete",
    "response_scale_vas",
    "branched_from",
    "branched_response",
    "beep_level",
    "beeps_per_day",
    "morning",
    "evening",
    "event",
    "other",
    "children",
    "adolescents",
    "adults",
    "elderly",
    "gen_pop",
    "outpatient",
    "inpatient",
    "which",
    "dataset",
    "contact",
    "open",
    "open_link",
    "request",
    "closed",
    "citation",
    "existing_adapt",
    "existing_ref",
    "how_admin",
    "how_long_beep",
    "reliability",
    "validity",
    "development",
    "item_use",
    "item_instructions",
    "item_other"
  )
  df$item_ID <- 1:nrow(df)
  
  df <- df %>%
    mutate(
      q_type = paste(
        ifelse(str_to_lower(beep_level) %in% c("yes", "x"), "regular", NA),
        ifelse(str_to_lower(morning) %in% c("yes", "x"), "morning", NA),
        ifelse(str_to_lower(evening) %in% c("yes", "x"), "evening", NA),
        ifelse(str_to_lower(event) %in% c("yes", "x"), "event", NA),
        sep = "; "
      ),
      q_type = str_replace_all(q_type, "(NA; )+|; NA", ""),
      # remove extra NAs and semicolons
      q_type = str_replace(q_type, "^NA$", "")                # make sure "NA" alone becomes blank
    )
  
  df <- df %>%
    mutate(
      population = paste(
        ifelse(str_to_lower(children) %in% c("yes", "x"), "children", NA),
        ifelse(
          str_to_lower(adolescents) %in% c("yes", "x"),
          "adolescents",
          NA
        ),
        ifelse(str_to_lower(adults) %in% c("yes", "x"), "adults", NA),
        ifelse(str_to_lower(elderly) %in% c("yes", "x"), "elderly", NA),
        ifelse(
          str_to_lower(gen_pop) %in% c("yes", "x"),
          "general population",
          NA
        ),
        ifelse(str_to_lower(outpatient) %in% c("yes", "x"), "outpatient", NA),
        ifelse(str_to_lower(inpatient) %in% c("yes", "x"), "inpatient", NA),
        sep = "; "
      ),
      population = str_replace_all(population, "(NA; )+|; NA", ""),
      # remove extra NAs and semicolons
      population = str_replace(population, "^NA$", "")                # make sure "NA" alone becomes blank
    )
  
  tags_plain <- read.csv("tags_plain.csv")[, 2:3] # TO DO: crosscheck tags
  df <- merge(df, tags_plain, by = "item_ID", all.x = TRUE)
  colnames(df)[colnames(df) == "tag_final"] <- "tag"
  df$tag[df$tag == ""] <- NA
  df$tag_list <- strsplit(as.character(df$tag), ";\\s*")
  
  format_tags <- function(tag_string) {
    if (is.na(tag_string) || tag_string == "")
      return("")
    tags <- strsplit(tag_string, ";\\s*")[[1]]
    html <- paste0(sapply(tags, function(tg) {
      col <- tag_colors[[tg]]
      paste0('<span class="badge-tag" style="background-color:',
             col,
             ';">',
             tg,
             '</span>')
    }), collapse = " ")
    return(html)
  }
  
  selected_tags <- reactiveVal(character(0))
  
  output$tag_selector <- renderUI({
    tagList(lapply(all_tags, function(tag) {
      style <- paste0("background-color:", tag_colors[[tag]], ";")
      classes <- "badge-tag"
      if (tag %in% selected_tags()) {
        classes <- paste(classes, "selected")
      }
      span(
        tag,
        class = classes,
        style = style,
        onclick = sprintf(
          "Shiny.setInputValue('tag_click', '%s', {priority: 'event'});",
          tag
        )
      )
    }))
  })
  
  observeEvent(input$tag_click, {
    current <- selected_tags()
    if (input$tag_click %in% current) {
      selected_tags(setdiff(current, input$tag_click))
    } else {
      selected_tags(c(current, input$tag_click))
    }
  })
  
  filtered_data <- reactive({
    data <- df
    if (length(selected_tags()) > 0) {
      data <- data[sapply(data$tag_list, function(tags) {
        if (is.null(tags))
          return(FALSE)
        any(tags %in% selected_tags())
      }), ]
    }
    output_df <- data[, c(
      "item_ID",
      "label",
      "english",
      "description",
      "dataset",
      "q_type",
      "population",
      "citation",
      "contact",
      "tag"
    )]
    colnames(output_df) <- c(
      "Item ID",
      "Item in original language",
      "Item in English",
      "Description",
      "Dataset",
      "Questionnaire type",
      "Population",
      "Citation",
      "Contact",
      "Tags"
    )
    output_df$Tags <- sapply(output_df$Tags, format_tags)
    output_df
  })
  
  proxy <- dataTableProxy("filtered_table")
  
  output$filtered_table <- renderDT({
    datatable(
      filtered_data(),
      filter = "top",
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
        autoWidth = TRUE,
        dom = 'lfrtip',
        language = list(search = "Search all columns:"),
        columnDefs = list(
          list(targets = 0, width = "4.545455%"),
          list(targets = 1, width = "13.63636%"),
          list(targets = 2, width = "13.63636%"),
          list(targets = 3, width = "12.63636%"),
          list(targets = 4, width = "6.818182%"),
          list(targets = 5, width = "6.818182%"),
          list(targets = 6, width = "6.818182%"),
          list(targets = 7, width = "18.18182%"),
          list(targets = 8, width = "5.818182%"),
          list(
            targets = 9,
            width = "11.09091%",
            searchable = FALSE
          )
        ),
        drawCallback = JS(
          "function(settings) {",
          "  var tooltips = [",
          "    'A unique number to identify each item',",
          "    '🗣️The item in its original language',",
          "    'The item translated to English. This may be blank if English is the original language of the item.',",
          "    '📝A description of the item as specified by the contributor(s), e.g., what the item measures',",
          "    'The possible name of the dataset that the item was used in',",
          "    'What kind of questionnaire the item was part of (regular, morning, evening, and/or event)',",
          "    'The population type the item was used for (children, adolescents, adults, elderly, general population, outpatient, and/or inpatient)',",
          "    '📚References to publications using the item.',",
          "    '📧Contact information for the item contributor(s)',",
          "    '🏷️Item tags expressing, for example, the measured construct of the item'",
          "  ];",
          "  this.api().columns().every(function(i) {",
          "    $(this.header()).attr('title', tooltips[i]);",
          "  });",
          "}"
        )
      )
    )
  }, server = FALSE)
  
  
  
  observeEvent(input$reset_btn, {
    proxy %>% selectRows(NULL) %>% selectPage(1) %>% clearSearch()
    selected_tags(character(0))
  })
  
  output$download_csv <- downloadHandler(
    filename = function()
      paste0("filtered_data_", Sys.Date(), ".csv"),
    content = function(file) {
      data_to_save <- if (is.null(input$filtered_table_rows_all))
        df
      else
        df[input$filtered_table_rows_all, ]
      data_to_save$tag_list <- NULL
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function()
      paste0("filtered_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      data_to_save <- if (is.null(input$filtered_table_rows_all))
        df
      else
        df[input$filtered_table_rows_all, ]
      data_to_save$tag_list <- NULL
      write.xlsx(data_to_save, file)
    }
  )
  
  output$download_citation <- downloadHandler(
    filename = function() {
      input$citation_format
    },
    content = function(file) {
      file.copy(from = file.path("www", input$citation_format),
                to = file)
    }
  )
  
}

shinyApp(ui, server)

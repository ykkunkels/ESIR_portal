library(shiny)
library(DT)
library(stringr)

# All tags
all_tags <- c(
  "activity", "anxiety", "appraisal", "arousal", "attention", "avoidance", "body image", "cognition",
  "coping", "depression", "emotion regulation", "event", "fatigue", "identity", "location",
  "methodological", "motivation", "negative affect", "physical health", "pleasure", "positive affect",
  "psychosis", "rumination", "self-esteem", "self-harm", "sleep", "social context", "social interaction",
  "social support", "stress", "substance use", "suicidality", "symptom", "worry"
)

# Fixed tag color palette
tag_colors <- setNames(
  c(
    "#1abc9c", "#3498db", "#9b59b6", "#e67e22", "#34495e", "#16a085", "#f39c12", "#7f8c8d",
    "#27ae60", "#e74c3c", "#8e44ad", "#2ecc71", "#95a5a6", "#d35400", "#c0392b", "#2980b9",
    "#f1c40f", "#bdc3c7", "#2c3e50", "#ff6f69", "#6c5ce7", "#00cec9", "#a29bfe", "#fd79a8",
    "#fab1a0", "#ffeaa7", "#636e72", "#b2bec3", "#dfe6e9", "#fdcb6e", "#e17055", "#ff7675",
    "#74b9ff", "#55efc4"
  ),
  all_tags
)

# UI
ui <- fluidPage(
  
  # Custom CSS and Google Fonts
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Oswald:wght@500&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
  body {
    font-family: 'Arial', sans-serif;
    background-color: #f4f6f7;
    margin: 0;
    padding: 0;
  }

  .title-panel {
  background-color: #54a36e;
  padding: 30px 40px;
  color: white;
  display: flex;
  align-items: center;
  justify-content: center;
  width: 100vw;
  margin-left: calc(-50vw + 50%);
  box-sizing: border-box;
  margin-bottom: 30px;
}


  .title-panel strong {
    font-family: 'Oswald', sans-serif;
    font-size: 44px;
    font-weight: 500;
    text-transform: uppercase;
    letter-spacing: 1.5px;
  }

  .selectize-input {
    border-radius: 15px;
    padding: 5px 15px;
    font-size: 14px;
    border: 2px solid #ddd;
    box-shadow: 0 0 5px rgba(0, 0, 0, 0.1);
  }

  .selectize-dropdown {
    border-radius: 10px;
  }

  .badge-tag {
    display: inline-block;
    padding: 6px 12px;
    margin: 4px 4px 4px 0;
    background-color: #2ecc71;
    color: white;
    border-radius: 20px;
    font-size: 12px;
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
"))
    
  ),
  
  # Title Panel
  div(
    class = "title-panel",
    tags$strong("Welcome to the ESM Item Repository!")
  ),
  
  fluidRow(
    column(
      width = 12,
      tags$div(
        style = "margin-bottom: 10px;",
        tags$strong("Filter by tags:"),
        selectizeInput(
          inputId = "selected_tags",
          label = NULL,
          choices = all_tags,
          multiple = TRUE,
          options = list(placeholder = 'Select tags to filter...')
        )
      ),
      div(class = "table-container", DTOutput("filtered_table")),
      div(style = "height: 20px;")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load and clean data
  df <- read.csv(url("https://osf.io/5ba2c/download"), sep = ",", stringsAsFactors = FALSE)
  df <- df[(-(1:3)), ]
  
  colnames(df) <- c(
    "item_ID", "label", "english", "description", "comment", "response_scale_discrete", "response_scale_vas",
    "branched_from", "branched_response", "beep_level", "beeps_per_day", "morning", "evening",
    "event", "other", "children", "adolescents", "adults", "elderly", "gen_pop",
    "outpatient", "inpatient", "which", "dataset", "contact", "open", "open_link", "request",
    "closed", "citation", "existing_adapt", "existing_ref", "how_admin", "how_long_beep",
    "reliability", "validity", "development", "item_use", "item_instructions", "item_other"
  )
  
  df$item_ID <- 1:nrow(df)
  
  tags_plain <- read.csv("tags_plain.csv")
  df <- merge(df, tags_plain, by = "item_ID", all.x = TRUE)
  colnames(df)[colnames(df) == "tag_final"] <- "tag"
  df$tag[df$tag == ""] <- NA
  
  df$tag_list <- strsplit(as.character(df$tag), ";\\s*")
  
  # Format tags as colored badges
  format_tags <- function(tag_string) {
    if (is.na(tag_string) || tag_string == "") return("")
    tags <- strsplit(tag_string, ";\\s*")[[1]]
    html <- paste0(
      sapply(tags, function(tg) {
        col <- tag_colors[[tg]]
        paste0('<span class="badge-tag" style="background-color:', col, ';">', tg, '</span>')
      }),
      collapse = " "
    )
    return(html)
  }
  
  # Reactive filtered data
  filtered_data <- reactive({
    selected <- input$selected_tags
    data <- df[, c("label", "english", "description", "citation", "beeps_per_day", "tag")]
    colnames(data) <- c("Item in original language", "Item in English", "Description", "Citation", "Beeps per day", "Tags")
    data$`Beeps per day` <- as.numeric(data$`Beeps per day`)
    
    if (!is.null(selected) && length(selected) > 0) {
      data <- data[unlist(lapply(df$tag_list, function(x) any(selected %in% x))), ]
    }
    
    data$Tags <- sapply(data$Tags, format_tags)
    data
  })
  
  # Render datatable
  output$filtered_table <- renderDT({
    datatable(
      filtered_data(),
      filter = "top",
      escape = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
        autoWidth = TRUE,
        dom = 'lfrtip',
        language = list(search = "Search all columns:"),
        columnDefs = list(
          list(targets = 5, searchable = FALSE, visible = TRUE),
          list(targets = 4, width = "130px"),
          list(targets = 3, width = "600px")
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Beeps per day", digits = 0)
  }, server = FALSE)
}

shinyApp(ui, server)

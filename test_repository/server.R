# load data
df <- read.csv(url("https://osf.io/5ba2c/download"), sep = ",", stringsAsFactors = FALSE)
df <- df[(-(1:3)), ]

# load packages
library(dplyr)

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
df[, "item_ID"] <- 1:nrow(df)

# create toy dataset for this purpose
df <- df[1:200, ]

# determine levels of variables for toy dataset
df <- df %>% 
  select(label, english, description, citation, beeps_per_day)
df$beeps_per_day <- as.numeric(df$beeps_per_day)
df$tags <- rep(c("tag1", "tag1;tag4", "tag6", NA), length.out = nrow(df))

# extract unique tags
tags <- unique(unlist(strsplit(na.omit(df$tags), ";")))

server <- function(input, output, session) {
  
  selected_tags <- reactiveVal(character())
  
  output$tag_buttons <- renderUI({
    tagList(
      lapply(tags, function(tag) {
        actionButton(paste0("tag_", tag), tag, class = "tag-btn")
      })
    )
  })
  
  observe({
    lapply(tags, function(tag) {
      observeEvent(input[[paste0("tag_", tag)]], {
        current_tags <- selected_tags()
        if (tag %in% current_tags) {
          selected_tags(setdiff(current_tags, tag))
        } else {
          selected_tags(c(current_tags, tag))
        }
      })
    })
  })
  
  filtered_data <- reactive({
    data <- df
    
    if (input$search_label != "") {
      data <- data %>% filter(grepl(input$search_label, label, ignore.case = TRUE))
    }
    if (input$search_english != "") {
      data <- data %>% filter(grepl(input$search_english, english, ignore.case = TRUE))
    }
    if (input$search_description != "") {
      data <- data %>% filter(grepl(input$search_description, description, ignore.case = TRUE))
    }
    if (input$search_citation != "") {
      data <- data %>% filter(grepl(input$search_citation, citation, ignore.case = TRUE))
    }
    if (!is.na(input$search_beeps)) {
      data <- data %>% filter(beeps_per_day == input$search_beeps)
    }
    
    if (length(selected_tags()) > 0) {
      data <- data %>%
        filter(!is.na(tags) & grepl(paste(selected_tags(), collapse = "|"), tags, fixed = TRUE))
    }
    
    
    data %>% select(-tags) # Hide tags column
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  observeEvent(input$clear_filters, {
    updateTextInput(session, "search_label", value = "")
    updateTextInput(session, "search_english", value = "")
    updateTextInput(session, "search_description", value = "")
    updateTextInput(session, "search_citation", value = "")
    updateNumericInput(session, "search_beeps", value = NA)
    selected_tags(character())
  })
}

shinyApp(ui, server)



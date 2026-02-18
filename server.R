
#########################
### Server - ESMIR    ###
### 2026; MP, LP, YKK ###
###~*~*~*~*~*~*~*~*~*~###

## Part 0: Initiation ----------------------------------------------------------

## Here, variables can be initiated to be used in the server function.
## For example for adding or formatting tags.

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


## Part 1: Main body of server -------------------------------------------------

## Here, the main server function is initiated. All the following code is run
## within this server function.

server <- function(input, output, session) {
  
  ## Load main dataset --------------------------------------------------------- 
  ## Here the main data is downloaded from OSF and read in as a .csv file. 
  
  # df <- read.csv(url("https://osf.io/5ba2c/download"), stringsAsFactors = FALSE)[-(1:3), ] #! Dit is nog niet juist! Toch?
  #! waarom worden hier de eerste 3 items verwijderd?? (of ik zie iets over het hoofd)
  #! Voor nu hieronder een versie waar dit niet gebeurd. 
  
  df <- read.csv(url("https://osf.io/5ba2c/download"), stringsAsFactors = FALSE)
  
  
  ## Add column names ----------------------------------------------------------
  # Assign column names manually (ensures consistent names)
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
  
  
  #! Ik snap niet wat hieronder gebeurd? ID's uit de dataset lijken al sequenteel?
  # table(df$item_ID == 1:nrow(df))
  
  #! Bedoel je dat we soms de ID's uit de originele dataset niet kunnen vertrouwen?
  #! If so, dan misschien handig om er een sanity check van te maken, bijv:
  
  ## Sanity Check
  # if(!any(table(df$item_ID == 1:nrow(df)))){
  #   
  #   df$item_ID <- 1:nrow(df)
  #   
  # }
  
  # Re-assign sequential numeric IDs to each row
  df$item_ID <- 1:nrow(df)
  
  
  # -------------------------
  # Create 'q_type' column for questionnaire type
  # -------------------------
  df <- df %>%
    mutate(
      q_type = paste(
        ifelse(str_to_lower(beep_level) %in% c("yes", "x"), "regular", NA),
        ifelse(str_to_lower(morning) %in% c("yes", "x"), "morning", NA),
        ifelse(str_to_lower(evening) %in% c("yes", "x"), "evening", NA),
        ifelse(str_to_lower(event) %in% c("yes", "x"), "event", NA),
        sep = "; "
      ),
      # Clean up extra "NA" entries and semicolons
      q_type = str_replace_all(q_type, "(NA; )+|; NA", ""),
      q_type = str_replace(q_type, "^NA$", "")
    )
  
  # -------------------------
  # Create 'population' column
  # -------------------------
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
      # Clean extra "NA" and semicolons
      population = str_replace_all(population, "(NA; )+|; NA", ""),
      population = str_replace(population, "^NA$", "")
    )
  
  # -------------------------
  # Load tags file and merge with main dataset
  # -------------------------
  tags_plain <- read.csv("tags_plain.csv")[, 2:3]  # columns: item_ID, tag_final
  df <- merge(df, tags_plain, by = "item_ID", all.x = TRUE)
  colnames(df)[colnames(df) == "tag_final"] <- "tag"
  df$tag[df$tag == ""] <- NA
  # Split tags into a list for easier filtering
  df$tag_list <- strsplit(as.character(df$tag), ";\\s*")
  
  # -------------------------
  # Helper function: format tags as HTML badges
  # -------------------------
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
  
  # -------------------------
  # Reactive value to store selected tags
  # -------------------------
  selected_tags <- reactiveVal(character(0))
  
  # -------------------------
  # Helper: darken a hex color
  # -------------------------
  darken_hex <- function(hex, amount = 0.15) {
    hex <- gsub("#", "", hex)
    if (nchar(hex) == 3) {
      hex <- paste0(rep(substr(hex, 1, 1), 2), rep(substr(hex, 2, 2), 2), rep(substr(hex, 3, 3), 2))
    }
    vals <- sapply(c(1, 3, 5), function(i)
      strtoi(substr(hex, i, i + 1), 16L))
    dark <- pmax(0, round(vals * (1 - amount)))
    sprintf("#%02x%02x%02x", dark[1], dark[2], dark[3])
  }
  
  # -------------------------
  # Render UI for tag selector badges
  # -------------------------
  output$tag_selector <- renderUI({
    tagList(lapply(all_tags, function(tag) {
      base_col <- tag_colors[[tag]]
      style <- paste0("background-color:", base_col, ";")
      classes <- "badge-tag"
      if (tag %in% selected_tags()) {
        darker <- darken_hex(base_col, 0.35)
        style <- paste0("background-color:", darker, ";")
        classes <- paste(classes, "selected")
      }
      # JavaScript click handler updates selected tag
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
  
  # -------------------------
  # Update selected tags when a badge is clicked
  # -------------------------
  observeEvent(input$tag_click, {
    current <- selected_tags()
    if (input$tag_click %in% current) {
      selected_tags(setdiff(current, input$tag_click))  # deselect
    } else {
      selected_tags(c(current, input$tag_click))        # add
    }
  })
  
  # -------------------------
  # Filter dataset based on selected tags and filters
  # -------------------------
  filtered_data <- reactive({
    data <- df
    
    # Filter based on selected tags
    
    if (length(selected_tags()) > 0) {
      data <- data[sapply(data$tag_list, function(tags) {
        if (is.null(tags))
          return(FALSE)
        
        if (input$tag_logic == "and") {
          all(selected_tags() %in% tags)
        } else {
          any(tags %in% selected_tags())
        }
      }), ]
    }
    
    # Filter based on sidebar filters - Item (original language)
    
    if (input$filter_item_og != "") {
      data <- data[grepl(input$filter_item_og, data$label, ignore.case = T),]
    }
    
    # Sidebar filters - Item (English)
    
    if (input$filter_item_english != "") {
      data <- data[grepl(input$filter_item_english, data$english, ignore.case = T),]
    }
    
    # Sidebar filters - Description 
    
    if(input$filter_desc != "") {
      data <- data[grepl(input$filter_desc, data$description, ignore.case = T),]
    }
    
    # Sidebar filters - Dataset 
    
    if(input$filter_dataset != "") { 
      data <- data[grepl(input$filter_dataset, data$dataset, ignore.case = T),]
    }
    
    # Sidebar filters - Questionnaire type 
    
    if(!is.null(input$filter_qtype) && length(input$filter_qtype) > 0){
      data <- data[data$q_type %in% input$filter_qtype,]
    }

    # Siderbar filters - Population 
    
    if(!is.null(input$filter_population) && length(input$filter_population) > 0){

      if(input$filter_population_andor == "or"){
        pattern <- paste(input$filter_population, collapse = "|")
        data <- data[grepl(pattern, data$population, ignore.case = T),]
      } else {
        pattern <- paste0("(?=.*", input$filter_population, ")", collapse = "")
        data <- data[grepl(pattern, data$population, ignore.case = T, perl = TRUE),]
      }
    }
    
    # Select only relevant columns for display
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
    
    # Convert Item ID to character for DT table input compatibility
    output_df$`Item ID` <- as.character(output_df$`Item ID`)
    
    # Format tags as HTML badges
    output_df$Tags <- sapply(output_df$Tags, format_tags)
    
    output_df
  })
  
  # -------------------------
  # Column tooltips for DT table
  # -------------------------
  column_tooltips <- c(
    "A unique number to identify each item",
    "The item in its original language",
    "The item translated to English. This may be blank if English is the original language of the item",
    "A description of the item as specified by the contributor(s), e.g., what the item measures",
    "The possible name of the dataset that the item was used in",
    "What kind of questionnaire the item was part of (regular [i.e., the questionnaire shown at every signal-contingent beep], morning, evening, and/or event)",
    "The population type the item was used for (children, adolescents, adults, elderly, general population, outpatient, and/or inpatient)",
    "References to publications using the item",
    "Contact information for the item contributor(s)",
    "Item tags expressing, for example, the measured construct of the item"
  )
  
  # -------------------------
  # Render interactive DT table
  # -------------------------
  output$filtered_table <- renderDT({
    datatable(
      filtered_data(),
      filter = "none",
      escape = FALSE,
      # allows HTML tags in table (for badges)
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
        autoWidth = TRUE,
        dom = 'lfrtip',
        language = list(search = "Search all columns:"),
        columnDefs = list(
          list(
            targets = 0,
            width = "5.5%",
            type = 'num'
          ),
          list(targets = 1:3, width = "12-13%"),
          list(targets = 4:6, width = "6.8%"),
          list(targets = 7, width = "18%"),
          list(targets = 8, width = "5.8%"),
          list(
            targets = 9,
            width = "11%",
            searchable = FALSE
          )
        ),
        # Custom JavaScript for Item ID range filtering
        initComplete = JS(
          "function(settings, json) {
            var api = this.api();
            var tableNode = api.table().node();
            var $container = $(api.table().container());
            var $inputs = $container.find('thead input');
            var $idInput = $inputs.eq(0);
            if (!tableNode._rangeFilterAdded) {
              tableNode._rangeFilterAdded = true;
              $.fn.dataTable.ext.search.push(function(settingsParam, data) {
                if (settingsParam.nTable !== tableNode) return true;
                var val = $idInput.val();
                if (!val || val.indexOf('-') === -1) return true;
                var parts = val.split('-').map(s => s.trim());
                var min = parseInt(parts[0], 10);
                var max = parseInt(parts[1], 10);
                if (isNaN(min) || isNaN(max)) return true;
                var idVal = parseInt(data[0], 10);
                return idVal >= min && idVal <= max;
              });
            }
            $idInput.off('keyup.DT input.DT change.DT').on('keyup input change', function() {
              var val = $(this).val();
              if (val && val.indexOf('-') !== -1) { api.column(0).search('').draw(); }
              else { api.column(0).search(val).draw(); }
            });
          }"
        ),
        # Add tooltips to headers
        headerCallback = JS(
          sprintf(
            "function(thead, data, start, end, display) {
             var tips = %s;
             $('th', thead).each(function(i) { $(this).attr('title', tips[i]); });
           }",
            jsonlite::toJSON(column_tooltips)
          )
        )
      )
    )
  }, server = FALSE)
  
  # -------------------------
  # Reset button: clears search and selected tags
  # -------------------------
  observeEvent(input$reset_btn, {
    dataTableProxy("filtered_table") %>%
      selectRows(NULL) %>%
      selectPage(1) %>%
      clearSearch()
    
    selected_tags(character(0))
    
    updateTextInput(session, "filter_item_og", value = "")
    updateTextInput(session, "filter_item_english", value = "")
    updateTextInput(session, "filter_desc", value = "")
    updateTextInput(session, "filter_dataset", value = "")
    updateSelectInput(session, "filter_qtype", selected = character(0))
    updateSelectInput(session, "filter_population", selected = character(0))
    updateRadioButtons(session, "filter_population_andor", selected = "or")
  })
  
  observeEvent(input$toggle_info, {
    shinyjs::toggle(id = "info_box")
    
    updateActionButton(
      session,
      "toggle_info",
      label = ifelse(
        grepl("Hide", input$toggle_info),
        "ℹ️ Show instructions",
        "ℹ️ Hide instructions"
      )
    )
  })
  
  # -------------------------
  # Download CSV of filtered or full data
  # -------------------------
  output$download_csv <- downloadHandler(
    filename = function()
      paste0("filtered_data_", Sys.Date(), ".csv"),
    content = function(file) {
      fd <- filtered_data()
      ids_char <- if (!is.null(input$filtered_table_rows_all) &&
                      length(input$filtered_table_rows_all) > 0) {
        fd[input$filtered_table_rows_all, "Item ID"]
      } else if (!is.null(input$filtered_table_rows_selected) &&
                 length(input$filtered_table_rows_selected) > 0) {
        fd[input$filtered_table_rows_selected, "Item ID"]
      } else
        fd[["Item ID"]]
      
      ids_num <- suppressWarnings(as.numeric(ids_char))
      ids_num <- ids_num[!is.na(ids_num)]
      
      if (length(ids_num) == 0) {
        showNotification("No filtered rows detected — saving full dataset.", type = "warning")
        data_to_save <- df
      } else {
        data_to_save <- df[match(ids_num, df$item_ID), ]
      }
      data_to_save$tag_list <- NULL
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
  
  # -------------------------
  # Download Excel (.xlsx)
  # -------------------------
  output$download_excel <- downloadHandler(
    filename = function()
      paste0("filtered_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      fd <- filtered_data()
      ids_char <- if (!is.null(input$filtered_table_rows_all) &&
                      length(input$filtered_table_rows_all) > 0) {
        fd[input$filtered_table_rows_all, "Item ID"]
      } else if (!is.null(input$filtered_table_rows_selected) &&
                 length(input$filtered_table_rows_selected) > 0) {
        fd[input$filtered_table_rows_selected, "Item ID"]
      } else
        fd[["Item ID"]]
      
      ids_num <- suppressWarnings(as.numeric(ids_char))
      ids_num <- ids_num[!is.na(ids_num)]
      
      if (length(ids_num) == 0) {
        showNotification("No filtered rows detected — saving full dataset.", type = "warning")
        data_to_save <- df
      } else {
        data_to_save <- df[match(ids_num, df$item_ID), ]
      }
      data_to_save$tag_list <- NULL
      write.xlsx(data_to_save, file)
    }
  )
  
  # -------------------------
  # Download citation file in selected format
  # -------------------------
  output$download_citation <- downloadHandler(
    filename = function()
      input$citation_format,
    content = function(file) {
      file.copy(from = file.path("www", input$citation_format),
                to = file)
    }
  )
  
}

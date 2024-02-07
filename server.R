
#############################
### ESIR Portal in Shiny  ###
### server version 1.1.11 ###
### MP, YKK - 07/02/2023  ###
###~*~*~*~*~*~*~*~*~*~*~*~###

## SERVER ----
server <- function(input, output, session) {

  ## Initialise values, read and format data----
  ## Define & initialise reactiveValues objects
  search_input <- reactiveValues(search_text = NA, match_no = 1)
  search_output <- reactiveValues(search_results = NA, found_something = FALSE, item_selection = "", citation = NA)
  
  ## Read ESIR item data from URL
  df <- read.csv(url("https://osf.io/5ba2c/download"), sep = ",", stringsAsFactors = FALSE) # Fetches the "ESIR.csv" file from OSF
  
  ## Format .csv and add proper column names
  df <- df[(-(1:3)), ]
  
  colnames(df) <- c("item_ID", "label", "english", "description", "comment", "response_scale_discrete", "response_scale_vas", 
                    "branched_from", "branched_response", "beep_level", "beeps_per_day", "morning", "evening", 
                    "event", "other", "children", "adolescents", "adults", "elderly", "gen_pop", 
                    "outpatient", "inpatient", "which", "dataset", "contact", "open", "open_link", "request", 
                    "closed", "citation", "existing_adapt", "existing_ref", "how_admin", "how_long_beep", 
                    "reliability", "validity", "development", "item_use", "item_instructions", "item_other")
  
  df[, "item_ID"] <- 1:nrow(df) #fill the item_ID column

  
  ## Search, Show all, and Clear buttons----
  ## Observe: go button click
    observeEvent(input$go, {
        search_input$match_no <- 1
        search_input$search_text <- tolower(input$search_text)
        
        ## Add population columns
        for (i in 1:nrow(df)) {df[i, "population"] <- paste(colnames(df[, c(15:19)])[which(df[i, c(15:19)] == "YES")], collapse = ", ")}
        
        ## Define columns to search when "All" is selected
        all_columns <- c("item_ID", "label", "english", "description", "dataset", "beeps_per_day", "population", "citation", "existing_ref", "contact")
        
        ## Search through specified columns
        if (input$topic_select == "all") {
          # Search across specified columns
          if (any(sapply(df[, all_columns], function(x)
            any(
              grepl(search_input$search_text, as.character(x))
            )))) {
            search_output$item_selection <-
              which(apply(df[, all_columns], 1, function(x)
                any(
                  grepl(search_input$search_text, as.character(x))
                )))
            
            ## Multiple matches exception
            if (length(search_output$item_selection) > 1) {
              output$warningtext <- renderUI({
                  s1 <- paste("We found", length(search_output$item_selection), "matches")
                  s2 <- paste("Use the buttons below to navigate your matches")
                  HTML(paste(s1, s2, sep = '<br/>'))
              })
              
            }else {
              output$warningtext <- renderText({paste("")})
             }
            
          }else {
            search_output$item_selection <- NA
            output$warningtext <- renderText({paste("")})
           }
        }else {
          # Search in the selected column
          if (any(grepl(search_input$search_text, as.character(df[, input$topic_select])) == TRUE)) {
            search_output$item_selection <- which(grepl(search_input$search_text, as.character(df[, input$topic_select])) == TRUE)
            
            ## Multiple matches exception
            if (length(search_output$item_selection) > 1) {
                  output$warningtext <- renderUI({
                    s1 <- paste("We found", length(search_output$item_selection), "matches")
                    s2 <- paste("Use the buttons below to navigate your matches")
                    HTML(paste(s1, s2, sep = '<br/>'))
                  })
              
            } else {
              output$warningtext <- renderText({paste("")})
            }
          } else {
            search_output$item_selection <- NA
            output$warningtext <- renderText({paste("")})
          }
        }
  }) # closing observeEvent(input$go, ...)
  
  
  ## Observe: all button click
  observeEvent(input$all, {
    
    search_input$match_no <- 1
    search_output$item_selection <- (1:nrow(df))
    
    output$warningtext <- renderUI({
      s1 <- paste("We found", length(search_output$item_selection),"matches")
      s2 <- paste("Use the buttons below to navigate your matches")
      HTML(paste(s1, s2, sep = '<br/>'))
    })
  }) # closing observeEvent(input$all, ...)

  
    ## Observe: reset button click
    observeEvent(input$reset, {
      
      updateTextInput(session, inputId = "search_text", label = "Search the Repository", value="")
      search_output$item_selection <- NA
      search_input$match_no <- 1
      
      output$warningtext <- renderText({paste("")})
      
      shinyjs::disable("first")
      shinyjs::hide("first")
      shinyjs::disable("previous_10")
      shinyjs::hide("previous_10")
      shinyjs::disable("previous")
      shinyjs::hide("previous")
      shinyjs::hide("match_no")
      shinyjs::disable("nextb")
      shinyjs::hide("nextb")
      shinyjs::disable("nextb_10")
      shinyjs::hide("nextb_10")
      shinyjs::disable("last")
      shinyjs::hide("last")
      
    }) # closing observeEvent(input$reset, ...)

  
    ## Shinyjs show and disable navigation buttons
    ## Observe start for buttonnav disable
    observeEvent(if(length(search_output$item_selection) == 1){
      
      shinyjs::disable("first")
      shinyjs::disable("previous")
      shinyjs::disable("previous_10")
      shinyjs::disable("nextb")
      shinyjs::disable("nextb_10")
      shinyjs::disable("last")
      
    }, {
    })
    
    ## Observe start for buttonnav enable
    observeEvent(if(length(search_output$item_selection) > 1){
      
      shinyjs::show("first");
      shinyjs::enable("first")
      shinyjs::show("previous")
      shinyjs::enable("previous")
      shinyjs::show("previous_10")
      shinyjs::enable("previous_10")
      shinyjs::show("match_no")
      shinyjs::show("nextb")
      shinyjs::enable("nextb")
      shinyjs::show("nextb_10")
      shinyjs::enable("nextb_10")
      shinyjs::show("last")
      shinyjs::enable("last")
      
    }, {
    })  
    
    ## Observe: Item navigation button clicks---- 
    observeEvent(input$first, {search_input$match_no <- 1})
  
    observeEvent(input$previous_10, {
      if(search_input$match_no  > 10){search_input$match_no <- (search_input$match_no - 10)}
    })
      
    observeEvent(input$previous, {
      if(search_input$match_no  > 1){search_input$match_no <- (search_input$match_no - 1)}
    })
      
    observeEvent(input$nextb, {
      if(search_input$match_no < length(search_output$item_selection)){search_input$match_no <- (search_input$match_no + 1)}
    })
      
    observeEvent(input$nextb_10, {
      if(search_input$match_no < (length(search_output$item_selection) - 9)){search_input$match_no <- (search_input$match_no + 10)}
    })
      
    observeEvent(input$last, {search_input$match_no <- length(search_output$item_selection)})
    
  
  ## Output: text----
  output$item_selection <- renderText({
    paste("Item ID:", search_output$item_selection)[search_input$match_no]
  })
  
  output$item_show <- renderText({
    paste("Original language:", as.character(df[search_output$item_selection, "label"])[search_input$match_no])
  })
  
  output$item_english <- renderText({
    paste("English:", as.character(df[search_output$item_selection, "english"])[search_input$match_no])
  })
  
  output$item_description <- renderText({
    paste("Description:", as.character(df[search_output$item_selection, "description"])[search_input$match_no])
  })
  
  output$item_dataset <- renderText({
    paste("Dataset:", as.character(df[search_output$item_selection, "dataset"])[search_input$match_no])
  })
  
  output$item_beeps_per_day <- renderText({
    paste("Beeps per day:", as.character(df[search_output$item_selection, "beeps_per_day"])[search_input$match_no])
  })
  
  output$item_population <- renderText({
    if(!is.na(search_output$item_selection[search_input$match_no])){
      c(paste(c("Children:", "| Adolescents:", "| Adults:", "| Elderly:", "| General:", "| Outpatient:", "| Inpatient:"), 
              as.character(df[search_output$item_selection[search_input$match_no], 
                              c("children", "adolescents", "adults", "elderly", "gen_pop", "outpatient", "inpatient")])))
    }
  })

  output$item_citation <- renderText({
    paste("Item citation:", as.character(df[search_output$item_selection, "citation"])[search_input$match_no])
  })
  
  output$existing_ref <- renderText({
    paste("Existing reference:", as.character(df[search_output$item_selection, "existing_ref"])[search_input$match_no])
  })
  
  output$item_contact <- renderText({
    paste("Contact:", as.character(df[search_output$item_selection, "contact"])[search_input$match_no])
  })
  
  output$match_no <- renderText({
    search_input$match_no
  })
  
  
  ## Temp: confetti logic
  observeEvent(session, { # on session start
    sendConfetti();Sys.sleep(0.5)
    sendConfetti();Sys.sleep(0.5)
    sendConfetti();Sys.sleep(0.5)
  })
  
  observeEvent(input$confetti_start, { # on actionbutton
    sendConfetti()
    message("You have sent ", input$sentConfetti, " confetti")
  })


  ## Download handlers----
  ## Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("ESM_Item_Rep_selection", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df[search_output$item_selection, ], file, row.names = FALSE)
    }
  )
  
  ## Downloadable .xlsx of selected dataset
  output$downloadData_Excel <- downloadHandler(
    filename = function() {
      paste0("ESM_Item_Rep_selection", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(df[search_output$item_selection, ], file, row.names = FALSE)
    }
  )
  
  
  ## Output: Images----
  output$logo <- renderUI({
    src_logo <- "http://ykkunkels.com/wp-content/uploads/2019/05/Repository_logo.png"
    div(id = "logo", tags$img(src = src_logo, width = "100%", height = "auto"))
  })
  
output$flow <- renderUI({
    src_flow <- "http://ykkunkels.com/wp-content/uploads/2019/05/Contributors-Workflow-Phase-1_v2_small.jpg"
    div(id = "flow", tags$img(src = src_flow, width = "85%", height = "auto"))
  })

## Temp: celebrate_image
output$celebrate_1000 <- renderUI({
  src_celeb <- "http://ykkunkels.com/wp-content/uploads/2024/02/celebrate_1000_v2.jpg"
  div(id = "celebrate_1000", tags$img(src = src_celeb, width = "1025px", height = "250px"))
})
  
} # closing server
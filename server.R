
#######################################
#### ESIR Portal in Shiny - Server ####
#### YKK - 09/5/2020              ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~####


## SERVER ----
server <- function(input, output, session) {
  
  ## Load and / or Install required packages
  if(!require('shiny')){install.packages('shiny', dep = TRUE)};library('shiny')
  if(!require('shinyjs')){install.packages('shinyjs', dep = TRUE)};library('shinyjs')
  if(!require('mailR')){install.packages('mailR', dep = TRUE)};library('mailR')
  if(!require('xlsx')){install.packages('xlsx', dep = TRUE)};library('xlsx')
  
  ## Define & initialise reactiveValues objects----
  search_input <- reactiveValues(search_text = NA, match_no = 1)
  search_output <- reactiveValues(search_results = NA, found_something = FALSE, item_selection = "", citation = NA)
  
  ## Read ESIR item data from URL----
  df <- read.csv(url("https://osf.io/5ba2c/download"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE) # Fetches the "ESIR-test.csv" file from OSF
  # df <- read.csv(url("https://osf.io/pdn4f/download"), sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE) # Fetches the "ESIR-test.csv" file from OSF
  
  ## Format .csv and add proper column names----
  df <- df[(-(1:3)), ]
  
  colnames(df) <- c("item_ID", "label", "english", "description", "comment", "response_scale_discrete", "response_scale_vas", 
                    "branched_from", "branched_response", "beep_level", "beeps_per_day", "morning", "evening", 
                    "event", "other", "children", "adolescents", "adults", "elderly", "gen_pop", 
                    "outpatient", "inpatient", "which", "dataset", "contact", "open", "open_link", "request", 
                    "closed", "citation", "existing_adapt", "existing_ref", "how_admin", "how_long_beep", 
                    "reliability", "validity", "development", "item_use", "item_instructions", "item_other")
  
  df[, "item_ID"] <- 1:nrow(df) #fill the item_ID column
  
  #! Use when negatives cannot b hard-coded in data.
  # df[which(df[, c("children")] != "yes"), "children"] <- "no"
  # df[which(df[, c("adolescents")] != "yes"), "adolescents"] <- "no"
  # df[which(df[, c("adults")] != "yes"), "adults"] <- "no"
  # df[which(df[, c("elderly")] != "yes"), "elderly"] <- "no"
  # df[which(df[, c("gen_pop")] != "yes"), "gen_pop"] <- "no"
  # df[which(df[, c("outpatient")] != "yes"), "outpatient"] <- "no"
  # df[which(df[, c("inpatient")] != "yes"), "inpatient"] <- "no"
  
  
    ## Observe start for buttonnav disable----
    observeEvent(if(length(search_output$item_selection) == 1){
      
      shinyjs::disable("first")
      shinyjs::disable("previous")
      shinyjs::disable("previous_10")
      shinyjs::disable("nextb")
      shinyjs::disable("nextb_10")
      shinyjs::disable("last")
      
    }, {
    })
  
  ## Observe start for buttonnav enable----
  observeEvent(if(length(search_output$item_selection) > 1){
    
    shinyjs::show("first")
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
    
  
    ## Observe: go button click---- 
    observeEvent(input$go, {
      
      search_input$match_no <- 1
      
      search_input$search_text <- tolower(input$search_text) #! all input text as lower-case
      
      
      ## Add population columns----
      for(i in 1:nrow(df)){
        
        df[i, "population"] <- paste(colnames(df[, c(15:19)])[which(df[i, c(15:19)] == "YES")], collapse = ", ")
        
      }
      
      
      ## Search through item .csv to fetch items including user search input----
      if(any(grepl(search_input$search_text, as.character(df[, input$topic_select])) == TRUE)){
        
        search_output$item_selection <- which(grepl(search_input$search_text, 
                                                    as.character(df[, input$topic_select])) == TRUE)
        
        
        ## Multiple matches exception----
        ## Warning text
        if(length(search_output$item_selection) > 1){
          
          output$warningtext <- renderUI({
            s1 <- paste("We found", length(search_output$item_selection),"matches")
            s2 <- paste("Use the buttons below to navigate your matches")
            HTML(paste(s1, s2, sep = '<br/>'))
          })
          
        }else{
          output$warningtext <- renderText({
            paste("")
          })
        }
        
      }else{
        
        search_output$item_selection <- NA
        
      } 
      
    })
  
  
  ## Observe: all button click---- 
  observeEvent(input$all, {
    
    search_input$match_no <- 1
    
    search_output$item_selection <- (1:nrow(df))
    
    output$warningtext <- renderUI({
      s1 <- paste("We found", length(search_output$item_selection),"matches")
      s2 <- paste("Use the buttons below to navigate your matches")
      HTML(paste(s1, s2, sep = '<br/>'))
    })
    
    
  })

  
    ## Observe: reset button click---- 
    observeEvent(input$reset, {
      
      updateTextInput(session, inputId = "search_text", label = "Search the Repository", value="")
      search_output$item_selection <- NA
      search_input$match_no <- 1
      
      output$warningtext <- renderText({
        paste("")
      })
      
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
      
    })
  
  
  ## Observe: send feedback button click---- 
  observeEvent(input$send, {
    
    updateTextInput(session, inputId = "feedback_text", label = "Search the Repository", value="")

    sender <- "postmaster@esmitemrepository.com"
    recipients <- c("admin@esmitemrepository.com")
    subject_title <- "Feedback for ESM Item Repository"
    email_body <- input$feedback_text
    
    send.mail(from = sender,
              to = recipients,
              subject = subject_title,
              body = email_body,
              smtp = list(host.name = "send.one.com", port = 587, 
                          user.name="postmaster@esmitemrepository.com", passwd="thankyou", ssl=TRUE),
              authenticate = TRUE,
              send = TRUE)
    
  })
  
  
  ## Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("ESM_Item_Rep_selection", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df[search_output$item_selection, ], file, row.names = FALSE)
    }
  )
  
  ## Downloadable csv of selected dataset ----
  output$downloadData_Excel <- downloadHandler(
    filename = function() {
      paste0("ESM_Item_Rep_selection", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(df[search_output$item_selection, ], file, row.names = FALSE)
    }
  )

  
  
  ## Observe: Item navigation button clicks---- 
  observeEvent(input$first, {
    
    search_input$match_no <- 1
    
  })

  observeEvent(input$previous_10, {
    
    if(search_input$match_no  > 10){
      
      search_input$match_no <- (search_input$match_no - 10)
      
    }
    
  })
    
    observeEvent(input$previous, {
      
      if(search_input$match_no  > 1){
        
        search_input$match_no <- (search_input$match_no - 1)
        
      }
      
    })
    
    observeEvent(input$nextb, {
      
      if(search_input$match_no < length(search_output$item_selection)){
        
        search_input$match_no <- (search_input$match_no + 1)
        
      }
      
    })
    
    observeEvent(input$nextb_10, {
      
      if(search_input$match_no < (length(search_output$item_selection) - 9)){
        
        search_input$match_no <- (search_input$match_no + 10)
        
      }
      
    })
    
    observeEvent(input$last, {
      
      search_input$match_no <- length(search_output$item_selection)
      
    })
    
    
  
  ## Output: text
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
      c(paste(c("Children:", "| Adolescents:", "| Adults:", "| Elderly:", "| General population:", "| Outpatient:", "| Inpatient:"), as.character(df[search_output$item_selection[search_input$match_no], c("children", "adolescents", "adults", "elderly", "gen_pop", "outpatient", "inpatient")])))
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


  output$logo <- renderText({c('<img src="',
                                "http://ykkunkels.com/wp-content/uploads/2019/05/Repository_logo.png",
                                '">')})
  
  output$flow <- renderText({c('<img src="',
                               "http://ykkunkels.com/wp-content/uploads/2019/05/Contributors-Workflow-Phase-1_v2_small.jpg",
                               '">')})
  
  
}



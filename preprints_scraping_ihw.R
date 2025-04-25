## Scraping preprints from OSF ##

if (!require(httr)) install.packages("httr", dependencies = TRUE)
if (!require(jsonlite)) install.packages("jsonlite", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(stringr)) install.packages("stringr", dependencies = TRUE)

# === Function to fetch OSF preprint metadata ===
fetch_osf_results <- function(keyword, max_pages = 5, request_timeout = 30) {
  base_url <- "https://api.osf.io/v2/preprints/"
  next_url <- paste0(base_url, "?q=", URLencode(keyword))
  collected_data <- list()
  page_count <- 0
  
  repeat {
    if (page_count >= max_pages) break
    page_count <- page_count + 1
    
    response <- tryCatch(
      {
        GET(next_url, add_headers(Accept = "application/json"), timeout(request_timeout))
      },
      error = function(e) {
        warning(paste("Error fetching OSF data for", keyword, ":", e$message))
        return(NULL)
      }
    )
    
    if (is.null(response)) {
      message("Response is NULL (timeout/connection issue), skipping this page.")
      break
    }
    
    if (status_code(response) != 200) {
      warning(paste("Failed to fetch OSF data for", keyword, "with status code", status_code(response)))
      break
    }
    
    content_data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.null(content_data$data)) {
      filtered <- content_data$data %>%
        select(id,
               title = attributes.title,
               abstract = attributes.description,
               paper_keywords = attributes.tags,
               link = links.html)
      collected_data <- append(collected_data, list(filtered))
    }
    
    if (!is.null(content_data$links$`next`)) {
      next_url <- content_data$links$`next`
      Sys.sleep(1)
    } else {
      break
    }
  }
  
  if (length(collected_data) > 0) {
    return(bind_rows(collected_data))
  } else {
    return(data.frame())
  }
}

# === Fetch metadata for each keyword ===
keywords <- c("Experience Sampling Method", "Ecological Momentary Assessment", "Daily Diary")
osf_results <- list()

for (keyword in keywords) {
  cat("Searching OSF for:", keyword, "\n")
  results <- fetch_osf_results(keyword, max_pages = 1500)
  osf_results[[keyword]] <- results
}

all_osf <- bind_rows(osf_results) %>% distinct(id, .keep_all = TRUE)
cat("\nTotal preprints fetched:", nrow(all_osf), "\n")
print(head(all_osf))

# Optionally, save metadata to CSV:
# write.csv(all_osf, "osf_preprints_ESM.csv", row.names = FALSE)

# === Updated Download Function Using the Files Detail Endpoint ===
download_preprints <- function(osf_df, request_timeout = 30) {
  if (nrow(osf_df) == 0) {
    message("No preprints available for download.")
    return(NULL)
  }
  
  for (i in 1:nrow(osf_df)) {
    preprint_link <- paste0(osf_df$link[i], "download")
    title <- osf_df$title[i]
    safe_title <- gsub("[^A-Za-z0-9_]", "_", title)
    message("Processing: ", title)
    
    # Download preprint 
    preprint_resp <- tryCatch({
      download.file(url = preprint_link, destfile = paste0(getwd(),"/downloaded preprints/", safe_title, ".pdf"), mode = "wb")
    }, error = function(e) {
      message("Error fetching the preprint", title, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(preprint_resp) || status_code(preprint_resp) != 200) {
      message("Failed to fetch preprint detail for ", title)
      next
    }
    
    Sys.sleep(1)
  }
  
  message("Download process complete!")
}

# === Run the download function ===
download_preprints(all_osf)

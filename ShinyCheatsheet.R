# R Cheatsheets app
# easily retrieve and explore R cheat sheets all in one app
# R Shiny app demo - display PDF in app as reference document
library(shiny)
library(dplyr)
library(stringr)
library(tidyverse)

# Define UI for data download app ----
ui <- fluidPage(
  # App title ----
  # titlePanel("R Cheatsheet Explorer"),
  # Sidebar layout with input and output definitions ----
  # sidebarLayout(
    # Sidebar panel for inputs ----
    # sidebarPanel(
      # Input: Choose dataset ----
      # Button
    # ),
    # Main panel for displaying outputs ----
    mainPanel(
      fluidPage(
        titlePanel("R Cheatsheet Explorer"),
        # Create a new row for the table.
        DT::dataTableOutput("table")
      )
    )
  # )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  # Filter data based on selections ----
  output$table <- DT::renderDataTable(DT::datatable({
    # https://stackoverflow.com/questions/25485216/how-to-get-list-files-from-a-github-repository-folder-using-r
    library(httr)
    library(stringr)
    #note that there is a default public rate limit; authenticated users get higher rate limit
    #didnt set that up here
    tryCatch(
      expr = {
      csheet_data <- readr::read_csv("https://raw.githubusercontent.com/parmsam/R-cheatsheets-explorer/main/csheet_data.csv")
      },
      error = {
      req <- GET("https://api.github.com/repos/rstudio/cheatsheets/git/trees/master?recursive=1")
      stop_for_status(req)
      filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
      
      cheatsheet_list <- filelist[filelist %>% str_detect("pdf")]
      
      base_url <- "https://github.com/rstudio/cheatsheets/raw/master/"
      
      base_gh_url <-"https://github.com/rstudio/cheatsheets/blob/master/"
  
      csheet_data <- cheatsheet_list %>% 
        as_data_frame() %>% 
        rename(`PDF Links` = value)
      
      # readr::write_csv(csheet_data, "csheet_data.csv")
    })
    
    csheet_data2 <- csheet_data %>% 
      cbind(Tags = cheatsheet_list) %>% 
      mutate(Tags = str_replace_all(Tags, "/",", ")) %>%
      mutate(Tags = str_replace_all(Tags, "([a-z])([A-Z])","\\1 \\2")) %>%
      mutate(Tags = str_replace_all(Tags, "[-_]"," ")) %>%
      mutate(Tags = str_remove_all(Tags, "\\.pdf")) %>%
      mutate(`PDF Links` = str_c(base_url, cheatsheet_list)) %>%
      mutate(`Github Links` = str_c(base_gh_url, cheatsheet_list)) %>%
      # mutate(`Embedded` = str_c('<iframe src=', `Github Links`, '></iframe>')) %>%
      mutate(`Github Links` = str_c("<a href='", `Github Links`, "'>Preview here</a>")) %>%
      mutate(`PDF Links` = str_c("<a href='", `PDF Links`, "'>Download here</a>"))
    
    csheet_data3 <- csheet_data2 %>% mutate(Language = str_extract(Tags, "translations, .*,") ) %>% 
      mutate(Language = str_remove(Language, "translations, ")) %>% 
      mutate(Language = str_remove(Language, "|(, [a-zA-Z ,]*)")) %>%
      mutate(Language = ifelse(is.na(Language), "english",  str_remove(Language, ",") )) %>%
      relocate(Tags, Language, `PDF Title`)
      
    csheet_data3
  }, escape = FALSE))
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      file.copy("https://github.com/rstudio/cheatsheets/raw/master/translations/ukrainian/data-import_ua.pdf")
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)

# app.R
library(shiny)
library(shinyWidgets)

all_event_data <- data.frame()
data_for_bubble <- gpt4_predict
ui <- fluidPage(
  tabName = "pico-bubble",
  
  box(
    width= 12,
    status = "primary",
    id = "pico_bubble_search_tab",
    #side = "left",
    
    tabPanel(title = "Population",
             
             fluidRow(column(width = 6,
                             pickerInput(
                               inputId = "select_pop_inter",
                               label = tags$p("Select an Intervention Provider", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                               choices = sort(unique(data_for_bubble$intervention_provider)),
                               selected = sort(unique(data_for_bubble$intervention_provider)),
                               multiple = TRUE,
                               options = pickerOptions(noneSelectedText = "Please Select",
                                                       virtualScroll = 100,
                                                       actionsBox = TRUE,
                                                       size = 10,
                               )
                             )),
                      column(width = 6,
                             pickerInput(
                               inputId = "select_pop_target",
                               label = tags$p("Select a Target Population", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                               choices = sort(unique(data_for_bubble$target_population)),
                               selected = sort(unique(data_for_bubble$target_population)),
                               multiple = TRUE,
                               options = pickerOptions(noneSelectedText = "Please Select",
                                                       virtualScroll = 100,
                                                       actionsBox = TRUE,
                                                       size = 10,
                               )
                               
                               
                             )
                      ) ),
             
             fluidRow(column(width = 4, 
                             pickerInput(
                               inputId = "select_discipline",
                               label = tags$p("Select a Discipline", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                               choices = sort(unique(data_for_bubble$discipline)),
                               selected = sort(unique(data_for_bubble$discipline)),
                               multiple = TRUE,
                               options = pickerOptions(noneSelectedText = "Please Select",
                                                       virtualScroll = 100,
                                                       actionsBox = TRUE,
                                                       liveSearch = TRUE,
                                                       size = 10,
                               )
                             )),
                      
                      column(width = 4,
                             pickerInput(
                               inputId = "select_funder",
                               label = tags$p("Select a Funder", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                               choices = sort(unique(data_for_bubble$method)),
                               selected = sort(unique(data_for_bubble$method)),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 `live-search` = TRUE
                               ))
                             
                             
                      ),
                      column(width = 4,
                             pickerInput(
                               inputId = "select_continent",
                               label = tags$p("Select First Author Continent", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                               choices = sort(unique(data_for_bubble$research_stage)),
                               selected = sort(unique(data_for_bubble$research_stage)),
                               multiple = TRUE,
                               options = pickerOptions(noneSelectedText = "Please Select",
                                                       virtualScroll = 100,
                                                       actionsBox = TRUE,
                                                       size = 10,
                               )
                             )
                             
                             
                      )
                      
             )
             
             
    )),
  
  box(
    
    width = 12,
    height = 700,
    id = "intervention_outcome",
    status = "primary",
    
    
    
    plotlyOutput("bubble_plot"),
    verbatimTextOutput("click"),
    tags$br(),
    tags$br()
    
    
  ),
  
  box(
    
    width = 12,
    id = "intervention_outcome_datatable",
    status = "primary",
    
    DT::dataTableOutput("pop_table") %>% withSpinner(color="#96c296")
    
    
  )
  
  
  )

rm(col_vector)

server <- function(input, output) {
   #rm(col_vector)
  #data_for_bubble$col <- "#266080"
  #assign("col_vector", data_for_bubble$col, envir = .GlobalEnv)
  
  # observe({
  #   bubble_data <- bubble_react()
  #   
  #       if (!is.null(input$select_pop_inter)) {
  #     #rv$original_data <- data  # Reset to the original data
  #     
  #         select_pop_inter <- input$select_pop_inter
  #     rm(col_vector, envir = .GlobalEnv)
  #     #data$col <- "#266080"
  #     bubble_react()$col <- "#266080"
  #   }
  # })
      #rm(col_vector, envir = .GlobalEnv)
  
  previous_state <- reactiveValues(
    column1 = NULL,
    column2 = NULL
  )
  
  bubble_react <- reactive({

    #browser()
    data_for_bubble <- data_for_bubble %>%
      filter(intervention_provider %in% input$select_pop_inter) %>% 
      filter(target_population %in% input$select_pop_target) %>% 
      filter(discipline %in% input$select_discipline) %>% 
      filter(research_stage %in% input$select_continent) %>% 
      filter(method %in% input$select_funder) 
      
      data <- data_for_bubble %>% 
      group_by(intervention, outcome_measures) %>% 
      count()
    
    data$key <- row.names(data)
    data$col <- "#266080"
    
    click_data <- event_data("plotly_click", priority = "event", source = "B")
    
    #browser()

        if (!is.null(click_data)) {
      
      bubble_react_new <- data %>%
        mutate(selected_colour = key %in% click_data$customdata)

      bubble_react_new$selected_colour <- data$key %in% click_data$customdata

      if (exists("col_vector")){
      
        bubble_react_new$col <- col_vector
      }
      
      selected_row <- which(rownames(bubble_react_new) %in% click_data$customdata)
      
      if (!bubble_react_new$col[selected_row] == "#47B1A3"){
        # Update only the selected rows
      bubble_react_new$col[selected_row] <- "#47B1A3"
      
      assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
      
      } else{
        
        bubble_react_new$col[selected_row] <- "#266080"
        
        assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
        
      }
      
      #assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)

      
    }
    else {
      
      data$col <- "#266080"
      
      bubble_react_new <- data
      
    }
    
    #previous_state <- bubble_react_new
    
    #browser()
    try(print(col_vector))
    if (!is.null(previous_state$column1) &&
        identical(data_for_bubble$intervention_provider, previous_state$column1) &&
        identical(data_for_bubble$target_population, previous_state$column2)) {
      # Columns are unchanged
      print("Columns are unchanged")
    } else {
      # Columns have changed
      print("Columns have changed")

      # Update the previous state
      previous_state$column1 <- data_for_bubble$intervention_provider
      previous_state$column2 <- data_for_bubble$target_population

      rm(col_vector, envir = .GlobalEnv)

    }
    
    
    
    
    return(bubble_react_new)
  })
  
  
  
  table_react <- reactive({
    
      table <- data_for_bubble %>%
        filter(intervention_provider %in% input$select_pop_inter) %>%
        filter(target_population %in% input$select_pop_target) %>%
        filter(discipline %in% input$select_discipline) %>%
        filter(research_stage %in% input$select_continent) %>%
        filter(method %in% input$select_funder) %>%
        left_join(included_with_metadata, by = "uid") %>%
        select(uid, year, author, title, intervention, outcome_measures, discipline, method, research_stage, doi, url) %>%
        mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
        arrange(desc(year))

      table$title <- paste0("<a href='",table$link, "' target='_blank'>",table$title,"</a>")

      table <- table %>%
        select(-doi, -url, -link)
      
      
      table_filter <- bubble_react() %>% 
        filter(col == "#47B1A3")
      
        selected_studies <- table %>%
          filter(intervention %in% table_filter$intervention,
                 outcome_measures %in% table_filter$outcome_measures)
    
    
    return(selected_studies)
  })
    
  
  output$bubble_plot <- renderPlotly({

    
    p <- plot_ly(bubble_react(),
                 x = ~intervention, y = ~outcome_measures, size = ~n, 
                 colors = ~sort(unique(col)), color = ~col, customdata = ~key,
                 type = 'scatter',
                 source = "B",
                 # mode = 'markers',
                 marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.8,
                               line = list(color = '#FFFFFF', width = 2)),
                 hoverinfo = 'text',
                 textposition = "none",
                 text = ~paste("Intevention:", intervention,
                               "<br>Outcome:", outcome_measures,
                               "<br>Number of Citations:", n)) %>% 
      layout(p, yaxis = list(title = list(text = "Outcome", standoff = 25)),
             xaxis = list(title = list(text = "Intervention", standoff = 25),
                          tickangle = -20,
                          ticklen = 1),
             hoverlabel = list(bgcolor = "white",
                               font = list(size = 14)),
             height = 550,
             showlegend = FALSE,
             clickmode = "event + select"
             #dragmode = "select"
             # autosize = F, width = 600, height = 600
      ) %>% 
      event_register(event = "plotly_click")
    
    return(p)
    
  })

    #selected_studies_all <- data.frame()

  output$pop_table <- DT::renderDataTable({

  
    DT::datatable(
      table_react(),
      rownames = FALSE,
      escape = FALSE,
      # extensions = c('Buttons'),
      options = list(
        language = list(
          zeroRecords = "Click on a point to show data",
          emptyTable = "Click on a point to show data"),
        deferRender = FALSE,
        scrollY = 600,
        scrollX = 100,
        scroller = TRUE,
        columnDefs = list(
          list(
            targets = c(2), #target for JS code
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 100 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
              "}")),
          list(
            targets = c(1,2), #target for JS code
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 15 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
              "}")),
          # list(
          #   targets = c(3:7), # columns 4, 5, and 6
          #   render = JS(
          #     "function(data, type, row, meta) {",
          #     "  if (type === 'display' && data) {",
          #     "  var words = data.split(';');",
          #     " var formattedText = words.map(function(word) {",
          #     "  var color =  '#' + ('000000' + Math.floor(Math.random()*16777215).toString(16)).slice(-6);",
          #     "      var textColor = (parseInt(color.substring(1), 16) > 0xffffff / 2) ? 'black' : 'white';",
          #     "      return '<span style=\"background-color:' + color + '; color:' + textColor + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
          #     # "      return '<span style=\"background-color:' + color + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
          #     "    }).join('; ');",
          #     "    return formattedText;",
          #     "  }",
          #     "  return data;",
          #     "}")
          # ),
          list(width = '10%', targets = "_all")
        )
      )
      
    )
  })
  
  
  }

shinyApp(ui, server)

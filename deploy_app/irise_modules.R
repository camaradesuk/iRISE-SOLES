#' Generate a Yearly Bar Chart
#'
#' This Shiny module creates a bar chart displaying the count of occurrences by year for a selected column in a given table. The chart includes a legend with a color-coded representation of the column values. This module is designed to be used within a Shiny application.
#'
#' @param id The module identifier.
#' @param title The title of the tab.
#' @param theme The color (status) of the tab.
#' @param spinner_colour The color of the spinner shown during plot generation.
#' @param table The input data table.
#'
#' @export
yearBarUI <- function(id, title = "", theme = "", spinner_colour = "#76A8C1", table) {
  
  ns <- NS(id)
  
  tabPanel(
    
    title = title,
    height = "800px",
    status = theme,
    
    materialSwitch(inputId = ns("switch_to_percentage"),
                   label = "Show percentages", 
                   status = "info"),
    
    plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour),
    
    fluidRow(
      column(width = 1),
      column(width = 9,
             div(
               style = "color: #1A465F;",
               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to
                                 {background: #1A465F; border-top:#1A465F; border-bottom: #1A465F; background-color: #1A465F;}")),
               
               sliderInput(inputId =  ns("year_bar_slider"),
                           label = "Select Year Range:",
                           min = as.numeric(min(table$year, na.rm = TRUE)),
                           max = as.numeric(max(table$year, na.rm = TRUE)),
                           value = c(2000,
                                     as.numeric(max(table$year, na.rm = TRUE))),
                           step = 1,
                           sep = "")
             )
      )
    )
  )
  
  
}

#' Server Function for Percentage Yearly Bar Chart
#'
#' This server function generates a percentage bar chart by year based on the input table, column name, and display option. The resulting chart displays the percentage of occurrences for each year and includes a legend with a color-coded representation of the selected column values.
#'
#' @param id The module identifier.
#' @param table The data table used for chart generation.
#' @param column The name of the column to use for chart generation.
#' @param order The order of legend items. Default is c("reported", "not reported").
#' @param display The value of the column to display. Default is "reported".
#' @param text Additional annotation text for the chart.
#' @param colours Colors for the chart elements.
#'
#' @export
yearBarServer <- function(id, table, column, order = c("reported", "not reported", "unknown"), 
                          display="reported", text="", colours = c("#76A8C1", "#FFC076", "grey")){
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly({
        
        cols <- setNames(colours, order)
        
        
        if(input$switch_to_percentage){
          
          if(length(display)>1){
            
            table %>%
              filter(!year == "unknown") %>%
              filter(!year == "") %>%
              filter(year >= min(input$year_bar_slider), 
                     year <= max(input$year_bar_slider)) %>%
              select(uid, year, data_col = .data[[column]]) %>%
              distinct() %>%
              select(-uid) %>%
              group_by_all() %>%
              count() %>%
              group_by(year) %>%
              mutate(percent = n/sum(n) * 100) %>%
              filter(data_col %in% display) %>%
              ungroup() %>%
              mutate(data_col = factor(data_col, levels = order)) %>% 
              plot_ly(x = ~year,
                      type = 'bar',
                      y = ~percent,
                      colors = cols,
                      color = ~data_col,
                      marker = list(line = list(color = 'black', width = 1)),
                      text = ~paste("<b>Info:</b> ", data_col,
                                    "<br><b>Year:</b> ", year,
                                    "<br><b>Percentage:</b>", round(percent, 2), "<b>%<b>"),
                      hoverinfo = "text") %>%
              layout(showlegend = TRUE,
                     yaxis = list(title = paste0("% of publications (", display, ")"), range = c(0, 100)),
                     xaxis = list(title = ""), barmode = "stack",
                     annotations =
                       list(x = 1, y = -0.2, text = text,
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                            font=list(size=12, color="black")))
            
          } else {
            
            
            table %>%
              filter(!year == "unknown", !year == "") %>%
              select(uid, year, data_col = .data[[column]]) %>%
              filter(year >= min(input$year_bar_slider), 
                     year <= max(input$year_bar_slider)) %>%
              distinct() %>%
              select(-uid) %>%
              group_by_all() %>%
              count() %>%
              group_by(year) %>%
              mutate(percent = n/sum(n) * 100) %>%
              mutate(
                data_col = factor(data_col, levels = c(TRUE, FALSE)) 
              ) %>%              
              plot_ly(x = ~year, 
                      y = ~percent,
                      type = "bar",
                      color = ~data_col,
                      colors = cols,
                      hoverinfo = 'text',
                      textposition = "none",
                      marker = list(line = list(color = 'black', width = 1)),
                      text = ~paste("<b>Year:</b> ", year,
                                    "<br><b>Percentage:</b>", round(percent, 2), "<b>%<b>")
              ) %>%
              layout(showlegend = FALSE,
                     yaxis = list(title = paste0("% of publications (", display, ")"), range = c(0, 100)),
                     xaxis = list(title = ""), barmode = "stack",
                     annotations = list(x = 1, y = -0.2, text = text,
                                        showarrow = F, xref = "paper", yref = "paper",
                                        xanchor = "right", yanchor = "bottom", xshift = 0, yshift = 0,
                                        font = list(size = 12, color = "black")))
            
          }
          
        } else {
          
          
          table %>%
            filter(!year == "unknown", !year == "") %>%
            filter(year >= min(input$year_bar_slider), 
                   year <= max(input$year_bar_slider)) %>%
            select(uid, year, data_col = .data[[column]]) %>%
            distinct() %>%
            group_by(year, data_col) %>%
            count() %>%
            mutate(data_col = factor(data_col, levels = order)) %>% 
            plot_ly(x = ~year,
                    type = 'bar',
                    y = ~n,
                    colors = cols,
                    color = ~data_col,
                    hoverinfo = 'text',
                    textposition = "none",
                    marker = list(line = list(color = 'black', width = 1)),
                    text = ~paste("<b>Info:</b> ", data_col,
                                  "<br><b>Number of Publications:</b>", n,
                                  "<br><b>Year:</b>", year)) %>%
            layout(showlegend = TRUE,
                   yaxis = list(title = 'Number of publications'),
                   xaxis = list(title = ""), barmode='stack',
                   annotations =
                     list(x = 1, y = -0.2, text = text,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                          font=list(size=12, color="black")))
          
          
        }
        
      })
    }
  )}


#' Generate a Bar Chart by Year
#'
#' This Shiny module creates a count bar chart by year based on a specified table and selected column. The resulting chart displays the count of occurrences of the values of the selected column for each year, along with a legend featuring a color-coded representation of those values. Intended for use within a Shiny application.
#'
#' @param id The module identifier.
#' @param title The title of the tab.
#' @param theme The color (status) of the tab.
#' @param spinner_colour The color of the spinner shown while the plot is generating.
#' @param table The input data table.
#'
#' @export
yearBarUI_included_only <- function(id, title = "", theme = "", spinner_colour = "#96c296", table) {
  
  ns <- NS(id)
  
  tabPanel(
    
    title = title,
    height = "800px",
    status = theme,
    
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour)
      )),
    
    fluidRow(
      column(width = 1),
      column(width = 9,
             div(
               style = "color: #1A465F;",
               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to
                                 {background: #1A465F; border-top:#1A465F; border-bottom: #1A465F; background-color: #1A465F;}")),
               
               sliderInput(inputId =  ns("included_year_slider"),
                           label = "Select Year Range:",
                           min = as.numeric(min(table$year, na.rm = TRUE)),
                           max = as.numeric(max(table$year, na.rm = TRUE)),
                           value = c(2000,
                                     as.numeric(max(table$year, na.rm = TRUE))),
                           step = 1,
                           sep = "")
             )
      )
    )
  )
  
  
}

#' Server Function for Percentage Yearly Bar Chart
#'
#' This server function generates a percentage bar chart by year based on the input table, column name, and display option. The resulting chart displays the percentage of occurrences for each year and includes a legend with a color-coded representation of the selected column values.
#'
#' @param id The module identifier.
#' @param table The data table used for chart generation.
#' @param column The name of the column to use for chart generation.
#' @param text Additional annotation text for the chart.
#' @param colour Color for the chart elements.
#'
#' @export
yearBarServer_included_only <- function(id, table, column, 
                                        text="", 
                                        colour = "#73D055FF"
){
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly({
        
        table %>%
          filter(
            year != "unknown",
            year != "",
            .data[[column]] == "included",
            year >= min(input$included_year_slider),
            year <= max(input$included_year_slider)
          ) %>%
          count(year, data_col = .data[[column]]) %>%
          plot_ly(
            x = ~year,
            y = ~n,
            type = 'bar',
            color = ~data_col,
            colors = colour,
            hoverinfo = 'text',
            textposition = "none",
            text = ~paste(
              "<br><b>Number of Publications:</b>", n,
              "<br><b>Year:</b>", year
            ),
            marker = list(
              line = list(color = 'black', width = 1)   # <-- thin black border
            )
          ) %>%
          layout(
            showlegend = FALSE,
            yaxis = list(title = 'Number of publications'),
            xaxis = list(title = "", tickangle = -45, ticklen = 4),
            barmode = 'stack',
            hoverlabel = list(bgcolor = "white", font = list(size = 14)),
            annotations = list(
              x = 1, y = -0.2, text = text,
              showarrow = FALSE, xref = 'paper', yref = 'paper',
              xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
              font = list(size = 12, color = "black")
            )
          )
        
      })
    }
  )}

#' Bar Chart by Year
#'
#' This shiny module generates a count bar chart by year based on a table and a selected column. The chart will display the count of occurrences of the values of the selected column for each year. The chart will also display a legend with a color-coded representation of the values of the selected column. The module is intended to be used within a Shiny application.
#'
#' @param id The module identifier.
#' @param title The title of the tab.
#' @param theme The colour (status) of the tab.
#' @param spinner_colour The colour of the spinner shown while the plot is generating.
#'
#' @export
yearBarUI_filters <- function(id, title = "", theme = "", spinner_colour = "#96c296", table) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(width = 12,
             div(
               # style = "margin-top: -20px; margin-left: -45px;",
               style = "margin-top: -20px; margin-left: -20px;;",  # adjust -40px as needed
               
               plotlyOutput(ns("plot"), height = "250px")
               # %>% withSpinner(color = spinner_colour)
             )
      )),
    fluidRow(
      style = "margin-top: -40px; margin-right: -39px; margin-left: -10px;",  # nudges row left
      column(width = 1),
      column(width = 11,
             div(
               style = "color: #1A465F; width: 90%;",  # <--- narrower than full column
               tags$head(
                 tags$style(HTML("
               /* Hide default grid labels */
               .irs-grid-text { display: none; }
               /* Hide min & max labels */
               .irs-min, .irs-max { display: none; }
               /* Move selected values below the slider */
               .irs-single, .irs-from, .irs-to {
                 top: 40px !important;
               }
             "))
               ),
               sliderInput(
                 inputId = ns("filter_year_slider"),
                 label = NULL,
                 min = min(table$year),
                 max = max(table$year),
                 value = c(min(table$year), max(table$year)),
                 step = 1,
                 sep = "",
                 ticks = FALSE
               )
             )
      )
    )
    
  )
}

#' Percentage Bar Chart by Year Server Function
#'
#' This is the server function for the percentbarYear module. It takes a table, a column name, and a display option as inputs. It generates a percentage bar chart by year based on the inputs. The chart will display the percentage of occurrences of the values of the selected column for each year. The chart will also display a legend with a color-coded representation of the values of the selected column.
#'
#' @param id The module identifier.
#' @param table The data table to use for generating the chart.
#' @param column The name of the column to use for generating the chart.
#' @param display The value of the column to display. Default is "reported".
#'
#' @export
yearBarServer_filters <- function(id, filter_table, column,
                                  text = "",
                                  colour = "#73D055FF") {
  moduleServer(
    id,
    function(input, output, session) {
      
      observe({
        tbl <- filter_table()
        req(tbl, tbl$year)
        
        
        if (is.null(input$reset_filters) || input$reset_filters == 0) {
          
          min_year <- min(tbl$year, na.rm = TRUE)
          max_year <- max(tbl$year, na.rm = TRUE)
          
          # If you want to reset the slider to the full range each time:
          updateSliderInput(
            session,
            "filter_year_slider",
            min = min_year,
            max = max_year,
            value = c(min_year, max_year)
          )
        }
      })
      
      filtered_tbl <- reactive({
        
        req(input$filter_year_slider)
        req(length(input$filter_year_slider) == 2)
        
        tbl <- filter_table()
        
        # Add highlight column instead of filtering out
        tbl <- tbl %>%
          mutate(
            highlight = ifelse(
              year >= min(input$filter_year_slider) &
                year <= max(input$filter_year_slider),
              "in_range", "out_range"
            )
          )
        tbl
      })
      
      # -------------------------------
      # Plotly output (cached)
      # -------------------------------
      output$plot <- 
        
        bindCache(
          renderPlotly({
            
            
            # tbl <- debounced_tbl()  # now this actually waits for 1 second of inactivity
            
            tbl <- filtered_tbl()
            cat("renderPlotly executing (cached), nrows:", nrow(tbl), "\n")
          
            tbl %>%
              plot_ly(
                x = ~year,
                type = 'bar', 
                y = ~is_included,
                # colors = colour,
                color = ~highlight,
                colors = c("out_range" = "grey80", "in_range" = colour),
                hoverinfo = 'text',
                textposition = "none",
                text = ~paste("<br><b>Number of Publications:</b>", is_included,
                              "<br><b>Year:</b>", year)
              ) %>%
              layout(
                showlegend = FALSE,
                title = list(
                  text = "Publications by Year", 
                  x = 0.05,
                  y = 0.95,
                  xanchor = "left",
                  font = list(size = 14, color = "#1A465F") 
                ),
                yaxis = list(
                  title = "",
                  tickfont = list(
                    size = 10,          
                    color = "black"   
                  )
                ),
                
                xaxis = list(
                  title = "",
                  ticks = "",
                  ticklen = 0,
                  showticklabels = FALSE
                ),
                barmode = 'group',   # <- change this
                annotations = list(
                  x = 1, y = -0.2, text = text,
                  showarrow = FALSE, xref = 'paper', yref = 'paper',
                  xanchor = 'right', yanchor = 'bottom',
                  font = list(size = 12, color = "black")
                )
              )
          })
          ,
          digest::digest(filter_table()),  # cache key
          input$filter_year_slider
        )
      
      outputOptions(output, "plot", suspendWhenHidden = FALSE)
      
      # -------------------------------
      # Return slider reactive
      # -------------------------------
      return(
        reactive({
          req(input$filter_year_slider)
          input$filter_year_slider
          
        })
      )
    }
  )
}


#' Pie chart indicating completion
#'
#' This shiny module generates a count bar chart by year based on a table and a selected column. The chart will display the count of occurrences of the values of the selected column for each year. The chart will also display a legend with a color-coded representation of the values of the selected column. The module is intended to be used within a Shiny application.
#'
#' @param id The module identifier.
#'
#' @export
completionPieUI <- function(id) {
  ns <- NS(id)

  plotlyOutput(ns("plot"), height="150px")
}


#' Pie chart showing % completion of tagging
#'
#' This is the server function for the percentbarYear module. It takes a table, a column name, and a display option as inputs. It generates a percentage bar chart by year based on the inputs. The chart will display the percentage of occurrences of the values of the selected column for each year. The chart will also display a legend with a color-coded representation of the values of the selected column.
#'
#' @param id The module identifier.
#' @param table The data table to use for generating the chart.
#' @param column The name of the column to use for generating the chart.
#' @param display The value of the column to display. Default is "reported".
#'
#' @export
completionPieServer <- function(id, table, con, identifier, included_studies, remove_failed = FALSE){
  moduleServer(
    id,
    function(input, output, session) {

      output$plot <- renderPlotly({

        colors <- c("#450e44", '#808080')

        if(remove_failed == TRUE){

          table <- table %>%
            filter(!status == "failed")
          table$status <- "tagged"

        } else {

          table$status <- "tagged"
        }

        df_count <- included_studies %>%
          left_join(table, by=identifier, multiple="all") %>%
          mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
          select(uid, cat) %>%
          distinct() %>%
          group_by(cat) %>%
          count()

        plot_ly(type='pie', labels=df_count$cat, values=df_count$n,
                textinfo='label+percent',
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 2)),
                insidetextorientation='radial') %>%
          layout(showlegend = FALSE,
                 margin = list(b = 30, l = 30, r = 30, t = 30, pad = 0, autoexpand = TRUE))

      })
    })
}


#' Box containing pie chart indicating completion
#'
#' This shiny modules generates a box containing a pie chart showing the tagged completion % of a given dataframe.
#'
#' @param id The module identifier.
#' @param title The title of the box/pie chart.
#' @param theme The status or colour theme of the box.
#' @param spinner_colour The colour of the loading spinner.
#' @param info_text The text to be shown in the box above the pie chart.
#'
#' @export
pie_completion_UI <- function(id, title, theme, spinner_colour, info_text) {
  ns <- NS(id)

  box(

    title = title,
    status = theme,
    width = 4,
    height = "300px",
    info_text,
    plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour)
  )

}

#' Pie chart showing % completion of tagging
#'
#' This shiny modules generates a box containing a pie chart showing the tagged completion % of a given dataframe.
#'
#' @param id The module identifier.
#' @param table The data table to use for generating the chart.
#' @param identifier The name of the column to use for generating the chart.
#' @param included_studies The table used for bringing in included studies.
#' @param colour_not_complete The colour for the section of pie showing "not complete".
#' @param colour_complete The colour for the section of pie showing "complete".
#' @param remove_failed Removes the rows where the status == "failed".
#'
#'
#' @export
pie_completion_Server <- function(id, table, identifier, included_studies, colour_not_complete, colour_complete, remove_failed = FALSE){
  moduleServer(
    id,
    function(input, output, session) {

      output$plot <- renderPlotly({

        colors <- c(colour_complete, colour_not_complete)

        if(remove_failed == TRUE){

          table <- table %>%
            filter(!status == "failed")
          table$status <- "tagged"

        } else {

          table$status <- "tagged"
        }

        df_count <- included_studies %>%
          left_join(table, by=identifier, multiple="all") %>%
          mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
          select(uid, cat) %>%
          distinct() %>%
          group_by(cat) %>%
          count()

        plot_ly(type='pie', labels=df_count$cat, values=df_count$n,
                textinfo='label+percent',
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 2)),
                height = 150,
                insidetextorientation='radial') %>%
          layout(showlegend = FALSE,
                 # height = 150,
                 margin = list(b = 30, l = 20, r = 20, t = 30, pad = 0, autoexpand = FALSE))

      })
    })
}



#' Sunburst plot UI
#'
#' This shiny module creates a sunburst plot
#' @param id The module identifier.
#'
#' @export
sunburstUI <- function(id, title = "", theme = "", spinner_colour) {
  ns <- NS(id)

  tabPanel(

    title = title,
    status = theme,
    width = 12,
    height = "800px",
    plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour)

  )

}

#' Sunburst Module
#'
#' This module generates a sunburst plot based on the provided data.
#'
#' @param input The input values from the Shiny app.
#' @param output The output values to be rendered in the Shiny app.
#' @param session The Shiny session object.
#' @param data The dataset containing the dataset you want to visualise
#'
#' @export
sunburstSever <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {

      output$plot <- renderPlotly({

        sb_data <- data %>%
          filter(!grepl("^Unknown", name))
        # CHANGED FOR IRISE
        df <- format_as_sunburst_2(sb_data, main_category, name)

        plot_ly(df,
                ids = ~ids,
                labels = ~labels,
                parents = ~parents,
                type = 'sunburst',
                values =  ~value,
                branchvalues = "total",
                insidetextorientation = 'auto',
                insidetextfont = list(size = 12),
                height = 800,
                width = 800,
                marker = list(colors = viridis::viridis(length(unique(data$main_category))),
                              line = list(color = "white", width = 1)),
                source = "sunburstPlot") %>%
          layout(autosize = F,
                 paper_bgcolor = "transparent",
                 plot_bgcolor = 'transparent',
                 margin = list(b = 40, l = 30, r = 30, t = 0, pad = 0, autoexpand = FALSE)
          )
      })
    })
}


#' PICO multi-select UI
#'
#' This shiny module creates a bar plot showing the number of publications based on the column inputs.
#' @param id The module identifier.
#' @param multi_select Changes to UI to 1 or 2 dropdown menus.
#' @param table The data table to use for generating the chart.
#' @param column The name of the column to use for generating the chart.
#' @param column The name of the second column to use for generating the chart (if multi-select is TRUE and 2 drowdown menus are required).
#' @param label1 The label shown above the first input.
#' @param label2 The label shown above the second input.
#' @param title The title of the tab.
#' @param theme The colour status of the tab.
#' @param spinner_colour The colour of the spinner used for loading the plot.
#'
#' @export
pico_multi_select_UI <- function(id,
                                 multi_select = TRUE,
                                 table,
                                 column,
                                 column2,
                                 label1,
                                 label2,
                                 title = "",
                                 theme,
                                 spinner_colour) {

  ns <- NS(id)
  shinyFeedback::useShinyFeedback()

  if (multi_select){

    tabPanel(
      title = title,
      status = theme,

      tags$style(HTML('.btn-light {
                    background-color: #efefef !important;
                    color: black !important;
                    }')),

      # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),

      pickerInput(
        inputId = ns("select_cat_picker"),
        label = label1,
        choices = sort(unique(column)),
        selected = sort(unique(column[!column %in% c("Unknown")])),
        multiple = TRUE,
        options = pickerOptions(noneSelectedText = "Please Select",
                                virtualScroll = 100,
                                actionsBox = TRUE,
                                size = 10,
        )
      ),

      suppressWarnings({
        selectizeInput(
          inputId = ns("select_type_picker"),
          label = label2,
          choices = sort(unique(column2)),
          selected = sort(unique(column2)),
          multiple = TRUE,
          options =  list(maxItems = 10,
                          virtualScroll = 100,
                          actionsBox = TRUE,
                          size = 10,
                          liveSearch = TRUE,
                          placeholder = "Select up to 10",
                          server = TRUE
          )
        )
      }),

      plotlyOutput(ns("multi_select_plot")) %>% withSpinner(color = spinner_colour),

      fluidRow(
        column(width = 1),
        column(width = 9,
               div(
                 style = "color: #1A465F;",
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to
                                 {background: #1A465F; border-top:#1A465F; border-bottom: #1A465F; background-color: #1A465F;}")),
                 sliderInput(inputId =  ns("pico_year_slider"),
                             label = "Select Year Range:",
                             min = as.numeric(min(table$year, na.rm = TRUE)),
                             max = as.numeric(max(table$year, na.rm = TRUE)) + 2,
                             value = c(2000,
                                       as.numeric(max(table$year, na.rm = TRUE))),
                             step = 1,
                             sep = ""))
        )
      )
    )
  } else {

    tabPanel(
      title = title,
      status = theme,

      tags$style(HTML('.btn-light {
                    background-color: #efefef !important;
                    color: black !important;
                    }')),

      pickerInput(
        inputId = ns("select_cat_picker"),
        label = label1,
        choices = sort(unique(column)),
        selected = sort(unique(column[!column %in% c("Unknown")])),
        multiple = TRUE,
        options = pickerOptions(noneSelectedText = "Please Select",
                                virtualScroll = 100,
                                actionsBox = TRUE,
                                size = 10,
        )
      ),

      plotlyOutput(ns("single_select_plot")) %>% withSpinner(color = spinner_colour),

      fluidRow(
        column(width = 1),
        column(width = 9,
               sliderInput(inputId =  ns("pico_year_slider"),
                           label = "Select Year Range:",
                           min = as.numeric(min(table$year, na.rm = TRUE)),
                           max = as.numeric(max(table$year, na.rm = TRUE)) + 2,
                           value = c(2000,
                                     as.numeric(max(table$year, na.rm = TRUE))),
                           step = 1,
                           sep = "")
        )
      )

    )
  }
}



#' Multi-select bar chart for PICO tabs
#'
#' This is the server function for the pico multi-select module. It takes a table, a multi-select boolean, a column name, a 2nd column name, and a text option as inputs. It generates a bar chart by year based on the inputs. The chart will display the number of publications of the selected column for each year. The chart will also display a legend with a color-coded representation of the values of the selected column.
#'
#' @param id The module identifier.
#' @param multi_select Changes to UI to 1 or 2 dropdown menus.
#' @param table The data table to use for generating the chart.
#' @param column The name of the column to use for generating the chart.
#' @param column The name of the second column to use for generating the chart (if multi-select is TRUE and 2 drowdown menus are required).
#' @param text The text to display below the chart stating the origin of the data.
#'
#' @export
pico_multi_select_Server  <- function(id,
                                      multi_select = TRUE,
                                      table,
                                      column,
                                      column2,
                                      text){
  moduleServer(
    id,
    function(input, output, session) {

      if (multi_select){

        interventions_in_cat <- reactive({

          data <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            filter(column %in% input$select_cat_picker) %>%
            filter(column2 %in% input$select_type_picker)
          return(data)

        })

        output$multi_select_plot <- renderPlotly({

          colours_col2 <- interventions_in_cat() %>%
            select(column2) %>%
            distinct() %>%
            top_n(10)

          color_palette <- c("#696969", "#800000", "#006400", "#000080", "#9acd32",
                             "#ff0000", "#ff8c00", "#ffd700", "#40e0d0", "#00ff00",
                             "#ba55d3", "#00fa9a", "#0000ff", "#ff00ff", "#1e90ff",
                             "#fa8072", "#dda0dd", "#ff1493", "#87cefa", "#ffe4b5")
          #color_palette <- brewer.pal(n = 10, name = 'Paired')
          color_mapping <- setNames(color_palette, colours_col2$column2)

          interventions_in_cat() %>%
            filter(year >= min(input$pico_year_slider),
                   year <= max(input$pico_year_slider)) %>%
            distinct() %>%
            #separate_rows(column, sep = ", ") %>%
            group_by(year, column, column2) %>%
            count() %>%
            plot_ly(x = ~year,
                    type = 'bar',
                    y = ~n,
                    color = ~column2,
                    colors = color_mapping,
                    hoverinfo = 'text',
                    textposition = "none",
                    text = ~paste("<b>Target:</b> ", column,
                                  "<br><b>Type:</b> ", column2,
                                  "<br><b>Number of Publications:</b>", n,
                                  "<br><b>Year:</b>", year)

            ) %>%
            layout(showlegend = TRUE,
                   yaxis = list(title = 'Number of Publications'),
                   xaxis = list(title = "",
                                #tickangle = -45,
                                ticklen = 5),
                   barmode = 'dodge',
                   hoverlabel = list(bgcolor = "white",
                                     font = list(size = 14)),
                   annotations =
                     list(x = 1, y = -0.2, text = text,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                          font=list(size=12, color="black")))

        })

        dynamic_updated_target_selection <- reactive({

          table_list <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            filter(column %in% input$select_cat_picker)

          table_list <- unique(table_list$column2)
          table_list <- as.vector(table_list)
          table_list <- sort(table_list)

          return(table_list)

        })

        observe({
          updateSelectizeInput(session, "select_type_picker",
                               server = TRUE,
                               choices = dynamic_updated_target_selection(),
                               selected = dynamic_updated_target_selection()[1:5])

        })

        observeEvent(input$select_type_picker, {

          selected_years <- table %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            filter(column2 %in% input$select_type_picker) %>%
            pull(year)

          updateSliderInput(session, "pico_year_slider",
                            min = min(selected_years),
                            max = max(selected_years),
                            value = c(2000,
                                      max(selected_years)),
                            step = 1)
        })

      } else {


        interventions_in_cat <- reactive({

          data <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>%
            filter(column %in% input$select_cat_picker)

          return(data)

        })

        output$single_select_plot <- renderPlotly({

          colours_col <- interventions_in_cat() %>%
            select(column) %>%
            distinct()

          #color_palette <- brewer.pal(n = 20, name = 'Paired')
          color_palette <- c("#696969", "#800000", "#006400", "#000080", "#9acd32",
                             "#ff0000", "#ff8c00", "#ffd700", "#40e0d0", "#00ff00",
                             "#ba55d3", "#00fa9a", "#0000ff", "#ff00ff", "#1e90ff",
                             "#fa8072", "#dda0dd", "#ff1493", "#87cefa", "#ffe4b5")
          color_mapping <- setNames(color_palette, colours_col$column)

          interventions_in_cat() %>%
            filter(year >= min(input$pico_year_slider),
                   year <= max(input$pico_year_slider)) %>%
            distinct() %>%
            #separate_rows(column, sep = ", ") %>%
            group_by(year, column) %>%
            count() %>%
            plot_ly(x = ~year,
                    type = 'bar',
                    y = ~n,
                    color = ~column,
                    colors = color_mapping,
                    hoverinfo = 'text',
                    textposition = "none",
                    text = ~paste("<b>Main Category:</b> ", column,
                                  #"<br><b>Type:</b> ", column2,
                                  "<br><b>Number of Publications:</b>", n,
                                  "<br><b>Year:</b>", year)

            ) %>%
            layout(showlegend = TRUE,
                   yaxis = list(title = 'Number of Publications'),
                   xaxis = list(title = "",
                                #tickangle = -45,
                                ticklen = 5),
                   barmode = 'dodge',
                   hoverlabel = list(bgcolor = "white",
                                     font = list(size = 14)),
                   annotations =
                     list(x = 1, y = -0.2, text = text,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                          font=list(size=12, color="black")))

        })

        observeEvent(input$select_cat_picker, {

          selected_years <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>%
            filter(column %in% input$select_cat_picker) %>%
            pull(year)

          updateSliderInput(session, "pico_year_slider",
                            min = min(selected_years),
                            max = max(selected_years),
                            value = c(2000,
                                      max(selected_years)),
                            step = 1)
        })

      }
    }
  )
}

# Module for info box ----
plot_interpret_UI <- function(id,
                              title = "How to interpret this plot",
                              info_text = "",
                              theme) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = title,
      background = theme,
      info_text)

  )
}

plot_interpret_Server  <- function(id){
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}


## Search UI ----
#' Search Database
#'
#' This shiny module creates the search page for the app
#'
#' @param id The module identifier.
#'
#' @export
# search_UI <- function(id, table) {
#   ns <- NS(id)
# 
# 
#   tagList(
# 
#     tabBox(width= 12,
#            status = "primary",
#            id = ns("search_tabs"),
#            side = "left",
# 
#            tabPanel(
#              value="basic_search_tab",
#              title = "Basic search",
# 
# 
#              fluidRow(
#                column(3,
#                       tags$p("Conduct a search for relevant articles", style = "color: black !important;font-family: KohinoorBangla, Sans-serif;")
#                       %>% shinyhelper::helper(type = "markdown", content = "searching", size="l", inline=T),
#                )),
# 
# 
#              textAreaInput(
#                inputId = ns("topic1"),
#                label = "add keywords separated by commas:",
#                value = ""),
# 
#              radioGroupButtons(
#                inputId = ns("search1_type"),
#                label = "Combine keywords with",
#                choices = c("AND", "OR"),
#                status = "primary",
#                individual = TRUE,
#                checkIcon = list(
#                  yes = tags$i(class = "fa fa-circle",
#                               style = "color: green"),
#                  no = tags$i(class = "fa fa-circle-o",
#                              style = "color: green"))
#              ),
# 
# 
#              actionBttn(
#                inputId = ns("search_button"),
#                label = "Search database",
#                style = "unite",
#                color = "primary"
#              ),
# 
#              actionBttn(
#                inputId = ns("reset_search"),
#                label = "Reset search query",
#                style = "unite",
#                color = "success"
#              )),
# 
#            tabPanel(
#              title = "Advanced search",
#              value="adv_search_tab",
# 
# 
#              textAreaInput(
#                inputId = ns("topic1_adv"),
#                label = "Search #1: add keywords separated by commas:",
#                value = ""),
# 
#              radioGroupButtons(
#                inputId = ns("search1_type_adv"),
#                label = "Combine keywords with",
#                choices = c("AND", "OR"),
#                status = "primary",
#                individual = TRUE,
#                checkIcon = list(
#                  yes = tags$i(class = "fa fa-circle",
#                               style = "color: green"),
#                  no = tags$i(class = "fa fa-circle-o",
#                              style = "color: green"))
#              ),
# 
#              textAreaInput(
#                inputId = ns("topic2_adv"),
#                label = "Search #2: add keywords separated by commas:",
#                value = ""
#              ),
# 
#              radioGroupButtons(
#                inputId = ns("search2_type_adv"),
#                label = "Combine keywords with",
#                choices = c("AND", "OR"),
#                status = "primary",
#                individual = TRUE,
#                checkIcon = list(
#                  yes = tags$i(class = "fa fa-circle",
#                               style = "color: green"),
#                  no = tags$i(class = "fa fa-circle-o",
#                              style = "color: green"))
#              ),
# 
#              radioGroupButtons(
#                inputId = ns("comb_1_2"),
#                label = "Combine searches with...",
#                choices = c("AND", "OR"),
#                status = "primary"),
# 
#              actionBttn(
#                inputId = ns("adv_search_button"),
#                label = "Search database",
#                style = "unite",
#                color = "danger"
#              ),
# 
#              actionBttn(
#                inputId = ns("adv_reset_search"),
#                label = "Reset search query",
#                style = "unite",
#                color = "success"
#              )
#            )
#     ),
# 
# 
#     box(width= 12,
#         maximizable = TRUE,
#         solidHeader = TRUE,
#         status = "secondary",
#         title = "Selected studies in database",
# 
#         textOutput(ns("search_results_text")),
#         tags$head(tags$style("#search_results_text{color: green;
#                                        font-size: 20px;
#                                        font-style: italic;
#                                        }"
#         )),
# 
#         div(style="display: inline-block;vertical-align:top; width: 50px;",
# 
# 
#             dropdown(inputId = ns("dropdown_menu"),
# 
#                      tags$h3("Filter studies"),
# 
#                      shinyjs::useShinyjs(),
# 
#                      #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
#                      sliderInput(ns("year_slider"),
#                                  "Year Published",
#                                  as.numeric(min(table$year, na.rm=TRUE)),
#                                  as.numeric(max(table$year, na.rm=TRUE)),
#                                  value = c(min(table$year, na.rm=TRUE), max(table$year, na.rm=TRUE)), sep=""),
# 
# 
# 
#                      uiOutput(ns("dynamic_dropdowns")),
# 
#                      style = "unite",
#                      icon = icon("filter"),
#                      inline =TRUE,
#                      status = "danger", width = "600px",
#                      animate = animateOptions(
#                        enter = animations$fading_entrances$fadeInLeftBig,
#                        exit = animations$fading_exits$fadeOutRightBig),
#                      tooltip = tooltipOptions(title = "Click to filter studies"),
# 
#                      actionBttn(inputId = ns("submit_filters"),
#                                 label = "Apply filters"),
# 
#                      # prettySwitch(inputId = ns("highly_sensitive"),
#                      #              label = "High sensitivity")
#             )
#         ),
# 
#         div(style="display: inline-block;vertical-align:top; width: 50px;",
# 
#             dropdown(
#               downloadBttn(
#                 ns("download_csv"),
#                 label = "Download citations in CSV format",
#                 style = "unite",
#                 color = "primary",
#                 size = "sm",
#                 block = FALSE,
#                 no_outline = TRUE
#               ),
#               downloadBttn(
#                 ns("download_endnote"),
#                 label = "Download citations in Endnote tab delimited format",
#                 style = "unite",
#                 color = "primary",
#                 size = "sm",
#                 block = FALSE,
#                 no_outline = TRUE
#               ),
#               downloadBttn(
#                 ns("download_syrf"),
#                 label = "Download citations in SyRF upload format",
#                 style = "unite",
#                 color = "primary",
#                 size = "sm",
#                 block = FALSE,
#                 no_outline = TRUE
#               ),
# 
#               br(),
#               p("Note for Rayyan export option below: download file and open in MS Excel first on your computer. Save as .csv in excel, then import saved file into Rayyan"),
#               downloadBttn(
#                 ns("download_rayyan"),
#                 label = "Download citations in Rayyan upload format",
#                 style = "unite",
#                 color = "primary",
#                 size = "sm",
#                 block = FALSE,
#                 no_outline = TRUE
#               ),
# 
#               style = "unite", icon = icon("download"),
#               inline = TRUE,
#               status = "success", width = "600px",
#               animate = animateOptions(
#                 enter = animations$fading_entrances$fadeInLeftBig,
#                 exit = animations$fading_exits$fadeOutRightBig),
#               tooltip = tooltipOptions(title = "Click to download relevant studies")
# 
#             ),
# 
#         ),
# 
# 
#         DT::dataTableOutput(ns("search_results_studies")) %>% withSpinner(color="#96c296")
# 
#     )
# 
# 
#   )
# 
# 
# }


#' Search Page Module
#'
#' This Shiny module generates the search page for the app, allowing users to search and filter studies.
#'
#' @param id The module identifier.
#' @param table A dataframe of included studies with metadata.
#' @param combined_pico_table A combined dataframe of PICO tags.
#' @param pico_data A list of dynamic search updates based on the PICO dropdown filters.
#' @param citations_for_download A dataframe containing citations for download.
#' @param project_name The name of the project.
#'
#' @export
# search_Server <- function(id,
#                           table,
#                           combined_pico_table,
#                           pico_data = list(),
#                           citations_for_download,
  #                         project_name = "") {
  # moduleServer(
  #   id,
  #   function(input, output, session) {
  # 
  #     ns <- NS(id)
  # 
  # 
  #     # Creates list for dropdown menus
  #     dynamic_dropdowns <- list()
  #     output$dynamic_dropdowns <- renderUI({
  #       dynamic_dropdowns <- lapply(pico_data, function(item) {
  #         pico_dropdown_UI(
  #           id = ns(item$id),
  #           label1 = item$label1,
  #           label2 = item$label2,
  #           label3 = item$label3,
  #           label4 = item$ilabel4,
  #           column1 = item$table[[item$column1]],
  #           column2 = item$table[[item$column2]],
  #           column3 = item$table[[item$column3]],
  #           column4 = item$table[[item$column4]],
  #           filter_no = item$filter_no
  #         )
  #       })
  #       do.call(tagList, dynamic_dropdowns)
  #     })
  # 
  #     # Creates list for dropdown menus reactivity
  #     pico_element_list <- list()
  #     pico_element_list <- lapply(pico_data, function(pico_item) {
  #       pico_dropdown_Server(
  #         id = pico_item$id,
  #         table = pico_item$table,
  #         column1 = pico_item$column1,
  #         column2 = pico_item$column2,
  #         column3 = pico_item$column3,
  #         column4 = pico_item$column4,
  #         filter_no = pico_item$filter_no
  # 
  #       )
  #     })
  # 
  #     # Creates table list for filtering data
  #     pico_table_list <- list()
  #     pico_table_list <- lapply(pico_data, function(element) element$table)
  # 
  # 
  #     # Create reactive values as triggers
  #     values <- reactiveValues()
  #     values$search_query <- ""
  #     values$reset_button <- ""
  #     values$submit_filters <- ""
  # 
  # 
  #     observeEvent(input$reset_search, {
  # 
  # 
  #       updateTextAreaInput(session, "topic1",
  #                           value = "")
  # 
  #       values$search_query <- ""
  #       values$reset_button <- "reset"
  #       values$submit_filters <- ""
  # 
  #       dynamic_dropdowns <- list()
  #       output$dynamic_dropdowns <- renderUI({
  #         dynamic_dropdowns <- lapply(pico_data, function(item) {
  #           pico_dropdown_UI(
  #             id = ns(item$id),
  #             label1 = item$label1,
  #             label2 = item$label2,
  #             label3 = item$label3,
  #             label4 = item$ilabel4,
  #             column1 = item$table[[item$column1]],
  #             column2 = item$table[[item$column2]],
  #             column3 = item$table[[item$column3]],
  #             column4 = item$table[[item$column4]],
  #             filter_no = item$filter_no
  #           )
  #         })
  #         do.call(tagList, dynamic_dropdowns)
  # 
  #       })
  # 
  #       updateSliderInput(session = session,
  #                         inputId = "year_slider",
  #                         label = "Year Published",
  #                         min = as.numeric(min(table$year, na.rm=TRUE)),
  #                         max = as.numeric(max(table$year, na.rm=TRUE)),
  #                         value = c(min(table$year, na.rm=TRUE), max(table$year, na.rm=TRUE)))
  # 
  #       shinyjs::click("dropdown_menu")
  # 
  #       observe({
  #         # Introduce a delay of 1 second
  #         shinyjs::delay(250, {
  #           # Run the click function after the delay
  #           shinyjs::click("dropdown_menu")
  #         })
  #       })
  #     })
  # 
  #     observeEvent(input$adv_reset_search, {
  # 
  # 
  #       updateTextAreaInput(session, "topic1_adv",
  #                           value = "")
  # 
  #       updateTextAreaInput(session, "topic2_adv",
  #                           value = "")
  # 
  #       values$search_query <- ""
  #       values$reset_button <- "reset"
  #       values$submit_filters <- ""
  # 
  #       dynamic_dropdowns <- list()
  #       output$dynamic_dropdowns <- renderUI({
  #         dynamic_dropdowns <- lapply(pico_data, function(item) {
  #           pico_dropdown_UI(
  #             id = ns(item$id),
  #             label1 = item$label1,
  #             label2 = item$label2,
  #             label3 = item$label3,
  #             label4 = item$ilabel4,
  #             column1 = item$table[[item$column1]],
  #             column2 = item$table[[item$column2]],
  #             column3 = item$table[[item$column3]],
  #             column4 = item$table[[item$column4]],
  #             filter_no = item$filter_no
  #           )
  #         })
  #         do.call(tagList, dynamic_dropdowns)
  # 
  #       })
  # 
  #       updateSliderInput(session = session,
  #                         inputId = "year_slider",
  #                         label = "Year Published",
  #                         min = as.numeric(min(table$year, na.rm=TRUE)),
  #                         max = as.numeric(max(table$year, na.rm=TRUE)),
  #                         value = c(min(table$year, na.rm=TRUE), max(table$year, na.rm=TRUE)))
  # 
  #       shinyjs::click("dropdown_menu")
  # 
  #       observe({
  #         # Introduce a delay of 1 second
  #         shinyjs::delay(250, {
  #           # Run the click function after the delay
  #           shinyjs::click("dropdown_menu")
  #         })
  #       })
  #     })
  # 
  # 
  #     observeEvent(c(input$search_button, input$adv_search_button),  {
  # 
  #       values$search_query <- "NOT BLANK"
  #       values$reset_button <- ""
  # 
  #     },  ignoreInit = TRUE)
  # 
  #     observeEvent(c(input$submit_filters),  {
  # 
  #       values$reset_button <- ""
  #       values$submit_filters <- "clicked"
  #     },  ignoreInit = TRUE)
  # 
  # 
  #     # getting your search results - reactive object search_results runs query on data and returns datatable
  #     search_query <- eventReactive(c(input$search_button, input$adv_search_button), {
  # 
  #       if(input$search_tabs == "basic_search_tab"){
  # 
  #         query1 <- input$topic1 %>%
  #           str_trim() %>%
  #           str_replace_all(pattern = ", ", repl = ",") %>%
  #           str_replace_all(pattern = " ,", repl = ",") %>%
  #           str_split("\\,") %>%
  #           unlist() %>%
  #           as.list() %>%
  #           lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
  #                                   x, ignore.case = TRUE))
  # 
  #         query1 <- ifelse(input$search1_type == "OR",
  #                          paste(query1,collapse="|"),
  #                          paste0("^(?=.*", paste0(query1, collapse=")(?=.*"),
  #                                 ").*$"))
  # 
  #         query1 <- query1 %>%
  #           str_trim()
  # 
  #         return(query1)
  # 
  #       }
  # 
  #       else{
  # 
  #         query1_adv <- input$topic1_adv %>%
  #           str_trim() %>%
  #           str_replace_all(pattern = ", ", repl = ",") %>%
  #           str_replace_all(pattern = " ,", repl = ",") %>%
  #           str_split("\\,") %>%
  #           unlist() %>%
  #           as.list() %>%
  #           lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
  #                                   x, ignore.case = TRUE))
  # 
  #         query1_adv <- ifelse(input$search1_type_adv == "OR",
  #                              paste(query1_adv,collapse="|"),
  #                              paste0("^(?=.*", paste0(query1_adv, collapse=")(?=.*"),
  #                                     ").*$"))
  # 
  # 
  #         query1_adv <- query1_adv %>%
  #           str_trim()
  # 
  # 
  #         query2_adv <- input$topic2_adv %>%
  #           str_trim() %>%
  #           str_replace_all(pattern = ", ", repl = ",") %>%
  #           str_replace_all(pattern = " ,", repl = ",") %>%
  #           str_split("\\,") %>%
  #           unlist() %>%
  #           as.list() %>%
  #           lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
  #                                   x, ignore.case = TRUE))
  # 
  #         query2_adv <- ifelse(input$search2_type_adv == "OR",
  #                              paste(query2_adv,collapse="|"),
  #                              paste0("^(?=.*", paste0(query2_adv, collapse=")(?=.*"),
  #                                     ").*$"))
  # 
  # 
  #         query2_adv <- query2_adv %>%
  #           str_trim()
  # 
  #         #try(query_final <- query1)
  #         try(query_final <- c(query1_adv, query2_adv))
  #         return(query_final)
  #       }
  #     })
  # 
  # 
  #     search_results <- reactive({
  # 
  #       # If there is no search query and reset button pressed, return entire table
  #       if(values$search_query == "" & values$reset_button == "reset"){
  # 
  #         selected_studies <- table
  # 
  #       }
  # 
  #       # If there is no search query and no reset then proceed with entire table
  #       else if(values$search_query == ""){
  # 
  #         selected_studies <- table
  # 
  #       }
  # 
  # 
  #       else if(search_query()[1] == ""){
  # 
  #         selected_studies <- table
  # 
  #       }
  # 
  #       # if there is a search query
  #       else{
  # 
  #         selected_studies <- table
  # 
  #         if(input$search_tabs == "basic_search_tab"){
  # 
  #           # If search has been performed in Adv search then user goes back to basic search there is a reset.
  #           if (length(search_query()) > 1){
  # 
  #             shinyjs::click("adv_reset_search")
  # 
  #           } else{
  # 
  #             selected_studies <- selected_studies[with(selected_studies,
  #                                                       grepl(search_query(),
  #                                                             paste(title, abstract, keywords),
  #                                                             ignore.case=TRUE,
  #                                                             perl=TRUE)),]
  # 
  #             withProgress(message = 'Performing search',
  #                          detail = 'This may take a little while...', value = 0, {
  #                            for (i in 1:25) {
  #                              incProgress(1/15)
  #                              Sys.sleep(0.5)
  #                            }
  #                          })
  #           }
  #         } else if (is.na(search_query()[1]) | is.na(search_query()[2])){
  # 
  # 
  #           shinyjs::click("adv_reset_search")
  # 
  #           selected_studies <- table
  # 
  #         } else{
  # 
  #           selected_studies1 <- table[with(table,
  #                                           grepl(search_query()[1],
  #                                                 paste(title, abstract, keywords),
  #                                                 ignore.case=TRUE,
  #                                                 perl=TRUE)),]
  #           selected_studies2 <- table[with(table,
  #                                           grepl(search_query()[2],
  #                                                 paste(title, abstract, keywords),
  #                                                 ignore.case=TRUE,
  #                                                 perl=TRUE)),]
  # 
  # 
  #           if(input$comb_1_2 == "AND"){
  # 
  #             selected_studies <- table %>%
  #               filter(uid %in% selected_studies1$uid & uid %in% selected_studies2$uid) %>%
  #               distinct()
  #           }
  # 
  #           else{
  #             selected_studies <- table %>%
  #               filter(uid %in% c(selected_studies1$uid, selected_studies2$uid)) %>%
  #               distinct()
  #           }
  # 
  #           withProgress(message = 'Performing search',
  #                        detail = 'This may take a little while...', value = 0, {
  #                          for (i in 1:25) {
  #                            incProgress(1/15)
  #                            Sys.sleep(0.5)
  #                          }
  #                        })
  # 
  #         }}
  #       return(selected_studies)
  # 
  #     })
  # 
  # 
  #     filter_results <- reactive({
  # 
  #       selected_studies <- search_results()
  # 
  #       # If reset button clicked then tidy entire table and return
  #       if(values$reset_button == "reset"){
  # 
  #         selected_studies <- as.data.frame(selected_studies)
  # 
  #         combined_pico_table <- unique(combined_pico_table)
  # 
  #         selected_studies <- selected_studies %>%
  #           mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
  #           arrange(desc(year))
  # 
  #         selected_studies <- selected_studies %>%
  #           mutate(title = ifelse(!is.na(doi) & doi != "",
  #                                 paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
  #                                 title)) %>%
  #           select(uid, year, author, journal, title) %>%
  #           left_join(combined_pico_table, by="uid") %>%
  #           distinct()
  # 
  #         if ("intervention" %in% colnames(selected_studies)){
  # 
  #         selected_studies <- selected_studies %>%
  #           distinct()
  # 
  #         selected_studies <- as.data.frame(selected_studies) %>%
  #           ungroup() %>%
  #           select(uid, Year = year, Author = author, Journal = journal, Title = title, Intervention = intervention,
  #                  Discipline = discipline, "Outcome Measures" = outcome_measures) %>%
  #           arrange(is.na(Intervention))
  # 
  # 
  #         } else{
  # 
  # 
  #            selected_studies <- as.data.frame(selected_studies) %>%
  #             distinct() %>%
  #             select(uid, Year = year, Author = author, Journal = journal, Title = title, "Publication Type" = name)
  # 
  # 
  # 
  # 
  #         }
  # 
  #         return(selected_studies)
  # 
  # 
  #       }
  # 
  #       # If apply filter button pressed, proceed to filter section
  #       if(values$submit_filters == "clicked") {
  # 
  # 
  #         input$submit_filters
  # 
  #         # If number of pico dataframes for dropdowns is > 0 then...
  #         if (length(pico_table_list) > 0) {
  # 
  #           for (i in (1:length(pico_table_list))){
  # 
  #              # Loop through each dataframe and filter
  #             new_table <- pico_table_list[[i]] %>%
  #               filter(name %in% isolate(pico_element_list[[i]]())) %>%
  #               select(uid)
  # 
  #             # Only keep the rows that have a matching "uid"
  #             selected_studies <- selected_studies %>%
  #               semi_join(new_table, by = "uid")
  # 
  #           }
  #         }
  # 
  #         # Use year slider to filter
  #         selected_studies <- selected_studies %>%
  #           mutate(year = as.numeric(as.character(year))) %>%
  #           filter(year >= isolate(input$year_slider[[1]])) %>%
  #           filter(year <= isolate(input$year_slider[[2]]))
  # 
  #       }
  # 
  #       # Warning if no results found
  #       if(nrow(selected_studies) < 1){
  #         shinyalert("Warning",
  #                    "Search returned 0 results. Please make a new selection.", type = "info")
  #         return(selected_studies)
  #       }
  # 
  #       # Tidy section for use in the datatable
  #       selected_studies <- as.data.frame(selected_studies)
  # 
  #       selected_studies <- selected_studies %>%
  #         mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
  #         arrange(desc(year))
  # 
  #       if (!is.null(combined_pico_table)){
  # 
  # 
  #       combined_pico_table <- unique(combined_pico_table)
  # 
  #         selected_studies <- selected_studies %>%
  #         mutate(title = ifelse(!is.na(doi) & doi != "",
  #                               paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
  #                               title)) %>%
  #         select(uid, year, author, journal, title) %>%
  #         left_join(combined_pico_table, by="uid") %>%
  #         distinct()
  # 
  #       colnames(selected_studies) <- toTitleCase(colnames(selected_studies))
  # 
  #       if ("Intervention" %in% colnames(selected_studies)){
  # 
  #       selected_studies <- as.data.frame(selected_studies) %>%
  #         ungroup() %>%
  #         arrange(Intervention == "Unknown") %>%
  #         rename("uid" = "Uid", "Outcome Measures" = "Outcome_measures", "Intervention Provider" = "Intervention_provider")
  # 
  #         } else {
  # 
  #           selected_studies <- as.data.frame(selected_studies) %>%
  #             ungroup() %>%
  #             select(uid = Uid, Year, Author, Journal, Title, "Publication Type" = Name) %>%
  #             distinct()
  # 
  #         }
  #       } else {
  # 
  #         selected_studies <- selected_studies %>%
  #           mutate(title = ifelse(!is.na(doi) & doi != "",
  #                                 paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
  #                                 title)) %>%
  #           select(uid, Year = year, Author = author, Journal = journal, Title = title) %>%
  #           distinct()
  # 
  #       }
  # 
  #       return(selected_studies)
  #     })
  # 
  # 
  #     output$search_results_text <- renderText({
  # 
  #       # If there is no query and no filters
  #       if(values$search_query == "" & values$submit_filters == ""){
  # 
  # 
  #         paste0("All ", length(filter_results()$uid), " citations loaded. Use the search box above or apply filters to identify relevant studies!")
  # 
  #       }
  # 
  #       # If there is no query and only filters
  #       else if(values$submit_filters == "clicked" & values$search_query == ""){
  #         paste0("Your filters identified a total of ", length(filter_results()$uid), " citations")
  #       }
  # 
  #       # If there is an advanced search query
  #       else if(search_query()[1] != "" & !is.na(search_query()[2])){
  # 
  #         if(input$comb_1_2 == "AND"){
  # 
  #           translated_query <- paste0(search_query()[1],  " AND ", search_query()[2])
  #         } else {
  # 
  #           translated_query <- paste0(search_query()[1],  " OR ", search_query()[2])
  #         }
  # 
  #         translated_query <- gsub("\\|", " OR ", translated_query)
  #         translated_query <- gsub("\\)\\(\\?\\=\\.\\*", " AND ", translated_query)
  #         translated_query <- gsub("\\?\\=\\.\\*", "", translated_query)
  #         translated_query <- gsub("\\.\\*\\$", "", translated_query)
  #         translated_query <- gsub("\\^", "", translated_query)
  #         translated_query <- gsub("AND", " AND ", translated_query)
  #         translated_query <- gsub("OR", " OR ", translated_query)
  #         translated_query <- gsub("\\[.{5}\\]", "", translated_query)
  # 
  #         if(values$submit_filters == "clicked" & values$search_query == "NOT BLANK"){
  # 
  #           paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ", translated_query,
  #                  " in the title, abstract, and keywords fields. This search is NOT sensitive to case.",
  #                  " Your search and additional filters identified a total of ", length(filter_results()$uid), " studies")
  #         }
  # 
  #         else{
  # 
  #           paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ",
  #                  translated_query,
  #                  " in the title OR abstract OR keywords fields. This search is NOT sensitive to case.",
  #                  " Your search identified a total of ", length(filter_results()$uid), " studies with no additional filters.")
  #         }}
  # 
  #       # If there is a basic search query
  #       else if(search_query()[1] != "" & is.na(search_query()[2])){
  # 
  #         translated_query <- search_query()[1]
  # 
  #         translated_query <- gsub("\\|", " OR ", translated_query)
  #         translated_query <- gsub("\\)\\(\\?\\=\\.\\*", " AND ", translated_query)
  #         translated_query <- gsub("\\?\\=\\.\\*", "", translated_query)
  #         translated_query <- gsub("\\.\\*\\$", "", translated_query)
  #         translated_query <- gsub("\\^", "", translated_query)
  #         translated_query <- gsub("AND", " AND ", translated_query)
  #         translated_query <- gsub("OR", " OR ", translated_query)
  #         translated_query <- gsub("\\[.{5}\\]", "", translated_query)
  # 
  # 
  #         # If there is a search query with filters added
  #         if(values$submit_filters == "clicked" & values$search_query == "NOT BLANK"){
  # 
  #           paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ", translated_query,
  #                  " in the title, abstract, and keywords fields. This search is NOT sensitive to case.",
  #                  " Your search and additional filters identified a total of ", length(filter_results()$uid), " studies")
  #         }
  # 
  #         # If there is a search query with no filters added
  #         else if (values$submit_filters == "" & values$search_query == "NOT BLANK") {
  # 
  #           paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ",
  #                  translated_query,
  #                  " in the title OR abstract OR keywords fields. This search is NOT sensitive to case.",
  #                  " Your search identified a total of ", length(filter_results()$uid), " studies with no additional filters.")
  #         }}
  # 
  # 
  # 
  # 
  # 
  #       else{
  # 
  #         paste0("All ", length(filter_results()$uid), " citations loaded. Use the search box above or apply filters to identify relevant studies!")
  # 
  #       }
  # 
  #     })
  # 
  # 
  #     # Reactive datatable showing studies and search results
  #     output$search_results_studies <- DT::renderDataTable({
  # 
  # 
  #       DT::datatable(
  #         filter_results()[,2:ncol(filter_results())],
  #         rownames = FALSE,
  #         escape = FALSE,
  #         # extensions = c('Buttons'),
  #         options = list(
  #           language = list(
  #             zeroRecords = "No records found",
  #             emptyTable = "No records found"),
  #           deferRender = FALSE,
  #           scrollY = 600,
  #           scrollX = 100,
  #           scroller = TRUE,
  #           columnDefs = list(
  #             list(
  #               targets = c(1), #target for JS code
  #               render = JS(
  #                 "function(data, type, row, meta) {",
  #                 "return type === 'display' && data.length > 15 ?",
  #                 "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
  #                 "}"),
  # 
  #               width = "10%"
  #             ),
  #             list(
  #               # targets = c(1), #target for JS code
  #               # render = JS(
  #               #   "function(data, type, row, meta) {",
  #               #   "return type === 'display' && data.length > 15 ?",
  #               #   "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
  #               #   "}")
  #               # width = "200px"
  #             )
  #             # list(
  #             #   targets = c(3:7), # columns 4, 5, and 6
  #             #   render = JS(
  #             #     "function(data, type, row, meta) {",
  #             #     "  if (type === 'display' && data) {",
  #             #     "  var words = data.split(';');",
  #             #     " var formattedText = words.map(function(word) {",
  #             #     "  var color =  '#' + ('000000' + Math.floor(Math.random()*16777215).toString(16)).slice(-6);",
  #             #     "      var textColor = (parseInt(color.substring(1), 16) > 0xffffff / 2) ? 'black' : 'white';",
  #             #     "      return '<span style=\"background-color:' + color + '; color:' + textColor + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
  #             #     # "      return '<span style=\"background-color:' + color + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
  #             #     "    }).join('; ');",
  #             #     "    return formattedText;",
  #             #     "  }",
  #             #     "  return data;",
  #             #     "}")
  #             # ),
  #             #list(width = '10%', targets = "_all")
  #           )
  #         )
  # 
  #       )
  #     })
  # 
  # 
  # 
  # 
  # 
  #     # Download citations sever side --------
  #     # download refs button server side -csv
  #     output$download_csv <- downloadHandler(
  #       filename = function() {
  #         paste0("citations-", Sys.Date(),
  #                ".csv", sep="")
  #       },
  #       content = function(file) {
  #         write.csv(search_results_download(), file, row.names = FALSE)
  #       }
  #     )
  # 
  #     search_results_download <- reactive({t
  # 
  #       results <- citations_for_download %>%
  #         filter(uid %in% !!filter_results()$uid)
  # 
  # 
  # 
  #     })
  # 
  #     search_results_download_syrf <- reactive({
  # 
  #       # tbl(con, "unique_citations"), filter, collect
  #       results <- citations_for_download %>%
  #         filter(uid %in% !!filter_results()$uid)
  # 
  #       results <- results %>%
  #         rename(Authors = author,
  #                Title = title,
  #                Abstract = abstract,
  #                Url = url,
  #                Year = year,
  #                DOI= doi,
  #                PublicationName = journal) %>%
  #         mutate(AlternateName = "",
  #                AuthorAddress = "",
  #                ReferenceType = "",
  #                Keywords = keywords,
  #                CustomId = uid,
  #                PdfRelativePath = paste0(uid, ".pdf")) %>%
  #         select(Title,
  #                Authors,
  #                PublicationName,
  #                AlternateName,
  #                Abstract,
  #                Url,
  #                AuthorAddress,
  #                Year,
  #                DOI,
  #                ReferenceType,
  #                Keywords,
  #                CustomId,
  #                PdfRelativePath)
  # 
  # 
  #     })
  # 
  #     # download refs button server side - endnote
  #     output$download_syrf <- downloadHandler(
  #       filename = function() {
  #         paste0("citations-srf-", Sys.Date(),
  #                ".csv", sep="")
  #       },
  #       content = function(file) {
  #         write.csv(search_results_download_syrf(), file,
  #                   #col.names=TRUE,
  #                   row.names = F, na="")
  #       })
  # 
  #     search_results_download_endnote <- reactive({
  # 
  #       # tbl(con, "unique_citations"), filter, collect
  #       results <- citations_for_download %>%
  #         filter(uid %in% !!filter_results()$uid)
  # 
  #       results <- results %>%
  #         filter(uid %in% search_results()$uid) %>%
  #         mutate("Reference Type" = "Journal Article") %>%
  #         mutate(isbn = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
  #         rename("Custom 1" = uid,
  #                "Secondary Title" = journal,
  #                "ISBN/ISSN" = isbn) %>%
  #         select("Reference Type", "author", "year",
  #                "Secondary Title", "doi", "title",
  #                "pages", "volume", "number", "abstract",
  #                "Custom 1", "ISBN/ISSN") %>%
  #         mutate(abstract = gsub("\\r\\n|\\r|\\n", "", abstract))
  # 
  #       names(results) <- toTitleCase(names(results))
  # 
  #       results <- results %>%
  #         rename("DOI"= Doi)
  # 
  #       return(results)
  # 
  #     })
  # 
  #     # download refs button server side -endnote
  #     output$download_endnote <- downloadHandler(
  #       filename = function() {
  #         paste0("citations-", Sys.Date(),
  #                ".txt", sep="")
  #       },
  #       content = function(file) {
  #         write.table(search_results_download_endnote(), file, sep="\t",
  #                     col.names=TRUE, row.names = F, quote=FALSE, na="")
  #       })
  # 
  # 
  #   }
  # )
# }

## Search UI ----
#' Search Database
#'
#' This shiny module creates the search page for the app
#'
#' @param id The module identifier.
#'
#' @export
search_UI <- function(id, table) {
  ns <- NS(id)
  
  
  tagList(
    
    tabBox(width= 12,
           status = "primary",
           id = ns("search_tabs"),
           side = "left",
           
           tabPanel(
             value="basic_search_tab",
             title = tagList(
               icon("magnifying-glass", style = "font-size: 24px;"),
               span("Basic", style = "font-family: KohinoorBangla, Sans-serif;")
             ),
             h5("Search iRISE Database using keywords (comma-separated):"),
             fluidRow(
               column(width = 4,
                      
                      
                      radioGroupButtons(
                        inputId = ns("search1_type"),
                        label = "Combine keywords with",
                        choices = c("AND", "OR"),
                        status = "primary",
                        individual = TRUE,
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-circle",
                                       style = "color: green"),
                          no = tags$i(class = "fa fa-circle-o",
                                      style = "color: green"))
                      )
               )),
             
             fluidRow(
               column(
                 width = 6,
                 textAreaInput(
                   inputId = ns("topic1"),
                   placeholder = "e.g. scientific, manuscript, feedback",
                   label = NULL,
                   value = "",
                   width = "100%"
                 )
                 
               ),
               column(
                 width = 6,
                 # Search button (solid dark)
                 actionBttn(
                   inputId = ns("search_button"),
                   label = "Search",
                   icon = icon("search"),
                   style = "material-flat",
                   styleclass = NULL
                 ) %>% 
                   tagAppendAttributes(
                     style = "background-color: #1A465F; 
             border-radius: 12px;
             color: white; 
             border: 2px solid #1A465F; 
             text-transform: none; 
             padding: 10px 20px;"
                   ),
                 
                 # Clear button (inverse colors)
                 actionBttn(
                   inputId = ns("clear_search"),
                   label = "Clear",
                   icon = icon("times-circle"),
                   style = "material-flat",
                   styleclass = NULL
                 ) %>%
                   tagAppendAttributes(
                     style = "background-color: white;
             border-radius: 12px;
             color: #1A465F;
             border: 2px solid #1A465F;
             text-transform: none;
             padding: 10px 20px;"
                   )
                 
                 
                 
                 
                 
               )
               
             ),
             
             br(),
             
             fluidRow(
               dropdown(
                 downloadBttn(
                   ns("download_csv"),
                   label = "Download citations in CSV format",
                   style = "unite",
                   color = "primary",
                   size = "sm",
                   block = FALSE,
                   no_outline = TRUE
                 ),
                 downloadBttn(
                   ns("download_endnote"),
                   label = "Download citations in Endnote tab delimited format",
                   style = "unite",
                   color = "primary",
                   size = "sm",
                   block = FALSE,
                   no_outline = TRUE
                 ),
                 downloadBttn(
                   ns("download_syrf"),
                   label = "Download citations in SyRF upload format",
                   style = "unite",
                   color = "primary",
                   size = "sm",
                   block = FALSE,
                   no_outline = TRUE
                 ),
                 
                 br(),
                 # p("Note for Rayyan export option below: download file and open in MS Excel first on your computer. Save as .csv in excel, then import saved file into Rayyan"),
                 # downloadBttn(
                 #   ns("download_rayyan"),
                 #   label = "Download citations in Rayyan upload format",
                 #   style = "unite",
                 #   color = "primary",
                 #   size = "sm",
                 #   block = FALSE,
                 #   no_outline = TRUE
                 # ),
                 
                 label = "Download",
                 style = "stretch", 
                 icon = icon("download"),
                 inline = TRUE,
                 status = "success", width = "600px",
                 animate = animateOptions(
                   enter = animations$fading_entrances$fadeInLeftBig,
                   exit = animations$fading_exits$fadeOutRightBig)
                 # tooltip = tooltipOptions(title = "Click to download relevant studies")
                 
               )
             )
           ),
           
           tabPanel(
             title = tagList(
               icon("magnifying-glass-plus", style = "font-size: 24px;"),
               span("Advanced", style = "font-family: KohinoorBangla, Sans-serif;")
             ),
             h5("Build a more complex query using two keyword groups.", style = "color: black !important; font-family: KohinoorBangla, Sans-serif;"),
             
             value="adv_search_tab",
             h6("Search Group 1", style = "color: black !important; font-family: KohinoorBangla, Sans-serif;"),
             
             radioGroupButtons(
               inputId = ns("search1_type_adv"),
               label = "Combine keywords in Group 1 using:",
               choices = c("AND", "OR"),
               status = "primary",
               individual = TRUE,
               checkIcon = list(
                 yes = tags$i(class = "fa fa-circle",
                              style = "color: green"),
                 no = tags$i(class = "fa fa-circle-o",
                             style = "color: green"))
             ),
             fluidRow(
               column(6,
                      textAreaInput(
                        inputId = ns("topic1_adv"),
                        placeholder = "e.g. scientific, manuscript, feedback",
                        label = NULL,
                        value = "",
                        width = "100%"
                      ))),
             
             
             
             h6("Search Group 2", style = "color: black !important; font-family: KohinoorBangla, Sans-serif;"),
             radioGroupButtons(
               inputId = ns("search2_type_adv"),
               label = "Combine keywords in Group 2 using:",
               choices = c("AND", "OR"),
               status = "primary",
               individual = TRUE,
               checkIcon = list(
                 yes = tags$i(class = "fa fa-circle",
                              style = "color: green"),
                 no = tags$i(class = "fa fa-circle-o",
                             style = "color: green"))
             ),
             fluidRow(
               column(6, 
                      textAreaInput(
                        inputId = ns("topic2_adv"),
                        placeholder = "e.g. scientific, manuscript, feedback",
                        label = NULL,
                        value = "",
                        width = "100%"
                      ))),
             
             hr(),
             
             radioGroupButtons(
               inputId = ns("comb_1_2"),
               label = "Combine Group 1 and Group 2 with:",
               choices = c("AND", "OR"),
               status = "primary"),
             
             actionBttn(
               inputId = ns("search_button"),
               label = "Search",
               icon = icon("search"),
               style = "material-flat",
               styleclass = NULL
             ) %>% 
               tagAppendAttributes(
                 style = "background-color: #1A465F; 
             border-radius: 12px;
             color: white; 
             border: 2px solid #1A465F; 
             text-transform: none; 
             padding: 10px 20px;"
               ),
             
             actionBttn(
               inputId = ns("clear_search"),
               label = "Clear",
               icon = icon("times-circle"),
               style = "material-flat",
               styleclass = NULL
             ) %>%
               tagAppendAttributes(
                 style = "background-color: white;
             border-radius: 12px;
             color: #1A465F;
             border: 2px solid #1A465F;
             text-transform: none;
             padding: 10px 20px;"
               )
           ),
           
           tabPanel(
             value = "help_tab",
             title = tagList(
               icon("info-circle", style = "font-size: 24px;"),
               span("How to Search", style = "font-family: KohinoorBangla, Sans-serif;")
             ),
             div(
               style = "padding: 15px; max-height: 600px; overflow-y: auto;",
               includeMarkdown("helpfiles/searching.md")
             )
           )
           
           
    ),
    fluidRow(
      column(
        width = 3,
        box(
          width = 12,
          title = tagList(icon("sliders")),
          status = "primary",
          solidHeader = TRUE,
          
          yearBarUI_filters(ns("included_studies_over_time_bar"),
                            title = tags$p("Included Studies Over Time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                            theme = "danger",
                            spinner_colour = "#89CB93",
                            table = n_included_per_year_plot_data),
          tags$style(HTML("
    /* Change background & text color of selected tags */
    .vscomp-value-tag {
      background-color: #ff6666 !important; /* tag background */
      color: white !important;              /* tag text */
    }
    /* Optional: change the 'x' (remove) icon color */
    .vscomp-value-tag .vscomp-value-tag-clear {
      color: white !important;
    }

  ")),
          br(),
          actionBttn(
            inputId = ns("reset_filters"),
            label = "Clear Filters",
            icon = icon("times-circle"),
            style = "material-flat",
            styleclass = NULL, size = "xs"
          )
          %>% 
            tagAppendAttributes(style = "float: right; background-color:#1A465F; color: white;"), 
          br(),
          uiOutput(ns("dynamic_dropdowns"))
          
        )
      ),
      
      column(
        width = 9,
        box(
          width = 12,
          solidHeader = TRUE,
          status = "secondary",
          title = "Selected studies in database",
          fluidRow(
            column(
              width = 10,
              textOutput(ns("search_results_text"))
            ),
            column(
              width = 2,
              pickerInput(
                ns("cols_to_show"),
                label = NULL,
                selected = c("Title", "Author", "Year", "Journal"),
                choices = c("Title", "Author", "Year", "Journal"),
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  noneSelectedText = "Please select",
                  selectedTextFormat = "count > 0",
                  countSelectedText = "Add Columns",
                  style = "btn-primary"
                )
                
              )
            )
          ),
          
          br(),
          tags$style(HTML("
  .fa-tag, .fa-database, .fa-code, .fa-map-marker-alt {
    transition: transform 0.2s ease-in-out;
  }
  .fa-tag:hover, .fa-database:hover, .fa-code:hover, .fa-map-marker-alt:hover {
    transform: scale(1.5);
  }
  img[src='openalex.png'] {
    transition: transform 0.2s ease-in-out;
  }
  img[src='openalex.png']:hover {
    transform: scale(1.5);
  }
")),
          
          # Second row inside the box: datatable full width
          DT::dataTableOutput(ns("search_results_studies") )
          # %>% withSpinner(color="#96c296")
          
        )
      )
    )
  )
  
}


#' Search Page Module
#'
#' This Shiny module generates the search page for the app, allowing users to search and filter studies.
#'
#' @param id The module identifier.
#' @param table A dataframe of included studies with metadata.
#' @param combined_pico_table A combined dataframe of PICO tags.
#' @param pico_data A list of dynamic search updates based on the PICO dropdown filters.
#' @param citations_for_download A dataframe containing citations for download.
#' @param project_name The name of the project.
#'
#' @export
search_Server <- function(id,
                          table,
                          combined_pico_table,
                          pico_data = list(),
                          citations_for_download,
                          project_name = ""
                          ,
                          current_tab
) {
  moduleServer(
    id,
    function(input, output, session) {   # <-- only input/output/session here
      
      ns <- NS(id)
      
      # Creates list for dropdown menus
      dynamic_dropdowns <- list()
      render_pico_dropdowns <- function(pico_data, ns) {
        dynamic_dropdowns <- lapply(pico_data, function(item) {
          pico_dropdown_UI(
            id = ns(item$id),
            label1 = item$label1,
            label2 = item$label2,
            label3 = item$label3,
            label4 = item$ilabel4,
            column1 = item$table[[item$column1]],
            column2 = item$table[[item$column2]],
            column3 = item$table[[item$column3]],
            column4 = item$table[[item$column4]],
            filter_no = item$filter_no
          )
        })
        do.call(tagList, dynamic_dropdowns)
      }
      
      output$dynamic_dropdowns <- renderUI({
        cat("dynamic_dropdowns", Sys.time(), "\n")
        
        render_pico_dropdowns(pico_data, ns)
      })
      
      outputOptions(output, "dynamic_dropdowns", suspendWhenHidden = FALSE)
      
      
      
      
      # Creates list for dropdown menus reactivity
      pico_element_list <- list()
      pico_element_list <- lapply(pico_data, function(pico_item) {
        
        pico_dropdown_Server(
          id = pico_item$id,
          table = pico_item$table,
          column1 = pico_item$column1,
          column2 = pico_item$column2,
          column3 = pico_item$column3,
          column4 = pico_item$column4,
          filter_no = pico_item$filter_no
          
        )
      })
      
      # Creates table list for filtering data
      pico_table_list <- list()
      pico_table_list <- lapply(pico_data, function(element) element$table)
      
      
      # Create reactive values as triggers
      values <- reactiveValues()
      values$search_query <- ""
      values$reset_filters <- ""
      values$submit_filters <- ""
      values$reset_slider <- ""
      values$submit_slider <- ""
      
      
      # Reactive changes for clear search button
      observeEvent(input$clear_search, {
        
        updateTextAreaInput(session, "topic1",
                            value = "")
        
        updateTextAreaInput(session, "topic1_adv",
                            value = "")
        
        updateTextAreaInput(session, "topic2_adv",
                            value = "")
        
        values$search_query <- ""
        values$reset_filters <- "reset"
        values$submit_filters <- ""
        values$reset_slider <- "reset"
        values$submit_slider <- ""
        
        
        output$dynamic_dropdowns <- renderUI({
          render_pico_dropdowns(pico_data, ns)
        })
        
        
      })
      
      # Reactive changes for resest filters button
      observeEvent(input$reset_filters, {
        
        
        values$reset_filters <- "reset"
        values$submit_filters <- ""
        values$reset_slider <- "reset"
        values$submit_slider <- ""
        
        
        output$dynamic_dropdowns <- renderUI({
          render_pico_dropdowns(pico_data, ns)
        })
        
      })
      
      observeEvent(c(input$search_button),  {
        
        values$search_query <- "NOT BLANK"
        values$reset_filters <- ""
        
      },  ignoreInit = TRUE)
      
      # Run search query
      search_query <- eventReactive(c(input$search_button), {
        
        # When in the basic search...
        if (input$search_tabs == "basic_search_tab"){
          
          query1 <- input$topic1 %>%
            str_trim() %>%
            str_replace_all(pattern = ", ", repl = ",") %>%
            str_replace_all(pattern = " ,", repl = ",") %>%
            str_split("\\,") %>%
            unlist() %>%
            as.list() %>%
            lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
                                    x, ignore.case = TRUE))
          
          query1 <- ifelse(input$search1_type == "OR",
                           paste(query1,collapse="|"),
                           paste0("^(?=.*", paste0(query1, collapse=")(?=.*"),
                                  ").*$"))
          
          query1 <- query1 %>%
            str_trim()
          
          return(query1)
          
        }
        
        # When using Adv Search...
        else{
          
          query1_adv <- input$topic1_adv %>%
            str_trim() %>%
            str_replace_all(pattern = ", ", repl = ",") %>%
            str_replace_all(pattern = " ,", repl = ",") %>%
            str_split("\\,") %>%
            unlist() %>%
            as.list() %>%
            lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
                                    x, ignore.case = TRUE))
          
          query1_adv <- ifelse(input$search1_type_adv == "OR",
                               paste(query1_adv,collapse="|"),
                               paste0("^(?=.*", paste0(query1_adv, collapse=")(?=.*"),
                                      ").*$"))
          
          
          query1_adv <- query1_adv %>%
            str_trim()
          
          
          query2_adv <- input$topic2_adv %>%
            str_trim() %>%
            str_replace_all(pattern = ", ", repl = ",") %>%
            str_replace_all(pattern = " ,", repl = ",") %>%
            str_split("\\,") %>%
            unlist() %>%
            as.list() %>%
            lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
                                    x, ignore.case = TRUE))
          
          query2_adv <- ifelse(input$search2_type_adv == "OR",
                               paste(query2_adv,collapse="|"),
                               paste0("^(?=.*", paste0(query2_adv, collapse=")(?=.*"),
                                      ").*$"))
          
          
          query2_adv <- query2_adv %>%
            str_trim()
          
          try(query_final <- c(query1_adv, query2_adv))
          
          return(query_final)
        }
      })
      
      
      search_results <- reactiveVal(table) 
      
      observeEvent(input$search_button, {
        
        # If there is no search query and reset button pressed, return entire table
        if(values$search_query == "" & values$reset_filters == "reset"){
          
          selected_studies <- table
          
        }
        
        # If there is no search query and no reset then proceed with entire table
        else if(values$search_query == ""){
          
          selected_studies <- table
          
        }
        
        
        else if(search_query()[1] == ""){
          
          selected_studies <- table
          
        }
        
        # if there is a search query
        else{
          
          selected_studies <- table
          
          if(input$search_tabs == "basic_search_tab"){
            
            # If search has been performed in Adv search then user goes back to basic search there is a reset.
            if (length(search_query()) > 1){
              
              # shinyjs::click("adv_reset_search")
              
            } else{
              
              if (str_trim(input$topic1) == ""){
                
                search_results(selected_studies)  
                
                return() 
                
              } else {
                selected_studies <- selected_studies[with(selected_studies,
                                                          grepl(search_query(),
                                                                paste(title, abstract, keywords),
                                                                ignore.case=TRUE,
                                                                perl=TRUE)),]
                
                withProgress(message = 'Performing search',
                             detail = 'Please wait...', value = 0, {
                               for (i in 1:25) {
                                 incProgress(1/15)
                                 Sys.sleep(0.5)
                               }
                             })
              }}
          } else if (is.na(search_query()[1]) | is.na(search_query()[2])){
            
            
            selected_studies <- table
            
          } else{
            
            selected_studies1 <- table[with(table,
                                            grepl(search_query()[1],
                                                  paste(title, abstract, keywords),
                                                  ignore.case=TRUE,
                                                  perl=TRUE)),]
            selected_studies2 <- table[with(table,
                                            grepl(search_query()[2],
                                                  paste(title, abstract, keywords),
                                                  ignore.case=TRUE,
                                                  perl=TRUE)),]
            
            
            if(input$comb_1_2 == "AND"){
              
              selected_studies <- table %>%
                filter(uid %in% selected_studies1$uid & uid %in% selected_studies2$uid) %>%
                distinct()
            }
            
            else{
              selected_studies <- table %>%
                filter(uid %in% c(selected_studies1$uid, selected_studies2$uid)) %>%
                distinct()
            }
            
            withProgress(message = 'Performing search',
                         detail = 'Please wait...', value = 0, {
                           for (i in 1:25) {
                             incProgress(1/15)
                             Sys.sleep(0.5)
                           }
                         })
            
          }}
        
        
        search_results(selected_studies)   
        
      })
      
      
      observe({
        cat("Sidebar menu is:", current_tab(), "\n")
      })
      
      
      filter_results <- reactive({
        
        req(current_tab())
        req(current_tab() %in% c("module_search_database", "grey_lit_database"))
        
        reset_state <- values$reset_filters
        
        if(values$search_query == ""){
          
          
          # No search query - start with full table, don't trigger search_results()
          selected_studies <- table  # Use your base table directly
          
        } else {
          
          # There is a search query or reset - use search_results()
          selected_studies <- search_results()
        }
        
        # If reset button clicked then tidy entire table and return
        if(reset_state == "reset"){
          
          selected_studies <- as.data.frame(selected_studies)
          
          combined_pico_table <- unique(combined_pico_table)
          
          selected_studies <- selected_studies %>%
            mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
            arrange(desc(year))
          
          selected_studies <- selected_studies %>%
            mutate(title = ifelse(!is.na(doi) & doi != "",
                                  paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
                                  title)) %>%
            select(uid, year, author, journal, title) %>%
            left_join(combined_pico_table, by="uid") %>%
            distinct()
          
          if ("intervention" %in% colnames(selected_studies)){
            
            selected_studies <- selected_studies %>%
              distinct()
            
            selected_studies <- as.data.frame(selected_studies) %>%
              ungroup() %>%
              select(uid, Title = title, Author = author, Journal = journal, Year = year, Intervention = intervention,
                     "Outcome Measures" = outcome_measures, Discipline = discipline, "Intervention Provider" = "intervention_provider",
                     "Research Stage" = "research_stage", "Target Population" = "target_population", "Target Pop Location" = "location", "Annotated by" = annotated_by) %>%
              arrange(is.na(Intervention))
            
            
          } else{
            
            
            selected_studies <- as.data.frame(selected_studies) %>%
              distinct() %>%
              select(uid, Title = title, Author = author, Journal = journal, "Publication Type" = name,  Year = year)
            
            
          }
          
          
          return(selected_studies)
          
          
        }
        
        # If number of pico dataframes for dropdowns is > 0 then...
        if (length(pico_element_list) > 0) {
          
          for (i in (1:length(pico_element_list))){
            
            # Get current selections from the reactive
            selected_values <- pico_element_list[[i]]()
            
            table <- pico_data$pico_element_1$table
            # Get all possible values for this dropdown
            all_possible_values <- unique(pico_data[[i]]$table[[pico_data[[i]]$column1]])
            
            
            # Only filter if user made actual selections (not all selected)
            if (!is.null(selected_values) &&
                length(selected_values) > 0 &&
                length(selected_values) < length(all_possible_values)) {
              
              new_table <- pico_data[[i]]$table %>%
                filter(!!sym(pico_data[[i]]$column1) %in% selected_values) %>%
                select(uid)
              
              selected_studies <- selected_studies %>%
                semi_join(new_table, by = "uid")
              
              values$submit_filters <- "clicked"
              
            }
          }
          
        }
        
        # Warning if no results found
        if(nrow(selected_studies) < 1){
          
          selected_studies <- selected_studies %>%
            select(Title = title, Author = author, Journal = journal, Year = year)
          
          return(selected_studies)
        }
        
        # Tidy section for use in the datatable
        selected_studies <- as.data.frame(selected_studies)
        
        selected_studies <- selected_studies %>%
          mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
          arrange(desc(year))
        
        if (!is.null(combined_pico_table)){
          
          combined_pico_table <- unique(combined_pico_table)
          
          selected_studies <- selected_studies %>%
            mutate(title = ifelse(!is.na(doi) & doi != "",
                                  paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
                                  title)) %>%
            select(uid, title, author, journal, year) %>%
            left_join(combined_pico_table, by="uid") %>%
            distinct()
          
          colnames(selected_studies) <- toTitleCase(colnames(selected_studies))
          
          if ("Intervention" %in% colnames(selected_studies)){
            
            selected_studies <- as.data.frame(selected_studies) %>%
              ungroup() %>%
              arrange(Intervention == "Unknown") %>%
              rename("uid" = "Uid", "Outcome Measures" = "Outcome_measures", "Intervention Provider" = "Intervention_provider",
                     "Research Stage" = "Research_stage", "Target Population" = "Target_population", "Target Pop Location" = "Location", "Annotated by" = "Annotated_by")
            
          } else {
            
            selected_studies <- as.data.frame(selected_studies) %>%
              ungroup() %>%
              # select(uid = Uid, Year, Author, Journal, Title, "Publication Type" = Name) %>%
              select(uid = Uid, Title, Author, Journal, "Publication Type" = Name, Year) %>%
              distinct()
            
          }
        } else {
          
          selected_studies <- selected_studies %>%
            mutate(title = ifelse(!is.na(doi) & doi != "",
                                  paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
                                  title)) %>%
            select(uid, Title = title, Author = author, Journal = journal, Year = year) %>%
            distinct()
          
        }
        
        return(selected_studies)
      })
      
      
      
      
      translate_query <- function(query) {
        query %>%
          gsub("\\|", " OR ", ., perl = TRUE) %>%
          gsub("\\)\\(\\?\\=\\.\\*", " AND ", ., perl = TRUE) %>%
          gsub("\\?\\=\\.\\*", "", ., perl = TRUE) %>%
          gsub("\\.\\*\\$", "", ., perl = TRUE) %>%
          gsub("\\^", "", ., perl = TRUE) %>%
          gsub("AND", " AND ", ., perl = TRUE) %>%
          gsub("OR", " OR ", ., perl = TRUE) %>%
          gsub("\\[.{5}\\]", "", ., perl = TRUE)
      }
      
      
      
      
      output$search_results_text <- renderText({
        
        req(debounced_state())
        text_data <- debounced_state()
        
        cat("search_results_text", Sys.time(), "\n")
        
        # Extract all values from the debounced state
        search_q <- text_data$search_query
        submit_f <- text_data$submit_filters
        reset_f <- text_data$reset_filters
        submit_s <- text_data$submit_slider
        reset_s <- text_data$reset_slider
        
        n <- text_data$n_results
        
        search_query <- text_data$search_query_value
        
        comb <- text_data$combination_type
        
        if ((search_q == "" && submit_f == "" && submit_s == "")
        ) {
          
          return(paste0("All ", n,
                        " citations loaded. Use the search box above or apply filters to identify relevant studies!"))
        }
        
        # --- Case 1: filters only
        if (submit_f == "clicked" && search_q == "" || submit_s == "clicked" && search_q == "") {
          
          return(paste0("Your filters identified a total of ", n, " citations."))
        }
        
        # --- Case 2: advanced search (two terms)
        if (!is.na(search_query[1]) && search_query[1] != "" && !is.na(search_query[2]) && search_query[2] != "") {
          operator <- ifelse(comb == "AND", " AND ", " OR ")
          translated_query <- translate_query(paste0(search_query[1], operator, search_query[2]))
          
          if (submit_f == "clicked" && search_q == "NOT BLANK") {
            return(paste0(
              "Your translated search query tells the application to find ", project_name,
              " papers with a regex match to ", translated_query,
              " in the title, abstract, and keywords fields. This search is NOT case-sensitive. ",
              "Your search and additional filters identified a total of ", n, " studies."
            ))
          } else {
            return(paste0(
              "Your translated search query tells the application to find ", project_name,
              " papers with a regex match to ", translated_query,
              " in the title OR abstract OR keywords fields. This search is NOT case-sensitive. ",
              "Your search identified a total of ", n, " studies with no additional filters."
            ))
          }
        }
        
        # --- Case 3: basic search (single term)
        
        if (!is.na(search_query[1]) && search_query[1] != "" && (is.na(search_query[2]) || search_query[2] == "")) {
          translated_query <- translate_query(search_query[1])
          
          if (submit_f == "clicked" && search_q == "NOT BLANK" || submit_s == "clicked" && search_q == "NOT BLANK") {
            return(paste0(
              "Your translated search query tells the application to find ", project_name,
              " papers with a regex match to ", translated_query,
              " in the title, abstract, and keywords fields. This search is NOT case-sensitive. ",
              "Your search and additional filters identified a total of ", n, " studies."
            ))
          } else {
            return(paste0(
              "Your translated search query tells the application to find ", project_name,
              " papers with a regex match to ", translated_query,
              " in the title OR abstract OR keywords fields. This search is NOT case-sensitive. ",
              "Your search identified a total of ", n, " studies with no additional filters."
            ))
          }
        }
        
        # --- Fallback
        paste0("All ", n, " citations loaded. Use the search box above or apply filters to identify relevant studies!")
      })
      
      n_included_react <- (
        
        reactive({
          cat("n_included_react", Sys.time(), "\n")
          
          filter_results() %>%
            janitor::clean_names() %>%
            mutate(year = as.numeric(year)) %>%
            filter(!is.na(year)) %>%
            count(year, name = "is_included") %>%
            arrange(year)
        })
      ) 
      
      
      year_slider_values <- yearBarServer_filters("included_studies_over_time_bar",
                                                  filter_table = n_included_react,
                                                  column = "is_included",
                                                  colour = "#1A465F")
      
      
      
      # New reactive that combines your existing filters + year filter
      final_filtered_studies <- reactive({
        cat("final_filtered_studies", Sys.time(), "\n")
        
        
        selected_studies <- filter_results()
        
        # Get year range from the module
        year_range <- year_slider_values()
        
        req(selected_studies, year_range)
        
        
        # Apply year filtering
        selected_studies %>%
          
          mutate(Year = as.numeric(as.character(Year))) %>%
          filter(!is.na(Year),
                 Year >= min(year_range),
                 Year <= max(year_range))
      }) %>% bindCache(filter_results(), year_slider_values())
      
      
      observeEvent(year_slider_values(), {
        
        year_range <- year_slider_values()
        
        filter_data <- filter_results() %>% 
          filter(!is.na(Year))
        
        if(min(year_range) == min(filter_data$Year) && 
           max(year_range) == max(filter_data$Year)) {
          
          values$reset_slider <- ""
          
          # ALSO reset the filters flag if it's in reset state
          if (values$reset_filters == "reset") {
            values$reset_filters <- ""
          }
          
        } else {
          values$submit_slider <- "clicked" 
        }
      }, ignoreInit = TRUE)
      
      
      # Single debounced reactive that bundles both text + table state
      debounced_state <- debounce(reactive({
        
        current_filtered_studies <- final_filtered_studies()
        if (is.null(current_filtered_studies) || nrow(current_filtered_studies) == 0) {
          n_results <- 0
          
          table <- current_filtered_studies
        } else {
          
          n_results <- nrow(current_filtered_studies)
          
          table <- current_filtered_studies
          
          
          if ("Annotated by" %in% colnames(current_filtered_studies)) {
            
            table <- current_filtered_studies %>%
              mutate(row_colour = case_when(
                `Annotated by` == "Human" ~ "#d4f7d4",
                `Annotated by` == "AI (gpt-4o)"    ~ "#d4e6f7",
                TRUE                      ~ "white"
              )) 
          }
        }
        
        # Process table here
        cols <- input$cols_to_show
        
        
        if ("Annotated by" %in% cols) {
          table <- table %>%
            mutate(`Annotated by` = case_when(
              `Annotated by` == "Human" ~ "Human <i class='fa fa-user' title='Annotated by Human'></i>",
              `Annotated by` == "AI (gpt-4o)"    ~ "AI (gpt-4o) <i class='fa fa-laptop-code' title='Annotated by AI'></i>",
              TRUE                      ~ "None"
            ))
        }
        
        # Get search query safely
        search_query_value <- tryCatch({
          search_query()
        }, error = function(e) {
          c("", "")
        })
        
        # Return a list containing EVERYTHING both outputs need
        list(
          table = table,
          cols = cols,
          n_results = n_results,
          search_query_value = search_query_value,
          combination_type = input$comb_1_2,
          search_tab = input$search_tabs,
          
          # pass along values if you need them
          search_query = values$search_query,
          submit_filters = values$submit_filters,
          reset_filters = values$reset_filters,
          submit_slider = values$submit_slider,
          reset_slider = values$reset_slider
        )
        
      }), 300)
      
      observe({
        
        # Determine choices based on some condition
        if (current_tab() == "grey_lit_database") {
          
          choices <- c("Title", "Author", "Year", "Journal", "Publication Type")
          
        } else {
          
          choices = c("Title", "Author", "Year", "Journal", "Intervention", "Outcome Measures", "Discipline", "Intervention Provider",
                      "Research Stage", "Target Population", "Target Pop Location", "Annotated by") 
          
        }
        
        updatePickerInput(
          session = session,
          inputId = "cols_to_show",
          choices = choices,
          selected = c("Title", "Author", "Year", "Journal")
        )
      })
      
      
      # Simplify your renderDT to just formatting
      output$search_results_studies <- DT::renderDT({
        
        
        data_bundle <- debounced_state()
        
        n_results <- data_bundle$n_results
        table <- data_bundle$table
        cols <- data_bundle$cols
        
        if (n_results > 0){
          
          
          if ("row_colour" %in% colnames(table)){
            # Work only with displayed subset - KEEP row_colour for styling
            table_to_show <- table[, c(cols, "row_colour"), drop = FALSE]
            
            # Get the colors before creating the datatable
            row_colors <- table_to_show$row_colour
            
            # Columns that should not get any icon
            no_icon_cols <- c("uid", "row_colour")
            
            # Columns that should get database icon
            database_cols <- c("Title", "Year", "Author", "Journal")
            
            
          } else {
            
            table_to_show <- table[, c(cols), drop = FALSE]
            
            # Columns that should not get any icon
            no_icon_cols <- c("uid")
            
            # Add row_colour column with default value
            table_to_show$row_colour <- "white"
            
            # Columns that should not get any icon
            no_icon_cols <- c("uid", "row_colour")
            
            # Columns that should get database icon
            database_cols <- c("Title", "Year", "Author", "Journal", "Publication Type")
            
            
          }
          
        }
        else{
          
          # Work only with displayed subset - KEEP row_colour for styling
          table_to_show <- table
          
          # Columns that should not get any icon
          no_icon_cols <- c("uid")
          
          # Columns that should get database icon (only these 4 when no results)
          database_cols <- c("Title", "Year", "Author", "Journal")
          
        }
        
        display_colnames <- ifelse(
          colnames(table_to_show) %in% no_icon_cols,
          colnames(table_to_show),
          ifelse(
            colnames(table_to_show) %in% database_cols,
            
            paste0("<span style='white-space: nowrap;'>", colnames(table_to_show), " <i class='fa fa-database' title='Data Source: Original Publication Record'></i></span>"),
            paste0("<span style='white-space: nowrap;'>", colnames(table_to_show), " <i class='fa fa-tag' title='Data Source: Annotation by Human (green) or AI (blue) — see \"Data Transparency\" tab for more info'></i></span>")
          )
        )
        
        if (nrow(table_to_show) < 1) {
          
          shinyalert(
            "No Results Found",
            "Your search returned 0 results. Please clear the search and try again. Make sure you’ve read the 'How to Search' tab for guidance.",
            type = "info"
          )
        }
        dt <- DT::datatable(
          table_to_show,
          rownames = FALSE,
          escape = FALSE,
          colnames = display_colnames,
          options = list(
            dom = "rtp",
            language = list(
              zeroRecords = "No records found",
              emptyTable = "No records found"
            ),
            deferRender = TRUE,
            scrollY = 600,
            scrollX = 100,
            scroller = TRUE,
            columnDefs = list(
              list(
                targets = ncol(table_to_show) - 1,
                visible = FALSE
              ),
              list(
                targets = 1,
                render = DT::JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 50 ?",
                  "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                  "}"
                ),
                width = "10%"
              )
            )
          )
        )
        
        if ("row_colour" %in% names(table_to_show)) {
          dt <- dt %>%
            DT::formatStyle(
              columns = "row_colour",
              target = "row",
              backgroundColor = DT::styleEqual(
                levels = c("#d4f7d4", "#d4e6f7", "white"),
                values = c("#d4f7d4", "#d4e6f7", "white")
              )
            )
        } else {
          dt
        }
        
        
      }, server = FALSE) %>%
        bindCache(debounced_state())  # Cache on the debounced data
      
      outputOptions(output, "search_results_studies", suspendWhenHidden = FALSE)
      
      
      # Download citations sever side --------
      # download refs button server side -csv
      output$download_csv <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".csv", sep="")
        },
        content = function(file) {
          write.csv(search_results_download(), file, row.names = FALSE)
        }
      )
      
      search_results_download <- reactive({t
        
        results <- citations_for_download %>%
          filter(uid %in% !!filter_results()$uid)
        
      })
      
      search_results_download_syrf <- reactive({
        
        # tbl(con, "unique_citations"), filter, collect
        results <- citations_for_download %>%
          filter(uid %in% !!filter_results()$uid)
        
        results <- results %>%
          rename(Authors = author,
                 Title = title,
                 Abstract = abstract,
                 Url = url,
                 Year = year,
                 DOI= doi,
                 PublicationName = journal) %>%
          mutate(AlternateName = "",
                 AuthorAddress = "",
                 ReferenceType = "",
                 Keywords = keywords,
                 CustomId = uid,
                 PdfRelativePath = paste0(uid, ".pdf"),
                 Abstract = "") %>%
          select(Title,
                 Authors,
                 PublicationName,
                 AlternateName,
                 Abstract,
                 Url,
                 AuthorAddress,
                 Year,
                 DOI,
                 ReferenceType,
                 Keywords,
                 CustomId,
                 PdfRelativePath)
        
        
      })
      
      # download refs button server side - endnote
      output$download_syrf <- downloadHandler(
        filename = function() {
          paste0("citations-srf-", Sys.Date(),
                 ".csv", sep="")
        },
        content = function(file) {
          write.csv(search_results_download_syrf(), file,
                    #col.names=TRUE, 
                    row.names = F, na="")
        })
      
      search_results_download_endnote <- reactive({
        
        # tbl(con, "unique_citations"), filter, collect
        results <- citations_for_download %>%
          filter(uid %in% !!filter_results()$uid)
        
        results <- results %>%
          filter(uid %in% search_results()$uid) %>%
          mutate("Reference Type" = "Journal Article") %>%
          mutate(isbn = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
          rename("Custom 1" = uid,
                 "Secondary Title" = journal,
                 "ISBN/ISSN" = isbn) %>%
          select("Reference Type", "author", "year",
                 "Secondary Title", "doi", "title",
                 "pages", "volume", "number", "abstract",
                 "Custom 1", "ISBN/ISSN") %>%
          mutate(abstract = "")
        
        names(results) <- toTitleCase(names(results))
        
        results <- results %>%
          rename("DOI"= Doi)
        
        return(results)
        
      })
      
      # download refs button server side -endnote
      output$download_endnote <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".txt", sep="")
        },
        content = function(file) {
          write.table(search_results_download_endnote(), file, sep="\t",
                      col.names=TRUE, row.names = F, quote=FALSE, na="")
        })
      
      
    }
  )
}


#' PICO Dropdown Module UI
#'
#' This Shiny module creates reactive dropdown menus for filtering based on PICO elements.
#'
#' @param id The module identifier.
#' @param column1 The name of the first column to use for generating the dropdown.
#' @param column2 The name of the second column to use for generating the second dropdown.
#' @param column3 The name of the third column to use for generating the third dropdown.
#' @param column4 The name of the fourth column to use for generating the fourth dropdown.
#' @param label1 The label shown above the first dropdown.
#' @param label2 The label shown above the second dropdown.
#' @param label3 The label shown above the third dropdown.
#' @param label4 The label shown above the fourth dropdown.
#' @param filter_no The number of dropdown menus required.
#'
#' @export
pico_dropdown_UI <- function(id,
                             column1, column2, column3, column4,
                             label1, label2, label3, label4,
                             filter_no) {
  ns <- NS(id)

  # if (filter_no == 1){
  #   pickerInput(
  #     inputId = ns("dropdown_filter1"),
  #     label = label1,
  #     choices = c(sort(unique(column1))),
  #     multiple = TRUE,
  #     selected = c(sort(unique(column1))),
  #     options = list(
  #       `live-search` = TRUE,
  #       `actions-box` = TRUE,
  #       style = "btn-primary"))
  # 
  # }
  if (filter_no == 1){
    
    virtualSelectInput(
      inputId = ns("dropdown_filter1"),
      label = label1, 
      choices = c(sort(unique(column1))),
      # selected = c(sort(unique(column1))),
      selected = NULL,
      multiple = TRUE,
      showValueAsTags = TRUE, 
      width = "100%",
      dropboxWrapper = "body", 
      search = TRUE
    )
    
  }

  else if (filter_no == 2){
    tagList(

      pickerInput(
        inputId = ns("dropdown_filter2"),
        label = label2,
        choices = c(sort(unique(column2))),
        selected = c(sort(unique(column2))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),

      pickerInput(
        inputId = ns("dropdown_filter1"),
        label = label1,
        choices = c(sort(unique(column1))),
        multiple = TRUE,
        selected = c(sort(unique(column1))),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary"))
    )
  }

  else if (filter_no == 3){


    tagList(
      pickerInput(
        inputId = ns("dropdown_filter3"),
        label = label3,
        choices = c(sort(unique(column3))),
        selected = c(sort(unique(column3))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      pickerInput(
        inputId = ns("dropdown_filter2"),
        label = label2,
        choices = c(sort(unique(column2))),
        selected = c(sort(unique(column2))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),

      pickerInput(
        inputId = ns("dropdown_filter1"),
        label = label1,
        choices = c(sort(unique(column1))),
        multiple = TRUE,
        selected = c(sort(unique(column1))),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary"))
    )
  } else if (filter_no == 4){

    tagList(

      pickerInput(
        inputId = ns("dropdown_filter4"),
        label = label4,
        choices = c(sort(unique(column4))),
        selected = c(sort(unique(column4))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),

      pickerInput(
        inputId = ns("dropdown_filter3"),
        label = label3,
        choices = c(sort(unique(column3))),
        selected = c(sort(unique(column3))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),

      pickerInput(
        inputId = ns("dropdown_filter2"),
        label = label2,
        choices = c(sort(unique(column2))),
        selected = c(sort(unique(column2))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),

      pickerInput(
        inputId = ns("dropdown_filter1"),
        label = label1,
        choices = c(sort(unique(column1))),
        multiple = TRUE,
        selected = c(sort(unique(column1))),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary"))
    )

  }


}


#' PICO Dropdown Module Server
#'
#' This Shiny module server function handles the dynamic behavior of reactive dropdown menus based on PICO elements.
#'
#' @param id The module identifier.
#' @param table The data table containing the columns used for filtering.
#' @param column1 The name of the first column to be used for generating the first dropdown.
#' @param column2 The name of the second column to be used for generating the second dropdown.
#' @param column3 The name of the third column to be used for generating the third dropdown.
#' @param column4 The name of the fourth column to be used for generating the fourth dropdown.
#' @param filter_no The number of dropdown menus required.
#'
#' @export
pico_dropdown_Server <- function(id, table,
                                 column1, column2, column3, column4,
                                 filter_no) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      if (filter_no == 1){
        dynamic_dropdown_search <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            filter(column1 %in% input$dropdown_filter1) %>%
            distinct(name) %>%
            pull() %>%
            sort()
        })


      }

      else if (filter_no == 2){

        dynamic_dropdown_search2 <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            filter(column2 %in% input$dropdown_filter2) %>%
            distinct(column1) %>%
            mutate(column1 = as.character(column1)) %>%
            pull() %>%
            sort()

        })

        observe({
          updatePickerInput(session = session, ("dropdown_filter1"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })

        dynamic_dropdown_search <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            filter(column1 %in% input$dropdown_filter1) %>%
            distinct(name) %>%
            pull() %>%
            sort()
        })


      }

      else if (filter_no == 3){

        dynamic_dropdown_search3 <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>%
            filter(column3 %in% input$dropdown_filter3) %>%
            distinct(column2) %>%
            mutate(column2 = as.character(column2)) %>%
            pull() %>%
            sort()

        })
        observe({
          updatePickerInput(session = session, ("dropdown_filter2"),
                            choices = dynamic_dropdown_search3(),
                            selected = dynamic_dropdown_search3())
        })


        dynamic_dropdown_search2 <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>%
            filter(column3 %in% input$dropdown_filter3) %>%
            filter(column2 %in% input$dropdown_filter2) %>%
            distinct(column1) %>%
            mutate(column1 = as.character(column1)) %>%
            pull() %>%
            sort()

        })
        observe({
          updatePickerInput(session = session, ("dropdown_filter1"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })

        dynamic_dropdown_search <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            filter(column1 %in% input$dropdown_filter1) %>%
            distinct(name) %>%
            pull() %>%
            sort()
        })

      }

      else if (filter_no == 4){

        dynamic_dropdown_search3 <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>%
            mutate(column4 = as.factor(!!rlang::sym(column4))) %>%
            filter(column4 %in% input$dropdown_filter4) %>%
            distinct(column3) %>%
            mutate(column3 = as.character(column3)) %>%
            pull() %>%
            sort()

        })

        observe({
          updatePickerInput(session = session, ("dropdown_filter3"),
                            choices = dynamic_dropdown_search3(),
                            selected = dynamic_dropdown_search3())
        })

        dynamic_dropdown_search2 <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>%
            mutate(column4 = as.factor(!!rlang::sym(column4))) %>%
            filter(column4 %in% input$dropdown_filter4) %>%
            filter(column3 %in% input$dropdown_filter3) %>%
            distinct(column2) %>%
            mutate(column2 = as.character(column2)) %>%
            pull() %>%
            sort()

        })

        observe({
          updatePickerInput(session = session, ("dropdown_filter2"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })

        dynamic_dropdown_search <- reactive({

          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>%
            mutate(column4 = as.factor(!!rlang::sym(column4))) %>%
            filter(column4 %in% input$dropdown_filter4) %>%
            filter(column3 %in% input$dropdown_filter3) %>%
            filter(column2 %in% input$dropdown_filter2) %>%
            distinct(column1) %>%
            mutate(column1 = as.character(column1)) %>%
            pull() %>%
            sort()

        })

        observe({
          updatePickerInput(session = session, ("dropdown_filter1"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })


      }

      return(dynamic_dropdown_search)

    }
  )
}

download_table_UI <- function(id) {
  ns <- NS(id)


div(style="display: inline-block;vertical-align:top; width: 50px;",
    
    dropdown(
      downloadBttn(
        ns("download_csv"),
        label = "Download citations in CSV format",
        style = "unite",
        color = "primary",
        size = "sm",
        block = FALSE,
        no_outline = TRUE
      ),
      downloadBttn(
        ns("download_endnote"),
        label = "Download citations in Endnote tab delimited format",
        style = "unite",
        color = "primary",
        size = "sm",
        block = FALSE,
        no_outline = TRUE
      ),
      downloadBttn(
        ns("download_syrf"),
        label = "Download citations in SyRF upload format",
        style = "unite",
        color = "primary",
        size = "sm",
        block = FALSE,
        no_outline = TRUE
      ),
      
      br(),
      # p("Note for Rayyan export option below: download file and open in MS Excel first on your computer. Save as .csv in excel, then import saved file into Rayyan"),
      # downloadBttn(
      #   ns("download_rayyan"),
      #   label = "Download citations in Rayyan upload format",
      #   style = "unite",
      #   color = "primary",
      #   size = "sm",
      #   block = FALSE,
      #   no_outline = TRUE
      # ),
      
      style = "unite", icon = icon("download"),
      inline = TRUE,
      status = "success", width = "600px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig),
      tooltip = tooltipOptions(title = "Click to download relevant studies")
      
    )
    
)
}

download_table_Server <- function(id, table) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)  
      
      # Download citations sever side --------
      # download refs button server side -csv
      output$download_csv <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".csv", sep="")
        },
        
        content = function(file) {
          write.csv(search_results_download(), file, row.names = FALSE)
        }
      )
      
      search_results_download <- reactive({
        
        results <- citations_for_dl %>%
          filter(uid %in% !!table$uid)
        

      })
      
      search_results_download_syrf <- reactive({
        
        # tbl(con, "unique_citations"), filter, collect
        results <- citations_for_dl %>%
          filter(uid %in% !!table$uid)
        
        rresults <- results %>%
          rename(Authors = author,
                 Title = title,
                 Abstract = abstract,
                 Url = url,
                 Year = year,
                 DOI= doi,
                 PublicationName = journal) %>%
          mutate(AlternateName = "",
                 AuthorAddress = "",
                 ReferenceType = "",
                 Keywords = keywords,
                 CustomId = uid,
                 PdfRelativePath = paste0(uid, ".pdf")) %>%
          select(Title,
                 Authors,
                 PublicationName,
                 AlternateName,
                 Abstract,
                 Url,
                 AuthorAddress,
                 Year,
                 DOI,
                 ReferenceType,
                 Keywords,
                 CustomId,
                 PdfRelativePath)
        
        
      })
      
      # download refs button server side - endnote
      output$download_syrf <- downloadHandler(
        filename = function() {
          paste0("citations-srf-", Sys.Date(),
                 ".csv", sep="")
        },
        content = function(file) {
          write.csv(search_results_download_syrf(), file,
                    #col.names=TRUE, 
                    row.names = F, na="")
        })
      
      search_results_download_endnote <- reactive({
        
        # tbl(con, "unique_citations"), filter, collect
        results <- citations_for_dl %>%
          filter(uid %in% !!table$uid)
        
        results <- results %>%
          filter(uid %in% table$uid) %>%
          mutate("Reference Type" = "Journal Article") %>%
          mutate(isbn = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
          rename("Custom 1" = uid,
                 "Secondary Title" = journal,
                 "ISBN/ISSN" = isbn) %>%
          select("Reference Type", "author", "year",
                 "Secondary Title", "doi", "title",
                 "pages", "volume", "number", "abstract",
                 "Custom 1", "ISBN/ISSN") %>%
          mutate(abstract = gsub("\\r\\n|\\r|\\n", "", abstract))
        
        names(results) <- toTitleCase(names(results))
        
        results <- results %>%
          rename("DOI"= Doi)
        
        return(results)
        
      })
      
      # download refs button server side -endnote
      output$download_endnote <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".txt", sep="")
        },
        content = function(file) {
          write.table(search_results_download_endnote(), file, sep="\t",
                      col.names=TRUE, row.names = F, quote=FALSE, na="")
        })
    }
  )
}
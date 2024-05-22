library(dplyr)
library(shinythemes)
library(viridis)
library(viridisLite)
library(shiny)
library(shinyalert)
library(fst)
library(shinyWidgets)
library(ggplot2)
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(purrr)
library(networkD3)
library(RColorBrewer)
library(bs4Dash)
library(googlesheets4)
library(tools)
library(readr)
library(dbplyr)
library(odbc)
library(DBI)
library(fresh)
library(htmlwidgets)
library(lubridate)
library(stringr)
library(readr)
library(jsonlite)
library(tidyr)
library(pool)

source("irise_modules.R")

irise_colours <- list(
  c(dark_blue = "#1A465F",
    dot_text_green = "#64C296",
    doc_template_green = "#B1E0CB",
    gradient_arrow = "#89CB93",
    turquoise = "#47B1A3",
    dark_turquoise = "#266080"
    
  ))


# Set theme -------------------------------------------------
mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#64C296",
    navbar_light_active_color = "#000",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 105,
    text_dark = "#FFF", 
    text_light = "#FFF"
  ),
  bs4dash_layout(
    main_bg = "#FFF"
  ),
  bs4dash_sidebar_dark(
    bg = "#1A465F", 
    color = "#FFF",
    hover_color = "#FFF"
  ),
  
  bs4dash_status(
    primary = "#64C296", 
    danger = "#1A465F",  
    info = "#47B1A3", 
    secondary = "#266080",
    warning = "#89CB93",
    success = "#B1E0CB"
  ),
  bs4dash_color(
    gray_900 = "#64C296")
)

# Specify the directory where the fst files are located
dir_path <- "fst_files"

# Get a list of all fst files in the directory
all_files <- list.files(path = dir_path, pattern = "\\.fst$")

# Read each fst file
for (file in all_files) {
  assign(sub("\\.fst$", "", file), read_fst(file.path(dir_path, file)), envir = .GlobalEnv)
}

# Create list for pico elements in search filter dropdown menu
pico_elements_list <- list(
)

ui <- bs4DashPage(freshTheme = mytheme,
                  
                  
                  dark = NULL, 
                  
                  # tags$head(tags$style(HTML('* {font-family: "Arial"};')))
                  
                  dbHeader <- dashboardHeader(title = "iRISE-SOLES",
                                              tags$a(href= 'https://irise-project.eu/',
                                                     tags$img(src= "iRISE-lightlogo.png",
                                                              height = "50px"))
                  ),
                  dashboardSidebar(skin = "dark",
                                   sidebarMenu(
                                     bs4SidebarMenuItem(tags$p("Homepage", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "home", icon = icon("home")),
                                     bs4SidebarMenuItem(tags$p("Data Collection", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "dc-main", icon = icon("database", verify_fa = FALSE), startExpanded = FALSE,
                                                        bs4SidebarMenuSubItem(tags$p("Included Studies", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "studies-included-summary-dc")),
                                     #bs4SidebarMenuSubItem(tags$p("Tag Status", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "status-summary-dc"),
                                     #bs4SidebarMenuSubItem(tags$p("Workflow", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "workflow-accordion-dc")),
                                     bs4SidebarMenuItem(tags$p("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "data-summary-transparency", icon = icon("chart-pie", verify_fa = FALSE)),
                                     #bs4SidebarMenuItem(tags$p("Intervention / Outcome", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "data-int-out", icon = icon("chart-column", verify_fa = FALSE)),
                                     #bs4SidebarMenuItem(tags$p("Evidence Map", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "pico-bubble", icon = icon("users-gear", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Across Disciplines", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "int_ac_dis-bubble", icon = icon("users-gear", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Populations Bar", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "discipline_bar", icon = icon("users-gear", verify_fa = FALSE)),
                                     
                                     
                                     #bs4SidebarMenuItem(tags$p("Author Location", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "author_location", icon = icon("earth-americas")),
                                     bs4SidebarMenuItem(tags$p("iRISE Database", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "module_search_database", icon = icon("search")),
                                     bs4SidebarMenuItem(tags$p("About", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "about", icon = icon("info"))
                                   )
                  ),
                  
                  bs4DashBody(
                    
                    use_theme(mytheme),
                    
                    useShinyjs(),
                    
                    
                    tabItems(
                      
                      tabItem(tabName = "home",
                              
                              box(
                                div(
                                  style = "text-align: center;",
                                  tags$a(
                                    href = 'https://irise-project.eu/',
                                    tags$img(src = "iRISE_logo_dark_round.png", height = "300px")
                                  )
                                ),
                                
                                tags$br(),
                                #'Montserrat', 'Gotham', Arial, Helvetica, sans-serif, 
                                div(
                                  
                                  style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                  "Taking an integrated approach to understanding, investigating and guiding strategies to address irreproducibility"
                                ),
                                
                                
                                background = "primary",
                                width = 12,
                                solidHeader = TRUE,
                                title = "",
                                status = "primary"),
                              
                              plot_interpret_UI(id = "home_info",
                                                title = "",
                                                theme = "danger",
                                                div(
                                                  style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                                  "The overall aim for iRISE-SOLES is to systematically identify, synthesise and evaluate information on existing candidate interventions and tools to improve reproducibility. To do this, we have 
                                                  developed an integrated workflow of automated tools to collect and tag published research articles and visualise the evidence in this interactive web application. 
                                                  We tag studies by discipline, study type, author country, intervention type, and reproducibility relevant outcomes. We also assess the transparency metrics of studies witin iRISE-SOLES e.g. their open access status and presence of data/code sharing.")),
                              
                              fluidRow(
                                
                                column(4,
                                       
                                       box(
                                         div(
                                           style = "text-align: center;",
                                           tags$a(
                                             href = 'https://osf.io/9hzcv/?view_only=d74ff8089864468cb43daa06733e0be6',
                                             tags$img(src = "osf_logo.png", height = "300px")
                                           )
                                         ),
                                         
                                         tags$br(),
                                         #'Montserrat', 'Gotham', Arial, Helvetica, sans-serif, 
                                         div(
                                           style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                           "Read our iRISE-SOLES protocol on the Open Science Framework"),
                                         background = "warning",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         title = "",
                                         status = "warning")),
                                
                                column(4, 
                                       box(
                                         div(
                                           style = "text-align: center;",
                                           tags$a(
                                             href = 'https://portlandpress.com/clinsci/article/137/10/773/233083/Systematic-online-living-evidence-summaries',
                                             tags$img(src = "paper_screenshot.PNG", height = "300px")
                                           )
                                         ),
                                         
                                         tags$br(),
                                         #'Montserrat', 'Gotham', Arial, Helvetica, sans-serif, 
                                         div(
                                           style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                           "Read our SOLES paper to learn more about our workflow"),
                                         background = "secondary",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         title = "",
                                         status = "secondary")), 
                                
                                column(4,
                                       box(
                                         div(
                                           style = "text-align: center;",
                                           tags$a(
                                             href = 'https://irise-project.eu/',
                                             tags$img(src = "irise_website.png", height = "300px")
                                           )
                                         ),
                                         
                                         tags$br(),
                                         div(
                                           style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                           "Go to the iRISE website to learn more about the other work packages and wider project"),
                                         background = "info",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         title = "",
                                         status = "info")
                                       
                                ))),
                      
                      tabItem(tabName = "studies-included-summary-dc",
                              fluidRow(
                                valueBox(
                                  width=3,
                                  subtitle = tags$p("new citations this week", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  color = "success",
                                  value = tags$p(sum(as.numeric(include_by_date$n)[which(include_by_date$date >= Sys.Date()-7)]),
                                                 style = "font-size: 300%; color: white; font-family: KohinoorBangla, sans-serif !important"),
                                  icon = icon("clock", verify_fa = FALSE)),
                                
                                valueBox(
                                  width=3,
                                  subtitle = tags$p("new citations this month", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  color = "warning",
                                  value = tags$p(sum(as.numeric(include_by_date$n)[which(include_by_date$date >= Sys.Date()-30)]),
                                                 style = "font-size: 300%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  icon = icon("calendar")),
                                
                                valueBox(
                                  width=3,
                                  subtitle = tags$p("new citations in the last year", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  color = "info",
                                  value = tags$p(sum(as.numeric(include_by_date$n)[which(include_by_date$date >= Sys.Date()-365)]),
                                                 style = "font-size: 300%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  icon = icon("calendar", verify_fa = FALSE)),
                                
                                valueBox(
                                  width=3,
                                  subtitle = tags$p("citations in database", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  color = "secondary",
                                  value = tags$p(sum(as.numeric(include_by_date$n)),
                                                 style = "font-size: 300%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                                  icon = icon("database"))
                              ),
                              
                              fluidRow(
                                
                                tabBox(
                                  width=12,
                                  id = "tabcard_included_studies",
                                  title = "",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  type = "tabs",
                                  
                                  yearBarUI_included_only("included_studies_over_time_bar",
                                                          title = tags$p("Included Studies Over Time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                                                          theme = "danger",
                                                          spinner_colour = "#89CB93",
                                                          table = n_included_per_year_plot_data)
                                  
                                )
                              )
                      ),
                      
                      # Transparency info tab UI--------------------------------------------------------------------------------------------------------------------------
                      tabItem(tabName = "data-summary-transparency",
                              
                              bs4Jumbotron(
                                title = tags$h1("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                lead = tags$p("This summary shows the overall percentages of publications
                across different transparency measures, including open access publication, open data, and open code. You can
                also benchmark improvements by viewing the number of publications in each category over time.", style = ";font-family: Kohinoor Bangla;"),
                                status = "primary",
                                btnName = NULL
                              ),
                              
                              fluidRow(
                                
                                valueBox(
                                  width=4,
                                  subtitle = tags$h2("Open Access", style = "color: white;font-family: KohinoorBangla, sans-serif !important;"),
                                  color = "success",
                                  value = tags$p(round(length(oa_tag$uid[which(oa_tag$is_oa==TRUE)])/length(oa_tag$uid)*100,1), "%",
                                                 style = "font-size: 300%; color: white;"),
                                  icon = icon("lock")
                                ),
                                
                                valueBox(
                                  width=4,
                                  subtitle = tags$h2("Open Data", style = "color: white;font-family: KohinoorBangla, sans-serif !important;"),
                                  color = "info",
                                  value = tags$p(round(length(transparency$uid[which(transparency$is_open_data==TRUE)])/length(transparency$uid)*100,1), "%",
                                                 style = "font-size: 300%; color: white;"),
                                  icon = icon("bar-chart", verify_fa = FALSE)
                                ),
                                
                                valueBox(
                                  width=4,
                                  subtitle = tags$h2("Open Code", style = "color: white;font-family: KohinoorBangla, sans-serif !important;"),
                                  color = "secondary",
                                  value = tags$p(round(length(transparency$uid[which(transparency$is_open_code==TRUE)])/length(transparency$uid)*100,1), "%",
                                                 style = "font-size: 300%; color: white;"),
                                  icon = icon("code")
                                )
                              ),
                              
                              
                              tabBox(
                                
                                width = 12,
                                id = "tabcard",
                                title = "",
                                status = "secondary",
                                solidHeader = FALSE,
                                type = "tabs",
                                
                                yearBarUI("oa_pubs_per_year",
                                          title = tags$p("Open access over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                                          theme = "secondary",
                                          spinner_colour = "#89CB93",
                                          table = oa_tag),
                                
                                yearBarUI("oa_pub_type_per_year",
                                          title = tags$p("Open access type over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                                          theme = "danger",
                                          spinner_colour = "#89CB93",
                                          table = oa_tag),
                                
                                yearBarUI("open_data_pubs_per_year",
                                          title = tags$p("Open data availability over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                                          theme = "danger",
                                          spinner_colour = "#89CB93",
                                          table = transparency),
                                
                                yearBarUI("open_code_pubs_per_year",
                                          title = tags$p("Open code availability over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                                          theme = "danger",
                                          spinner_colour = "#89CB93",
                                          table = transparency)
                                
                                
                              ),
                              
                              plot_interpret_UI("transparency_intepret",
                                                title = tags$p("How To Interpret This Plot", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                
                                                div(
                                                  tags$p("Each bar plot shows the number of papers in each category over time.
                    Navigate between tabs to see different transparency measures.
                    You can hover your mouse over the bars to see the exact number of publications estimated to be in each category for any given year.
                    To see only a specific category, double click on the relevant coloured square in the
                    legend on the top right. To remove any category, click once on any coloured square in the legend.
                    The tools and resources used to obtain the data are shown under the x-axis. Note that many publications are
                    still missing a transparency status for one or more measures due to processing time or lack of available data.", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                  tags$br(),
                                                  tags$br(),
                                                  
                                                  tags$a(
                                                    href = 'https://research.library.gsu.edu/c.php?g=115588&p=754380',
                                                    tags$button("Open Access Type Info", class = "btn btn-primary", style = "background-color: #1A465F; border-color: #1A465F;font-family: KohinoorBangla, sans-serif !important;")
                                                  )),
                                                theme = "primary")
                              
                              
                      ),
                      
                      tabItem(tabName = "pico-bubble",
                              
                              box(
                                width= 12,
                                status = "primary",
                                id = "pico_bubble_search_tab",
                                tags$style(HTML('.btn-light {
                    background-color: #efefef !important;
                    color: black !important;
                    }')),
                                #side = "left",
                                
                                tabPanel(title = "Population",
                                         
                                         fluidRow(column(width = 6,
                                                         pickerInput(
                                                           inputId = "select_intervention_provider",
                                                           label = tags$p("Select an Intervention Provider", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                           choices = sort(unique(dummy_data_for_bubble$intervention_provider)),
                                                           selected = sort(unique(dummy_data_for_bubble$intervention_provider)),
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
                                                           choices = sort(unique(dummy_data_for_bubble$target_population)),
                                                           selected = sort(unique(dummy_data_for_bubble$target_population)),
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
                                                           choices = sort(unique(dummy_data_for_bubble$discipline)),
                                                           selected = sort(unique(dummy_data_for_bubble$discipline)),
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
                                                           inputId = "select_mod",
                                                           label = tags$p("Select Method of Delivery", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                           choices = sort(unique(dummy_data_for_bubble$method_of_delivery)),
                                                           selected = sort(unique(dummy_data_for_bubble$method_of_delivery)),
                                                           multiple = TRUE,
                                                           options = list(
                                                             `actions-box` = TRUE,
                                                             `live-search` = TRUE
                                                           ))
                                                         
                                                         
                                                  ),
                                                  column(width = 4,
                                                         pickerInput(
                                                           inputId = "select_research_stage",
                                                           label = tags$p("Select Research Stage", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                           choices = sort(unique(dummy_data_for_bubble$research_stage)),
                                                           selected = sort(unique(dummy_data_for_bubble$research_stage)),
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
                                
                                
                                
                                plotlyOutput("bubble_plot_multi"),
                                #verbatimTextOutput("error_message"),
                                tags$br(),
                                tags$br()
                                
                                
                              ),
                              
                              box(
                                
                                width = 12,
                                id = "intervention_outcome_datatable",
                                status = "primary",
                                
                                DT::dataTableOutput("pop_table") %>% withSpinner(color="#96c296")
                                #verbatimTextOutput("error_message")
                                
                                
                                
                              )),
                      # UI Interventions across disciplines ----
                      tabItem(tabName = "int_ac_dis-bubble",
                              
                              box(
                                width= 12,
                                status = "primary",
                                id = "pico_bubble_search_tab",
                                tags$head(
                                  tags$style(HTML('
      .btn-light {
        background-color: #efefef !important;
        color: black !important;
      }
    '))
                                ),
                                tabPanel(title = "Population",
                                         
                                         fluidRow(column(width = 6,
                                                         pickerInput(
                                                           inputId = "select_outcome",
                                                           label = tags$p("Select an Outcome Measure", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                           choices = sort(unique(dummy_data_for_bubble$outcome_measures)),
                                                           selected = c("Materials availability and re-use", "Data availability and re-use", "Computational reproducibility", "Code / analysis availability and re-use", "Reporting quality"),
                                                           #sort(unique(dummy_data_for_bubble$outcome_measures)),
                                                           multiple = TRUE,
                                                           options = pickerOptions(noneSelectedText = "Please Select",
                                                                                   virtualScroll = 100,
                                                                                   actionsBox = TRUE,
                                                                                   size = 10,
                                                           )
                                                         )
                                         ),
                                         column(width = 6,
                                                pickerInput(
                                                  inputId = "select_discipline_legend",
                                                  label = tags$p("Select a Discipline", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                  choices = sort(unique(dummy_data_for_bubble$discipline)),
                                                  selected = c("Biomedical and health sciences ", "Social sciences ", "Natural sciences "),
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
                                height = 900,
                                id = "intervention_discipline",
                                status = "primary",
                                
                                materialSwitch(inputId = "switch_over_time",
                                               label = "Over time", 
                                               status = "info"),
                                
                                
                                
                                plotlyOutput("int_ac_dis_bubble_plot"),
                                verbatimTextOutput("error_message"),
                                tags$br(),
                                tags$br()
                                
                                
                              ),
                              
                              box(
                                
                                width = 12,
                                id = "intervention_ac_dis_datatable",
                                status = "primary",
                                
                                DT::dataTableOutput("int_ac_dis_table") %>% withSpinner(color="#96c296")
                                #verbatimTextOutput("error_message")
                                
                                
                                
                              )),
                      
                      # Disciplines bar -----
                      tabItem(tabName = "discipline_bar",
                              
                              box(
                                width= 12,
                                status = "primary",
                                id = "discipline_bar_tab",
                                tags$style(HTML('.btn-light {
                      background-color: #efefef !important;
                      color: black !important;
                      }')),
                                #side = "left",
                                
                                tabPanel(title = "",
                                         
                                         fluidRow(
                                           column(width = 4,
                                                         pickerInput(
                                                           inputId = "select_outcome_bar",
                                                           label = tags$p("Select an Outcome Measure", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                           choices = sort(unique(dummy_data_for_bubble$outcome_measures)),
                                                           selected = sort(unique(dummy_data_for_bubble$outcome_measures)),
                                                           multiple = TRUE,
                                                           options = pickerOptions(noneSelectedText = "Please Select",
                                                                                   virtualScroll = 100,
                                                                                   actionsBox = TRUE,
                                                                                   size = 10,
                                                           )
                                                         )
                                                  
                                                  
                                                  ),
                                           column(width = 4,
                                                  pickerInput(
                                                    inputId = "legend_select",
                                                    label = tags$p("Select Legend", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                    choices = c("Intervention Provider", "Target Population", "Discipline"),
                                                    selected = c("Intervention Provider"),
                                                    multiple = FALSE,
                                                    options = pickerOptions(noneSelectedText = "Please Select",
                                                                            virtualScroll = 100,
                                                                            actionsBox = TRUE,
                                                                            size = 10,
                                                    )
                                                  )
                                           ),
                                           column(width = 4,
                                                  pickerInput(
                                                    inputId = "legend_select_specific",
                                                    label = tags$p("Select Population", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                    choices = list(),
                                                    selected = ,
                                                    multiple = TRUE,
                                                    options = pickerOptions(noneSelectedText = "Please Select",
                                                                            virtualScroll = 100,
                                                                            actionsBox = TRUE,
                                                                            size = 10,
                                                    )
                                                  )
                                           ),
                                           
                                         )
                                )),
                              
                              box(
                                
                                width = 12,
                                height = 700,
                                id = "disc_bar",
                                status = "primary",
                                
                                materialSwitch(inputId = "bar_switch_over_time",
                                               label = "Over time", 
                                               status = "info"),
                                
                                
                                plotlyOutput("discipline_bar"),
                                #verbatimTextOutput("error_message"),
                                tags$br(),
                                tags$br()
                                
                                
                              ),
                              
                              # box(
                              # 
                              #   width = 12,
                              #   id = "intervention_ac_dis_datatable",
                              #   status = "primary",
                              # 
                              #   DT::dataTableOutput("int_ac_dis_table") %>% withSpinner(color="#96c296")
                              #   #verbatimTextOutput("error_message")
                              # 
                              # 
                              # 
                              # )
                      ),
                      
                      # tabItem(tabName = "author_location",
                      # 
                      #         tabBox(
                      # 
                      #           width = 12,
                      #           id = "tabcard_location",
                      #           title = "",
                      #           status = "primary",
                      #           solidHeader = FALSE,
                      #           type = "tabs",
                      # 
                      #           tabPanel(
                      #             width=12,
                      #             title="World map of author location",
                      #             status="success",
                      #             solidHeader = TRUE,
                      #             plotlyOutput("tagged_author_map", height = "550px") %>% withSpinner(color="#96c296")
                      #           )
                      # 
                      #         )
                      # ),
                      
                      #Search database tab UI--------------------------------------------------------------------------------------------------------------------------
                      tabItem(tabName = "module_search_database",
                              
                              
                              search_UI("search_results",
                                        table = citations_for_dl)
                              
                              
                      ),
                      
                      tabItem(tabName = "about",
                              
                              
                              fluidRow(
                                
                                box(width = 5,
                                    title = "Funding",
                                    background="danger",
                                    solidHeader = T,
                                    status="danger",
                                    p("iRISE receives funding from the European Union's Horizon Europe research and innovation programme under grant agreement No 101094853. 
                                      Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the 
                                      European Research Executive Agency (ERA). Neither the European Union nor the ERA can be held responsible for them."),
                                    tags$img(class = "img-responsive img-rounded center-block",
                                             src="european_union_logo.jpg", height=100, width=470, align="center")),
                                
                                box(width = 7,
                                    title = "Using iRISE-SOLES data",
                                    background="warning",
                                    solidHeader = T,
                                    status = "warning",
                                    p("We license all data and information provided under a
                      Creative Commons Attribution 4.0 International license (CC BY 4.0)"),
                                    p("If you have used the iRISE-SOLES data for a research project or review, please cite our protocol: 
                      Kaitlyn Hair, Sean Smith, Ivan Buljan, Carlijn R. Hooijmans, Malcolm R. Macleod, Ana Marušić, Dora Pejdo, Torsten Rackoll, Kimberley E. Wever, Sarah Wendt,
                      Sarah McCann, and Emily S. Sena on behalf of the iRISE consortium (2023), A protocol for a systematic online living evidence summary of the interventions to improve reproducibility (iRISE-SOLES),
                                      Open Science Framework. https://doi.org/10.31222/osf.io/nbe5q"))
                              ),
                              
                              fluidRow(
                                
                                box(width = 12,
                                    title = "Development",
                                    status="info",
                                    solidHeader = T,
                                    background="info",
                                    p("iRISE-SOLES was developed as part of Work Package 2 in the iRISE project. The main developer behind the platform
                                      is Sean Smith."),
                                    p("If you have any questions about the iRISE-SOLES project, please contact:",
                                      strong(tags$a(href="mailto:kaitlyn.hair@ed.ac.uk", "kaitlyn.hair@ed.ac.uk")))))
                      )
                    ))
)





server <- function(input, output, session) {
  
  shinyalert("Welcome", "Welcome to the draft iRISE-SOLES Dashboard!
             Please note this app is still under development. The data presented are not representative and should not be used for any research purposes yet.", type = "info")
  
  
  yearBarServer_included_only("included_studies_over_time_bar",
                              table=n_included_per_year_plot_data,
                              column="is_included",
                              colour = "#89CB93")

  # Transparency bar plots--------
  yearBarServer("oa_pubs_per_year", table=oa_tag, column="is_oa", display=TRUE, order=c(TRUE, FALSE), text="Source:CrossRef", colours = c("#89CB93", "grey")) %>%
    bindCache(nrow(transparency))
  yearBarServer("oa_pub_type_per_year", table=oa_tag, column="oa_status", display=c("closed", "hybrid", "bronze", "gold", "green"), order=c("closed", "hybrid", "bronze", "gold", "green"),
                text="Source:CrossRef", colours = c("red", "lightblue", "orange", "gold", "green")) %>% bindCache(nrow(transparency))
  yearBarServer("open_data_pubs_per_year", table=transparency, column="is_open_data", display=TRUE, order=c(TRUE, FALSE),  text="Tool: OddPub, Riedel, N, et al. (2020), DOI:10.5334/dsj-2020-042", colours = c("#89CB93", "grey")) %>% bindCache(nrow(transparency))
  yearBarServer("open_code_pubs_per_year", table=transparency, column="is_open_code", display=TRUE, order=c(TRUE, FALSE),  text="Tool: OddPub, Riedel, N, et al. (2020), DOI:10.5334/dsj-2020-042", colours = c("#89CB93", "grey")) %>% bindCache(nrow(transparency))

  
  
  # Search Page Server
  search_Server("search_results",
                pico_data = pico_elements_list,
                table = included_with_metadata,
                combined_pico_table = pico,
                citations_for_download = citations_for_dl,
                project_name = "iRISE-SOLES")
  
  
  # # Tagged Author location map ----
  # output$tagged_author_map <- renderPlotly({
  #   
  #   country_map_n <- author_location_df  %>%
  #     filter(!name == "Unknown") %>%
  #     unnest(cols = name) %>%
  #     group_by(name) %>%
  #     count()
  #   
  #   df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
  #   
  #   data_country_merged <- left_join(df, country_map_n, by=c("COUNTRY"="name"))
  #   
  #   # light grey boundaries
  #   l <- list(color = toRGB("grey"), width = 0.5)
  #   
  #   # specify map projection/options
  #   g <- list(
  #     showframe = FALSE,
  #     showcoastlines = FALSE,
  #     projection = list(type = 'Mercator')
  #   )
  #   # light grey boundaries
  #   l <- list(color = toRGB("grey"), width = 0.5)
  #   
  #   # specify map projection/options
  #   g <- list(
  #     showframe = FALSE,
  #     showcoastlines = FALSE,
  #     projection = list(type = 'Mercator')
  #   )
  #   
  #   plot_geo(data_country_merged) %>%
  #     add_trace(
  #       z = ~n,
  #       color = ~n, 
  #       colors = 'Blues',
  #       text = ~COUNTRY,
  #       locations = ~CODE, 
  #       marker = list(line = l)
  #     ) 
  # })
  # 
  # 
  
  # Evidence Map Original -----
  #Bubble Multi Select----
  # previous_state <- reactiveValues(
  #   column1 = NULL,
  #   column2 = NULL
  # )

  # bubble_react_multi <- reactive({
  # 
  #   #browser()
  #   data_for_bubble <- dummy_data_for_bubble %>%
  #     filter(intervention_provider %in% input$select_intervention_provider) %>%
  #     filter(target_population %in% input$select_pop_target) %>%
  #     filter(discipline %in% input$select_discipline) %>%
  #     filter(research_stage %in% input$select_research_stage) %>%
  #     filter(method_of_delivery %in% input$select_mod)
  # 
  # 
  #   data <- data_for_bubble %>%
  #     group_by(uid, intervention, outcome_measures) %>%
  #     count() %>%
  #     ungroup() %>%
  #     count(intervention, outcome_measures)
  # 
  #   data$key <- row.names(data)
  #   data$col <- "#266080"
  # 
  #   click_data <- event_data("plotly_click", priority = "event", source = "B")
  # 
  #   if (!is.null(click_data)) {
  # 
  #     bubble_react_new <- data %>%
  #       mutate(selected_colour = key %in% click_data$customdata)
  # 
  #     bubble_react_new$selected_colour <- data$key %in% click_data$customdata
  # 
  #     if (exists("col_vector")){
  # 
  #       bubble_react_new$col <- col_vector
  #     }
  # 
  #     selected_row <- which(rownames(bubble_react_new) %in% click_data$customdata)
  # 
  #     if (!bubble_react_new$col[selected_row] == "#47B1A3"){
  # 
  #       # Update only the selected rows
  #       bubble_react_new$col[selected_row] <- "#47B1A3"
  # 
  #       assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
  # 
  #     } else{
  # 
  #       bubble_react_new$col[selected_row] <- "#266080"
  # 
  #       assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
  # 
  #     }
  # 
  # 
  #   }
  #   else {
  # 
  #     data$col <- "#266080"
  # 
  #     bubble_react_new <- data
  # 
  #   }
  # 
  # 
  #   if (!is.null(previous_state$column1) &&
  #       identical(data_for_bubble$intervention_provider, previous_state$column1) &&
  #       identical(data_for_bubble$target_population, previous_state$column2)) {
  #     # Columns are unchanged
  #     print("Columns are unchanged")
  # 
  #   } else {
  #     # Columns have changed
  #     print("Columns have changed")
  # 
  #     # Update the previous state
  #     previous_state$column1 <- data_for_bubble$intervention_provider
  #     previous_state$column2 <- data_for_bubble$target_population
  # 
  #     rm(col_vector, envir = .GlobalEnv)
  # 
  #   }
  # 
  # 
  #   return(bubble_react_new)
  # })

  
  
  # table_react <- reactive({
  #   #browser()
  #   table <- dummy_data_for_bubble %>%
  #     filter(intervention_provider %in% input$select_intervention_provider) %>%
  #     filter(target_population %in% input$select_pop_target) %>%
  #     filter(discipline %in% input$select_discipline) %>%
  #     filter(research_stage %in% input$select_research_stage) %>%
  #     filter(method_of_delivery %in% input$select_mod)
  # 
  #   table_filter <- bubble_react_multi() %>%
  #     filter(col == "#47B1A3")
  # 
  #   table <- table %>%
  #     filter(intervention %in% table_filter$intervention,
  #            outcome_measures %in% table_filter$outcome_measures)
  #   #browser()
  #   final_table <- dummy_data_for_bubble %>%
  #     filter(uid %in% table$uid) %>%
  #     left_join(included_with_metadata, by = "uid") %>%
  #     select(uid, year, author, title, intervention, outcome_measures, discipline, method_of_delivery, research_stage, doi, url) %>%
  #     mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
  #     arrange(desc(year))
  # 
  #   if (nrow(final_table) > 0 ){
  # 
  #     final_table$title <- paste0("<a href='",final_table$link, "' target='_blank'>",final_table$title,"</a>")
  # 
  # 
  #   }
  # 
  # 
  #   final_table <- final_table %>%
  #     select(-doi, -url, -link, - uid)
  # 
  # 
  # 
  # 
  #   return(final_table)
  # })
  
  
  # output$bubble_plot_multi <- renderPlotly({
  #   #browser()
  #   #plot <- bubble_react_multi()
  #   tryCatch({
  #     p <- plot_ly(bubble_react_multi(),
  #                  x = ~intervention, y = ~outcome_measures, size = ~n,
  #                  colors = ~sort(unique(col)), color = ~col, customdata = ~key,
  #                  type = 'scatter',
  #                  source = "B",
  #                  marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.8,
  #                                line = list(color = '#FFFFFF', width = 2)),
  #                  hoverinfo = 'text',
  #                  textposition = "none",
  #                  text = ~paste("Intervention:", intervention,
  #                                "Outcome:", outcome_measures,
  #                                "Number of Citations:", n)) %>%
  #       layout(yaxis = list(title = list(text = "Outcome", standoff = 25)),
  #              xaxis = list(title = list(text = "Intervention", standoff = 25),
  #                           tickangle = -20,
  #                           ticklen = 1),
  #              hoverlabel = list(bgcolor = "white",
  #                                font = list(size = 14)),
  #              height = 550,
  #              showlegend = FALSE,
  #              clickmode = "event + select")
  # 
  #     return(p)
  #   }, error = function(e) {
  # 
  #     # Here you could log the error message or handle it differently
  #     #output$error_message <- renderText({ "Error occurred: Please make another choice." })  # Inform the user to make another choice
  # 
  #     return(NULL)  # Return NULL to avoid further processing or showing an erroneous plot
  #   })
  # })


  # output$pop_table <- DT::renderDataTable({
  #   #browser()
  #   table <- table_react()
  #   DT::datatable(
  #     table_react(),
  #     rownames = FALSE,
  #     escape = FALSE,
  #     # extensions = c('Buttons'),
  #     options = list(
  #       language = list(
  #         zeroRecords = "Click on a point to show data",
  #         emptyTable = "Click on a point to show data"),
  #       deferRender = FALSE,
  #       scrollY = 600,
  #       scrollX = 100,
  #       scroller = TRUE,
  #       columnDefs = list(
  #         list(
  #           targets = c(2), #target for JS code
  #           render = JS(
  #             "function(data, type, row, meta) {",
  #             "return type === 'display' && data.length > 100 ?",
  #             "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
  #             "}")),
  #         list(
  #           targets = c(1,2), #target for JS code
  #           render = JS(
  #             "function(data, type, row, meta) {",
  #             "return type === 'display' && data.length > 15 ?",
  #             "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
  #             "}")),
  # 
  #         list(width = '10%', targets = "_all")
  #       )
  #     )
  # 
  #   )
  # 
  # })
  
  
  
  # Interventions across Disciplines Bubble ----
  previous_state <- reactiveValues(
    column1 = NULL,
    column2 = NULL
  )
  
  bubble_react <- reactive({
    #browser()
    data <- dummy_data_for_bubble %>%
      filter(outcome_measures %in% input$select_outcome) %>% 
      filter(discipline %in% input$select_discipline_legend)
  
    
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


    }
    else {

      data$col <- "#266080"

      bubble_react_new <- data

    }


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

      #rm(col_vector, envir = .GlobalEnv)

    }


    return(bubble_react_new)
  })
  
  
  
  table_react <- reactive({
    #browser()
    table <- dummy_data_for_bubble %>%
      filter(outcome_measures %in% input$outcome_measures)
    
    table_filter <- bubble_react() %>%
      filter(col == "#47B1A3")
    
    table <- table %>%
      filter(intervention %in% table_filter$intervention,
             discipline %in% table_filter$discipline)
    #browser()
    final_table <- dummy_data_for_bubble %>%
      filter(uid %in% table$uid) %>%
      left_join(included_with_metadata, by = "uid") %>%
      select(uid, year, author, title, intervention, outcome_measures, discipline, method_of_delivery, research_stage, doi, url) %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      arrange(desc(year))
    
    if (nrow(final_table) > 0 ){
      
      final_table$title <- paste0("<a href='",final_table$link, "' target='_blank'>",final_table$title,"</a>")
      
      
    }
    
    
    final_table <- final_table %>%
      select(-doi, -url, -link, - uid)
    
    
    
    
    return(final_table)
  })
  
  
  output$int_ac_dis_bubble_plot <- renderPlotly({
    
    plot <- bubble_react()


    if(input$switch_over_time){
      
      citations_years <- citations_for_dl %>% 
        select(uid, year)

      data <- plot %>%
        left_join(citations_years, by = "uid") %>%
        group_by(uid, year,  intervention, discipline, outcome_measures) %>%
        count() %>%
        ungroup() %>%
        group_by(year, intervention, discipline, outcome_measures) %>%
        count(name = "yearly_count") %>%
        ungroup() %>% 
        group_by(intervention, discipline, outcome_measures) %>%
        mutate(cumulative_count = cumsum(yearly_count)) %>%
        ungroup()
      
      all_combinations <- expand.grid(
        year = min(data$year):max(data$year),
        intervention = unique(data$intervention),
        discipline = unique(data$discipline),
        outcome_measures = unique(data$outcome_measures)
      )
      
      # Join this with the existing data
      data_filled <- all_combinations %>%
        left_join(data, by = c("year", "intervention", "discipline", "outcome_measures")) %>%
       replace_na(list(yearly_count = 0))  # Replace NA in yearly_count with 0
      
      # Recalculate the cumulative counts
      data_filled <- data_filled %>%
        arrange(intervention, discipline, outcome_measures, year) %>%
        group_by(intervention, discipline, outcome_measures) %>%
        mutate(cumulative_count = cumsum(yearly_count)) %>% 
        ungroup() %>% 
        mutate(cumulative_count = as.numeric(cumulative_count))
    
      sizeref_value <- (max(data_filled$cumulative_count) / 4)
      #browser()

      tryCatch({
      p <- plot_ly(data_filled,
                   x = ~outcome_measures, y = ~intervention, 
                  size = ~cumulative_count,
                   #colors = ~sort(unique(col)), 
                   color = ~discipline, 
                   #customdata = ~key,
                   type = 'scatter',
                   source = "B",
                   frame = ~year,
                   height = 750,
                   marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.8,
                                 line = list(color = '#FFFFFF'),
                                 legendgroup = ~discipline,
                                 sizeref = sizeref_value
               
                                 
                                 ),
                   hoverinfo = 'text',
                   textposition = "none",
                   text = ~paste(" Intervention:", intervention,"<br>",
                                 "Outcome:", outcome_measures,"<br>",
                                 "Discipline:", discipline,"<br>",
                                 "Number of Studies:", cumulative_count)
                   ) %>%
        layout(yaxis = list(title = list(text = "Intervention", standoff = 25)
                            #autotypenumbers = 'strict'
                         ),
               xaxis = list(title = list(text = "", standoff = 25),
                            tickangle = -15,
                            ticklen = 1
                            ),
               hoverlabel = list(bgcolor = "white",
                                 font = list(size = 14)),
               showlegend = TRUE,
               clickmode = "event + select")
      
      return(p)
    }, error = function(e) {
      
      #browser()
      # Here you could log the error message or handle it differently
      output$error_message <- renderText({ "Error occurred: Please make another choice." })  # Inform the user to make another choice
      
      return(NULL)  # Return NULL to avoid further processing or showing an erroneous plot
    })
    
      } else { 

      data_for_bubble <- plot %>%
        filter(outcome_measures %in% input$select_outcome) %>% 
        filter(discipline %in% input$select_discipline_legend)
      
      citations_years <- citations_for_dl %>% 
        select(uid, year)
      
      
      data <- data_for_bubble %>%
        left_join(citations_years, by = "uid") %>% 
        group_by(uid, intervention, discipline, outcome_measures) %>%
        count() %>%
        ungroup() %>%
        count(intervention, discipline, outcome_measures)
      
      tryCatch({
        p <- plot_ly(data,
                     x = ~outcome_measures, y = ~intervention, size = ~n,
                     #colors = ~sort(unique(col)), 
                     color = ~discipline, 
                     #customdata = ~key,
                     type = 'scatter',
                     source = "B",
                     height = 750,
                     marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.8,
                                   line = list(color = '#FFFFFF'),
                                   legendgroup = ~discipline),
                     hoverinfo = 'text',
                     textposition = "none",
                     text = ~paste(" Intervention:", intervention,"<br>",
                                   "Outcome:", outcome_measures,"<br>",
                                   "Discipline:", discipline,"<br>",
                                   "Number of Studies:", n)) %>%
          layout(yaxis = list(title = list(text = "Intervention", standoff = 25)
                              #autotypenumbers = 'strict'
          ),
          xaxis = list(title = list(text = "Outcome Measures", standoff = 25),
                       tickangle = -20,
                       ticklen = 1
          ),
          hoverlabel = list(bgcolor = "white",
                            font = list(size = 14)),
          #height = 550,
          showlegend = TRUE,
          clickmode = "event + select")
        
        return(p)
      }, error = function(e) {
        # Here you could log the error message or handle it differently
#        output$error_message <- renderText({ "Error occurred: Please make another choice." })  # Inform the user to make another choice
        
        return(NULL)  # Return NULL to avoid further processing or showing an erroneous plot
      })
      
      }
  })
  
  
  output$int_ac_dis_table <- DT::renderDataTable({
    #browser()
    table <- table_react()
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
          
          list(width = '10%', targets = "_all")
        )
      )
      
    )
    
  })
  
  bar_react <- reactive({
  
    click_data <- event_data("plotly_click", priority = "event", source = "A")
    
  })
  
  observe({
    # You can replace these with actual reactive data based on your application's needs
    choices <- switch(input$legend_select,
                      "Intervention Provider" = sort(unique(dummy_data_for_bubble$intervention_provider)),
                      "Target Population" = sort(unique(dummy_data_for_bubble$target_population)),
                      "Discipline" = sort(unique(dummy_data_for_bubble$discipline)))
    
    # Update pickerInput based on the selected value in the first picker
    updatePickerInput(session, "legend_select_specific",
                      choices = choices,
                      selected = choices[1:3])
  })
  
  
  
  output$discipline_bar <- renderPlotly({
    
    # Interventions across disciplines bar ----
    #browser()
    click <- event_data("plotly_click", priority = "event", source = "A")
    
    #input$legend_select
    # This works, what if 2 of them have the same number on x-axis
    
    if (input$legend_select == "Discipline"){

      if(input$bar_switch_over_time){
      #browser()
      data_for_bubble <- dummy_data_for_bubble %>%
      filter(outcome_measures %in% input$select_outcome_bar) %>% 
      filter(discipline %in% input$legend_select_specific)
    
    citations_year <- citations_for_dl %>% 
      select(uid, year)
    
    
    data <- data_for_bubble %>%
      left_join(citations_year, by = "uid") %>%
      group_by(uid, year,  intervention, discipline) %>%
      count() %>%
      ungroup() %>%
      group_by(year, intervention, discipline) %>%
      count(name = "yearly_count") %>%
      ungroup() %>% 
      group_by(intervention, discipline) %>%
      mutate(cumulative_count = cumsum(yearly_count)) %>%
      ungroup()
    
    all_combinations <- expand.grid(
      year = min(data$year):max(data$year),
      intervention = unique(data$intervention),
      discipline = unique(data$discipline)
      #outcome_measures = unique(data_new$outcome_measures)
    )

    # Join this with the existing data
    data_filled <- all_combinations %>%
      left_join(data, by = c("year", "intervention", "discipline")) %>%
      replace_na(list(yearly_count = 0))  # Replace NA in yearly_count with 0

    # Recalculate the cumulative counts
    data_filled <- data_filled %>%
      arrange(intervention, discipline, year) %>%
      #outcome_measures, 
      group_by(intervention, discipline) %>%
      #outcome_measures
      mutate(cumulative_count = cumsum(yearly_count)) %>%
      ungroup()
    
   
    p <- plot_ly(data_filled,
                 x = ~cumulative_count, y = ~intervention,
                 type = 'bar',
                 source = "A",
                 color = ~discipline,
                 frame = ~year
                 #customdata = ~key,
                 # hoverinfo = 'text',
                 # text = ~paste(" Discipline:", discipline, "<br>",
                 #               "Intervention:", intervention, "<br>",
                 #               #"Outcome:", outcome_measures, "<br>",
                 #               "Number of Citations:", cumulative_count)
    ) %>%
      layout(
        yaxis = list(title = list(text = "Intervention", standoff = 25)),
        xaxis = list(title = list(text = "Number of Studies", standoff = 25),
                     dtick = 1,                       # Set the interval of the tick marks
                     tick0 = 1,
                     ticklen = 1,
                     range = c(0,max(data_filled$cumulative_count))
                     ),
        hoverlabel = list(bgcolor = "white",
                          font = list(size = 14)),
        height = 550,
        showlegend = TRUE,
        clickmode = "event + select")
      } else {
        
        data_for_bubble <- dummy_data_for_bubble %>%
          filter(outcome_measures %in% input$select_outcome_bar) %>% 
          filter(discipline %in% input$legend_select_specific)
        
        data <- data_for_bubble %>%
          group_by(uid, intervention, discipline) %>%
          count() %>%
          ungroup() %>%
          count(intervention, discipline)
        
        p <- plot_ly(data,
                     x = ~n, y = ~intervention,
                     type = 'bar',
                     #source = "A",
                     color = ~discipline,
                     #frame = ~year,
                     #customdata = ~key,
                     hoverinfo = 'text',
                     text = ~paste(" Discipline:", discipline, "<br>",
                                   "Intervention:", intervention, "<br>",
                                   #"Outcome:", outcome_measures, "<br>",
                                   "Number of Citations:", n)
        ) %>%
          layout(
            yaxis = list(title = list(text = "Intervention", standoff = 25)),
            xaxis = list(title = list(text = "Number of Studies", standoff = 25),
                         dtick = 1,                       # Set the interval of the tick marks
                         tick0 = 1,
                         ticklen = 1),
            hoverlabel = list(bgcolor = "white",
                              font = list(size = 14)),
            height = 550,
            showlegend = TRUE,
            clickmode = "event + select")
        
        
        
        
      }
    } else if (input$legend_select == "Intervention Provider"){
      
      if(input$bar_switch_over_time){
        #browser()
        data_for_bubble <- dummy_data_for_bubble %>%
          filter(outcome_measures %in% input$select_outcome_bar) %>% 
          filter(intervention_provider %in% input$legend_select_specific)
        
        citations_year <- citations_for_dl %>% 
          select(uid, year)
        
        data <- data_for_bubble %>%
          left_join(citations_year, by = "uid") %>%
          group_by(uid, year,  intervention, intervention_provider) %>%
          count() %>%
          ungroup() %>%
          group_by(year, intervention, intervention_provider) %>%
          count(name = "yearly_count") %>%
          ungroup() %>% 
          group_by(intervention, intervention_provider) %>%
          mutate(cumulative_count = cumsum(yearly_count)) %>%
          ungroup()
        
        all_combinations <- expand.grid(
          year = min(data$year):max(data$year),
          intervention = unique(data$intervention),
          intervention_provider = unique(data$intervention_provider)
        )
        
        # Join this with the existing data
        data_filled <- all_combinations %>%
          left_join(data, by = c("year", "intervention", "intervention_provider")) %>%
          replace_na(list(yearly_count = 0))  # Replace NA in yearly_count with 0
        
        # Recalculate the cumulative counts
        data_filled <- data_filled %>%
          arrange(intervention, intervention_provider, year) %>%
          group_by(intervention, intervention_provider) %>%
          mutate(cumulative_count = cumsum(yearly_count)) %>%
          ungroup()
        
        
        p <- plot_ly(data_filled,
                     x = ~cumulative_count, y = ~intervention,
                     type = 'bar',
                     source = "A",
                     color = ~intervention_provider,
                     frame = ~year
                     #customdata = ~key,
                     # hoverinfo = 'text',
                     # text = ~paste(" intervention_provider:", intervention_provider, "<br>",
                     #               "Intervention:", intervention, "<br>",
                     #               #"Outcome:", outcome_measures, "<br>",
                     #               "Number of Citations:", cumulative_count)
        ) %>%
          layout(
            yaxis = list(title = list(text = "Intervention", standoff = 25)),
            xaxis = list(title = list(text = "Number of Studies", standoff = 25),
                         dtick = 1,                       # Set the interval of the tick marks
                         tick0 = 1,
                         ticklen = 1,
                         range = c(0,max(data_filled$cumulative_count))
            ),
            hoverlabel = list(bgcolor = "white",
                              font = list(size = 14)),
            height = 550,
            showlegend = TRUE,
            clickmode = "event + select")
      } else {
      
      data_for_bubble <- dummy_data_for_bubble %>%
        filter(outcome_measures %in% input$select_outcome_bar) %>% 
        filter(intervention_provider %in% input$legend_select_specific)
      
      
      data <- data_for_bubble %>%
        group_by(uid, intervention, intervention_provider) %>%
        count() %>%
        ungroup() %>%
        count(intervention, intervention_provider)
      
      p <- plot_ly(data,
                   x = ~n, y = ~intervention,
                   type = 'bar',
                   source = "B",
                   color = ~intervention_provider,
                   #customdata = ~key,
                   hoverinfo = 'text',
                   text = ~paste("Intervention Provider:", intervention_provider, "<br>",
                                 "Intervention:", intervention, "<br>",
                                 #"Outcome:", outcome_measures, "<br>",
                                 "Number of Citations:", n)
      ) %>%
        layout(
          yaxis = list(title = list(text = "Intervention", standoff = 25)),
          xaxis = list(title = list(text = "Number of Studies", standoff = 25),
                       tickangle = -20,
                       ticklen = 1),
          hoverlabel = list(bgcolor = "white",
                            font = list(size = 14)),
          height = 550,
          showlegend = TRUE,
          clickmode = "event + select")
      
      }
    }else if (input$legend_select == "Target Population"){
      
      if(input$bar_switch_over_time){
        #browser()
        data_for_bubble <- dummy_data_for_bubble %>%
          filter(outcome_measures %in% input$select_outcome_bar) %>% 
          filter(target_population %in% input$legend_select_specific)
        
        citations_year <- citations_for_dl %>% 
          select(uid, year)
        
        data <- data_for_bubble %>%
          left_join(citations_year, by = "uid") %>%
          group_by(uid, year,  intervention, target_population) %>%
          count() %>%
          ungroup() %>%
          group_by(year, intervention, target_population) %>%
          count(name = "yearly_count") %>%
          ungroup() %>% 
          group_by(intervention, target_population) %>%
          mutate(cumulative_count = cumsum(yearly_count)) %>%
          ungroup()
        
        all_combinations <- expand.grid(
          year = min(data$year):max(data$year),
          intervention = unique(data$intervention),
          target_population = unique(data$target_population)
        )
        
        # Join this with the existing data
        data_filled <- all_combinations %>%
          left_join(data, by = c("year", "intervention", "target_population")) %>%
          replace_na(list(yearly_count = 0))  # Replace NA in yearly_count with 0
        
        # Recalculate the cumulative counts
        data_filled <- data_filled %>%
          arrange(intervention, target_population, year) %>%
          group_by(intervention, target_population) %>%
          mutate(cumulative_count = cumsum(yearly_count)) %>%
          ungroup()
        
        
        p <- plot_ly(data_filled,
                     x = ~cumulative_count, y = ~intervention,
                     type = 'bar',
                     source = "A",
                     color = ~target_population,
                     frame = ~year
                     #customdata = ~key,
                     # hoverinfo = 'text',
                     # text = ~paste(" intervention_provider:", intervention_provider, "<br>",
                     #               "Intervention:", intervention, "<br>",
                     #               #"Outcome:", outcome_measures, "<br>",
                     #               "Number of Citations:", cumulative_count)
        ) %>%
          layout(
            yaxis = list(title = list(text = "Intervention", standoff = 25)),
            xaxis = list(title = list(text = "Number of Studies", standoff = 25),
                         dtick = 1,                       # Set the interval of the tick marks
                         tick0 = 1,
                         ticklen = 1,
                         range = c(0,max(data_filled$cumulative_count))
            ),
            hoverlabel = list(bgcolor = "white",
                              font = list(size = 14)),
            height = 550,
            showlegend = TRUE,
            clickmode = "event + select")
        
      } else {
      
      data_for_bubble <- dummy_data_for_bubble %>%
        filter(outcome_measures %in% input$select_outcome_bar) %>% 
        filter(target_population %in% input$legend_select_specific)
      
      data <- data_for_bubble %>%
        group_by(uid, intervention, target_population) %>%
        count() %>%
        ungroup() %>%
        count(intervention, target_population)
      
      p <- plot_ly(data,
                   x = ~n, y = ~intervention,
                   type = 'bar',
                   source = "B",
                   color = ~target_population,
                   #customdata = ~key,
                   hoverinfo = 'text',
                   text = ~paste("Target Population:", target_population, "<br>",
                                 "Intervention:", intervention, "<br>",
                                 #"Outcome:", outcome_measures, "<br>",
                                 "Number of Citations:", n)
      ) %>%
        layout(
          yaxis = list(title = list(text = "Intervention", standoff = 25)),
          xaxis = list(title = list(text = "Number of Studies", standoff = 25),
                       tickangle = -20,
                       ticklen = 1),
          hoverlabel = list(bgcolor = "white",
                            font = list(size = 14)),
          height = 550,
          showlegend = TRUE,
          clickmode = "event + select")
      
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)                     

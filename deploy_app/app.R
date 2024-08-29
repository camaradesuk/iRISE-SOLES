library(sf)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(viridis)
library(viridisLite)
library(shiny)
library(shinyalert)
library(shinyhelper)
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
library(dplyr)
library(rmapshaper)

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
  pico_element_1 = list(id = "dropdown_interventions",
                        table = interventions_df,
                        label1 = "Filter by intervention:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_2 = list(id = "dropdown_provider",
                        table = intervention_provider_df,
                        label1 = "Filter by intervention provider:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_3 = list(id = "dropdown_discipline",
                        table = discipline_df,
                        label1 = "Filter by discipline:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_4 = list(id = "dropdown_outcomes",
                        table = outcome_measures_df,
                        label1 = "Filter by outcome:",
                        column1 = "name",
                        filter_no = 1)
)

ui <- bs4DashPage(freshTheme = mytheme,
                  
                  dark = NULL,
                  help = NULL,
                  dbHeader <- dashboardHeader(
                    #title = HTML("iRISE-<br>SOLES"),
                    title = tags$h5("iRISE-SOLES", style = "color: white; text-align: center;padding-top: 10px;"),
                    
                    tags$a(href= 'https://irise-project.eu/',
                           tags$img(src= "iRISE-lightlogo.png",
                                    height = "50px"))
                  ),
                  dashboardSidebar(skin = "dark",
                                   collapsed = TRUE,
                                   sidebarMenu(
                                     bs4SidebarMenuItem(tags$p("Homepage", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "home", icon = icon("home")),
                                     bs4SidebarMenuItem(tags$p("Data Collection", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "dc-main", icon = icon("database", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Methodology", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "workflow-accordion-dc", icon = icon("question", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "data-summary-transparency", icon = icon("chart-pie", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Evidence Map", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "evidence_map_bubble", icon = icon("diagram-project", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Outcome Overview", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "outcome-overview-tab", icon = icon("file-code", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Funder", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "funder-tab", icon = icon("landmark", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Location", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "location-tab", icon = icon("earth-americas")),
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
                                                  We tag studies by discipline, intervention, intervention provider, institution location, and reproducibility relevant outcomes. We also assess the transparency metrics of studies witin iRISE-SOLES e.g. their open access status and presence of data/code sharing.")),
                              
                              fluidRow(
                                
                                column(4,
                                       
                                       box(height = 350,
                                           div(
                                             style = "text-align: center;",
                                             tags$a(
                                               href = 'https://osf.io/9hzcv/?view_only=d74ff8089864468cb43daa06733e0be6',
                                               tags$img(src = "osf_logo.png", height = "200px")
                                             )
                                           ),
                                           
                                           tags$br(),
                                           div(
                                             style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                             "Read our iRISE-SOLES protocol on the Open Science Framework"),
                                           background = "warning",
                                           width = NULL,
                                           solidHeader = TRUE,
                                           title = "",
                                           status = "warning")),
                                
                                column(4,
                                       box(height = 350,
                                           div(
                                             style = "text-align: center;",
                                             tags$a(
                                               href = 'https://portlandpress.com/clinsci/article/137/10/773/233083/Systematic-online-living-evidence-summaries',
                                               tags$img(src = "paper_screenshot.PNG", height = "200px")
                                             )
                                           ),
                                           
                                           tags$br(),
                                           div(
                                             style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                                             "Read our SOLES paper to learn more about our workflow"),
                                           background = "secondary",
                                           width = NULL,
                                           solidHeader = TRUE,
                                           title = "",
                                           status = "secondary")),
                                
                                column(4,
                                       box(height = 350,
                                           div(
                                             style = "text-align: center;",
                                             tags$a(
                                               href = 'https://irise-project.eu/',
                                               tags$img(src = "irise_website.png", height = "200px")
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
                      
                      # Data collection - ui -----
                      tabItem(tabName = "dc-main",
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
                      
                      # Methodology - ui ----
                      tabItem(tabName = "workflow-accordion-dc",
                              
                              uiOutput("workflow")
                      ),
                      
                      
                      # Transparency info - ui -----
                      tabItem(tabName = "data-summary-transparency",
                              
                              bs4Jumbotron(
                                title = tags$h1("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                lead = tags$p("This summary shows the overall percentages of publications
                across different transparency measures, including open access publication, open data, and open code. You can
                also benchmark improvements by viewing the number of publications in each category over time.", style = ";font-family: Kohinoor Bangla, sans-serif !important;"),
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
                      
                      
                      # Evidence map - ui ----
                      tabItem(tabName = "evidence_map_bubble",
                              
                              box(
                                title="Evidence map",
                                width= 12,
                                collapsable = FALSE,
                                closable=FALSE,
                                sidebar = boxSidebar(
                                  id = "int_ac_dis_sidebar",
                                  icon = icon("info-circle"),
                                  tags$div(
                                    style = "padding: 10px;",
                                    tags$h4("Guidance for Evidence Map"),
                                    tags$p("Use the map below to visualize evidence on interventions to improve different types of reproducibility and related outcomes. Click a bubble to see all the relevant evidence in the table below."),
                                    tags$p("You can select multiple outcome measures and subgroups to filter the data. The bubbles represent the number of studies, with larger bubbles indicating more studies.")
                                  )),
                                height = 900,
                                solidHeader = TRUE,
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
                                tags$p("Use the map below to visualise evidence on interventions to improve different types of reproducibility and related outcomes. Click a bubble to see all of the relevant evidence in the table below.", style = "color: black !important;font-family: KohinoorBangla, sans-serif !important;"),
                                fluidRow(column(width = 4,
                                                pickerInput(
                                                  inputId = "select_outcome",
                                                  label = tags$p("Select one or more reproducibility measures", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                                  choices = sort(unique(all_annotations$outcome_measures[!all_annotations$outcome_measures %in% c("Unknown", "Unspecified")])),
                                                  selected = c("Computational reproducibility"),
                                                  multiple = TRUE,
                                                  options = pickerOptions(noneSelectedText = "Please Select",
                                                                          virtualScroll = 100,
                                                                          actionsBox = TRUE,
                                                                          size = 10
                                                  )
                                                )
                                ),
                                
                                column(width = 4,
                                       pickerInput(
                                         inputId = "legend_bubble_select",
                                         label = tags$p("Select a subgroup", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                         choices = c("Discipline"="discipline", "Intervention provider"="intervention_provider", "Target population"="target_population"),
                                         selected = c("discipline"),
                                         multiple = FALSE,
                                         options = pickerOptions(noneSelectedText = "Please Select",
                                                                 virtualScroll = 100,
                                                                 actionsBox = TRUE,
                                                                 size = 10
                                         )
                                       )
                                ),
                                column(width = 4,
                                       pickerInput(
                                         inputId = "legend_bubble_specific",
                                         label = tags$p("Filter subgroup options", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                         choices = list(),
                                         selected = ,
                                         multiple = TRUE,
                                         options = pickerOptions(noneSelectedText = "Please Select",
                                                                 virtualScroll = 100,
                                                                 actionsBox = TRUE,
                                                                 size = 10
                                         )
                                       )
                                )
                                ),
                                
                                
                                verbatimTextOutput("error_message"),
                                plotlyOutput("evidence_map_plot") %>% withSpinner(color="#96c296"),
                                tags$br(),
                                tags$br()
                                
                                
                              ),
                              
                              box(
                                title = "Selected studies",
                                solidHeader = TRUE,
                                width = 12,
                                id = "bubble_evidence_map",
                                status = "secondary",
                                
                                download_table_UI("dl_evidence_map"),
                                
                                
                                
                                DT::dataTableOutput("int_ac_dis_table") %>% withSpinner(color="#96c296")
                                
                                
                              )),
                      
                      
                      tabItem(tabName = "funder-tab",
                              
                              box(width = 12,
                                  height = "600px",
                                  title = "Publications funded by year",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  sidebar = boxSidebar(
                                    id = "funder-tab-sidebar",
                                    icon = icon("info-circle"),
                                    tags$div(
                                      style = "padding: 10px;",
                                      tags$h4("Guidance for Funder Data"),
                                      tags$p("For each article indexed, we have obtained funding information from Open Alex where possible."),
                                      tags$p("This dashboard summarises the number of research studies funded over time by different funders and
                                             indicates the top reproducibility interventions funders have invested in.
                                             We also provide the % of funded studies that are open access, and provide open code or data, stratified
                                             by different funders")
                                    )),
                                  
                                  fluidRow(column(6,
                                                  pickerInput(inputId = "funder_select",
                                                              
                                                              label = tags$p("Select a funder", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                                              choices = unique(sort(funder_overall_count$funder_name)),
                                                              selected = c("National Institutes of Health"),
                                                              multiple = FALSE,
                                                              options = pickerOptions(noneSelectedText = "Please Select",
                                                                                      virtualScroll = 100,
                                                                                      actionsBox = TRUE,
                                                                                      size = 10))),
                                           
                                           
                                           column(6,
                                                  valueBoxOutput("funding_summary", width = 12)
                                           )),
                                  
                                  
                                  
                                  
                                  plotlyOutput("funder_year_plot") %>% withSpinner(color="#96c296")
                                  
                              ),
                              fluidRow(
                                
                                valueBoxOutput("oa_box"),
                                valueBoxOutput("od_box"),
                                valueBoxOutput("oc_box")
                                
                              ),
                              
                              box(width = 12,
                                  height = "600px",
                                  title = "Interventions",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  
                                  fluidRow(
                                    column(6,
                                           pickerInput(inputId = "funder_intervention_select",
                                                       label = tags$p("Select an intervention", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                                       choices = NULL,
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       
                                                       options = pickerOptions(noneSelectedText = "Please Select",
                                                                               virtualScroll = 100,
                                                                               actionsBox = TRUE,
                                                                               size = 10)),
                                    ),
                                    column(6,
                                           valueBoxOutput("funding_category_box", width = 12)
                                    )
                                  ),
                                  plotlyOutput("funder_category_bar")
                              ),
                              
                              uiOutput("data_table_box")
                      ),
                      
                      tabItem(tabName = "outcome-overview-tab",
                              box(
                                title = "Publications targetting reproducibility outcomes",
                                status = "primary",
                                solidHeader = TRUE,
                                height = "650px",
                                sidebar = boxSidebar(
                                  id = "outcome-overview-tab-sidebar",
                                  icon = icon("info-circle"),
                                  tags$div(
                                    style = "padding: 10px;",
                                    tags$h4("Guidance for Outcome Overview"),
                                    tags$p("This dashboard summarises the number of research studies targetting particular aspects of reproducibility / reproducibility proxies,
                                             stratified by intervention provider (the one who implements or provides the intervention) and by discipline. First, select
                                             a provider that is most relevant, then select the reproducibility measure you are interested in targetting with an intervention. You can
                                             then see the number of studies over time and the number of different interventions that have been evaluated to improve that reproducibility
                                             outcome. Select an additional reproducibility outcome if you want to understand the trends over time. Scroll down to see a list of relevant
                                             articles with relevant evidence related to your selected outcome measure" )
                                  )),
                                width=12,
                                
                                fluidRow(
                                  column(2,
                                         pickerInput(
                                           inputId = "provider_select",
                                           label = tags$p("Intervention Provider", style = "color: #47B1A3; font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                           choices = sort(unique(all_annotations$intervention_provider[!all_annotations$intervention_provider %in% c("Unknown", "Unspecified")])),
                                           selected = c("Researchers / Researcher Collaboration"),
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             noneSelectedText = "Please Select",
                                             virtualScroll = 100,
                                             actionsBox = TRUE,
                                             size = 10
                                           ))
                                  ),
                                  
                                  column(2,
                                         pickerInput(
                                           inputId = "outcome_select",
                                           label = tags$p("Outcome measure", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                           choices = NULL,
                                           selected = NULL,
                                           multiple = FALSE,
                                           options = pickerOptions(
                                             noneSelectedttText = "Please Select",
                                             virtualScroll = 100,
                                             actionsBox = TRUE,
                                             size = 10
                                           )
                                         )),
                                  
                                  column(3,
                                         pickerInput(
                                           inputId = "outcome_comparison_select",
                                           label = tags$p("Outcome(s) for comparison", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                           choices = NULL,
                                           selected = NULL,
                                           multiple = TRUE,
                                           options = pickerOptions(
                                             noneSelectedText = "Please Select",
                                             virtualScroll = 100,
                                             actionsBox = TRUE,
                                             size = 10
                                           ))),
                                  
                                  column(5,
                                         valueBoxOutput("top_int_five_years", width=NULL)
                                         # valueBoxOutput("top_int_five_no"),
                                         # valueBoxOutput("top_disc")
                                  )),
                                fluidRow(column(12,
                                                plotlyOutput("outcome_year_plot", width = "100%", height="450px") %>% withSpinner(color="#96c296")))),
                              uiOutput("dynamic_box"),
                              
                              
                              uiOutput("data_table_box_outcome")),
                      
                      tabItem(tabName = "location-tab",
                              
                              
                              box(
                                
                                
                                width = 12,
                                title = "Institution Location",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsable = FALSE,
                                closable=FALSE,
                                
                                sidebar = boxSidebar(
                                  width = 30,
                                  background = "#64C296",
                                  id = "inst_loc_sidebar",
                                  icon = icon("filter"),
                                  
                                  fluidRow(
                                    column(width = 11,
                                           
                                           tags$div(
                                             style = "padding: 0px;",
                                             selectizeInput(inputId = "country_select",
                                                            label = tags$p("Select a Country", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                                            choices = sort(unique(ror_data$country)),
                                                            selected = NULL,
                                                            multiple = TRUE,
                                                            options = list(
                                                              placeholder = "Please select one or more countries")
                                             ),
                                             pickerInput(
                                               inputId = "continent_select",
                                               label = tags$p("Select a Continent", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                               choices = sort(unique(ror_data$continent)),
                                               selected = sort(unique(ror_data$continent)),
                                               multiple = TRUE,
                                               options = pickerOptions(
                                                 noneSelectedText = "Please Select",
                                                 virtualScroll = 100,
                                                 actionsBox = TRUE,
                                                 size = 10
                                               )
                                             ),
                                             pickerInput(
                                               inputId = "inst_outcome_select",
                                               label = tags$p("Select an Outcome", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                               choices = sort(unique(ror_data$outcome_measures[!ror_data$outcome_measures %in% c("Unspecified")])),
                                               selected = sort(unique(ror_data$outcome_measures[!ror_data$outcome_measures %in% c("Unspecified")])),
                                               multiple = TRUE,
                                               options = pickerOptions(
                                                 noneSelectedText = "Please Select",
                                                 virtualScroll = 100,
                                                 actionsBox = TRUE,
                                                 size = 10
                                               )
                                             ),
                                             pickerInput(
                                               inputId = "inst_discipline_select",
                                               label = tags$p("Select a Discipline", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                               choices = sort(unique(ror_data$discipline)),
                                               selected = sort(unique(ror_data$discipline)),
                                               multiple = TRUE,
                                               options = pickerOptions(
                                                 noneSelectedText = "Please Select",
                                                 virtualScroll = 100,
                                                 actionsBox = TRUE,
                                                 size = 10
                                               )
                                             ),
                                             pickerInput(
                                               inputId = "inst_type_select",
                                               label = tags$p("Select Institution Type", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                               choices = sort(unique(ror_data$type)),
                                               selected = sort(unique(ror_data$type)),
                                               multiple = TRUE,
                                               options = pickerOptions(
                                                 noneSelectedText = "Please Select",
                                                 virtualScroll = 100,
                                                 actionsBox = TRUE,
                                                 size = 10
                                               )
                                             )
                                           )))),
                                fluidRow(
                                  column(width = 12,
                                         leafletOutput("institution_map", height = 500) %>% withSpinner(color="#96c296") ),
                                  
                                )
                                
                              ),
                              box(
                                title = "Selected Studies",
                                status = "danger",
                                solidHeader = TRUE,
                                width = 12,
                                tags$head(
                                  tags$style(HTML("
      .dataTables_wrapper .dataTable a {
        color: #1A465F;
        text-decoration: underline;
      }
    "))
                                ),
                                download_table_UI("dl_location"),
                                DT::dataTableOutput("location_table") %>% withSpinner(color="#96c296")
                              )
                      ),
                      
                      # Search - ui -----
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
                                    tags$div(
                                      style = "text-align: center;",  # This applies the center alignment to the content
                                      tags$img(src = "european_union_logo.jpg", height = "50px")
                                    )),
                                
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
                                      strong(tags$a(href="mailto:kaitlyn@ed.ac.uk", "kaitlyn.hair@ed.ac.uk", style="color: #1A465F;")))))
                      )
                    ))
)





server <- function(input, output, session) {
  
  observe_helpers(help_dir = "helpfiles")
  
  output$workflow <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "workflow.html",
      width = "100%",
      height = 800
    )
  })
  
  
  shinyalert("Welcome", "Welcome to the draft iRISE-SOLES Dashboard!
             Please note this app is still under development. The data presented are not representative and should not be used for any research purposes yet.", type = "info")
  
  
  yearBarServer_included_only("included_studies_over_time_bar",
                              table=n_included_per_year_plot_data,
                              column="is_included",
                              colour = "#89CB93")
  
  # Transparency - bar plots -----
  yearBarServer("oa_pubs_per_year", table=oa_tag, column="is_oa", display=TRUE, order=c(TRUE, FALSE), text="Source:CrossRef", colours = c("#89CB93", "grey")) %>%
    bindCache(nrow(transparency))
  yearBarServer("oa_pub_type_per_year", table=oa_tag, column="oa_status", display=c("closed", "hybrid", "bronze", "gold", "green"), order=c("closed", "hybrid", "bronze", "gold", "green"),
                text="Source:CrossRef", colours = c("red", "lightblue", "orange", "gold", "green")) %>% bindCache(nrow(transparency))
  yearBarServer("open_data_pubs_per_year", table=transparency, column="is_open_data", display=TRUE, order=c(TRUE, FALSE),  text="Tool: OddPub, Riedel, N, et al. (2020), DOI:10.5334/dsj-2020-042", colours = c("#89CB93", "grey")) %>% bindCache(nrow(transparency))
  yearBarServer("open_code_pubs_per_year", table=transparency, column="is_open_code", display=TRUE, order=c(TRUE, FALSE),  text="Tool: OddPub, Riedel, N, et al. (2020), DOI:10.5334/dsj-2020-042", colours = c("#89CB93", "grey")) %>% bindCache(nrow(transparency))
  
  # Search Page - server -----
  search_Server("search_results",
                pico_data = pico_elements_list,
                table = included_with_metadata,
                combined_pico_table = pico,
                citations_for_download = citations_for_dl,
                project_name = "iRISE-SOLES")
  
  
  download_table_Server("dl_evidence_map", table = dl_evidence_map)
  
  download_table_Server("dl_funder_data", table = dl_funder)
  
  download_table_Server("dl_outcome_overview", table = dl_outcome_overview)
  
  download_table_Server("dl_location", table = dl_location)
  
  
  
  # Evidence Map - server -----
  observe({
    choices <- switch(input$legend_bubble_select,
                      "intervention_provider" = sort(unique(all_annotations$intervention_provider[!all_annotations$intervention_provider %in% c("Unknown", "Unspecified")])),
                      "target_population" = sort(unique(all_annotations$target_population[!all_annotations$target_population %in% c("Unknown", "Unspecified")])),
                      "discipline" = sort(unique(all_annotations$discipline[!all_annotations$discipline %in% c("Unknown", "Unspecified")])))
    
    updatePickerInput(session, "legend_bubble_specific",
                      choices = choices,
                      selected = choices)
  })
  
  previous_state <- reactiveValues(
    column1 = NULL,
    column2 = NULL
  )
  
  bubble_react <- reactive({
    
    req(input$select_outcome, input$legend_bubble_specific)
    
    data_filter <- all_annotations %>%
      filter(outcome_measures %in% input$select_outcome) %>%
      filter(!!sym(input$legend_bubble_select) %in% input$legend_bubble_specific)
    
    
    citations_years <- citations_for_dl %>%
      select(uid, year)
    
    data <- data_filter %>%
      left_join(citations_years, by = "uid") %>%
      group_by(uid, intervention, !!sym(input$legend_bubble_select), outcome_measures) %>%
      count() %>%
      ungroup() %>%
      count(intervention, !!sym(input$legend_bubble_select), outcome_measures) %>%
      arrange(!!sym(input$legend_bubble_select), outcome_measures, intervention)
    
    data$key <- row.names(data)
    data$col <- "#266080"
    
    click_data <- event_data("plotly_click", priority = "event", source = "B")
    
    if (!is.null(click_data)) {
      
      bubble_react_new <- data %>%
        mutate(selected_colour = key %in% click_data$customdata)
      
      bubble_react_new$selected_colour <- data$key %in% click_data$customdata
      
      if (exists("col_vector")){
        
        bubble_react_new$col <- col_vector
      }
      
      selected_row <- which(rownames(bubble_react_new) %in% click_data$customdata)
      
      if (!bubble_react_new$col[selected_row] == "#47B1A3"){
        
        bubble_react_new$col[selected_row] <- "#47B1A3"
        
        assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
        
      } else{
        
        bubble_react_new$col[selected_row] <- "#266080"
        
        assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
        
      }
      
      
    }
    else {
      
      data$col <- "#266080"
      
      bubble_react_new <- data %>%
        mutate(selected_colour = FALSE)
      
      assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
      
    }
    
    return(bubble_react_new)
  })
  
  
  
  table_react <- reactive({
    
    table <- all_annotations %>%
      filter(outcome_measures %in% input$select_outcome) %>% 
      filter(!!sym(input$legend_bubble_select) %in% input$legend_bubble_specific)
    
    bubble_data <- bubble_react()
    
    
    if ("selected_colour" %in% names(bubble_data)) {
      table_filter <- bubble_data %>%
        filter(selected_colour == TRUE)
    } else {
      # If no such column exists, you can opt to leave the data as is or take other actions
      table_filter <- bubble_data
      message("Column 'selected_colour' does not exist. No rows filtered.")
    }
    
    table_new <- table %>%
      filter(intervention %in% table_filter$intervention,
             !!sym(input$legend_bubble_select) %in% table_filter[[input$legend_bubble_select]],
             outcome_measures %in% table_filter$outcome_measures)
    
    final_table <- all_annotations %>%
      filter(uid %in% table_new$uid) %>%
      filter(intervention %in% table_filter$intervention,
             !!sym(input$legend_bubble_select) %in% table_filter[[input$legend_bubble_select]],
             outcome_measures %in% table_filter$outcome_measures)
   
    final_table <- all_annotations_small %>%
      filter(uid %in% final_table$uid) %>%
      left_join(citations_for_dl, by = "uid") %>%
      select(uid, year, author, title, discipline, intervention, outcome_measures, research_stage, doi, url) %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      arrange(desc(year))
    
    if (nrow(final_table) > 0 ){
      
      final_table$title <- paste0("<a href='",final_table$link, "' target='_blank'>",final_table$title,"</a>")
      
    }
    
    final_table <- final_table %>%
      select(-doi, -url, -link) %>%
      select(uid, Year = year, Author = author, Title = title,
             Discipline = discipline,
             Intervention = intervention,
             "Outcome Measures" = outcome_measures,
             "Research Stage" = research_stage
      )
    
    
    return(final_table)
  })
  
  plot_data <- reactive({
    tryCatch({
      # Assuming fetch_data() might throw an error
      bubble_react()
    }, error = function(e) {
      NULL  # Indicative of an error
    })
  })
  
  output$error_message <- renderText({
    if (is.null(plot_data())) {
      "Please double-click to reset plot!"
    } else {
      ""
    }
  })
  
  # Evidence map - plot -----
  output$evidence_map_plot <- renderPlotly({
    
    tryCatch({
      
      subcat_count <- plot_data() %>%
        ungroup() %>%
        distinct(!!sym(input$legend_bubble_select)) %>%
        nrow()
      
      plot <- plot_data() %>%
        ungroup() %>%
        mutate(numeric_outcome = as.numeric(factor(outcome_measures))) %>%
        mutate(index = as.integer(factor(!!sym(input$legend_bubble_select)))) %>%
        group_by(intervention, outcome_measures) %>%
        mutate(
          jitter_base = ifelse(subcat_count > 1, 0.3 / (subcat_count - 1), 0),
          jittered_outcome = numeric_outcome + (index - (subcat_count + 1) / 2) * jitter_base) %>%
        ungroup() %>%
        mutate(shape = ifelse(selected_colour == TRUE, "circle-cross-open", "circle"))
      
      # Calculate midpoints for line positions
      unique_outcomes <- sort(unique(plot$numeric_outcome))
      line_positions <- head(unique_outcomes, -1) + diff(unique_outcomes) / 2
      
      max_n <- max(plot$n, na.rm = TRUE)
      sizeref_value <- 1 * (max_n/300)
      #sizeref_value <- 1 * (max_n/40)
      
      
      irise_colours <- c(
        dark_blue = "#1A465F",
        dot_text_green = "#64C296",
        coral = "#FF7F50",
        gold = "#FFD700",
        rose_quartz = "#A799B7",
        indian_red = "#D05353",
        slate_grey = "#708090",
        melon = "#DAA49A",
        coyote = "#735F3D"
      )
      
      # Generate a named color map based on the input variable for coloring
      color_var <- plot[[input$legend_bubble_select]]
      unique_color_var <- unique(color_var)
      color_map <- setNames(irise_colours[1:length(unique_color_var)], unique_color_var)
      
      formatLegendText <- function(text) {
        text <- gsub("_", " ", text)      
        toTitleCase(text)                  
      }
      
      
      p <- plot_ly(plot,
                   x = ~jittered_outcome, y = ~intervention, size = ~n,
                   color = as.formula(paste0("~`", input$legend_bubble_select, "`")),
                   colors = color_map,
                   customdata = ~key,
                   type = 'scatter',
                   mode = 'markers',
                   source = "B",
                   height = 750,
                   fill = ~'',
                   marker = list(symbol = ~shape, sizemode = 'area',
                                 opacity = 0.8, sizeref = sizeref_value,
                                 line = list(color = '#FFFFFF', width = 1),
                                 legendgroup = ~as.formula(paste0("~`", input$legend_bubble_select, "`"))),
                   hoverinfo = 'text',
                   textposition = "none",
                   text = ~paste0(
                     "<br><b>Intervention:</b> ", intervention, 
                     "<br><b>Outcome:</b> ", outcome_measures, 
                     "<br><b>", formatLegendText(input$legend_bubble_select),"</b>: ", get(input$legend_bubble_select),
                     "<br><b>Number of Publications:</b> ", n
                   )) %>% 
        layout(yaxis = list(title = list(text = "Intervention", standoff = 25),
                            showgrid = TRUE
        ),
        xaxis = list(
          title = list(text = "", standoff = 25),
          ticklen = 4,
          tickvals = unique(plot$numeric_outcome),
          ticktext = unique(plot$outcome_measures),
          showgrid = FALSE,
          tickfont = list(
            size = 14,  # Increase the font size as desired
            color = "black",
            family = "Arial, bold"  # Specify bold here
          )
        ),
        hoverlabel = list(bgcolor = "white",
                          font = list(size = 14)),
        showlegend = TRUE,
        legend = list(title = list(text = formatLegendText(input$legend_bubble_select)),
                      bordercolor = 'black',
                      borderwidth = 2,
                      y = 0.95,
                      yanchor = 'top'),
        clickmode = "event + select",
        shapes =
          lapply(line_positions, function(pos) {
            list(
              type = "line",
              x0 = pos, y0 = 0,
              x1 = pos, y1 = 1,
              xref = 'x', yref = 'paper',  # Vertical lines along x
              line = list(color = 'grey', width = 1)
            )
          })
        
        )
      return(p)
    }, error = function(e) {
      
      return(NULL)  # Return NULL to avoid further processing or showing an erroneous plot
    })
    
    #}
  })
  
  # Evidence map - datatable -----
  output$int_ac_dis_table <- DT::renderDataTable({
    dl_evidence_map <<- table_react()
    
    DT::datatable(
      table_react()[,2:ncol(table_react())],
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
  
  
  # Funder - server -----
  output$funding_summary <- renderValueBox({
    
    df_count <- included_with_metadata %>%
      left_join(funder, by = "doi", multiple="all") %>%
      mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
      select(doi, cat) %>%
      distinct() %>%
      group_by(cat) %>%
      count() %>%
      ungroup()
    
    df_percentage <- df_count %>%
      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
      filter(cat == "Complete") %>%
      pull(percentage)
    
    percentage_complete <- paste0(df_percentage, " %")
    
    valueBox(
      width = 12,
      value = tags$h2(percentage_complete, style = "font-size: 200%; color: white;"),
      subtitle = tags$p("Included studies in iRISE-SOLES which currently have funding data", style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      icon = icon("university"),
      color = "primary"
    )
  })
  
  output$funding_category_box <- renderValueBox({
    
   
    df <- funder_metadata %>%
      filter(funder_name == input$funder_select) %>%
      select(uid, funder_name, intervention) %>%
      distinct() %>%
      filter(!intervention %in% c("Unknown", "Other")) %>% 
      group_by(intervention) %>%
      count() %>%
      ungroup() %>%
      filter(!is.na(intervention)) %>%
      arrange(desc(n)) %>%
      slice_head() %>%
      pull(intervention)
    
    
    valueBox(
      width = 12,
      value = tags$p(HTML(paste0("The top Intervention funded by <strong>", input$funder_select, "</strong> is")), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      subtitle = tags$h2(df, style = "font-size: 150%; color: white;"),
      icon = icon("university"),
      color = "primary",
      elevation = 2
    )
  })
  
  funder_filtered <- reactive({
    funder_transparency %>%
      filter(funder_name == input$funder_select)
  })
  
  # Funder - transparency boxes -----
  calculate_transparency_percent <- function(column) {
    
    if (length(column) > 0) {
      
      true_values <- na.omit(column)
      num_true = sum(true_values)
      
      total_non_na = length(true_values)
      percentage_true = round((num_true / total_non_na) * 100, 1)
      
      paste0(percentage_true, " %")
    } else {
      "0%"
    }
  }
  
  oa_percentage <- reactive({
    data <- funder_filtered()
    
    calculate_transparency_percent(data$is_oa)
  })
  
  
  output$oa_box <- renderValueBox({
    valueBox(
      width = 4,
      subtitle = tags$h2("Open Access", style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "success",
      value = tags$p(oa_percentage(), style = "font-size: 300%; color: white;"),
      icon = icon("lock"),
      elevation = 2
    )
  })
  
  
  od_percentage <- reactive({
    data <- funder_filtered()
    calculate_transparency_percent(data$is_open_data)
  })
  
  
  output$od_box <- renderValueBox({
    valueBox(
      width = 4,
      subtitle = tags$h2("Open Data", style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "info",
      value = tags$p(od_percentage(), style = "font-size: 300%; color: white;"),
      icon = icon("bar-chart"),
      elevation = 2
      
    )
  })
  
  oc_percentage <- reactive({
    data <- funder_filtered()
    
    calculate_transparency_percent(data$is_open_code)
  })
  
  
  output$oc_box <- renderValueBox({
    valueBox(
      width = 4,
      subtitle = tags$h2("Open Code", style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "secondary",
      value = tags$p(oc_percentage(), style = "font-size: 300%; color: white;"),
      icon = icon("code"),       elevation = 2
      
    )
  })
  
  
  # Funder - publications by year plot -----
  output$funder_year_plot <- renderPlotly({
    
    funder_year %>%
      filter(funder_name == input$funder_select) %>%
      plot_ly(x = ~year,
              type = 'scatter',
              mode = 'lines+markers',
              y = ~n,
              marker = list(color = "#266080", line = list(
                color = "#266080",
                width = 1
              )),
              hoverinfo = 'text',
              textposition = "none",
              text = ~paste(
                "<br><b>Funder:</b>", input$funder_select,
                "<br><b>Year:</b>", year,
                "<br><b>Number of Publications:</b>", n
              )
              
      ) %>%
      layout(showlegend = FALSE,
             yaxis = list(title = 'Number of publications', 
                          showgrid = TRUE, 
                          rangemode = "tozero"),
             xaxis = list(title = "",
                          tickangle = -45, ticklen = 4, showgrid = FALSE, tickmode = "linear",
                          tick0 = 0,          
                          dtick = 1),
             barmode='stack',
             annotations =
               list(x = 1, y = -0.2, text = "",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                    font=list(size=12, color="black")))
    
  })
  
  # Reactive expression to filter dataset based on the selected provider
  funder_picker <- reactive({
    funder_metadata %>%
      filter(funder_name == input$funder_select)
  })
  
  # Observe any changes in provider selection and update outcome fields
  observeEvent(input$funder_select, {
    
    #funder_interventions <- sort(unique(funder_picker()$intervention))
    funder_interventions <- sort(unique(funder_picker()$intervention[!funder_picker()$intervention %in% c("Unknown")]))
    
    # Update the intervention selections
    updatePickerInput(session, "funder_intervention_select", choices = funder_interventions, selected = funder_interventions)
    
    
  })
  
  # Funder - interventions plot
  output$funder_category_bar <- renderPlotly({
    
    funder_metadata_table <- funder_metadata %>%
      filter(funder_name == input$funder_select) %>%
      filter(intervention %in% input$funder_intervention_select) %>%
      filter(!intervention == "other") %>%
      select(uid, funder_name, intervention) %>%
      distinct() %>%
      group_by(intervention) %>%
      count() %>%
      ungroup() %>%
      arrange(n)
    
    
    plot <- plot_ly(data = funder_metadata_table, x = ~n, type = 'bar', orientation = 'h',
                    y = ~factor(intervention, levels = unique(intervention)),
                    marker = list(color = '#B1E0CB', line = list(color = 'black', width = 1)),
                    hoverinfo = 'text',
                    textposition = "none",
                    # text = ~paste(
                    #   "<br><b>Number of Publications:</b>", n,
                    #   "<br><b>Year:</b>", year)
                    text = ~paste0("<br><b>Funder: </b>", input$funder_select,
                                   "<br><b>Intervention: </b>", intervention,
                                   "<br><b>Number of Publications: </b>", n
                    ),
                    hoverlabel = list(bgcolor = "white",
                                      font = list(color = "black"))) %>%
      layout(showlegend = FALSE,
             yaxis = list(title = '', showgrid = FALSE, ticklen = 4, standoff = 20),
             xaxis = list(title = "", ticklen = 2, tick0 = 0,
                          dtick = 1),
             barmode = 'stack',
             annotations = list(x = 1, y = -0.2, text = "", showarrow = FALSE,
                                xref = 'paper', yref = 'paper', xanchor = 'right',
                                yanchor = 'bottom', xshift = 0, yshift = 0,
                                font = list(size = 12, color = "black")))
    
    
  })
  
  # Funder - renderUI for datatable
  output$data_table_box <- renderUI({
    title_value <- input$funder_select
    
    if (is.null(title_value)){
      
      title_value <- "Select a Funder"
    }
    box(width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = HTML(paste0("Studies Funded by <strong>", title_value, "</strong>")),
        
        download_table_UI("dl_funder_data"),
        DT::dataTableOutput("funder_data_table") %>% withSpinner(color="#96c296")
    )
  })
  
  # Funder - datatable
  output$funder_data_table <- DT::renderDataTable({
    
    funder_table <- funder_metadata_small %>%
      filter(funder_name == input$funder_select)
    
    selected_studies <- funder_table %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      arrange(desc(year))
    
    selected_studies$title <- paste0("<a href='",selected_studies$link, "' target='_blank'>",selected_studies$title,"</a>")
    
    selected_studies <- selected_studies %>%
      distinct() %>%
      select(uid, Year = year, Author = author, Title = title, Intervention = intervention, "Outcome Measures" = outcome_measures, Discipline = discipline) %>%
      arrange(Intervention == "Unknown")
    
    dl_funder <<- selected_studies
    
    DT::datatable(
      selected_studies[,2:ncol(selected_studies)],
      rownames = FALSE,
      escape = FALSE,
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
  
  # Outcome overview - renderUI -----
  output$dynamic_box <- renderUI({
    selected_outcome <- input$outcome_select
    selected_provider <- input$provider_select
    
    if (length(selected_outcome) > 1) {
      selected_outcome_str <- paste(selected_outcome, collapse = " and ")
      
      
      selected_title <- paste0("Published articles with interventions from <strong>", selected_provider, "</strong> to improve <strong>", selected_outcome, "</strong>")
      
    } else if (length(selected_outcome) == 1) {
      selected_title <- paste0("Published articles with interventions from <strong>", selected_provider, "</strong> to improve <strong>", selected_outcome, "</strong>")
      
    } else {
      selected_title <- "Interventions by Outcome"
    }
    
    box(
      width = 12,
      height = "550px",
      title = HTML(selected_title), style = "color: white; font-family: KohinoorBangla, sans-serif !important;",
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        column(6,
               pickerInput(
                 inputId = "intervention_select",
                 label = "Choose an Intervention (Top 10 Selected):",
                 choices = sort(unique(intervention_picker()$intervention)),
                 selected = head(intervention_picker()$intervention, 10),
                 multiple = TRUE,
                 options = pickerOptions(
                   noneSelectedText = "Please Select",
                   virtualScroll = 100,
                   actionsBox = TRUE,
                   size = 10
                 )
               )
        ),
        column(6,
               valueBoxOutput("intervention_box", width = 12)
        )
      ),
      plotlyOutput("interventions_by_outcome_bar")
    )
  })
  
  output$data_table_box_outcome <- renderUI({
    provider_select_val <- input$provider_select
    outcome_select_val <- input$outcome_select
    
    if (is.null(provider_select_val)){
      
      provider_select_val <- "Select a Provider"
    }
    
    if (is.null(outcome_select_val)){
      
      outcome_select_val <- "Select an Outcome"
    }
    
    box(width = 12,
        solidHeader = TRUE,
        status = "secondary",
        title = HTML(paste0("Selected studies with interventions from <strong>", provider_select_val, "</strong> to improve <strong>", outcome_select_val, "</strong>")),
        download_table_UI("dl_outcome_overview"),
        DT::dataTableOutput("outcome_overview_data_table") %>% withSpinner(color="#96c296")
    )
  })
  
  # Outcome overview - server -----
  
  # Reactive expression to filter dataset based on the selected provider
  outcome_picker <- reactive({
    all_annotations %>%
      filter(intervention_provider == input$provider_select)
  })
  
  # Observe any changes in provider selection and update outcome fields
  observeEvent(input$provider_select, {
    
    outcomes <-  sort(unique(outcome_picker()$outcome_measures[!outcome_picker()$outcome_measures %in% c("Unknown")]))
    
    # Update the outcome selections
    updatePickerInput(session, "outcome_select", choices = outcomes)
    updatePickerInput(session, "outcome_comparison_select", choices = outcomes)
    
  })
  
  # Reactive expression to filter dataset based on the selected provider
  intervention_picker <- reactive({
    # all_annotations %>%
    #   filter(intervention_provider == input$provider_select,
    #          outcome_measures %in% input$outcome_select)
    
    all_annotations %>%
      filter(intervention_provider %in% input$provider_select) %>%
      filter(outcome_measures %in% input$outcome_select) %>%
      select(uid, intervention, outcome_measures, discipline) %>%
      distinct() %>%
      group_by(intervention, outcome_measures, discipline) %>%
      count() %>%
      ungroup() %>%
      group_by(intervention) %>% 
      mutate(total = sum(n)) %>%
      ungroup() %>% 
      arrange(desc(total)) %>% 
      select(intervention) %>% 
      distinct() 
  })
  
  # Observe any changes in provider selection and update outcome fields
  observeEvent(input$provider_select, {
    
    interventions <- sort(unique(intervention_picker()$intervention))
    
  })
  
  output$outcome_overview_data_table <- DT::renderDataTable({
    
    outcome_table <- all_annotations %>%
      filter(intervention_provider == input$provider_select) %>%
      filter(outcome_measures == input$outcome_select)
    
    table_small <- all_annotations_small %>%
      filter(uid %in% outcome_table$uid)
    
    outcome_table <- left_join(table_small, included_with_metadata)
    
    
    selected_studies <- outcome_table %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      arrange(desc(year))
    
    selected_studies$title <- paste0("<a href='",selected_studies$link, "' target='_blank'>",selected_studies$title,"</a>")
    
    selected_studies <- selected_studies %>%
      distinct() %>%
      select(uid, Year = year, Author = author, Title = title, Intervention = intervention, "Outcome Measures" = outcome_measures, Discipline = discipline)
    
    dl_outcome_overview <<- selected_studies
    
    DT::datatable(
      selected_studies[,2:ncol(selected_studies)],
      rownames = FALSE,
      escape = FALSE,
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
  
  # Outcome overview - intervention provider plot -----
  output$outcome_year_plot <- renderPlotly({
    
   
    data <- all_annotations %>%
      filter(outcome_measures %in% c(input$outcome_select, input$outcome_comparison_select)) %>%
      filter(intervention_provider %in% input$provider_select) %>%
      left_join(citations_for_dl, by = "uid") %>%
      select(uid, year, outcome_measures, intervention_provider) %>%
      distinct() %>%
      group_by(year, outcome_measures, intervention_provider) %>%
      count() %>%
      ungroup()
    
    if (nrow(data) == 0) {
      # If no data, create an empty plot with text indicating no data available
      plot_ly() %>%
        layout(title = "No data available for the selected filters",
               xaxis = list(showticklabels = FALSE),
               yaxis = list(showticklabels = FALSE))
    } else {
      min_year <- min(data$year)
      max_year <- max(data$year)
      
      all_combinations <- expand.grid(
        year = min_year:max_year,
        outcome_measures = unique(data$outcome_measures),
        intervention_provider = unique(data$intervention_provider)
      )
      
      data_complete <- left_join(all_combinations, data, by = c("year", "outcome_measures", "intervention_provider")) %>%
        replace_na(list(n = 0)) %>%
        group_by(outcome_measures) %>%
        arrange(year, outcome_measures) %>%
        mutate(cumulative_n = cumsum(n)) %>%
        ungroup()
      
      plot_ly(data_complete,
              x = ~year,
              type = 'bar',
              color = ~outcome_measures,
              y = ~n,
              hoverinfo = 'text',
              marker = list(line = list(color = 'black', width = 1)),
              textposition = "none",
              text = ~paste(
                "<br><b>Intervention Provider:</b>", intervention_provider, 
                "<br><b>Outcome:</b>", outcome_measures, 
                "<br><b>Number of Publications:</b>", n,
                "<br><b>Year:</b>", year)
      ) %>%
        add_trace(data = data_complete, y = ~cumulative_n, type = 'scatter', mode = 'lines',
                  line = list(width = 2, dash = 'dot'),
                  marker = list(line = list(color = 'black', width = 0)),
                  name = 'Cumulative Publications',
                  hoverinfo = 'text',
                  text = ~paste(
                    "<br><b>Intervention Provider:</b>", intervention_provider, 
                    "<br><b>Outcome:</b>", outcome_measures, 
                    "<br><b>Cumulative Publications:</b>", cumulative_n,
                    "<br><b>Year:</b>", year),
                  showlegend = FALSE) %>%
        layout(showlegend = TRUE,
               yaxis = list(title = 'Number of publications', 
                            #dtick = 1, 
                            rangemode = "tozero",
                            showgrid = TRUE),
               xaxis = list(title = "", 
                            tickangle = -45, 
                            ticklen = 4,
                            dtick = 1,
                            tick0 = 0, 
                            showgrid = FALSE),
               legend = list(orientation = 'h', x = 0, y = -0.2),  # Horizontal legend at the bottom
               margin = list(l = 50, r = 50, t = 50, b = 100),  # Adjust margins to maximize plot area
               barmode = 'group',
               annotations =
                 list(x = 1, y = -0.2, text = "",
                      showarrow = F, xref = 'paper', yref = 'paper',
                      xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
                      font = list(size = 12, color = "black")))
    }
  })
  
  # Outcome overview - top intervention last 5 years -----
  output$top_int_five_years <- renderValueBox({
    
    current_year <- 2024
    start_year <- current_year - 5
    
   
    int_out_table <- all_annotations %>%
      filter(outcome_measures %in% input$outcome_select) %>%
      #filter(intervention %in% input$intervention_select) %>%
      filter(intervention_provider %in% input$provider_select) %>%
      left_join(citations_for_dl, by = "uid") %>%
      select(year, intervention) %>%
      distinct() %>%
      filter(year >= start_year & year <= current_year) %>%
      group_by(intervention) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      ungroup() %>%
      slice_head(n = 1) %>%
      pull(intervention)
    
    interventions_html <- paste(int_out_table, collapse="<br>")
    
    valueBox(
      subtitle = tags$p(HTML(paste0("Intervention with the most evidence in the last 5 years")), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "secondary",
      value = tags$p(HTML(interventions_html), style = "font-size: 150%; color: white;"),
      icon = icon("code"),
      elevation = 2
      
    )
  })
  
  
  # Outcome overview - interventions by outcome bar -----
  output$interventions_by_outcome_bar <- renderPlotly({
    
   
    # # Work out Multidisciplinary studies - uncomment mutate below!
    # multi_discipline <- all_annotations %>%
    #   filter(outcome_measures %in% input$outcome_select) %>%
    #   filter(intervention %in% input$intervention_select) %>%
    #   filter(intervention_provider %in% input$provider_select) %>%
    #   select(uid, intervention, outcome_measures, discipline) %>%
    #   distinct() %>%
    #   group_by(uid, intervention, outcome_measures) %>%
    #   count() %>%
    #   ungroup() %>%
    #   filter(n > 1) %>%
    #   pull(uid)
    
    int_out_table <- all_annotations %>%
      filter(intervention_provider %in% input$provider_select) %>%
      filter(outcome_measures %in% input$outcome_select) %>%
      filter(intervention %in% input$intervention_select) %>%
      select(uid, intervention, outcome_measures, discipline) %>%
      #mutate(discipline = ifelse(uid %in% multi_discipline, "Multidisciplinary", discipline)) %>%
      distinct() %>%
      #filter(!discipline == "Other") %>%
      group_by(intervention, outcome_measures, discipline) %>%
      count() %>%
      ungroup() %>%
      group_by(intervention) %>% 
      mutate(total = sum(n)) %>%
      ungroup() %>% 
      arrange(total)
    
    
    
    # Create plot
    plot <- plot_ly(data = int_out_table, x = ~n, type = 'bar', orientation = 'h',
                    y = ~factor(intervention, levels = unique(intervention)),
                    color = ~discipline,
                    marker = list(line = list(color = 'black', width = 1)),
                    hoverinfo = 'text',
                    text = ~paste0("<br><b>Intervention Provider:</b> ", input$provider_select,
                                   "<br><b>Intervention:</b> ", intervention,
                                   "<br><b>Outcome:</b> ", outcome_measures,
                                   "<br><b>Discipline:</b> ", discipline,
                                   "<br><b>Number of Publications:</b> ", n),
                    textposition = "none") %>%
      layout(showlegend = TRUE,
             yaxis = list(title = '', showgrid = FALSE, ticklen = 4, standoff = 25),
             xaxis = list(title = "Number of Publications",
                          ticklen = 2,
                          #dtick = 1,
                          tick0 = 0),
             barmode = 'stack',
             legend = list(title = list(text = "Discipline"),
                           bordercolor = 'black',
                           borderwidth = 2,
                           y = 0.95,
                           yanchor = 'top'),
             annotations = list(x = 1, y = -0.2, text = "", showarrow = FALSE,
                                xref = 'paper', yref = 'paper', xanchor = 'right',
                                yanchor = 'bottom', xshift = 0, yshift = 0,
                                font = list(size = 12, color = "black")))
    
    
  })
  
  
  # Location - server -----
  scale_size <- function(num) {
    scales::rescale(num, c(3, 15))  # Adjust size range as necessary
  }
  
  # Create a reactive color palette
  color_palette <- reactive({
    colorFactor(palette = "Set2", domain = ror_data$type)
  })
  
  
  # Location - filtered data -----
  filtered_data <- reactive({
    
    
    # If country is null
    if (is.null(input$country_select)) {
      
      inst_locations_filter <- ror_data %>%
        filter(continent %in% input$continent_select,
               outcome_measures %in% input$inst_outcome_select,
               discipline %in% input$inst_discipline_select,
               type %in% input$inst_type_select) %>%
        distinct()
      
    } else {
      
      inst_locations_filter <- ror_data %>%
        filter(country %in% input$country_select,
               continent %in% input$continent_select,
               outcome_measures %in% input$inst_outcome_select,
               discipline %in% input$inst_discipline_select,
               type %in% input$inst_type_select) %>%
        distinct()
    }
    
    if (nrow(inst_locations_filter >= 1)){
      inst_locations_filter <- inst_locations_filter %>%
        distinct()
      
    } else {
      
      inst_locations_filter <- ror_data %>%
        distinct()
      
    }
    
    
    return(inst_locations_filter)
  })
  
  # Location - render leaflet map -----
  output$institution_map <- renderLeaflet({
    data <- filtered_data() %>%
      group_by(name) %>%
      mutate(filter_no = n_distinct(uid)) %>%
      ungroup()
    
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldStreetMap
      ) %>%
      addCircleMarkers(
        ~long,
        ~lat,
        popup = ~paste0("<b>", name, "</b><br>",
                        "Institution Type: ", type, "<br>",
                        "Filtered No. of Publications: ", filter_no, "<br>",
                        "Total No. of Publications: ", number_pub),
        radius = ~scale_size(filter_no),
        color = "black",
        fillColor = ~color_palette()(type),
        fillOpacity = 1,
        label = ~name,
        weight = 1,
        layerId = ~name
        
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = color_palette(),
        values = ~type,
        title = "Institution Type",
        opacity = 0.8
      ) %>%
      setView(lat = 0, lng = 0, zoom = 1) %>%
      fitBounds(lng1 = min(filtered_data()$long, na.rm = TRUE) - 3,
                lat1 = min(filtered_data()$lat, na.rm = TRUE) - 3,
                lng2 = max(filtered_data()$long, na.rm = TRUE) + 3,
                lat2 = max(filtered_data()$lat, na.rm = TRUE) + 3)
    
    
  })
  
  filtered_table_data <- reactiveVal()
  
  observeEvent(input$institution_map_marker_click, {
    
    click <- input$institution_map_marker_click
  
    if (!is.null(click$id)) {
      
      table_data <- filtered_data() %>%
        filter(name == click$id)
      
      
      filtered_table_data(table_data)
      
    }
    
  })
  
  observeEvent(input$institution_map_click, {
    filtered_table_data(NULL)
  })
  
  observe({
    leafletProxy("institution_map", data = filtered_data()) %>%
      #clearShapes() %>%
      fitBounds(lng1 = min(filtered_data()$long, na.rm = TRUE) - 3,
                lat1 = min(filtered_data()$lat, na.rm = TRUE) - 3,
                lng2 = max(filtered_data()$long, na.rm = TRUE) + 3,
                lat2 = max(filtered_data()$lat, na.rm = TRUE) + 3)
    
  })
  
  # Location - datatable -----
  output$location_table <- DT::renderDataTable({
    
    if (is.null(filtered_table_data()) || nrow(filtered_table_data()) == 0) {
      
      table <- filtered_data()
      
    }else{
      
      table <- filtered_table_data()
      
    }
    
    table <- table %>%
      distinct(uid) %>%
      left_join(ror_data_small, by = "uid")
    
    
    table <- table %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url))
    
    table$title <- paste0("<a href='",table$link, "' target='_blank'>",table$title,"</a>")
    
    
    table$name <- paste0("<a href='",table$ror, "' target='_blank'>",table$name,"</a>")
    
    table_select <- table %>%
      select(uid, Institution = name, Title = title, Country = country, "Institution Type" = type, Discipline = discipline, Outcome = outcome_measures) %>%
      distinct() %>%
      arrange(Discipline == "Unknown")
    
    dl_location <<- table_select
    
    DT::datatable(
      table_select[,2:ncol(table_select)],
      rownames = FALSE,
      escape = FALSE,
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
            targets = c(2, 3), #target for JS code
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 100 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
              "}")),
          list(
            targets = c(2, 3), #target for JS code
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
  
}

# Run the application
shinyApp(ui = ui, server = server)

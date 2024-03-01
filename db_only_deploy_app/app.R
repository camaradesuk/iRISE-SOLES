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
                      # bs4SidebarMenuItem(tags$p("Data Collection", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "dc-main", icon = icon("database", verify_fa = FALSE), startExpanded = FALSE,
                      #                    bs4SidebarMenuSubItem(tags$p("Included Studies", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "studies-included-summary-dc"),
                      #                    bs4SidebarMenuSubItem(tags$p("Tag Status", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "status-summary-dc"),
                      #                    bs4SidebarMenuSubItem(tags$p("Workflow", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "workflow-accordion-dc")),
                      #bs4SidebarMenuItem(tags$p("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "data-summary-transparency", icon = icon("chart-pie", verify_fa = FALSE)),
                      #bs4SidebarMenuItem(tags$p("Intervention / Outcome", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "data-int-out", icon = icon("chart-column", verify_fa = FALSE)),
                      #bs4SidebarMenuItem(tags$p("Evidence Map", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "pico-bubble", icon = icon("users-gear", verify_fa = FALSE)),
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
                                    #style = "text-align: center;font-family: 'Kohinoor Bangla' ; font-size: 20px;",
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
                          #style = "text-align: center;font-family: 'Kohinoor Bangla' ; font-size: 20px;",
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
                          #style = "text-align: center;font-family: 'Kohinoor Bangla' ; font-size: 20px;",
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
                        #'Montserrat', 'Gotham', Arial, Helvetica, sans-serif, 
                        div(
                          style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                          #style = "text-align: center;font-family: 'Kohinoor Bangla' ; font-size: 20px;",
                          "Go to the iRISE website to learn more about the other work packages and wider project"),
                        background = "info",
                        width = NULL,
                        solidHeader = TRUE,
                        title = "",
                        status = "info")
                      
                    ))),
                      
                      # tabItem(tabName = "studies-included-summary-dc",
                      #         fluidRow(
                      #           valueBox(
                      #             width=3,
                      #             subtitle = tags$p("new citations this week", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             color = "success",
                      #             value = tags$p(sum(as.numeric(include_by_date$n)[which(include_by_date$date >= Sys.Date()-7)]),
                      #                            style = "font-size: 300%; color: white; font-family: KohinoorBangla, sans-serif !important"),
                      #             icon = icon("clock", verify_fa = FALSE)),
                      #           
                      #           valueBox(
                      #             width=3,
                      #             subtitle = tags$p("new citations this month", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             color = "warning",
                      #             value = tags$p(sum(as.numeric(include_by_date$n)[which(include_by_date$date >= Sys.Date()-30)]),
                      #                            style = "font-size: 300%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             icon = icon("calendar")),
                      #           
                      #           valueBox(
                      #             width=3,
                      #             subtitle = tags$p("new citations in the last year", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             color = "info",
                      #             value = tags$p(sum(as.numeric(include_by_date$n)[which(include_by_date$date >= Sys.Date()-365)]),
                      #                            style = "font-size: 300%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             icon = icon("calendar", verify_fa = FALSE)),
                      #           
                      #           valueBox(
                      #             width=3,
                      #             subtitle = tags$p("citations in database", style = "font-size: 120%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             color = "secondary",
                      #             value = tags$p(sum(as.numeric(include_by_date$n)),
                      #                            style = "font-size: 300%; color: white;font-family: KohinoorBangla, sans-serif !important"),
                      #             icon = icon("database"))
                      #         ),
                      #         
                      #         fluidRow(
                      #           
                      #           tabBox(
                      #             width=12,
                      #             id = "tabcard_included_studies",
                      #             title = "",
                      #             status = "primary",
                      #             solidHeader = FALSE,
                      #             type = "tabs",
                      #             
                      #             yearBarUI_included_only("included_studies_over_time_bar",
                      #                                     title = tags$p("Included Studies Over Time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                     theme = "danger",
                      #                                     spinner_colour = "#89CB93",
                      #                                     table = n_included_per_year_plot_data)
                      #             
                      #           )
                      #         )
                      # ),
                      
#                       tabItem(tabName = "status-summary-dc",
#                               
#                               
#                               div(
#                                 style = "color: #1A465F; font-family: KohinoorBangla, sans-serif !important;",
#                               fluidRow(
#                                 
#                                 tags$head(
#                                   tags$style(HTML("
#       /* Center the loading spinner */
#                              .load-container {
#   position: absolute;
#   top: 20%;
#   -webkit-transform: translateY(-50%);
#   transform: translateY(-50%);
#   width: 100%;
# }
#      "))),
#       
#                                 
#                                 pie_completion_UI("full_text_tagging",
#                                                   title = "Full Texts",
#                                                   theme = "info",
#                                                   info_text = p("% of PDFs identified for included citations"),
#                                                   spinner_colour = "#64C296"),
#                                 
#                                 pie_completion_UI("oa_status_tagging",
#                                                   title = "OA status tagging",
#                                                   theme = "info",
#                                                   p("% of papers tagged for open access status"),
#                                                   spinner_colour = "#64C296"),
#                                 
#                                 pie_completion_UI("od_status_tagging",
#                                                   title = "Open data tagging",
#                                                   theme = "info",
#                                                   p("% of papers tagged for open data/code status"),
#                                                   spinner_colour = "#64C296")
#                   
#                               ),
#                               
#                               fluidRow(
#                                 
# 
#                                 
#                                 pie_completion_UI("author_country_tagging",
#                                                   title = "Author Country Tagging",
#                                                   theme = "info",
#                                                   p("% of papers tagged for country of first author affiliation"),
#                                                   spinner_colour = "#64C296"),
#                                 
#                                 pie_completion_UI("discipline_tagging",
#                                                   title = "Discipline tagging",
#                                                   theme = "info",
#                                                   p("% of papers tagged by discipline"),
#                                                   spinner_colour = "#64C296"),
#                                 
#                                 pie_completion_UI("funder_tagging",
#                                                   title = "Funder tagging",
#                                                   theme = "info",
#                                                   p("% of papers tagged by funder"),
#                                                   spinner_colour = "#64C296"),
#                                 
#                                 
#                               ))
#                               
#                       ),
                      
                      # Transparency info tab UI--------------------------------------------------------------------------------------------------------------------------
                #       tabItem(tabName = "data-summary-transparency",
                #               
                #               bs4Jumbotron(
                #                 title = tags$h1("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important;"),
                #                 lead = tags$p("This summary shows the overall percentages of publications
                # across different transparency measures, including open access publication, open data, and open code. You can
                # also benchmark improvements by viewing the number of publications in each category over time.", style = ";font-family: Kohinoor Bangla;"),
                #                 status = "primary",
                #                 btnName = NULL
                #               ),
                #               
                #               fluidRow(
                #                 
                #                 valueBox(
                #                   width=4,
                #                   subtitle = tags$h2("Open Access", style = "color: white;font-family: KohinoorBangla, sans-serif !important;"),
                #                   color = "success",
                #                   value = tags$p(round(length(oa_tag$uid[which(oa_tag$is_oa==TRUE)])/length(oa_tag$uid)*100,1), "%",
                #                                  style = "font-size: 300%; color: white;"),
                #                   icon = icon("lock")
                #                 ),
                #                 
                #                 valueBox(
                #                   width=4,
                #                   subtitle = tags$h2("Open Data", style = "color: white;font-family: KohinoorBangla, sans-serif !important;"),
                #                   color = "info",
                #                   value = tags$p(round(length(transparency$uid[which(transparency$is_open_data==TRUE)])/length(transparency$uid)*100,1), "%",
                #                                  style = "font-size: 300%; color: white;"),
                #                   icon = icon("bar-chart", verify_fa = FALSE)
                #                 ),
                #                 
                #                 valueBox(
                #                   width=4,
                #                   subtitle = tags$h2("Open Code", style = "color: white;font-family: KohinoorBangla, sans-serif !important;"),
                #                   color = "secondary",
                #                   value = tags$p(round(length(transparency$uid[which(transparency$is_open_code==TRUE)])/length(transparency$uid)*100,1), "%",
                #                                  style = "font-size: 300%; color: white;"),
                #                   icon = icon("code")
                #                 )
                #               ),
                #               
                #               
                #               tabBox(
                #                 
                #                 width = 12,
                #                 id = "tabcard",
                #                 title = "",
                #                 status = "secondary",
                #                 solidHeader = FALSE,
                #                 type = "tabs",
                #                 
                #                 yearBarUI("oa_pubs_per_year",
                #                           title = tags$p("Open access over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                #                           theme = "secondary",
                #                           spinner_colour = "#89CB93",
                #                           table = oa_tag),
                #                 
                #                 yearBarUI("oa_pub_type_per_year",
                #                           title = tags$p("Open access type over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                #                           theme = "danger",
                #                           spinner_colour = "#89CB93",
                #                           table = oa_tag),
                #                 
                #                 yearBarUI("open_data_pubs_per_year",
                #                           title = tags$p("Open data availability over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                #                           theme = "danger",
                #                           spinner_colour = "#89CB93",
                #                           table = transparency),
                #                 
                #                 yearBarUI("open_code_pubs_per_year",
                #                           title = tags$p("Open code availability over time", style = " color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                #                           theme = "danger",
                #                           spinner_colour = "#89CB93",
                #                           table = transparency)
                #                 
                #                 
                #               ),
                #               
                #               plot_interpret_UI("transparency_intepret",
                #                                 title = tags$p("How To Interpret This Plot", style = "font-family: KohinoorBangla, sans-serif !important;"),
                #                                 
                #                                 div(
                #                                   tags$p("Each bar plot shows the number of papers in each category over time.
                #     Navigate between tabs to see different transparency measures.
                #     You can hover your mouse over the bars to see the exact number of publications estimated to be in each category for any given year.
                #     To see only a specific category, double click on the relevant coloured square in the
                #     legend on the top right. To remove any category, click once on any coloured square in the legend.
                #     The tools and resources used to obtain the data are shown under the x-axis. Note that many publications are
                #     still missing a transparency status for one or more measures due to processing time or lack of available data.", style = "font-family: KohinoorBangla, sans-serif !important;"),
                #                                 tags$br(),
                #                                 tags$br(),
                #                                 
                #                                 tags$a(
                #                                   href = 'https://research.library.gsu.edu/c.php?g=115588&p=754380',
                #                                   tags$button("Open Access Type Info", class = "btn btn-primary", style = "background-color: #1A465F; border-color: #1A465F;font-family: KohinoorBangla, sans-serif !important;")
                #                                 )),
                #                                 theme = "primary")
                #                                 
                #               
                #       ),
                      
                      # tabItem(tabName = "data-int-out",
                      #         
                      #         tabBox(
                      #           
                      #           width = 12,
                      #           id = "tabcard_population",
                      #           title = "",
                      #           status = "primary",
                      #           solidHeader = FALSE,
                      #           type = "tabs",
                      #           
                      #           pico_multi_select_UI(id = "intervention_outcome",
                      #                                multi_select = TRUE,
                      #                                table = int_out_df,
                      #                                column = int_out_df$outcome,
                      #                                column2 = int_out_df$intervention,
                      #                                label1 = tags$p("Select a Target Outcome", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                label2 = tags$p("Select an Intervention", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                title = tags$p("Interventions and Outcomes", style = "color: #1A465F;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                theme = "danger",
                      #                                spinner_colour = "#96c296")
                      #           
                      #         )
                      # ),
                      
                      # tabItem(tabName = "pico-bubble",
                      #         
                      #         box(
                      #           width= 12,
                      #                status = "primary",
                      #                id = "pico_bubble_search_tab",
                      #                #side = "left",
                      #                
                      #                tabPanel(title = "Population",
                      #                  
                      #                  fluidRow(column(width = 6,
                      #                  pickerInput(
                      #                    inputId = "select_pop_inter",
                      #                    label = tags$p("Select an Intervention Provider", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                    choices = sort(unique(data_for_bubble$pop_inter)),
                      #                    selected = sort(unique(data_for_bubble$pop_inter)),
                      #                    multiple = TRUE,
                      #                    options = pickerOptions(noneSelectedText = "Please Select",
                      #                                            virtualScroll = 100,
                      #                                            actionsBox = TRUE,
                      #                                            size = 10,
                      #                    )
                      #                  )),
                      #                  column(width = 6,
                      #                         pickerInput(
                      #                           inputId = "select_pop_target",
                      #                           label = tags$p("Select a Target Population", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                           choices = sort(unique(data_for_bubble$pop_target)),
                      #                           selected = sort(unique(data_for_bubble$pop_target)),
                      #                           multiple = TRUE,
                      #                           options = pickerOptions(noneSelectedText = "Please Select",
                      #                                                   virtualScroll = 100,
                      #                                                   actionsBox = TRUE,
                      #                                                   size = 10,
                      #                           )
                      #                           
                      #                           
                      #                         )
                      #                  ) ),
                      #                  
                      #                  fluidRow(column(width = 4, 
                      #                                  pickerInput(
                      #                                    inputId = "select_discipline",
                      #                                    label = tags$p("Select a Discipline", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                    choices = sort(unique(data_for_bubble$discipline)),
                      #                                    selected = sort(unique(data_for_bubble$discipline)),
                      #                                    multiple = TRUE,
                      #                                    options = pickerOptions(noneSelectedText = "Please Select",
                      #                                                            virtualScroll = 100,
                      #                                                            actionsBox = TRUE,
                      #                                                            liveSearch = TRUE,
                      #                                                            size = 10,
                      #                                    )
                      #                                  )),
                      #                           
                      #                           column(width = 4,
                      #                                  pickerInput(
                      #                                    inputId = "select_funder",
                      #                                    label = tags$p("Select a Funder", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                    choices = sort(unique(data_for_bubble$funder)),
                      #                                    selected = sort(unique(data_for_bubble$funder)),
                      #                                    multiple = TRUE,
                      #                                    options = list(
                      #                                      `actions-box` = TRUE,
                      #                                      `live-search` = TRUE
                      #                                  ))
                      #                                  
                      #                                  
                      #                           ),
                      #                           column(width = 4,
                      #                                  pickerInput(
                      #                                    inputId = "select_continent",
                      #                                    label = tags$p("Select First Author Continent", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                      #                                    choices = sort(unique(data_for_bubble$author_aff_continent)),
                      #                                    selected = sort(unique(data_for_bubble$author_aff_continent)),
                      #                                    multiple = TRUE,
                      #                                    options = pickerOptions(noneSelectedText = "Please Select",
                      #                                                            virtualScroll = 100,
                      #                                                            actionsBox = TRUE,
                      #                                                            size = 10,
                      #                                    )
                      #                                  )
                      #                                  
                      #                                  
                      #                           )
                      #                           
                      #                  )
                      #                  
                      #                  
                      #                )),
                      #         
                      #         box(
                      #           
                      #           width = 12,
                      #           height = 700,
                      #           id = "intervention_outcome",
                      #           status = "primary",
                      # 
                      #           
                      #           
                      #           plotlyOutput("bubble_plot"),
                      #           verbatimTextOutput("click"),
                      #           tags$br(),
                      #           tags$br()
                      #           
                      #           
                      #           ),
                      #         
                      #         box(
                      #           
                      #           width = 12,
                      #           id = "intervention_outcome_datatable",
                      #           status = "primary",
                      #         
                      #           DT::dataTableOutput("pop_table") %>% withSpinner(color="#96c296")
                      #           
                      #         
                      #                               )),
                      
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
                                        table = unique_citations_for_db)
                              
                              
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
  
  
  # yearBarServer_included_only("included_studies_over_time_bar", 
  #                             table=n_included_per_year_plot_data, 
  #                             column="is_included",
  #                             colour = "#89CB93")
  # 
  # pie_completion_Server("full_text_tagging", pdfs, identifier = "doi",
  #                       included_studies = included_with_metadata, remove_failed = TRUE,
  #                       colour_not_complete = "#266080",
  #                       colour_complete = "#89CB93")
  # 
  # pie_completion_Server("oa_status_tagging", oa_tag, identifier="uid",
  #                       included_studies = included_with_metadata,
  #                       colour_not_complete = "#266080",
  #                       colour_complete = "#89CB93")
  # 
  # pie_completion_Server("od_status_tagging", transparency, identifier="uid",
  #                       included_studies = included_with_metadata,
  #                       colour_not_complete = "#266080",
  #                       colour_complete = "#89CB93")
  # 
  # pie_completion_Server("author_country_tagging", author_location_df, identifier="uid",
  #                       included_studies = included_with_metadata,
  #                       colour_not_complete = "#266080",
  #                       colour_complete = "#89CB93")
  # 
  # pie_completion_Server("discipline_tagging", discipline_df, identifier="uid",
  #                       included_studies = included_with_metadata,
  #                       colour_not_complete = "#266080",
  #                       colour_complete = "#89CB93")
  # 
  # pie_completion_Server("funder_tagging", funder_tag_df, identifier="uid",
  #                       included_studies = included_with_metadata,
  #                       colour_not_complete = "#266080",
  #                       colour_complete = "#89CB93")
  # 
  # # Transparency bar plots--------
  # yearBarServer("oa_pubs_per_year", table=oa_tag, column="is_oa", display=TRUE, order=c(TRUE, FALSE), text="Source:CrossRef", colours = c("#89CB93", "grey")) %>%
  #   bindCache(nrow(transparency))
  # yearBarServer("oa_pub_type_per_year", table=oa_tag, column="oa_status", display=c("closed", "hybrid", "bronze", "gold", "green"), order=c("closed", "hybrid", "bronze", "gold", "green"),
  #               text="Source:CrossRef", colours = c("red", "lightblue", "orange", "gold", "green")) %>% bindCache(nrow(transparency))
  # yearBarServer("open_data_pubs_per_year", table=transparency, column="is_open_data", display=TRUE, order=c(TRUE, FALSE),  text="Tool: OddPub, Riedel, N, et al. (2020), DOI:10.5334/dsj-2020-042", colours = c("#89CB93", "grey")) %>% bindCache(nrow(transparency))
  # yearBarServer("open_code_pubs_per_year", table=transparency, column="is_open_code", display=TRUE, order=c(TRUE, FALSE),  text="Tool: OddPub, Riedel, N, et al. (2020), DOI:10.5334/dsj-2020-042", colours = c("#89CB93", "grey")) %>% bindCache(nrow(transparency))
  # 
  # sunburstSever("sunburst_pop", population_df) %>%  bindCache(nrow(interventions_df))
  # 
  # pico_multi_select_Server("population",
  #                          multi_select = TRUE,
  #                          table = population_df,
  #                          column = "main_category",
  #                          column2 = "name",
  #                          text = "Tool: custom regex drug dictionary") 
  # 
  # pico_multi_select_Server("intervention_outcome",
  #                          multi_select = TRUE,
  #                          table = int_out_df,
  #                          column = "outcome",
  #                          column2 = "intervention",
  #                          text = "Tool: custom regex drug dictionary")
  # 
  # pico_multi_select_Server("interventions",
  #                          multi_select = FALSE,
  #                          table = interventions_df,
  #                          column = "name",
  #                          text = "Tool: custom regex drug dictionary") 
  
  
  # Search Page Server
  search_Server("search_results",
                pico_data = pico_elements_list,
                table = unique_citations_for_db,
                combined_pico_table = pico,
                citations_for_download = unique_citations_for_db,
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
  # bubble_react <- reactive({
  #   data <- data_for_bubble %>%
  #     # mutate(column = as.factor(!!rlang::sym(column))) %>% 
  #     # mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
  #     filter(pop_inter %in% input$select_pop_inter) %>% 
  #     filter(pop_target %in% input$select_pop_target) %>% 
  #     filter(discipline %in% input$select_discipline) %>% 
  #     filter(author_aff_continent %in% input$select_continent) %>% 
  #     filter(funder %in% input$select_funder) %>% 
  #     group_by(intervention, outcome) %>% 
  #     count()
  #   
  #   data$key <- row.names(data)
  #   data$col <- "#266080"
  #   
  # 
  #   return(data)
  #    })
  #   
  # table_react <- reactive({
  #   table <- data_for_bubble %>%
  #     filter(pop_inter %in% input$select_pop_inter) %>% 
  #     filter(pop_target %in% input$select_pop_target) %>% 
  #     filter(discipline %in% input$select_discipline) %>% 
  #     filter(author_aff_continent %in% input$select_continent) %>% 
  #     filter(funder %in% input$select_funder) %>% 
  #     left_join(included_with_metadata, by = "uid") %>% 
  #     select(year, author, title, intervention, outcome, discipline, funder, continent = author_aff_continent, doi, url) %>% 
  #     mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
  #     arrange(desc(year))
  #   
  #     table$title <- paste0("<a href='",table$link, "' target='_blank'>",table$title,"</a>")
  #     
  #     table <- table %>% 
  #       select(-doi, -url, -link)
  #     
  #   
  #   
  #   
  #     
  #   return(table)
  # })
  # 
  # 
  # output$bubble_plot <- renderPlotly({
  # 
  #       click_data <- event_data("plotly_click", priority = "event")
  #   #select_data <- event_data("plotly_selecting", priority = "event")
  # 
  #   if (!is.null(click_data)) {
  #     
  #     bubble_react_new <- bubble_react() %>% 
  #       mutate(selected_colour = key %in% click_data$customdata)
  #     
  #     bubble_react_new$selected_colour <- bubble_react()$key %in% click_data$customdata
  #     
  #     bubble_react_new <- bubble_react_new %>% 
  #       mutate(col = case_when(
  #         selected_colour == FALSE ~ "#266080",
  #         selected_colour == TRUE ~ "#47B1A3"
  #       ))
  # 
  #   }
  #     else {
  #       bubble_react_new <- bubble_react()
  # 
  #    }
  #   
  #   p <- plot_ly(bubble_react_new,
  #           x = ~intervention, y = ~outcome, size = ~n, 
  #           colors = ~sort(unique(col)), color = ~col, customdata = ~key,
  #           type = 'scatter', 
  #           # mode = 'markers',
  #           marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.8,
  #                         line = list(color = '#FFFFFF', width = 2)),
  #           hoverinfo = 'text',
  #           textposition = "none",
  #           text = ~paste("Intevention:", intervention,
  #                         "<br>Outcome:", outcome,
  #                         "<br>Number of Citations:", n)) %>% 
  #     layout(p, yaxis = list(title = list(text = "Outcome", standoff = 25)),
  #            xaxis = list(title = list(text = "Intervention", standoff = 25),
  #                         tickangle = -20,
  #                         ticklen = 1),
  #            hoverlabel = list(bgcolor = "white",
  #                              font = list(size = 14)),
  #            height = 550,
  #            showlegend = FALSE
  #            # autosize = F, width = 600, height = 600
  #     )
  #     
  #   event_register(p, event = "plotly_selecting")
  #   
  # })
  # 
  # output$pop_table <- DT::renderDataTable({
  # 
  #       d <- event_data("plotly_click")
  #   #s <- event_data("plotly_selecting")
  #   
  #   selected_studies <- table_react() %>% 
  #     filter(intervention %in% d$x,
  #            outcome %in% d$y)
  #   
  #   
  #   DT::datatable(
  #     selected_studies,
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
  #         # list(
  #         #   targets = c(3:7), # columns 4, 5, and 6
  #         #   render = JS(
  #         #     "function(data, type, row, meta) {",
  #         #     "  if (type === 'display' && data) {",
  #         #     "  var words = data.split(';');",
  #         #     " var formattedText = words.map(function(word) {",
  #         #     "  var color =  '#' + ('000000' + Math.floor(Math.random()*16777215).toString(16)).slice(-6);",
  #         #     "      var textColor = (parseInt(color.substring(1), 16) > 0xffffff / 2) ? 'black' : 'white';",
  #         #     "      return '<span style=\"background-color:' + color + '; color:' + textColor + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
  #         #     # "      return '<span style=\"background-color:' + color + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
  #         #     "    }).join('; ');",
  #         #     "    return formattedText;",
  #         #     "  }",
  #         #     "  return data;",
  #         #     "}")
  #         # ),
  #         list(width = '10%', targets = "_all")
  #       )
  #     )
  #     
  #   )
  # })
  
}
                    


# Run the application 
shinyApp(ui = ui, server = server)                     

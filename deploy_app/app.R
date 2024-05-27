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
library(shinyhelper)

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
                                     bs4SidebarMenuItem(tags$p("Evidence Map", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "int_ac_dis-bubble", icon = icon("users-gear", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Intervention/Outcome", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "discipline_bar", icon = icon("chart-column", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Outcome Overview", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "outcome-overview-tab", icon = icon("file-code", verify_fa = FALSE)),
                                     

                                     bs4SidebarMenuItem(tags$p("Funder", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "funder-tab", icon = icon("landmark", verify_fa = FALSE)),

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

                                         fluidRow(column(width = 4,
                                                         pickerInput(
                                                           inputId = "select_outcome",
                                                           label = tags$p("Select an Outcome Measure", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                           choices = sort(unique(dummy_data_for_bubble$outcome_measures)),
                                                           selected = c("Materials availability and re-use", "Data availability and re-use", "Computational reproducibility", "Code / analysis availability and re-use"),
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
                                                  label = tags$p("Select Legend", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
                                                  choices = colnames(dummy_data_for_bubble)[colnames(dummy_data_for_bubble) %in% c("discipline", "intervention_provider", "target_population")],
                                                  #choices = stringr::str_replace_all(stringr::str_to_title(colnames(dummy_data_for_bubble)[colnames(dummy_data_for_bubble) %in% c("discipline", "intervention_provider", "target_population")]), pattern = "_", replacement = " "),
                                                  
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
                                                  label = tags$p("Select Specific", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
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
                                         )
                                )),

                              box(

                                width = 12,
                                height = 900,
                                id = "intervention_discipline",
                                status = "primary",
                                
                                # materialSwitch(inputId = "switch_over_time",
                                #                label = "Over time", 
                                #                status = "info"),
                                
                                
                                verbatimTextOutput("error_message"),
                                plotlyOutput("int_ac_dis_bubble_plot") %>% withSpinner(color="#96c296"),
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
                                                                            size = 10
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
                                                                            size = 10
                                                    )
                                                  )
                                           ),
                                           column(width = 4,
                                                  pickerInput(
                                                    inputId = "legend_select_specific",
                                                    label = tags$p("Legend Specfic", style = "color: #47B1A3;font-family: KohinoorBangla, sans-serif !important;"),
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


                              )

                      ),

                      tabItem(tabName = "funder-tab",

                              box(width = 12,
                                  height = "600px",
                                  title = "Studies Funded by Year",
                                  status = "primary",
                                  solidHeader = TRUE,

                                  fluidRow(column(6,
                                                  pickerInput(inputId = "funder_select",

                                                              label = "Choose a Funder:",
                                                              choices = unique(funder_overall_count$funder_name),
                                                              selected = c("National Institutes of Health"),
                                                              multiple = FALSE,
                                                              options = pickerOptions(noneSelectedText = "Please Select",
                                                                                      virtualScroll = 100,
                                                                                      actionsBox = TRUE,
                                                                                      size = 10))),


                                           column(6,
                                                  valueBoxOutput("funding_summary", width = 12)
                                           )),
                                  # fluidRow(
                                  #   column(6,
                                  # pickerInput(inputId = "funder_comp_select",
                                  #             label = "Choose a Funder for comparison:",
                                  #             choices = unique(funder_overall_count$funder_name),
                                  #             selected = c("National Science Foundation"),
                                  #             multiple = TRUE,
                                  #             options = pickerOptions(noneSelectedText = "Please Select",
                                  #                                     virtualScroll = 100,
                                  #                                     actionsBox = TRUE,
                                  #                                     size = 10)))
                                  # ),




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
                                                       label = "Choose a Category:", 
                                                       choices = sort(unique(dummy_data_for_bubble$intervention)),
                                                       selected = sort(unique(dummy_data_for_bubble$intervention)),
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
                              
                              fluidRow(
                                box(
                                  title = "Select Filters",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = 3,
                                  height = "500px",
                                  tags$br(),

                                  pickerInput(
                                    inputId = "provider_select",
                                    label = tags$p("Select an Intervention Provider", style = "color: #47B1A3; font-family: KohinoorBangla, sans-serif;"),
                                    choices = sort(unique(dummy_data_for_bubble$intervention_provider)),
                                    selected = c("Institution"),
                                    multiple = FALSE,
                                    options = pickerOptions(
                                      noneSelectedText = "Please Select",
                                      virtualScroll = 100,
                                      actionsBox = TRUE,
                                      size = 10
                                    )
                                  ),
                                  tags$br(),
                                  
                                  pickerInput(
                                    inputId = "outcome_select",
                                    label = tags$p("Select an Outcome Measure", style = "color: #47B1A3; font-family: KohinoorBangla, sans-serif;"),
                                    choices = sort(unique(dummy_data_for_bubble$outcome_measures)),
                                    selected = c("Code / analysis availability and re-use"),
                                    multiple = FALSE,
                                    options = pickerOptions(
                                      noneSelectedText = "Please Select",
                                      virtualScroll = 100,
                                      actionsBox = TRUE,
                                      size = 10
                                    )
                                  ),
                                  
                                  tags$br(),
                                  
                                  pickerInput(
                                    inputId = "outcome_comparison_select",
                                    label = tags$p("Select Outcome/Outcomes for Comparison", style = "color: #47B1A3; font-family: KohinoorBangla, sans-serif;"),
                                    choices = sort(unique(dummy_data_for_bubble$outcome_measures)),
                                    selected = NULL,
                                    multiple = TRUE,
                                    options = pickerOptions(
                                      noneSelectedText = "Please Select",
                                      virtualScroll = 100,
                                      actionsBox = TRUE,
                                      size = 10
                                    )
                                  )
                                  
                                ),
                                box(
                                  title = "Outcomes by Year",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = 9,
                                  height = "500px",
                                  plotlyOutput("outcome_year_plot") %>% withSpinner(color="#96c296")
                                )),
                              
                              
                              
                              uiOutput("dynamic_box"),
                              
                              # box(width = 12,
                              #     height = "600px",
                              #     title = "Interventions by Outcome",
                              #     status = "primary",
                              #     solidHeader = TRUE,
                              # 
                              #     fluidRow(
                              #       column(6,
                              #              pickerInput(inputId = "intervention_select",
                              #                          label = "Choose an Intervention:",
                              #                          choices = sort(unique(dummy_data_for_bubble$intervention)),
                              #                          selected = sort(unique(dummy_data_for_bubble$intervention)),
                              #                          multiple = TRUE,
                              #                          options = pickerOptions(noneSelectedText = "Please Select",
                              #                                                  virtualScroll = 100,
                              #                                                  actionsBox = TRUE,
                              #                                                  size = 10)),
                              #       ),
                              #       column(6,
                              #              valueBoxOutput("intervention_box", width = 12)
                              #       )
                              #     ),

                              #     plotlyOutput("interventions_by_outcome_bar")
                              # )
                              
                              #uiOutput("data_table_box")        
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

  observe_helpers(help_dir = "helpfiles")

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
      group_by(intervention, year) %>%
      count() %>%
      ungroup() %>%
      filter(year == 2023) %>%
      arrange(desc(n)) %>%
      slice_head() %>%
      pull(intervention)


    valueBox(
      width = 12,
      value = tags$p(paste0("The top Intervention funded by ", input$funder_select, " in 2023 was"), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      subtitle = tags$h2(df, style = "font-size: 150%; color: white;"),
      icon = icon("university"),
      color = "primary",
      elevation = 2
    )
  })

  filtered_data <- reactive({
    funder_transparency %>%
      filter(funder_name == input$funder_select)
  })

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
    data <- filtered_data()

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
    data <- filtered_data()

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
    data <- filtered_data()

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

  # observe({
  #   funder_category_choices <- switch(input$funder_category_select,
  #                     "Intervention" =  sort(unique(funder_metadata$intervention)),
  #                     "Intervention Provider" = sort(unique(funder_metadata$intervention_provider)),
  #                     "Target Population" = sort(unique(funder_metadata$target_population)),
  #                     "Method of Delivery" = sort(unique(funder_metadata$method_of_delivery)),
  #                     "Outcome Measures" = sort(unique(funder_metadata$outcome_measures)),
  #                     "Method of Delivery" = sort(unique(funder_metadata$research_stage)),
  #                     "Discipline" = sort(unique(funder_metadata$discipline)))
  #
  # })


  output$funder_tag_pie <- renderPlotly({

    colors <- c("#266080", "#89CB93")

    df_count <- included_with_metadata %>%
      left_join(funder, by = "doi", multiple="all") %>%
      mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
      select(doi, cat) %>%
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
             margin = list(b = 30, l = 20, r = 20, t = 30, pad = 0,
                           autoexpand = TRUE)

      )
  })


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
                "<br><b>Number of Publications:</b>", n,
                "<br><b>Year:</b>", year)
      ) %>%
      layout(showlegend = FALSE,
             yaxis = list(title = 'Number of publications', showgrid = TRUE),
             xaxis = list(title = "", tickangle = -45, ticklen = 4, showgrid = FALSE), barmode='stack',
             annotations =
               list(x = 1, y = -0.2, text = "",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                    font=list(size=12, color="black")))

  })
  
  output$funder_category_bar <- renderPlotly({
    
      funder_metadata_table <- funder_metadata %>%
        filter(funder_name == input$funder_select) %>%
        filter(intervention %in% input$funder_intervention_select) %>% 
        filter(!intervention == "other") %>% 
        group_by(intervention) %>%
        count() %>%
        ungroup() %>%
        arrange(n)

      # Create plot
      plot <- plot_ly(data = funder_metadata_table, x = ~n, type = 'bar', orientation = 'h',
                      y = ~factor(intervention, levels = unique(intervention)),
                      marker = list(color = '#B1E0CB', line = list(color = 'black', width = 1)),
                      hoverinfo = 'text',
                      text = ~paste("Number of Publications:", n, "<br>", 
                                    "Intervention:", intervention),
                      textposition = "none") %>%
        layout(showlegend = FALSE,
               yaxis = list(title = '', showgrid = FALSE, ticklen = 4, standoff = 20),
               xaxis = list(title = "", ticklen = 2),
               barmode = 'stack',
               annotations = list(x = 1, y = -0.2, text = "", showarrow = FALSE,
                                  xref = 'paper', yref = 'paper', xanchor = 'right',
                                  yanchor = 'bottom', xshift = 0, yshift = 0,
                                  font = list(size = 12, color = "black")))
      
  })

  output$data_table_box <- renderUI({
    title_value <- input$funder_select

    if (is.null(title_value)){

      title_value <- "Select a Funder"
    }
    box(width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = paste0("Studies Funded by ", title_value),
        DT::dataTableOutput("funder_data_table") %>% withSpinner(color="#96c296")
    )
  })

  output$funder_data_table <- DT::renderDataTable({

    funder_table <- funder_metadata %>%
      filter(funder_name == input$funder_select)


    selected_studies <- funder_table %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      arrange(desc(year))

    selected_studies$title <- paste0("<a href='",selected_studies$link, "' target='_blank'>",selected_studies$title,"</a>")

    selected_studies <- selected_studies %>%
      distinct() %>%
      select(year, author, title, intervention, outcome_measures, discipline)

    DT::datatable(
      selected_studies,
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
  
  output$dynamic_box <- renderUI({
    selected_outcome <- input$outcome_select
    selected_provider <- input$provider_select
    
    if (length(selected_outcome) > 1) {
      selected_outcome_str <- paste(selected_outcome, collapse = ", ")
      
      selected_title <- paste0("Interventions provided by: ", selected_provider, "<br>",
                               "Outcome Measures: ", selected_outcome_str)
      
    } else if (length(selected_outcome) == 1) {
      selected_title <- paste0("Interventions provided by: ", selected_provider, "<br>",
                               "Outcome Measures: ", selected_outcome)
    } else {
      selected_title <- "Interventions by Outcome" 
    }
    
    box(
      width = 12,
      height = "800px",
      title = tags$p(HTML(selected_title), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        valueBoxOutput("top_int_five_years"),
        valueBoxOutput("top_int_five_no"),
        valueBoxOutput("top_disc"),
        
      ),
      fluidRow(
        column(6,
               pickerInput(
                 inputId = "intervention_select",
                 label = "Choose an Intervention:",
                 choices = sort(unique(dummy_data_for_bubble$intervention)),
                 selected = sort(unique(dummy_data_for_bubble$intervention))[2:3],
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
  
  
  
  output$outcome_year_plot <- renderPlotly({
    
    #browser()
    data <- dummy_data_for_bubble %>% 
      filter(outcome_measures %in% c(input$outcome_select, input$outcome_comparison_select)) %>%
      filter(intervention_provider %in% input$provider_select) %>%
      left_join(citations_for_dl, by = "uid") %>% 
      select(year, outcome_measures, intervention_provider) %>% 
      group_by(year, outcome_measures, intervention_provider) %>% 
      count() %>% 
      ungroup()
    
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
            #line = list(width = 3),
            # marker = list(color = "#266080", line = list(
            #   color = "#266080", 
            #   width = 1        
            # )),
            hoverinfo = 'text',
            textposition = "none",
            text = ~paste(
              "<br><b>Number of Publications:</b>", n,
              "<br><b>Year:</b>", year)
    ) %>%
      add_trace(data = data_complete, y = ~cumulative_n, type = 'scatter', mode = 'lines',
                line = list(width = 2),
                #marker = list(size = 4),
                hoverinfo = 'text',
                text = ~paste(
                  "<br><b>Cumulative Publications:</b>", cumulative_n,
                  "<br><b>Year:</b>", year),
                showlegend = FALSE) %>%
      layout(showlegend = TRUE,
             yaxis = list(title = 'Number of publications', showgrid = TRUE),
             xaxis = list(title = "", tickangle = -45, ticklen = 4, showgrid = FALSE), 
             barmode='group',
             annotations =
               list(x = 1, y = -0.2, text = "",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                    font=list(size=12, color="black")))  
    
  })
  
  output$top_int_five_years <- renderValueBox({
    #browser()
    
    current_year <- 2024
    start_year <- current_year - 5
    
    int_out_table <- dummy_data_for_bubble %>%
      left_join(citations_for_dl, by = "uid") %>% 
      filter(outcome_measures %in% input$outcome_select) %>%
      #filter(intervention %in% input$intervention_select) %>%
      filter(intervention_provider %in% input$provider_select) %>% 
      filter(year >= start_year & year <= current_year) %>%
      group_by(intervention) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>% 
      ungroup() %>% 
      slice_head(n = 3) %>%  
      pull(intervention)
    
    interventions_html <- paste(int_out_table, collapse="<br>")
    
    
    
    valueBox(
      width = 4,
      subtitle = tags$p(HTML(paste0("The top 3 tested interventions with the most evidence in the last 5 years")), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "secondary",
      value = tags$p(HTML(interventions_html), style = "font-size: 150%; color: white;"),
      icon = icon("code"),       
      elevation = 2
      
    )
  })
  
  
  
  output$top_int_five_no <- renderValueBox({
    
    current_year <- 2024
    start_year <- current_year - 5
    
    int_no_five_years <- dummy_data_for_bubble %>%
      left_join(citations_for_dl, by = "uid") %>% 
      filter(outcome_measures %in% input$outcome_select) %>%
      #filter(intervention %in% input$intervention_select) %>%
      filter(intervention_provider %in% input$provider_select) %>% 
      filter(year >= start_year & year <= current_year) %>%
      group_by(intervention) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>% 
      ungroup() %>% 
      slice_head() 
    
    no_published <- int_no_five_years%>% 
      pull(count)
    
    intervention <- int_no_five_years%>% 
      pull(intervention)
    
    
    
    valueBox(
      width = 4,
      subtitle = tags$p(HTML(paste0("Studies have been published in the last 5 years <br> testing Intervention: ", intervention)), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "secondary",
      value = tags$p(no_published, style = "font-size: 300%; color: white;"),
      icon = icon("code"),       
      elevation = 2
      
    )
  })
  
  output$top_disc <- renderValueBox({
    
    #browser()
    int_all_time <- dummy_data_for_bubble %>%
      left_join(citations_for_dl, by = "uid") %>% 
      filter(outcome_measures %in% input$outcome_select) %>%
      #filter(intervention %in% input$intervention_select) %>%
      filter(intervention_provider %in% input$provider_select) %>% 
      #filter(year >= start_year & year <= current_year) %>%
      group_by(discipline) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>% 
      ungroup() %>% 
      slice_head() %>% 
      pull(discipline)
    
    
    
    valueBox(
      width = 4,
      subtitle = tags$p(HTML(paste0("The discipline with the most evidence")), style = "color: white; font-family: KohinoorBangla, sans-serif !important;"),
      color = "secondary",
      value = tags$p(int_all_time, style = "font-size: 150%; color: white;"),
      icon = icon("code"),       
      elevation = 2
      
    )
  })
  
  output$interventions_by_outcome_bar <- renderPlotly({
    
    int_out_table <- dummy_data_for_bubble %>%
      filter(outcome_measures %in% input$outcome_select) %>%
      filter(intervention %in% input$intervention_select) %>%
      filter(intervention_provider %in% input$provider_select) %>%
      filter(!discipline == "Other") %>% 
      group_by(intervention, outcome_measures, discipline) %>%
      count() %>%
      ungroup() %>%
      arrange(n) 
    
    # Create plot
    plot <- plot_ly(data = int_out_table, x = ~n, type = 'bar', orientation = 'h',
                    y = ~factor(intervention, levels = unique(intervention)),
                    color = ~discipline,
                    marker = list(line = list(color = 'black', width = 1)),
                    hoverinfo = 'text',
                    text = ~paste(" Number of Publications:", n,"<br>", 
                                  "Intervention:", intervention, "<br>",
                                  "Outcome:", outcome_measures, "<br>",
                                  "Discipline:", discipline),
                    textposition = "none") %>%
      layout(showlegend = TRUE,
             yaxis = list(title = '', showgrid = FALSE, ticklen = 4, standoff = 25),
             xaxis = list(title = "Number of Publications", ticklen = 2),
             barmode = 'stack',
             legend = list(title = list(text = "Discipline")),
             annotations = list(x = 1, y = -0.2, text = "", showarrow = FALSE,
                                xref = 'paper', yref = 'paper', xanchor = 'right', 
                                yanchor = 'bottom', xshift = 0, yshift = 0, 
                                font = list(size = 12, color = "black")))
    
    
  })
  
  observe({
    choices <- switch(input$legend_bubble_select,
                      "intervention_provider" = sort(unique(dummy_data_for_bubble$intervention_provider)),
                      "target_population" = sort(unique(dummy_data_for_bubble$target_population)),
                      "discipline" = sort(unique(dummy_data_for_bubble$discipline)))

    updatePickerInput(session, "legend_bubble_specific",
                      choices = choices,
                      selected = choices[1:3])
  })


  # Interventions across Disciplines Bubble ----
  previous_state <- reactiveValues(
    column1 = NULL,
    column2 = NULL
  )

  bubble_react <- reactive({
    
    req(input$select_outcome, input$legend_bubble_specific)
    
    data_filter <- dummy_data_for_bubble %>%
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
      
      #browser()
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

      #bubble_react_new$col[selected_row] <- "#266080"

      assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)

    }
    # bubble_react_new <- bubble_react_new %>% 
    #    mutate(shape = ifelse(selected_colour == FALSE, "circle", "square"))
    
   
    # if (!is.null(previous_state$column1) &&
    #     identical(data_for_bubble$intervention_provider, previous_state$column1) &&
    #     identical(data_for_bubble$target_population, previous_state$column2)) {
    #   print("Columns are unchanged")
    #
    # } else {
    #   print("Columns have changed")
    #
    #   # Update the previous state
    #   previous_state$column1 <- data_for_bubble$intervention_provider
    #   previous_state$column2 <- data_for_bubble$target_population
    #
    #   rm(col_vector, envir = .GlobalEnv)
    #
    # }

    return(bubble_react_new)
  })



  table_react <- reactive({

    table <- dummy_data_for_bubble %>%
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

    #browser()
    final_table <- dummy_data_for_bubble %>%
      filter(uid %in% table_new$uid) %>%
      filter(intervention %in% table_filter$intervention,
             !!sym(input$legend_bubble_select) %in% table_filter[[input$legend_bubble_select]],
             outcome_measures %in% table_filter$outcome_measures) %>%
      left_join(citations_for_dl, by = "uid") %>%
      select(uid, year, author, title, discipline, intervention, outcome_measures, method_of_delivery, research_stage, doi, url) %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      arrange(desc(year))

    if (nrow(final_table) > 0 ){

      final_table$title <- paste0("<a href='",final_table$link, "' target='_blank'>",final_table$title,"</a>")

    }

    final_table <- final_table %>%
      select(-doi, -url, -link, - uid)


    return(final_table)
  })

  plot_data <- reactive({
    tryCatch({
      # Assuming fetch_data() might throw an error
      bubble_react()
    }, error = function(e) {
      # Instead of directly changing the output, signal an error state via a reactiveVal
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


  output$int_ac_dis_bubble_plot <- renderPlotly({
    
    # if(input$switch_over_time){
    #   #browser()
    #   
    #   data_filter <- dummy_data_for_bubble %>%
    #     filter(outcome_measures %in% input$select_outcome) %>% 
    #     filter(discipline %in% input$select_discipline_legend)
    #   
    #   citations_years <- citations_for_dl %>% 
    #     select(uid, year)
    #   
    #   data <- data_filter %>%
    #     left_join(citations_years, by = "uid") %>%
    #     group_by(uid, year,  intervention, discipline, outcome_measures) %>%
    #     count() %>%
    #     ungroup() %>% 
    #     group_by(year, intervention, discipline, outcome_measures) %>%
    #     count(name = "yearly_count") %>%
    #     ungroup() %>% 
    #     group_by(intervention, discipline, outcome_measures) %>%
    #     mutate(cumulative_count = cumsum(yearly_count)) %>%
    #     ungroup()
    #   
    #   all_combinations <- expand.grid(
    #     year = min(data$year):max(data$year),
    #     intervention = unique(data$intervention),
    #     discipline = unique(data$discipline),
    #     outcome_measures = unique(data$outcome_measures)
    #   )
    #   
    #   # Join this with the existing data
    #   data_filled <- all_combinations %>%
    #     left_join(data, by = c("year", "intervention", "discipline", "outcome_measures")) %>%
    #     replace_na(list(yearly_count = 0))
    #   
    #   # Recalculate the cumulative counts
    #   data_filled <- data_filled %>%
    #     arrange(intervention, discipline, outcome_measures, year) %>%
    #     group_by(intervention, discipline, outcome_measures) %>%
    #     mutate(cumulative_count = cumsum(yearly_count)) %>% 
    #     ungroup() %>% 
    #     mutate(cumulative_count = as.numeric(cumulative_count)) %>% 
    #     mutate(numeric_outcome = as.numeric(factor(outcome_measures)))
    #   
    #   
    #   unique_outcomes <- sort(unique(data_filled$numeric_outcome))
    #   line_positions <- head(unique_outcomes, -1) + diff(unique_outcomes) / 2
    #   
    #   max_n <- max(data_filled$cumulative_count, na.rm = TRUE)
    #   sizeref_value <- 2 * max_n/ 100
    #   
    #   tryCatch({
    #     p <- plot_ly(data_filled,
    #                  x = ~outcome_measures, y = ~intervention, 
    #                  size = ~cumulative_count,
    #                  #colors = ~sort(unique(col)), 
    #                  color = ~discipline, 
    #                  #customdata = ~key,
    #                  type = 'scatter',
    #                  mode = 'markers',
    #                  source = "B",
    #                  frame = ~year,
    #                  height = 750,
    #                  fill = ~'',
    #                  marker = list(symbol = 'circle', sizemode = 'area', opacity = 0.8,
    #                                line = list(color = '#FFFFFF'),
    #                                legendgroup = ~discipline,
    #                                sizeref = sizeref_value
    #                                
    #                                
    #                  ),
    #                  hoverinfo = 'text',
    #                  textposition = "none",
    #                  text = ~paste(" Intervention:", intervention,"<br>",
    #                                "Outcome:", outcome_measures,"<br>",
    #                                "Discipline:", discipline,"<br>",
    #                                "Number of Studies:", cumulative_count)
    #     ) %>%
    #       layout(yaxis = list(title = list(text = "Intervention", standoff = 25)
    #                           #autotypenumbers = 'strict'
    #       ),
    #       xaxis = list(title = list(text = "Outcome Measures", standoff = 25),
    #                    tickangle = -20,
    #                    ticklen = 4,
    #                    tickvals = unique(data_filled$numeric_outcome),
    #                    ticktext = unique(data_filled$outcome_measures),
    #                    showgrid = FALSE
    #       ),
    #       hoverlabel = list(bgcolor = "white",
    #                         font = list(size = 14)),
    #       showlegend = TRUE,
    #       clickmode = "event + select",
    #       shapes =
    #         lapply(line_positions, function(pos) {
    #           list(
    #             type = "line",
    #             x0 = pos, y0 = 0,
    #             x1 = pos, y1 = 1,
    #             xref = 'x', yref = 'paper',  # Vertical lines along x
    #             line = list(color = 'grey', width = 1)
    #           )
    #         })
    #       )
    #     
    #     return(p)
    #   }, error = function(e) {
    #     
    #     #browser()
    #     #output$error_message <- renderText({ "Error occurred: Please make another choice." })
    #     
    #     return(NULL)  # Return NULL to avoid further processing or showing an erroneous plot
    #   })
    #   
    # } else { 
      
      
      tryCatch({
        
        # browser()
        
        plot <- plot_data() %>%
          ungroup() %>% 
          mutate(numeric_outcome = as.numeric(factor(outcome_measures))) %>%
          group_by(intervention, outcome_measures) %>%
          mutate(index = row_number(),  # Create an indexer within each group
                 jitter_base = ifelse(n() > 1, 0.15 / (n() - 1), 0),  # Jitter base depending on count
                 jittered_outcome = numeric_outcome + (jitter_base * n()) * ((index - 1) - (n() - 1) / 2)) %>%
          ungroup() %>%
          mutate(shape = ifelse(selected_colour == TRUE, "circle-cross-open", "circle"))
        
        # Calculate midpoints for line positions
        unique_outcomes <- sort(unique(plot$numeric_outcome))
        line_positions <- head(unique_outcomes, -1) + diff(unique_outcomes) / 2
        
        max_n <- max(plot$n, na.rm = TRUE)
        sizeref_value <- 1 * (max_n/ 100)
        
        p <- plot_ly(plot,
                     x = ~jittered_outcome, y = ~intervention, size = ~n,
                     color = as.formula(paste0("~`", input$legend_bubble_select, "`")),
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
                     text = ~paste0(" Intervention: ", intervention,"<br>",
                                    "Outcome: ", outcome_measures,"<br>",
                                    input$legend_bubble_select,": ", get(input$legend_bubble_select), "","<br>",
                                    "Number of Studies: ", n)) %>%
          layout(yaxis = list(title = list(text = "Intervention", standoff = 25),
                              # tickvals = unique(plot$numeric_intervention),
                              # ticktext = unique(plot$intervention),
                              showgrid = TRUE
          ),
          xaxis = list(title = list(text = "Outcome Measures", standoff = 25),
                       tickangle = -20,
                       ticklen = 4,
                       tickvals = unique(plot$numeric_outcome),
                       ticktext = unique(plot$outcome_measures),
                       showgrid = FALSE
          ),
          hoverlabel = list(bgcolor = "white",
                            font = list(size = 14)),
          showlegend = TRUE,
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
          # lapply(horizontal_line_positions, function(pos) {
          #   list(
          #     type = "line",
          #     x0 = 0, y0 = pos,
          #     x1 = 1, y1 = pos,
          #     xref = 'paper', yref = 'y',  # Horizontal lines along y
          #     line = list(color = 'grey', width = 1)
          #   )
          # })

          )
        return(p)
      }, error = function(e) {

        return(NULL)  # Return NULL to avoid further processing or showing an erroneous plot
      })
    
    #}
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
    choices <- switch(input$legend_select,
                      "Intervention Provider" = sort(unique(dummy_data_for_bubble$intervention_provider)),
                      "Target Population" = sort(unique(dummy_data_for_bubble$target_population)),
                      "Discipline" = sort(unique(dummy_data_for_bubble$discipline)))

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

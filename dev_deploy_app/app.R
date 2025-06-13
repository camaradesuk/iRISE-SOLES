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
library(DT)

source("irise_modules.R")

# Connect to db
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

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
                        label1 = "Filter by Intervention:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_2 = list(id = "dropdown_provider",
                        table = intervention_provider_df,
                        label1 = "Filter by Intervention Provider:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_3 = list(id = "dropdown_discipline",
                        table = discipline_df,
                        label1 = "Filter by Discipline:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_4 = list(id = "dropdown_outcomes",
                        table = outcome_measures_df,
                        label1 = "Filter by Outcome:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_5 = list(id = "dropdown_target_population",
                        table = target_population_df,
                        label1 = "Filter by Target Population:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_6 = list(id = "dropdown_research_stage",
                        table = research_stage_df,
                        label1 = "Filter by Research Stage:",
                        column1 = "name",
                        filter_no = 1),
  pico_element_7 = list(id = "dropdown_target_pop_location",
                        table = target_pop_location_df,
                        label1 = "Filter by Target Population Location:",
                        column1 = "name",
                        filter_no = 1)
)

grey_pico_elements_list <- list(
  pico_element_1 = list(id = "dropdown_ptype",
                        table = grey_lit_pico,
                        label1 = "Filter by Publication Type:",
                        column1 = "name",
                        filter_no = 1)
)

ui <- bs4DashPage(freshTheme = mytheme,

                  dark = NULL,
                  help = NULL,
                  dbHeader <- dashboardHeader(
                    title = tags$h5("iRISE-SOLES", style = "color: white; text-align: center;padding-top: 10px;"),

                    tags$a(href= 'https://irise-project.eu/',
                           tags$img(src= "iRISE-lightlogo.png",
                                    height = "50px"))
                  ),
                  dashboardSidebar(skin = "dark",
                                   collapsed = FALSE,
                                   sidebarMenu(
                                     id = "sidebarmenu",
                                     bs4SidebarMenuItem(tags$p("Homepage", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "home", icon = icon("home")),
                                     bs4SidebarMenuItem(tags$p("Data Collection", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "dc-main", icon = icon("database", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Methodology", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "workflow-accordion-dc", icon = icon("question", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(tags$p("Find Evidence", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "search_database", icon = icon("search"),
                                                        bs4SidebarMenuSubItem(tabName = "module_search_database",
                                                                              tagList(
                                                                                tags$span("Published literature", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                                                tags$span(icon("file-lines"), style = "margin-left: 8px;")
                                                                              )),
                                                        bs4SidebarMenuSubItem(tabName = "grey_lit_database",
                                                                              tagList(
                                                                                tags$span("Grey literature", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                                                tags$span(icon("book"), style = "margin-left: 8px;")
                                                                              ))),
                                     bs4SidebarMenuItem(tags$p("Evidence Map", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "evidence_map_bubble", icon = icon("diagram-project", verify_fa = FALSE)),
                                     bs4SidebarMenuItem(
                                       tags$p("Visual Summaries", style = "font-family: KohinoorBangla, sans-serif !important"),
                                       tabName = "visual_summaries", icon = icon("chart-bar"),

                                       bs4SidebarMenuSubItem(
                                         tagList(
                                           tags$span("Transparency Metrics", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                           tags$span(icon("chart-pie"), style = "margin-left: 8px;")
                                         ),
                                         tabName = "data-summary-transparency"
                                       ),

                                       bs4SidebarMenuSubItem(
                                         tagList(
                                           tags$span("Outcome Overview", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                           tags$span(icon("file-code"), style = "margin-left: 8px;")
                                         ),
                                         tabName = "outcome-overview-tab"
                                       ),

                                       bs4SidebarMenuSubItem(
                                         tagList(
                                           tags$span("By Funder", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                           tags$span(icon("landmark"), style = "margin-left: 8px;")
                                         ),
                                         tabName = "funder-tab"
                                       ),

                                       bs4SidebarMenuSubItem(
                                         tagList(
                                           tags$span("By Location", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                           tags$span(icon("earth-americas"), style = "margin-left: 8px;")
                                         ),
                                         tabName = "location-tab"
                                       )
                                     ),
                                     bs4SidebarMenuItem(tags$p("Edit Database", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "edit_database", icon = icon("edit"),
                                                        bs4SidebarMenuSubItem(tagList(
                                                          tags$span("Add Study", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                          tags$span(icon("plus"), style = "margin-left: 8px;")
                                                        ),
                                                        tabName = "add-study-tab"),
                                                        bs4SidebarMenuSubItem(tagList(
                                                          tags$span("Remove Study", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                          tags$span(icon("trash"), style = "margin-left: 8px;")
                                                        ),
                                                        tabName = "remove-study-tab"),
                                                        bs4SidebarMenuSubItem(tagList(
                                                          tags$span("Edit Study", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                          tags$span(icon("edit"), style = "margin-left: 8px;")
                                                        ),
                                                        tabName = "edit-study-tab"),
                                                        bs4SidebarMenuSubItem(tagList(
                                                          tags$span("Review Study", style = "font-family: KohinoorBangla, sans-serif !important;"),
                                                          tags$span(icon("user-check"), style = "margin-left: 8px;")
                                                        ),
                                                        tabName = "review-study-tab")),
                                     bs4SidebarMenuItem(tags$p("About", style = "font-family: KohinoorBangla, sans-serif !important"), tabName = "about", icon = icon("info"))
                                   )
                  ),

                  bs4DashBody(

                    use_theme(mytheme),

                    useShinyjs(),


                  tags$head(
                      if (file.exists("google-analytics.html")) {
                        includeHTML("google-analytics.html")
                      }
                    ),

                  tabItems(
                    tabItem(tabName = "home", class = "tab-pane home-tab",

                            # Set full-page dark blue background and floating logo
                            tags$head(
                              tags$style(HTML("

/* Default background for all content wrappers = white */
.content-wrapper {
  background-color: white !important;
}

/* Home tab content wrapper gets dark blue */
.home-tab {
  background-color: #18465F !important;
  min-height: 100vh;  /* ensure full height */
  margin: 0;
  padding: 0;
  width: 100%;
}

/* Make sure the inner content of home tab also inherits dark bg */
.home-tab .content {
  background-color: #18465F !important;
}
    .irise-logo {
      top: 40px;
      height: 385px;
      z-index: 1000;
    }
    .feature-box ul {
      list-style-type: none;
      padding-left: 0;
      margin-top: 20px;
      font-size: 20px;
      line-height: 2;
      color: white;
    }
    .feature-box i {
      margin-right: 10px;
      color: #64C296;
    }

.custom-card {
  background-color: #2c3e50;  /* fallback dark base */
  border-radius: 12px;
  height: 220px;
  text-align: center;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
  display: flex;
  flex-direction: column;
  justify-content: center;
}
.custom-card-text {
  font-size: 20px;
  color: white;
}
  "))
),

fluidRow(
  column(6,
    # Floating logo
    tags$img(src = "irise_soles_logo.png", class = "irise-logo")
    ),

  column(6,
      div(
        class = "feature-box",
        style = "color: white;",  # Added margin-left
        tags$br(),
        h2("Welcome to iRISE-SOLES", style = "color: #ffffff; font-weight: bold;"),
        h5("A continuously updated, curated summary of interventions to improve reproducibility",
           style = "color: #ffffff; font-style: italic;"),
        HTML("<ul>
            <li><i class='fas fa-file-alt'></i> Quickly summarise the evidence behind interventions </li>
<li><i class='fas fa-search'></i> Search and filter to find relevant published and unpublished studies </li>
<li><i class='fas fa-chart-pie'></i> Visualise evidence across outcomes, disciplines, and target groups </li>
<li><i class='fas fa-arrow-trend-up'></i> Explore trends in the literature </li>
<li><i class='fas fa-globe'></i> See the geographic spread of researchers working in this space</li>

          </ul>")
      )
    )
  ),

    # Row with three link boxes
fluidRow(
  column(4,
       div(class = "custom-card warning-card",
           tags$a(href = 'https://osf.io/9hzcv/?view_only=d74ff8089864468cb43daa06733e0be6',
                  tags$img(src = "osf_logo.png", height = "140px")),
           div(class = "custom-card-text",
               "Read our protocol for the iRISE-SOLES project on the Open Science Framework")
       )
),
column(4,
       div(class = "custom-card secondary-card",
           tags$a(href = 'https://portlandpress.com/clinsci/article/137/10/773/233083/Systematic-online-living-evidence-summaries',
                  tags$img(src = "paper_screenshot.PNG", height = "140px")),
           div(class = "custom-card-text",
               "Read our SOLES paper")
       )
),
column(4,
       div(class = "custom-card info-card",
           tags$a(href = 'https://irise-project.eu/',
                  tags$img(src = "irise_website.png", height = "140px")),
           div(class = "custom-card-text",
               "Visit the iRISE project website")
       )
)
                    )),



                    #   tabItem(tabName = "home",
                    #
                    #           box(
                    #             div(
                    #               style = "text-align: center;",
                    #               tags$a(
                    #                 href = 'https://irise-project.eu/',
                    #                 tags$img(src = "iRISE_logo_dark_round.png", height = "300px")
                    #               )
                    #             ),
                    #
                    #             tags$br(),
                    #             div(
                    #
                    #               style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                    #               "Taking an integrated approach to understanding, investigating and guiding strategies to address irreproducibility"
                    #             ),
                    #
                    #
                    #             background = "primary",
                    #             width = 12,
                    #             solidHeader = TRUE,
                    #             title = "",
                    #             status = "primary"),
                    #
                    #           plot_interpret_UI(id = "home_info",
                    #                             title = "",
                    #                             theme = "danger",
                    #                             div(
                    #                               style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                    #                               p("The overall aim for iRISE-SOLES is to systematically identify, synthesise and evaluate information on existing candidate interventions and tools to improve reproducibility. To do this, we have
                    #                               developed an integrated workflow of automated tools to collect and tag published research articles and visualise the evidence in this interactive web application.
                    #                               We tag studies by discipline, intervention, intervention provider, institution location, and reproducibility relevant outcomes. We also assess the transparency metrics of studies witin iRISE-SOLES e.g. their open access status and presence of data/code sharing."),
                    #                               p("To search for peer-reviewed studies in the iRISE database, go to the iRISE Database tab. If you would like to search our grey literature database for pre-prints and conference abstracts etc, go to the iRISE Grey Literature tab.")
                    #
                    #                             )),
                    #
                    #           fluidRow(
                    #
                    #             column(4,
                    #
                    #                    box(height = 350,
                    #                        div(
                    #                          style = "text-align: center;",
                    #                          tags$a(
                    #                            href = 'https://osf.io/9hzcv/?view_only=d74ff8089864468cb43daa06733e0be6',
                    #                            tags$img(src = "osf_logo.png", height = "200px")
                    #                          )
                    #                        ),
                    #
                    #                        tags$br(),
                    #                        div(
                    #                          style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                    #                          "Read our iRISE-SOLES protocol on the Open Science Framework"),
                    #                        background = "warning",
                    #                        width = NULL,
                    #                        solidHeader = TRUE,
                    #                        title = "",
                    #                        status = "warning")),
                    #
                    #             column(4,
                    #                    box(height = 350,
                    #                        div(
                    #                          style = "text-align: center;",
                    #                          tags$a(
                    #                            href = 'https://portlandpress.com/clinsci/article/137/10/773/233083/Systematic-online-living-evidence-summaries',
                    #                            tags$img(src = "paper_screenshot.PNG", height = "200px")
                    #                          )
                    #                        ),
                    #
                    #                        tags$br(),
                    #                        div(
                    #                          style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                    #                          "Read our SOLES paper to learn more about our workflow"),
                    #                        background = "secondary",
                    #                        width = NULL,
                    #                        solidHeader = TRUE,
                    #                        title = "",
                    #                        status = "secondary")),
                    #
                    #             column(4,
                    #                    box(height = 350,
                    #                        div(
                    #                          style = "text-align: center;",
                    #                          tags$a(
                    #                            href = 'https://irise-project.eu/',
                    #                            tags$img(src = "irise_website.png", height = "200px")
                    #                          )
                    #                        ),
                    #
                    #                        tags$br(),
                    #                        div(
                    #                          style = "text-align: center;font-family: KohinoorBangla, sans-serif;font-size: 20px !important;",
                    #                          "Go to the iRISE website to learn more about the other work packages and wider project"),
                    #                        background = "info",
                    #                        width = NULL,
                    #                        solidHeader = TRUE,
                    #                        title = "",
                    #                        status = "info")
                    #
                    #             ))),

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
                                title="Evidence map of controlled studies evaluating interventions",
                                width= 12,
                                collapsable = FALSE,
                                closable=FALSE,
                                sidebar = boxSidebar(
                                  id = "int_ac_dis_sidebar",
                                  icon = icon("info-circle"),
                                  tags$div(
                                    style = "padding: 10px;",
                                    tags$h4("Guidance for Evidence Map"),
                                    tags$p("Use the map below to visualize evidence on interventions to improve different types of reproducibility and related outcomes. This visualisation contains
                                           all articles which have been classified as controlled, primary research studies evaluating an intervention to improve reproducibility. Click a bubble to see all the relevant evidence in the table below."),
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
                                tags$p("Use the map below to visualise controlled observational studies and experiments evaluating an intervention to improve reproducibility and/or related outcomes. ",
                                       tags$strong("By default, the selected interventions are the 20 with the highest number of publications combined with the chosen reproducibility measure."), "Click a bubble to see all of the relevant evidence in the table below.",
                                       style = "color: black !important;font-family: KohinoorBangla, sans-serif !important;"),
                                fluidRow(column(width = 6,
                                                pickerInput(
                                                  inputId = "select_outcome",
                                                  label = tags$p("Select Reproducibility Measure(s)", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
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

                                column(width = 6,
                                       pickerInput(
                                         inputId = "legend_bubble_select",
                                         label = tags$p("Select a Subgroup", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                         choices = c("Discipline"="discipline", "Intervention provider"="intervention_provider", "Target population"="target_population"),
                                         selected = c("discipline"),
                                         multiple = FALSE,
                                         options = pickerOptions(noneSelectedText = "Please Select",
                                                                 virtualScroll = 100,
                                                                 actionsBox = TRUE,
                                                                 size = 10
                                         )
                                       )
                                )

                                ),
                                fluidRow(column(width = 6,
                                                pickerInput(
                                                  inputId = "select_intervention",
                                                  label = tags$p("Select Intervention(s) (20 Selections Max)", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                                  choices = sort(unique(all_annotations$intervention[!all_annotations$intervention %in% c("Unknown", "Unspecified")])),
                                                  selected = sort(unique(all_annotations$intervention[!all_annotations$intervention %in% c("Unknown", "Unspecified")])),
                                                  multiple = TRUE,
                                                  options = pickerOptions(noneSelectedText = "Please Select",
                                                                          virtualScroll = 100,
                                                                          #maxOptions = 20,
                                                                          actionsBox = TRUE,
                                                                          size = 10
                                                  )
                                                )
                                ),
                                column(width = 6,
                                       pickerInput(
                                         inputId = "legend_bubble_specific",
                                         label = tags$p("Filter Subgroup", style = "color: #47B1A3;font-family: KohinoorBangla, Sans-serif; margin: 0; padding: 0;"),
                                         choices = list(),
                                         selected = ,
                                         multiple = TRUE,
                                         options = pickerOptions(noneSelectedText = "Please Select",
                                                                 virtualScroll = 100,
                                                                 actionsBox = TRUE,
                                                                 size = 10
                                         )
                                       )
                                )),


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
                                  title = "Funding behind controlled studies evaluating interventions",
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
                                title = "Controlled studies of interventions targeting reproducibility outcomes",
                                status = "primary",
                                solidHeader = TRUE,
                                height = "650px",
                                sidebar = boxSidebar(
                                  id = "outcome-overview-tab-sidebar",
                                  icon = icon("info-circle"),
                                  tags$div(
                                    style = "padding: 10px;",
                                    tags$h4("Guidance for Outcome Overview"),
                                    tags$p("This dashboard summarises the number of research studies targeting particular aspects of reproducibility / reproducibility proxies,
                                             stratified by intervention provider (the one who implements or provides the intervention) and by discipline. First, select
                                             a provider that is most relevant, then select the reproducibility measure you are interested in targeting with an intervention. You can
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
                                title = "Instutitons conducting controlled studies evaluating interventions",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsable = FALSE,
                                closable=FALSE,

                                sidebar = c(
                                  # First sidebar with filter icon
                                  boxSidebar(
                                    width = 40,
                                    background = "#64C296",
                                    id = "inst_loc_sidebar",
                                    icon = icon("info"),
                                    fluidRow(
                                      column(width = 11,
                                             p("This map contains data on the location of first authors from across acticles represented in the iRISE database (including both controlled evaluations of interventions and other studies evaluating interventions). We were only able to obtain data
                                               for article with a DOI and where the author's institutional information was present in OpenAlex. For more information on our methodology, please visit the methodology page."),
                                             tags$div(
                                               style = "padding: 0px;",
                                               selectizeInput(inputId = "country_select",
                                                              label = tags$p("Select a Country", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                                              choices = sort(unique(ror_data$country)),
                                                              selected = NULL,
                                                              multiple = TRUE,
                                                              options = list(
                                                                placeholder = "Please select one or more countries"
                                                              )
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
                                                 inputId = "name_select",
                                                 label = tags$p("Select an Institution", style = "color: #ffffff; font-family: KohinoorBangla, sans-serif;margin: 0; padding: 0;"),
                                                 choices = sort(unique(ror_data$inst_name)),
                                                 selected = sort(unique(ror_data$inst_name)),
                                                 multiple = TRUE,
                                                 options = pickerOptions(
                                                   noneSelectedText = "Please Select",
                                                   virtualScroll = 100,
                                                   actionsBox = TRUE,
                                                   size = 10,
                                                   liveSearch = TRUE
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
                                             )
                                      )
                                    )
                                  ),

                                  # Second sidebar with info-circle icon
                                  boxSidebar(
                                    id = "int_ac_dis_sidebar",
                                    icon = icon("info-circle"),
                                    tags$div(
                                      style = "padding: 10px;",
                                      tags$h4("Guidance for Evidence Map"),
                                      tags$p("Use the map below to visualize evidence on interventions to improve different types of reproducibility and related outcomes. This visualization contains all articles which have been classified as controlled, primary research studies evaluating an intervention to improve reproducibility. Click a bubble to see all the relevant evidence in the table below."),
                                      tags$p("You can select multiple outcome measures and subgroups to filter the data. The bubbles represent the number of studies, with larger bubbles indicating more studies.")
                                    )
                                  )
                                ),
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
                      # Search Grey Literature -----
                      tabItem(tabName = "grey_lit_database",


                              search_UI("grey_lit_results",
                                        table = grey_lit)


                      ),

                      # Add Study Tab Content
                      tabItem(tabName = "add-study-tab",
                              tags$head(
                                tags$style(HTML("


    #clear_form {
      background-color: #47B1A3 !important;   /* Green */
      color: white !important;
      border: none;
        }

    #submit_study {
      background-color: #89CB93 !important;   /* Green */
      color: white !important;
      border: none;
    }

    #submit_study:enabled {
      background-color: #47B1A3 !important;   /* Darker green */
    }

    #submit_study:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }


  "))
                              ),
                              fluidRow(

                                column(
                                  width = 9,

                                  tagList(
                                    box(
                                      title = "Adding a Study to iRISE-SOLES",
                                      status = "warning",
                                      width = 12,
                                      solidHeader = TRUE
                                    ),
                                    box(
                                      title = "Suggest Study to Add",
                                      width = 12,
                                      solidHeader = TRUE,
                                      status = "info",
                                      tags$div(
                                        style = "font-family: KohinoorBangla, sans-serif !important; color: #47B1A3;",
                                        fluidRow(
                                          column(9, textInput("study_title", "Title", placeholder = "required")),
                                          column(3, textInput("study_doi", "DOI"))
                                        ),
                                        textInput("reason", "Reason", placeholder = "required"),
                                        br(),
                                        fluidRow(
                                          column(6, textInput("email", "Your Email", placeholder = "required")),
                                          column(6, textInput("orcid", "Your ORCiD"))
                                        ),
                                        br(),
                                        hr(),
                                        br(),
                                        fluidRow(
                                          column(6, actionButton("submit_study", "Submit", class = "btn-block", disabled = TRUE)),
                                          column(6, actionButton("clear_form", "Clear Form", class = "btn-primary btn-block"))
                                        )
                                      )
                                    )
                                  )
                                ),


                                column(
                                  width = 3,
                                  box(
                                    title = "Inclusion/Exclusion Criteria",
                                    width = 12,
                                    solidHeader = TRUE,
                                    status = "warning",
                                    tags$div(
                                      style = "font-family: KohinoorBangla, sans-serif !important;",

                                      p(tags$strong("Inclusion Criteria:"), style = "color: #47B1A3;"),

                                      tags$ul(
                                        tags$li(style = "color: #47B1A3;", "Research which evaluates the effectiveness of an intervention on reproducibility or reproducibility-proxies (RPs)."),
                                        tags$li(style = "color: #47B1A3;", "Research which suggests or promotes interventions to improve reproducibility or RPs."),
                                        tags$li(style = "color: #47B1A3;", "Research which evaluates other aspects of the intervention suggested to improve reproducibility or RPs.")
                                      ),

                                      br(),

                                      p(tags$strong("Exclusion Criteria:"), style = "color: #1A465F;"),

                                      tags$ul(
                                        tags$li(style = "color: #1A465F;", "Research which does not meet any of the three aims in the inclusion criteria."),
                                        tags$li(style = "color: #1A465F;", "Conference abstracts, review articles, editorials, opinion pieces are excluded.")
                                      ),

                                      br(),

                                      # Link to OSF protocol with logo
                                      div(
                                        class = "text-center",
                                        tags$a(
                                          href = "https://osf.io/2vufx",
                                          target = "_blank",
                                          tags$img(src = "osf_logo.png", height = "40px", alt = "OSF Logo"),
                                          style = "display: inline-block;"
                                        ),
                                        br(),
                                        tags$a(
                                          href = "https://osf.io/2vufx",
                                          "View Full Protocol on OSF",
                                          target = "_blank",
                                          style = "color: #1A465F; text-decoration: underline;"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                      ),

                      tabItem(tabName = "remove-study-tab",
                              fluidRow(
                                column(
                                  width = 9,
                                  tagList(
                                    box(
                                      title = "Removing a Study from iRISE-SOLES",
                                      status = "warning",
                                      width = 12,
                                      solidHeader = TRUE
                                    ),
                                    box(
                                      title = "Suggest Study to Remove",
                                      width = 12,
                                      #height = "520px",
                                      solidHeader = TRUE,
                                      status = "info",
                                      sidebar = boxSidebar(
                                        id = "remove_filter_sidebar",
                                        background = "#1A465F",
                                        icon = icon("filter"),
                                        tags$h3("Filter Studies"),
                                        shinyjs::useShinyjs(),
                                        fluidRow(column(11,
                                                        uiOutput("dynamic_dropdowns_remove"),
                                                        br(),
                                                        actionBttn(
                                                          inputId = "submit_filters_remove",
                                                          label = "Apply filters",
                                                          style = "jelly",
                                                          color = "danger"
                                                        )
                                        ))),

                                      DTOutput("study_remove_table"),
                                      br(),
                                      actionButton("remove_study_btn",
                                                   "Submit Removal Suggestion",
                                                   size = "sm",
                                                   icon = icon("trash"),
                                                   class = "btn-block",
                                                   style = "background-color: #47B1A3; color: white;margin-left: auto; margin-right: auto;",
                                                   width = "33%",
                                                   disabled = TRUE)

                                    )
                                  )),
                                column(
                                  width = 3,
                                  box(
                                    title = "Inclusion/Exclusion Criteria",
                                    width = 12,
                                    solidHeader = TRUE,
                                    status = "warning",
                                    tags$div(
                                      style = "font-family: KohinoorBangla, sans-serif !important;",

                                      p(tags$strong("Inclusion Criteria:"), style = "color: #47B1A3;"),

                                      tags$ul(
                                        tags$li(style = "color: #47B1A3;", "Research which evaluates the effectiveness of an intervention on reproducibility or reproducibility-proxies (RPs)."),
                                        tags$li(style = "color: #47B1A3;", "Research which suggests or promotes interventions to improve reproducibility or RPs."),
                                        tags$li(style = "color: #47B1A3;", "Research which evaluates other aspects of the intervention suggested to improve reproducibility or RPs.")
                                      ),

                                      br(),

                                      p(tags$strong("Exclusion Criteria:"), style = "color: #1A465F;"),

                                      tags$ul(
                                        tags$li(style = "color: #1A465F;", "Research which does not meet any of the three aims in the inclusion criteria."),
                                        tags$li(style = "color: #1A465F;", "Conference abstracts, review articles, editorials, opinion pieces are excluded.")
                                      ),

                                      br(),

                                      # Link to OSF protocol with logo
                                      div(
                                        class = "text-center",
                                        tags$a(
                                          href = "https://osf.io/2vufx",
                                          target = "_blank",
                                          tags$img(src = "osf_logo.png", height = "40px", alt = "OSF Logo"),
                                          style = "display: inline-block;"
                                        ),
                                        br(),
                                        tags$a(
                                          href = "https://osf.io/2vufx",
                                          "View Full Protocol on OSF",
                                          target = "_blank",
                                          style = "color: #1A465F; text-decoration: underline;"
                                        )
                                      )
                                    )
                                  )

                                )

                              )
                      ),
                      tabItem(tabName = "edit-study-tab",
                              fluidRow(
                                column(
                                  width = 12,
                                  box(
                                    title = "How to edit a study annotation on iRISE-SOLES",
                                    status = "warning",
                                    width = 12,
                                    solidHeader = TRUE
                                  ),
                                  box(
                                    title = "Suggest Annotation to Edit",
                                    width = 12,
                                    solidHeader = TRUE,
                                    status = "info",
                                    collapsible = TRUE,
                                    sidebar = boxSidebar(
                                      id = "edit_filter_sidebar",
                                      background = "#1A465F",
                                      icon = icon("filter"),
                                      tags$h3("Filter Studies"),
                                      shinyjs::useShinyjs(),
                                      fluidRow(column(11,
                                                      uiOutput("dynamic_dropdowns_edit"),
                                                      br(),
                                                      actionBttn(
                                                        inputId = "submit_filters_edit",
                                                        label = "Apply filters",
                                                        style = "jelly",
                                                        color = "danger"
                                                      )
                                      ))),
                                    DTOutput("study_edit_table"),
                                    br(),
                                    actionButton(
                                      "edit_study_btn",
                                      "Submit Annotation Suggestion",
                                      size = "sm",
                                      icon = icon("edit"),
                                      class = "btn-block",
                                      style = "background-color: #47B1A3; color: white; margin-left: auto; margin-right: auto;",
                                      width = "33%",
                                      disabled = TRUE
                                    )
                                  )
                                )

                              )

                      ),
                      tabItem(tabName = "review-study-tab",
                              fluidRow(column(12,
                                              uiOutput("review_study_ui")
                              ))
                      ),
                      tabItem(tabName = "about",

                              fluidRow(

                                box(width = 5,
                                    title = tagList(icon("hand-holding-dollar"), "Funding"),
                                    background="danger",
                                    solidHeader = T,
                                    status="danger",
                                    p("iRISE receives funding from the European Union's Horizon Europe research and innovation programme under grant agreement No 101094853.
                                      Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the
                                      European Research Executive Agency (ERA). Neither the European Union nor the ERA can be held responsible for them."),
                                    tags$div(
                                      style = "text-align: center;",
                                      tags$img(src = "european_union_logo.jpg", height = "100px")
                                    )),

                                box(width = 7,
                                    title = tagList(icon("database"), "Using iRISE-SOLES data"),
                                    background = "warning",
                                    solidHeader = TRUE,
                                    status = "warning",
                                    p("We license all data and information provided under a Creative Commons Attribution 4.0 International license (CC BY 4.0)."),

                                    tags$div(
                                      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-top: 15px; border-radius: 6px; color:black;",
                                     strong("Please cite the iRISE-SOLES project as:"), br(),
                                             "Hair, K, Smith S., Buljan, I, Hooijmans, C.R., Macleod, M. R., Marušić, a., Pejdo, D., Rackoll, T., Wever, K.E., Wendt, S., McCann, S. K., and Sena, E. S. on behalf of the iRISE consortium. (2023).
                                            A protocol for a systematic online living evidence summary of the interventions to improve reproducibility (iRISE-SOLES),",
                                           "Open Science Framework. ",
                                             tags$a(href = "https://doi.org/10.31222/osf.io/nbe5q",
                                                    "https://doi.org/10.31222/osf.io/nbe5q",
                                                    target = "_blank"))
                                      )


                              ),

                              fluidRow(

                                box(width = 12,
                                    title = tagList(icon("laptop-code"), "Development"),
                                    status="info",
                                    solidHeader = T,
                                    background="info",
                                    p("This platform was developed as part of a task within the iRISE project, led by ", strong("Kaitlyn Hair"), "(Task Lead).",
                                      "The iRISE-SOLES app was developed by ", strong("Sean Smith"), "(Developer).",
                                      "The platform was developed in collaboration with members of Work Package 2 and the wider iRISE consortium, which is led by ", strong("Sarah McCann and Emily Sena"), "(Project Leads)."),

                                    p("For any questions about the iRISE-SOLES project, please contact: ",
                                      strong(tags$a(href = "mailto:sean.smith@ed.ac.uk",
                                                    "sean.smith@ed.ac.uk",
                                                    style = "color: #1A465F;"),
                                             "or",
                                      strong(tags$a(href = "mailto:kaitlyn@ucl.ac.uk",
                                                    "kaitlyn.hair@ucl.ac.uk",
                                                    style = "color: #1A465F;")))
                                )))
                              )
                  )
))


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



  # shinyalert(
  #   title="Welcome!",
  #   text="iRISE-SOLES is a living evidence summary dashboard
  # summarising the evidence on interventions to improve reproducibility.",
  #   type = "info",
  #   size = "s",
  #   animation = TRUE,
  #   confirmButtonText = "Enter iRISE-SOLES",
  #   confirmButtonCol = "#1A465F")


  observeEvent(input$sidebarmenu, {
    if (input$sidebarmenu == "evidence_map_bubble"){

      shinyalert(
        title="Need more information?",
        text="Look for information icons on each box as you navigate through the dashboard.
  Head to our methodology page for more detailed information.",
        type = "info",
        size = "s",
        animation = TRUE,
        confirmButtonText = "OK!",
        confirmButtonCol = "#1A465F",)
    }
  })

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

  observeEvent(input$sidebarmenu, {
    if (input$sidebarmenu == "grey_lit_database"){

      shinyalert(
        title = "Grey Literature Search",
        text = "Use this page to search for grey literature only, such as pre-prints and conference abstracts. This database is separate from our main database containing peer-reviewed articles.",
        type = "info",
        size = "s",
        animation = TRUE,
        confirmButtonText = "OK!",
        confirmButtonCol = "#1A465F",)
    }
  })
  # Search Page - server -----
  search_Server("grey_lit_results",
                pico_data = grey_pico_elements_list,
                table = grey_lit,
                combined_pico_table = grey_lit_pico,
                citations_for_download = grey_lit,
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


  observeEvent(input$select_outcome, {

    data_filter <- all_annotations %>%
      filter(outcome_measures %in% input$select_outcome)


    citations_years <- citations_for_dl %>%
      select(uid, year)

    data <- data_filter %>%
      left_join(citations_years, by = "uid") %>%
      group_by(uid, intervention, !!sym(input$legend_bubble_select), outcome_measures) %>%
      count() %>%
      ungroup() %>%
      count(intervention, !!sym(input$legend_bubble_select), outcome_measures) %>%
      arrange(!!sym(input$legend_bubble_select), outcome_measures, intervention)

    interventions <- data %>%
      arrange(desc(n)) %>%
      distinct(intervention, .keep_all = TRUE) %>%
      pull(intervention)

    updatePickerInput(session, "select_intervention",
                      choices = sort(interventions),
                      selected = interventions[1:20],
                      options = pickerOptions(noneSelectedText = "Please Select",
                                              virtualScroll = 100,
                                              maxOptions = 20,
                                              actionsBox = TRUE,
                                              size = 10
                      ))
  })

  previous_state <- reactiveValues(
    column1 = NULL,
    column2 = NULL
  )

  bubble_react <- reactive({

    req(input$select_outcome, input$legend_bubble_specific, input$select_intervention)

    data_filter <- all_annotations %>%
      filter(outcome_measures %in% input$select_outcome) %>%
      filter(intervention %in% input$select_intervention) %>%
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
      filter(intervention %in% input$select_intervention) %>%
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
      # %>%
      #   filter(n > 1)

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

  observeEvent(input$name_select, {

    inst_outcomes <- ror_data %>%
      filter(
        inst_name %in% input$name_select,
        discipline %in% input$inst_discipline_select,
        type %in% input$inst_type_select) %>%
      select(outcome_measures) %>%
      distinct() %>%
      arrange(outcome_measures) %>%
      pull(outcome_measures)

    updatePickerInput(session, "inst_outcome_select",
                      choices = sort(inst_outcomes),
                      selected = inst_outcomes,
                      options = pickerOptions(noneSelectedText = "Please Select",
                                              virtualScroll = 100,
                                              maxOptions = 20,
                                              actionsBox = TRUE,
                                              size = 10
                      ))
  })

  # Location - server -----
  scale_size <- function(num) {
    scales::rescale(num, c(3, 20))  # Adjust size range as necessary
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
        filter(
          continent %in% input$continent_select,
          inst_name %in% input$name_select,
          outcome_measures %in% input$inst_outcome_select,
          discipline %in% input$inst_discipline_select,
          type %in% input$inst_type_select) %>%
        distinct()

    } else {

      inst_locations_filter <- ror_data %>%
        filter(country %in% input$country_select,
               continent %in% input$continent_select,
               inst_name %in% input$name_select,
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
      group_by(inst_name) %>%
      mutate(filter_no = n_distinct(uid)) %>%
      ungroup()

    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldStreetMap
      ) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        popup = ~paste0("<b>", inst_name, "</b><br>",
                        "Institution Type: ", type, "<br>",
                        "Filtered No. of Publications: ", filter_no, "<br>",
                        "Total No. of Publications: ", number_pub),
        #data = subset(data, number_pub > 1),

        radius = ~scale_size(filter_no),
        color = "black",
        fillColor = ~color_palette()(type),
        fillOpacity = 1,
        label = ~inst_name,
        weight = 1,
        layerId = ~inst_name

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
        filter(inst_name == click$id)


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



  ### Feedback Loop start
  selected_study <- reactiveVal(NULL)  # Store selected study row

  remove_table <- reactive({

    included_with_metadata %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      select(year, title, journal, author, uid, link, doi) %>%
      left_join(pico, by = "uid") %>%
      # mutate(title = ifelse(!is.na(doi) & doi != "",
      #                       paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
      #                       title)) %>%
      select(uid, link, doi, year, title, journal, author, intervention, outcome = outcome_measures, discipline, provider = intervention_provider) %>%
      distinct() %>%
      filter(uid %in% filter_studies_remove()$uid)

  })

  edit_table <- reactive({

    included_with_metadata %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
      select(year, title, journal, author, uid, link, doi) %>%
      # mutate(title = ifelse(!is.na(doi) & doi != "",
      #                       paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
      #                       title)) %>%
      left_join(pico, by = "uid") %>%
      select(uid, link, doi, year, title, journal, author, intervention, outcome = outcome_measures, discipline, provider = intervention_provider, research_stage, target_population, target_pop_location = location) %>%
      distinct() %>%
      filter(uid %in% filter_studies_edit()$uid)

  })

  output$study_remove_table <- renderDT({

    remove_table_final <- remove_table() %>%
      mutate(title = ifelse(!is.na(doi) & doi != "",
                            paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
                            title)) %>%
      select(Year = year, Title = title, Journal = journal, Author = author)

    datatable(
      remove_table_final,
      selection = "single",
      options = list(
        pageLength = 5,
        dom = '<"top"f>t<"bottom"ip>',
        initComplete = JS("
      function(settings, json) {
        $('.dataTables_filter').css({
          'float': 'left',
          'text-align': 'left',
          'margin-bottom': '10px'
        });
        $('.dataTables_filter input')
          .css({
            'width': '300px',
            'display': 'inline-block'
          })
          .attr('placeholder', 'Search...');
      }
    "),
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(
            targets = c(1, 3, 4),
            render = JS("
          function(data, type, row, meta) {
            if (type === 'display' && data.length > 30) {
              return '<span title=\"' + data + '\">' + data.substr(0, 30) + '...' + '</span>';
            } else {
              return data;
            }
          }
        ")
          )
        )
      ),
      escape = FALSE
    )

  })

  # Enable "Remove Study" button when row is selected
  observeEvent(input$study_remove_table_rows_selected, ignoreNULL = FALSE, {


    if (length(input$study_remove_table_rows_selected) > 0) {

      updateActionButton(session, "remove_study_btn", disabled = FALSE)

    } else {

      updateActionButton(session, "remove_study_btn", disabled = TRUE)

    }
  })

  # Capture selected study and show removal form
  observeEvent(input$remove_study_btn, {

    row_selected <- input$study_remove_table_rows_selected
    if (length(row_selected) == 0) return()

    selected_study(remove_table()[row_selected, ])

    showModal(modalDialog(
      tags$head(
        tags$style(HTML("

    #submit_removal {
      background-color: #FF0000 !important;
      color: white !important;
      border: none;
    }

    #submit_removal:enabled {
      background-color: #FF0000 !important;
    }

    #submit_removal:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }


  "))
      ),

      title = tags$div("Suggest Study to Remove", style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;"),
      size = "xl",
      easyClose = FALSE,
      fade = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("submit_removal", "Submit Removal Suggestion", class = "btn-danger", disabled = TRUE)
      ),
      tags$div(style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;",

               fluidRow(
                 tags$head(
                   tags$style(HTML("
    .link-box {
      border: 1px solid #ccc;
      padding: 8px;
      background-color: #f8f9fa;
      width: 100%;
      border-radius: 8px;
      transition: background-color 0.3s ease, text-decoration 0.3s ease;
      text-decoration: none;
    }

    .link-box:hover {
      background-color: #e2e6ea;
      cursor: pointer;
      text-decoration: underline;
    }
  "))
                 ),
                 column(8,
                        tags$div(
                          tags$strong("Title:", style = "display: block; margin-bottom: 8px;"),
                          tags$div(
                            selected_study()$title,
                            style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                          )
                        )
                 ),

                 column(4,
                        tags$div(
                          tags$strong("DOI:", style = "display: block; margin-bottom: 8px;"),
                          tags$a(
                            href = selected_study()$link,
                            target = "_blank",
                            style = "text-decoration: none; color: inherit;",
                            tags$div(
                              class = "link-box",
                              tags$span(selected_study()$doi),
                              tags$i(class = "fas fa-link", style = "margin-left: 8px;")
                            )
                          )
                        )
                 )
               ),


               br(),
               fluidRow(
                 column(12, textInput("remove_study_reason", "Reason",
                                      width = "100%"))),
               fluidRow(
                 column(6, textInput("remove_email", "Your Email", placeholder = "required",
                                     width = "100%")),
                 column(6, textInput("remove_orcid", "Your ORCiD",
                                     width = "100%"))
               )
      )
    )
    )


  })


  # Define a reactive expression to check validity
  is_submit_form_valid <- reactive({

    str_detect(input$email, "@") && trimws(input$study_title) != "" && trimws(input$reason) != ""
  })

  # Observe changes and update the button
  observe({
    updateActionButton(session, "submit_study", disabled = !is_submit_form_valid())
  })

  # Observe Submit Button Click
  observeEvent(input$submit_study, {

    withProgress(message = "Submitting suggestion for review...", value = 0, {

      incProgress(0.2)

      date <- format(Sys.Date(), "%d%m%Y")


      latest_suggester_session_id <- dbReadTable(con, "suggester_session") %>%
        pull(session_id) %>%
        max()

      suggester_session <- data.frame(session_id = latest_suggester_session_id + 1,
                                      email = input$email,
                                      orcid = input$orcid,
                                      date_added = date,
                                      stringsAsFactors = FALSE
      )

      dbWriteTable(con, "suggester_session", suggester_session, append = TRUE, row.names = FALSE)
      incProgress(0.4)

      latest_suggestion_id <- dbReadTable(con, "feedback_review") %>%
        pull(suggestion_id) %>%
        max()

      add_study <- data.frame(
        suggester_session_id = suggester_session$session_id,
        suggestion_id = latest_suggestion_id + 1,
        title = input$study_title,
        doi = trimws(input$study_doi),
        reason = input$reason,
        stringsAsFactors = FALSE
      )


      dbWriteTable(con, "add_study_test", add_study, append = TRUE, row.names = FALSE)
      incProgress(0.6)

      latest_review_id <- dbReadTable(con, "feedback_review") %>%
        pull(review_session_id) %>%
        max()

      feedback_review <- data.frame(review_session_id = latest_review_id + 1,
                                    suggestion_id = add_study$suggestion_id,
                                    review_status = "incomplete",
                                    reviewer_notes = "",
                                    stringsAsFactors = FALSE

      )

      dbWriteTable(con, "feedback_review", feedback_review, append = TRUE, row.names = FALSE)
      incProgress(0.8)

    })

    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Study has been submitted for review.<br><br>
          This will now be reviewed by our team before changes are made to the database and app.<br><br>
          Thanks for your submission!
        </div>'),
      easyClose = TRUE
    ))


    # Clear input fields after submission
    updateTextInput(session, "study_title", value = "")
    updateTextInput(session, "study_doi", value = "")
    updateTextInput(session, "reason", value = "")
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "orcid", value = "")

  })


  observeEvent(input$clear_form, {

    # Clear input fields after submission
    updateTextInput(session, "study_title", value = "")
    updateTextInput(session, "study_doi", value = "")
    updateTextInput(session, "reason", value = "")
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "orcid", value = "")


  })

  ### Remove Study Server


  # Define a reactive expression to check validity
  is_remove_form_valid <- reactive({

    !is.null(input$remove_email) && str_detect(input$remove_email, "@")

  })

  # Observe changes and update the button
  observe({
    updateActionButton(session, "submit_removal", disabled = !is_remove_form_valid())
  })


  # Observe Submit Removal Button Click
  observeEvent(input$submit_removal, {


    withProgress(message = "Submitting removal suggestion for review...", value = 0, {

      incProgress(0.2)

      date <- format(Sys.Date(), "%d%m%Y")
      study <- selected_study()

      latest_suggester_session_id <- dbReadTable(con, "suggester_session") %>%
        pull(session_id) %>%
        max()

      suggester_session <- data.frame(session_id = latest_suggester_session_id + 1,
                                      email = input$remove_email,
                                      orcid = input$remove_orcid,
                                      date_added = date,
                                      stringsAsFactors = FALSE
      )

      dbWriteTable(con, "suggester_session", suggester_session, append = TRUE, row.names = FALSE)
      incProgress(0.4)

      latest_suggestion_id <- dbReadTable(con, "feedback_review") %>%
        pull(suggestion_id) %>%
        max()

      suggestions_table <- data.frame(
        type = "relevance",
        suggestion = "exclude",
        uid = study$uid,
        session_id = suggester_session$session_id,
        id = latest_suggestion_id + 1,
        reason = input$remove_study_reason,
        stringsAsFactors = FALSE

      )


      dbWriteTable(con, "suggestions_table", suggestions_table, append = TRUE, row.names = FALSE)
      incProgress(0.6)

      latest_review_id <- dbReadTable(con, "feedback_review") %>%
        pull(review_session_id) %>%
        max()

      feedback_review <- data.frame(review_session_id = latest_review_id + 1,
                                    suggestion_id = suggestions_table$id,
                                    review_status = "incomplete",
                                    reviewer_notes = "",
                                    stringsAsFactors = FALSE

      )

      dbWriteTable(con, "feedback_review", feedback_review, append = TRUE, row.names = FALSE)
      incProgress(0.8)


      incProgress(1)

    })

    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Study removal has been submitted for review.<br><br>
          This will now be reviewed by our team before changes are made to the database and app.<br><br>
          Thanks for your submission!
        </div>'),
      easyClose = TRUE
    ))

    # Clear input fields after submission
    updateTextInput(session, "remove_study_reason", value = "")
    updateTextInput(session, "remove_email", value = "")
    updateTextInput(session, "remove_orcid", value = "")

  })


  #### Edit Study Server
  # Render edit study DataTable
  output$study_edit_table <- renderDT({

    edit_final <- edit_table() %>%
      mutate(title = ifelse(!is.na(doi) & doi != "",
                            paste0("<a href='", link, "' target='_blank'>", title, "</a>"),
                            title)) %>%
      select(-uid, -link, -doi)

    colnames(edit_final) <- str_to_title(str_replace_all(colnames(edit_final), "_", " "))

    datatable(
      edit_final,
      selection = "single",
      options = list(
        pageLength = 5,
        dom = '<"top"f>t<"bottom"ip>',
        initComplete = JS("
      function(settings, json) {
        $('.dataTables_filter').css({
          'float': 'left',
          'text-align': 'left',
          'margin-bottom': '10px'
        });
        $('.dataTables_filter input')
          .css({
            'width': '300px',
            'display': 'inline-block'
          })
          .attr('placeholder', 'Search...');
      }
    "),
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(
            targets = c(1, 3, 4),
            render = JS("
          function(data, type, row, meta) {
            if (type === 'display' && data.length > 30) {
              return '<span title=\"' + data + '\">' + data.substr(0, 30) + '...' + '</span>';
            } else {
              return data;
            }
          }
        ")
          )
        )
      ),
      escape = FALSE
    )

  })

  # Enable "Edit Study" button when row is selected, ignoreNULL allows this to work for de-selection
  observeEvent(input$study_edit_table_rows_selected, ignoreNULL = FALSE, {

    if (length(input$study_edit_table_rows_selected) > 0) {

      updateActionButton(session, "edit_study_btn", disabled = FALSE)

    } else {

      updateActionButton(session, "edit_study_btn", disabled = TRUE)

    }
  })

  edit_study_modal <- function(study) {


    intervention_result        <- create_comparison_box_ui(study, "intervention", "annotate_intervention")
    outcome_result             <- create_comparison_box_ui(study, "outcome", "annotate_outcome")
    discipline_result          <- create_comparison_box_ui(study, "discipline", "annotate_discipline")
    provider_result            <- create_comparison_box_ui(study, "provider", "annotate_provider")
    target_population_result   <- create_comparison_box_ui(study, "target_population", "annotate_target_population")
    research_stage_result      <- create_comparison_box_ui(study, "research_stage", "annotate_research_stage")
    target_pop_location_result <- create_comparison_box_ui(study, "target_pop_location", "annotate_target_pop_location")


    create_matrix_row <- function(row_label, predicted, selectize_id, choices_df, changes_ui_id, checkbox_id, has_changed = FALSE) {

      fluidRow(
        column(2,
               tags$strong(row_label, style = "line-height: 80px; display: block;")
        ),
        column(3,
               tags$div(predicted,
                        style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; height: 80px; overflow-y: auto;")
        ),
        column(3,
               selectizeInput(selectize_id,
                              label = NULL,
                              choices = choices_df %>%
                                distinct(name) %>%
                                arrange(name) %>%
                                pull(name),
                              selected = study %>%
                                select(uid) %>%
                                left_join(choices_df, by = "uid") %>%
                                pull(name),
                              multiple = TRUE,
                              width = "100%",
                              options = list(
                                create = TRUE,
                                placeholder = "Please Select",
                                maxOptions = 100,
                                plugins = list("remove_button")
                              )
               )
        ),


        column(3,
               uiOutput(changes_ui_id)
        ),
        column(
          width = 1,
          div(
            style = "display: flex; align-items: center; justify-content: center; height: 100%;",
            tags$div(
              style = "transform: scale(1.5); transform-origin: center;",
              checkboxInput(checkbox_id, label = NULL, value = FALSE, width = "20px")
            )
          )
        )




      )
    }

    showModal(
      modalDialog(
        tags$head(
          tags$style(HTML("
          #submit_edit {
            background-color: #1A465F !important;
            color: white !important;
            border: none;
          }
          #submit_edit:enabled {
            background-color: #1A465F !important;
          }
          #submit_edit:disabled {
            opacity: 0.6;
            cursor: not-allowed;
          }
          .link-box {
            border: 1px solid #ccc;
            padding: 8px;
            background-color: #f8f9fa;
            border-radius: 8px;
            transition: background-color 0.3s ease, text-decoration 0.3s ease;
            text-decoration: none;
          }
          .link-box:hover {
            background-color: #e2e6ea;
            cursor: pointer;
            text-decoration: underline;
          }
        "))
        ),
        title = tags$div(
          "Suggest Annotation to Edit",
          style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;"
        ),
        size = "xl",
        easyClose = FALSE,
        fade = TRUE,
        footer = tagList(
          br(),
          modalButton("Close"),
          actionButton("submit_edit", "Submit Annotation Suggestion", class = "btn-danger", disabled = TRUE)
        ),
        tags$div(style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;",

                 fluidRow(
                   column(8,
                          tags$div(
                            tags$strong("Title:", style = "display: block; margin-bottom: 8px;"),
                            tags$div(
                              study$title,
                              style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                            )
                          )
                   ),

                   column(4,
                          tags$div(
                            tags$strong("DOI:", style = "display: block; margin-bottom: 8px;"),
                            tags$a(
                              href = study$link,
                              target = "_blank",
                              style = "text-decoration: none; color: inherit;",
                              tags$div(
                                class = "link-box",
                                tags$span(study$doi),
                                tags$i(class = "fas fa-link", style = "margin-left: 8px;")
                              )
                            )
                          )
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(2, ""),
                   column(3,
                          div(style = "text-align: center;",
                              icon("robot", class = "fa-2x"),
                              br(),
                              tags$strong("LLM Prediction")
                          )
                   ),

                   column(3,
                          div(style = "text-align: center;",
                              icon("user-secret", class = "fa-2x"),
                              br(),
                              tags$strong("User Suggestion")
                          )
                   ),


                   column(3,
                          div(style = "text-align: center;",
                              icon("user-pen", class = "fa-2x"),
                              br(),
                              tags$strong("Changes")
                          )
                   ),
                   column(1,
                          div(style = "text-align: center;",
                              icon("handshake", class = "fa-2x"),
                              br(),
                              tags$strong("Agree with LLM")
                          )
                   )



                 ),
                 hr(),

                 # Annotation rows
                 create_matrix_row("Intervention", study$intervention, "annotate_intervention", interventions_df, "intervention_display", "agree_intervention", has_changed = intervention_result$has_changed),
                 hr(),
                 create_matrix_row("Outcome Measure", study$outcome, "annotate_outcome", outcome_measures_df, "outcome_display", "agree_outcome", has_changed = outcome_result$has_changed),
                 hr(),
                 create_matrix_row("Discipline", study$discipline, "annotate_discipline", discipline_df, "discipline_display", "agree_discipline", has_changed = discipline_result$has_changed),
                 hr(),
                 create_matrix_row("Intervention Provider", study$provider, "annotate_provider", intervention_provider_df, "provider_display", "agree_provider", has_changed = provider_result$has_changed),
                 hr(),
                 create_matrix_row("Target Population", study$target_population, "annotate_target_population", target_population_df, "target_population_display", "agree_target_population", has_changed = target_population_result$has_changed),
                 hr(),
                 create_matrix_row("Research Stage", study$research_stage, "annotate_research_stage", research_stage_df, "research_stage_display", "agree_research_stage", has_changed = research_stage_result$has_changed),
                 hr(),
                 create_matrix_row("Target Pop Location", study$target_pop_location, "annotate_target_pop_location", target_pop_location_df, "target_pop_location_display", "agree_target_pop_location", has_changed = target_pop_location_result$has_changed),
                 hr(),
                 br(),
                 fluidRow(
                   column(6, textInput("edit_email", "Your Email", placeholder = "required", width = "100%")),
                   column(6, textInput("edit_orcid", "Your ORCiD", width = "100%"))
                 )
        )
      )
    )
  }

  observe_checkbox_toggle <- function(field) {
    observe({
      req(input[[paste0("annotate_", field)]])
      original <- sort(trimws(unlist(strsplit(selected_study()[[field]], ";"))))
      suggested <- sort(trimws(input[[paste0("annotate_", field)]]))
      has_changed <- !setequal(original, suggested)
      checkbox_id <- paste0("agree_", field)

      if (has_changed) {
        updateCheckboxInput(session, checkbox_id, value = FALSE)
        shinyjs::disable(checkbox_id)

      } else {
        shinyjs::enable(checkbox_id)

        }
    })
  }

  fields <- c(
    "intervention",
    "outcome",
    "discipline",
    "provider",
    "target_population",
    "research_stage",
    "target_pop_location"
  )

  lapply(fields, observe_checkbox_toggle)




  create_comparison_box_ui <- function(study, column, input_id) {

    original_split <- sort(trimws(unlist(strsplit(study[[column]], ";"))))
    original <- paste(original_split, collapse = "; ")

    suggested <- if (is.null(input[[input_id]]) || length(input[[input_id]]) == 0) {
      ""
    } else {
      sort(trimws(paste(input[[input_id]], collapse = "; ")))

    }

    has_changed <- !setequal(original, suggested)


     ui <- tags$div(
        style = "
        padding: 12px;
        background-color: #ffffff;
        border: 1px solid #ccc;
        border-radius: 4px;
        width: 100%;
        font-family: KohinoorBangla, sans-serif !important;
        color: #1A465F;
      ",
        if (has_changed) {
          tagList(
            span("Was: ", style = "font-weight: bold; color: red;"),
            span(original, style = "color: red;"),
            tags$br(),
            span("Now: ", style = "font-weight: bold; color: #47B1A3;"),
            span(suggested, style = "color: #47B1A3;")
          )


        } else {
          span("No changes", style = "color: #47B1A3; font-weight: bold;")
        }
      )
     return(list(ui = ui, has_changed = has_changed))

  }

  output$intervention_display <- renderUI({

    study <- selected_study()
    result <- create_comparison_box_ui(study, "intervention", "annotate_intervention")
    result$ui

    # study <- selected_study()
    # create_comparison_box_ui(study, "intervention", "annotate_intervention")
  })

  output$discipline_display <- renderUI({
    study <- selected_study()
    result <- create_comparison_box_ui(study, "discipline", "annotate_discipline")
    result$ui
  })

  output$outcome_display <- renderUI({
    study <- selected_study()
    result <- create_comparison_box_ui(study, "outcome", "annotate_outcome")
    result$ui
  })

  output$provider_display <- renderUI({

    study <- selected_study()
    result <- create_comparison_box_ui(study, "provider", "annotate_provider")
    result$ui
  })

  output$research_stage_display <- renderUI({

    study <- selected_study()
    result <- create_comparison_box_ui(study, "research_stage", "annotate_research_stage")
    result$ui
  })

  output$target_population_display <- renderUI({

    study <- selected_study()
    result <- create_comparison_box_ui(study, "target_population", "annotate_target_population")
    result$ui
    })

  output$target_pop_location_display <- renderUI({

    study <- selected_study()
    result <- create_comparison_box_ui(study, "target_pop_location", "annotate_target_pop_location")
    result$ui
  })


  edit_study <- reactiveVal(NULL)

  observe({


    edit_study(list(
      intervention = input$annotate_intervention,
      discipline = input$annotate_discipline,
      outcome = input$annotate_outcome,
      intervention_provider = input$annotate_provider,
      target_population = input$annotate_target_population,
      research_stage = input$annotate_research_stage,
      target_pop_location = input$annotate_target_pop_location

    ))


  })



  # Capture selected study and show removal form
  observeEvent(input$edit_study_btn, {

    row_selected <- input$study_edit_table_rows_selected
    if (length(row_selected) == 0) return()

    # study <- annotation_table[row_selected, ]
    study <- edit_table()[row_selected, ]
    selected_study(study)

    edit_study_modal(study)


  })

  # Define a reactive expression to check validity
  is_edit_form_valid <- reactive({

    str_detect(input$edit_email, "@") && !is.null(input$edit_email)
  })

  # Observe changes and update the button
  observe({
    updateActionButton(session, "submit_edit", disabled = !is_edit_form_valid())
  })


  # Observe Submit Removal Button Click
  observeEvent(input$submit_edit, {

    withProgress(message = "Submitting edit suggestions for review...", value = 0, {

      date <- format(Sys.Date(), "%d%m%Y")
      incProgress(0.2)
      study <- selected_study()

      latest_suggester_session_id <- dbReadTable(con, "suggester_session") %>%
        pull(session_id) %>%
        max()

      suggester_session <- data.frame(session_id = latest_suggester_session_id + 1,
                                      email = input$edit_email,
                                      orcid = input$edit_orcid,
                                      date_added = date,
                                      stringsAsFactors = FALSE
      )

      dbWriteTable(con, "suggester_session", suggester_session, append = TRUE, row.names = FALSE)
      incProgress(0.4)

      agree_with_llm_flags <- list(
        intervention        = input$agree_intervention,
        discipline          = input$agree_discipline,
        outcome             = input$agree_outcome,
        provider            = input$agree_provider,
        target_population   = input$agree_target_population,
        research_stage      = input$agree_research_stage,
        target_pop_location = input$agree_target_pop_location
      )


      suggestions_table <- data.frame(
        intervention = paste(edit_study()$intervention, collapse = "; "),
        discipline = paste(edit_study()$discipline, collapse = "; "),
        outcome = paste(edit_study()$outcome, collapse = "; "),
        provider = paste(edit_study()$intervention_provider, collapse = "; "),
        target_population = paste(edit_study()$target_population, collapse = "; "),
        research_stage = paste(edit_study()$research_stage, collapse = "; "),
        target_pop_location = paste(edit_study()$target_pop_location, collapse = "; "),


        session_id = suggester_session$session_id,
        stringsAsFactors = FALSE

      )
      edit <- edit_study()
      latest_suggestion_id <- dbReadTable(con, "feedback_review") %>%
        pull(suggestion_id) %>%
        max()

      # suggestions_long <- suggestions_table %>%
      #   pivot_longer(cols = c("intervention", "discipline", "outcome", "provider", "target_population", "research_stage", "target_pop_location"), names_to = "type", values_to = "suggestion" ) %>%
      #   mutate(reason = "") %>%
      #   mutate(id = latest_suggestion_id + row_number()) %>%
      #   mutate(reason = "",
      #          uid = study$uid)


      # Reshape the table from wide to long format
      suggestions_long <- suggestions_table %>%
        pivot_longer(
          cols = names(agree_with_llm_flags),
          names_to = "type",
          values_to = "suggestion"
        ) %>%
        mutate(
          # For each row, lookup the corresponding LLM agreement flag
          agree_with_llm = sapply(type, function(t) agree_with_llm_flags[[t]]),
          reason = "",
          id = latest_suggestion_id + row_number(),
          uid = study$uid
        )

      dbWriteTable(con, "suggestions_table", suggestions_long, append = TRUE, row.names = FALSE)
      incProgress(0.6)

      latest_review_id <- dbReadTable(con, "feedback_review") %>%
        pull(review_session_id) %>%
        max()

      feedback_review <- data.frame(review_session_id = latest_review_id + 1,
                                    suggestion_id = suggestions_long$id,
                                    review_status = "incomplete",
                                    reviewer_notes = "",
                                    stringsAsFactors = FALSE

      )

      dbWriteTable(con, "feedback_review", feedback_review, append = TRUE, row.names = FALSE)
      incProgress(0.8)

      incProgress(1)


    })
    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Study annotation suggestions have been submitted for review.<br><br>
          These will now be reviewed by our team before changes are made to the database and app.<br><br>
          Thanks for your submission!
        </div>'),
      easyClose = TRUE
    ))

  })

  # Creates list for dropdown menus
  dynamic_dropdowns <- list()
  output$dynamic_dropdowns_remove <- renderUI({

    dynamic_dropdowns <- lapply(pico_elements_list, function(item) {
      pico_dropdown_UI(
        id = item$id,
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
  })

  output$dynamic_dropdowns_edit <- renderUI({

    dynamic_dropdowns <- lapply(pico_elements_list, function(item) {
      pico_dropdown_UI(
        id = item$id,
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
  })

  # Creates list for dropdown menus reactivity
  pico_element_list <- list()
  pico_element_list <- lapply(pico_elements_list, function(pico_item) {

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
  pico_table_list <- lapply(pico_elements_list, function(element) element$table)

  # Define selected_studies as a reactiveVal
  filter_studies_remove <- reactiveVal(included_with_metadata)
  filter_studies_edit <- reactiveVal(included_with_metadata)


  observeEvent(input$submit_filters_edit, {

    # If number of pico dataframes for dropdowns is > 0 then...
    if (length(pico_table_list) > 0) {
      selected_studies <- included_with_metadata
      for (i in (1:length(pico_table_list))){

        # Loop through each dataframe and filter
        new_table <- pico_table_list[[i]] %>%
          filter(name %in% isolate(pico_element_list[[i]]())) %>%
          select(uid)

        # Only keep the rows that have a matching "uid"
        selected_studies <- selected_studies %>%
          semi_join(new_table, by = "uid")

      }
      filter_studies_edit(selected_studies)

    }

  })

  observeEvent(input$submit_filters_remove, {

    # If number of pico dataframes for dropdowns is > 0 then...
    if (length(pico_table_list) > 0) {
      selected_studies <- included_with_metadata
      for (i in (1:length(pico_table_list))){

        # Loop through each dataframe and filter
        new_table <- pico_table_list[[i]] %>%
          filter(name %in% isolate(pico_element_list[[i]]())) %>%
          select(uid)

        # Only keep the rows that have a matching "uid"
        selected_studies <- selected_studies %>%
          semi_join(new_table, by = "uid")

      }
      filter_studies_remove(selected_studies)

    }

  })

  # Review study server -----
  credentials <- reactiveValues(authenticated = NULL)
  reviewer_name <- reactiveValues(name = NULL)
  add_study_refresh_trigger <- reactiveVal(0)
  remove_study_refresh_trigger <- reactiveVal(0)
  edit_study_refresh_trigger <- reactiveVal(0)
  edit_study_table_refresh_trigger <- reactiveVal(0)


  observeEvent(input$login_button, {


    credentials_df <- read.csv("www/credentials.csv", stringsAsFactors = FALSE)

    user_row <- credentials_df[credentials_df$user == input$username, ]

    if (nrow(user_row) == 1 && input$username == user_row$username && input$password == user_row$password) {

      credentials$authenticated <- TRUE
      reviewer_name$name <- input$username
    } else {
      credentials$authenticated <- FALSE

    }
  })


  output$review_study_ui <- renderUI({

    if (is.null(credentials$authenticated)) {

      tabItem(tabName = "review-study-tab",
              div(style = "color: #1A465F;",
                  h3("Login Required"),
                  textInput("username", "Username"),
                  passwordInput("password", "Password"),
                  actionButton("login_button", "Login",   icon = icon("right-to-bracket"))
              )
      )

    } else if (credentials$authenticated == FALSE) {

      tabItem(tabName = "review-study-tab",
              div(style = "color: #1A465F;",
                  h3("Login Required"),
                  textInput("username", "Username"),
                  passwordInput("password", "Password"),
                  actionButton("login_button", "Login"),
                  p("Incorrect password! Please try again")
              )
      )


    } else if (credentials$authenticated == TRUE){

      add_study_data <- dbReadTable(con, "add_study_test")
      remove_study_data <- dbReadTable(con, "remove_study_test")
      edit_study_data <- dbReadTable(con, "edit_study_test")



      tabItem(tabName = "review-study-tab",
              tags$style(HTML("
    .center-toast {
      position: fixed !important;
      top: 50% !important;
      left: 50% !important;
      transform: translate(-50%, -50%) !important;
      z-index: 9999;
    }

    .big-toast {
      font-size: 1.4rem;
      padding: 1.2rem 2rem;
    }

    .big-toast .toast-header {
      font-size: 1rem;
      font-weight: bold;
    }

    .big-toast .toast-body {
      font-size: 1.2rem;
    }

    .big-toast .bi {
      font-size: 1.4rem;
    }
  ")),
              fluidRow(
                column(9,

                       accordion(
                         id = "reviewAccordion",
                         accordionItem(
                           title = div(
                             icon("plus", class = "fa-2x"),
                             br(),
                             hr(),
                             p(paste0("Review suggested studies to be added. Number to be reviewed: ", nrow(add_study_review())))
                           ),

                           status = "info",
                           withSpinner(DTOutput("add_study_table")),
                           br(),
                           actionButton("review_add_btn",
                                        "Review",
                                        size = "sm",
                                        icon = icon("plus"),
                                        class = "btn-block",
                                        style = "background-color: #266080; color: white;margin-left: auto; margin-right: auto;",
                                        width = "33%",
                                        disabled = TRUE)

                         ),
                         accordionItem(
                           title = div(
                             icon("trash", class = "fa-2x"),
                             br(),
                             hr(),
                             p(paste0("Review suggested studies to be removed. Number to be reviewed: ", nrow(remove_study_review())))
                           ),

                           status = "primary",
                           withSpinner(DTOutput("remove_study_table")),
                           br(),
                           actionButton("review_remove_btn",
                                        "Review",
                                        size = "sm",
                                        icon = icon("trash"),
                                        class = "btn-block",
                                        style = "background-color: #266080; color: white;margin-left: auto; margin-right: auto;",
                                        width = "33%",
                                        disabled = TRUE)
                         ),
                         accordionItem(
                           title = div(
                             icon("edit", class = "fa-2x"),
                             br(),
                             hr(),
                             p(paste0("Review suggested changes to annotations. Number to be reviewed: ", nrow(edit_study_review_table())))
                           ),
                           status = "secondary",
                           withSpinner(DTOutput("edit_study_table")),
                           br(),
                           actionButton("review_edit_btn",
                                        "Review",
                                        size = "sm",
                                        icon = icon("edit"),
                                        class = "btn-block",
                                        style = "background-color: #266080; color: white;margin-left: auto; margin-right: auto;",
                                        width = "33%",
                                        disabled = TRUE)
                         )
                       ),

                       toast(
                         title = "Login Success",
                         body = "Welcome, authorised reviewer!",
                         options = list(
                           autohide = TRUE,
                           icon = "fas fa-home",
                           close = FALSE,
                           class = "center-toast big-toast"
                         )
                       )

                ),
                column(
                  width = 3,
                  box(
                    title = "Inclusion/Exclusion Criteria",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tags$div(
                      style = "font-family: KohinoorBangla, sans-serif !important;",

                      p(tags$strong("Inclusion Criteria:"), style = "color: #47B1A3;"),

                      tags$ul(
                        tags$li(style = "color: #47B1A3;", "Research which evaluates the effectiveness of an intervention on reproducibility or reproducibility-proxies (RPs)."),
                        tags$li(style = "color: #47B1A3;", "Research which suggests or promotes interventions to improve reproducibility or RPs."),
                        tags$li(style = "color: #47B1A3;", "Research which evaluates other aspects of the intervention suggested to improve reproducibility or RPs.")
                      ),

                      br(),

                      p(tags$strong("Exclusion Criteria:"), style = "color: #1A465F;"),

                      tags$ul(
                        tags$li(style = "color: #1A465F;", "Research which does not meet any of the three aims in the inclusion criteria."),
                        tags$li(style = "color: #1A465F;", "Conference abstracts, review articles, editorials, opinion pieces are excluded.")
                      ),

                      br(),

                      # Link to OSF protocol with logo
                      div(
                        class = "text-center",
                        tags$a(
                          href = "https://osf.io/2vufx",
                          target = "_blank",
                          tags$img(src = "osf_logo.png", height = "40px", alt = "OSF Logo"),
                          style = "display: inline-block;"
                        ),
                        br(),
                        tags$a(
                          href = "https://osf.io/2vufx",
                          "View Full Protocol on OSF",
                          target = "_blank",
                          style = "color: #1A465F; text-decoration: underline;"
                        )
                      )
                    ),
                    hr(),
                    actionButton("logout_button", "Logout", icon = icon("sign-out-alt"), class = "btn-danger")

                  )
                )
              )

      )

    }
  })

  observeEvent(input$logout_button, {
    credentials$authenticated <- NULL
    reviewer_name$name <- ""
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
  })


  add_study_review <- reactive({

    add_study_refresh_trigger()

    dbReadTable(con, "add_study_test") %>%
      left_join(dbReadTable(con, "feedback_review"), by = "suggestion_id") %>%
      filter(review_status == "incomplete")

  })

  remove_study_review <- reactive({

    remove_study_refresh_trigger()

    dbReadTable(con, "suggestions_table") %>%
      left_join(dbReadTable(con, "feedback_review"), by = c("id" = "suggestion_id")) %>%
      filter(type == "relevance") %>%
      filter(review_status == "incomplete")
  })

  edit_study_review <- reactive({

    edit_study_refresh_trigger()

    dbReadTable(con, "suggestions_table") %>%
      left_join(dbReadTable(con, "feedback_review"), by = c("id" = "suggestion_id")) %>%
      filter(!type == "relevance") %>%
      filter(review_status == "incomplete")

  })

  edit_study_review_table <- reactive({

    edit_study_table_refresh_trigger()

    dbReadTable(con, "suggestions_table") %>%
      left_join(dbReadTable(con, "feedback_review"), by = c("id" = "suggestion_id")) %>%
      filter(!type == "relevance") %>%
      filter(review_status == "incomplete") %>%
      distinct(uid)

  })


  output$add_study_table <- renderDT({

    datatable(
      add_study_review() %>%
        select(Title = title, DOI = doi),
      selection = "single",
      options = list(
        pageLength = 10,
        dom = 'tip',

        columnDefs = list(
          list(targets = 0, visible = FALSE)

        )
      ),
      escape = FALSE
    )
  })

  output$remove_study_table <- renderDT({

    datatable(
      remove_study_review() %>% left_join(included_with_metadata, by = "uid") %>%
        select(Title = title, DOI = doi),
      selection = "single",
      options = list(
        pageLength = 10,
        dom = 'tip',

        columnDefs = list(
          list(targets = 0, visible = FALSE)

        )
      ),
      escape = FALSE
    )
  })

  output$edit_study_table <- renderDT({

    datatable(
      edit_study_review_table() %>% left_join(included_with_metadata, by = "uid") %>%
        select(Title = title) %>% distinct(),
      selection = "single",
      options = list(
        pageLength = 10,
        dom = 'tip',

        columnDefs = list(
          list(targets = 0, visible = FALSE)

        )
      ),
      escape = FALSE
    )
  })


  # Enable "Add Study" button when row is selected, ignoreNULL allows this to work for de-selection
  observeEvent(input$add_study_table_rows_selected, ignoreNULL = FALSE, {

    if (length(input$add_study_table_rows_selected) > 0) {

      updateActionButton(session, "review_add_btn", disabled = FALSE)

    } else {

      updateActionButton(session, "review_add_btn", disabled = TRUE)

    }
  })

  # Capture selected study and show removal form
  observeEvent(input$review_add_btn, {


    row_selected <- input$add_study_table_rows_selected
    if (length(row_selected) == 0) return()

    study <- add_study_review()[row_selected, ] %>%
      mutate(link = paste0("https://doi.org/", doi))

    selected_study(study)


    showModal(modalDialog(
      tags$head(
        tags$style(HTML("

    #study_add_confirm {
      background-color: #89CB93 !important;
      color: white !important;
      border: none;
    }

    #study_add_confirm:enabled {
      background-color: #89CB93 !important;
    }

    #study_add_confirm:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }

    #study_add_reject {
      background-color: #FF0000 !important;
      color: white !important;
      border: none;
    }

    #study_add_reject:enabled {
      background-color: #FF0000 !important;
    }

    #study_add_reject:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }

  "))
      ),

      title = tags$div("Review Suggested Study to Add", style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;"),
      size = "xl",
      easyClose = FALSE,
      fade = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("study_add_reject", "Reject Suggestion", class = "btn-danger", disabled = TRUE),
        actionButton("study_add_confirm", "Confirm Suggestion", class = "btn-danger", disabled = TRUE)

      ),
      tags$div(style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;",

               fluidRow(
                 tags$head(
                   tags$style(HTML("
    .link-box {
      border: 1px solid #ccc;
      padding: 8px;
      background-color: #f8f9fa;
      width: 100%;
      border-radius: 8px;
      transition: background-color 0.3s ease, text-decoration 0.3s ease;
      text-decoration: none;
    }

    .link-box:hover {
      background-color: #e2e6ea;
      cursor: pointer;
      text-decoration: underline;
    }
  "))
                 ),
                 column(8,

                        tags$div(
                          tags$strong("Title:", style = "display: block; margin-bottom: 8px;"),
                          tags$div(
                            selected_study()$title,
                            style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                          )
                        )
                 ),

                 column(4,
                        tags$div(
                          tags$strong("DOI:", style = "display: block; margin-bottom: 8px;"),
                          tags$a(
                            href = selected_study()$link,
                            target = "_blank",
                            style = "text-decoration: none; color: inherit;",
                            tags$div(
                              class = "link-box",
                              tags$span(selected_study()$doi),
                              tags$i(class = "fas fa-link", style = "margin-left: 8px;")                            )
                          )
                        )
                 )),
               br(),
               fluidRow(
                 column(12,
                        tags$div(
                          tags$strong("Reason:", style = "display: block; margin-bottom: 8px;"),
                          tags$div(
                            selected_study()$reason,
                            style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                          )
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(12, textInput("add_study_review_notes", "Review Notes", placeholder = "required",
                                      width = "100%"))),
               br()
      )
    )
    )
  })

  # Define a reactive expression to check validity
  add_form_valid <- reactive({

    !is.null(input$add_study_review_notes) &&
      nchar(trimws(input$add_study_review_notes)) > 0

  })

  # Observe changes and update the button
  observe({
    updateActionButton(session, "study_add_confirm", disabled = !add_form_valid())
    updateActionButton(session, "study_add_reject", disabled = !add_form_valid())

  })

  # Observe Submit Button Click
  observeEvent(input$study_add_confirm, {

    withProgress(message = "Submitting review...", value = 0, {
      incProgress(0.2)

    date <- format(Sys.Date(), "%d%m%Y")
    study <- selected_study()
    name <- reviewer_name$name

    feedback_review_approve <- dbReadTable(con, "feedback_review") %>%
      filter(suggestion_id == study$suggestion_id) %>%
      mutate(review_status = "approve",
             reviewer_notes = input$add_study_review_notes)

    incProgress(0.4)

    feedback_review_update <- dbReadTable(con, "feedback_review") %>%
      filter(!suggestion_id == study$suggestion_id) %>%
      rbind(feedback_review_approve)

    dbWriteTable(con, "feedback_review", feedback_review_update, overwrite = T)

    incProgress(0.6)

    review_session <- data.frame(
      review_session_id = feedback_review_approve$review_session_id,
      reviewer_email = name,
      date_added = date
    )


    dbWriteTable(con, "reviewer_session", review_session, append = T)

    incProgress(0.8)

    # Update the datatable
    add_study_refresh_trigger(add_study_refresh_trigger() + 1)

    })

    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Review Complete - Study Added.<br><br>
          Changes will be made to the database and app in the next weekly update.<br><br>
          Thank you for reviewing!
        </div>'),
      easyClose = TRUE
    ))

  })

  # Observe Submit Button Click
  observeEvent(input$study_add_reject, {

    withProgress(message = "Submitting review...", value = 0, {
      incProgress(0.2)

    date <- format(Sys.Date(), "%d%m%Y")
    study <- selected_study()
    name <- reviewer_name$name

    feedback_review_approve <- dbReadTable(con, "feedback_review") %>%
      filter(suggestion_id == study$suggestion_id) %>%
      mutate(review_status = "reject",
             reviewer_notes = input$add_study_review_notes)

    feedback_review_update <- dbReadTable(con, "feedback_review") %>%
      filter(!suggestion_id == study$suggestion_id) %>%
      rbind(feedback_review_approve)

    incProgress(0.4)

    dbWriteTable(con, "feedback_review", feedback_review_update, overwrite = T)

    review_session <- data.frame(
      review_session_id = feedback_review_approve$review_session_id,
      reviewer_email = name,
      date_added = date
    )

    incProgress(0.6)

    dbWriteTable(con, "reviewer_session", review_session, append = T)

    # Update the datatable
    add_study_refresh_trigger(add_study_refresh_trigger() + 1)

    incProgress(0.8)

    })
    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Review Complete - Study Not Added.<br><br>
          Changes will be made to the database and app in the next weekly update.<br><br>
          Thank you for reviewing!
        </div>'),
      easyClose = TRUE
    ))

  })

  observeEvent(input$remove_study_table_rows_selected, ignoreNULL = FALSE, {

    if (length(input$remove_study_table_rows_selected) > 0) {

      updateActionButton(session, "review_remove_btn", disabled = FALSE)

    } else {

      updateActionButton(session, "review_remove_btn", disabled = TRUE)

    }
  })

  # Capture selected study and show removal form
  observeEvent(input$review_remove_btn, {

    row_selected <- input$remove_study_table_rows_selected
    if (length(row_selected) == 0) return()

    selected_study(remove_study_review()[row_selected, ])

    study_tiab <- included_with_metadata %>%
      filter(uid %in% selected_study()$uid) %>%
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url))

    showModal(modalDialog(
      tags$head(
        tags$style(HTML("

    #study_remove_confirm {
      background-color: #89CB93 !important;
      color: white !important;
      border: none;
    }

    #study_remove_confirm:enabled {
      background-color: #89CB93 !important;
    }

    #study_remove_confirm:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }

    #study_remove_reject {
      background-color: #266080 !important;
      color: white !important;
      border: none;
    }

    #study_remove_reject:enabled {
      background-color: #266080 !important;
    }

    #study_remove_reject:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }

  "))
      ),

      title = tags$div("Review Suggested Study to Remove", style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;"),
      size = "xl",
      easyClose = FALSE,
      fade = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("study_remove_reject", "Keep Study", class = "btn-danger", disabled = TRUE),
        actionButton("study_remove_confirm", "Remove Study", class = "btn-danger", disabled = FALSE)

      ),
      tags$div(style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;",

               fluidRow(
                 tags$head(
                   tags$style(HTML("
    .link-box {
      border: 1px solid #ccc;
      padding: 8px;
      background-color: #f8f9fa;
      width: 100%;
      border-radius: 8px;
      transition: background-color 0.3s ease, text-decoration 0.3s ease;
      text-decoration: none;
    }

    .link-box:hover {
      background-color: #e2e6ea;
      cursor: pointer;
      text-decoration: underline;
    }
  "))
                 ),
                 column(8,
                        tags$div(
                          tags$strong("Title:", style = "display: block; margin-bottom: 8px;"),
                          tags$div(
                            study_tiab$title,
                            style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                          )
                        )
                 ),

                 column(4,
                        tags$div(
                          tags$strong("DOI:", style = "display: block; margin-bottom: 8px;"),
                          tags$a(
                            href = study_tiab$link,
                            target = "_blank",
                            style = "text-decoration: none; color: inherit;",
                            tags$div(
                              class = "link-box",
                              tags$span(study_tiab$doi),
                              tags$i(class = "fas fa-link", style = "margin-left: 8px;")                            )
                          )
                        )
                 )),
               br(),
               fluidRow(
                 column(12,
                        tags$div(
                          tags$strong("Abstract:", style = "display: block; margin-bottom: 8px;"),
                          tags$div(
                            study_tiab$abstract,
                            style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                          )
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        tags$div(
                          tags$strong("Reason:", style = "display: block; margin-bottom: 8px;"),
                          tags$div(
                            selected_study()$reason,
                            style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                          )
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(12, textInput("remove_study_review_notes", "Review Notes", placeholder = "required",
                                      width = "100%"))),
               br()
      )
    )
    )
  })

  # Define a reactive expression to check validity
  remove_form_valid <- reactive({

    !is.null(input$remove_study_review_notes) &&
      nchar(trimws(input$remove_study_review_notes)) > 0

  })

  # Observe changes and update the button
  observe({
    updateActionButton(session, "study_remove_confirm", disabled = !remove_form_valid())
    updateActionButton(session, "study_remove_reject", disabled = !remove_form_valid())
  })



  # Observe Submit Button Click
  observeEvent(input$study_remove_confirm, {

    withProgress(message = "Submitting review...", value = 0, {
      incProgress(0.2)

    date <- format(Sys.Date(), "%d%m%Y")
    study <- selected_study()
    name <- reviewer_name$name

    feedback_review_agree <- dbReadTable(con, "feedback_review") %>%
      filter(suggestion_id == study$id) %>%
      mutate(review_status = "remove",
             reviewer_notes = input$remove_study_review_notes)

    feedback_review_update <- dbReadTable(con, "feedback_review") %>%
      filter(!suggestion_id == study$id) %>%
      rbind(feedback_review_agree)

    incProgress(0.4)

    dbWriteTable(con, "feedback_review", feedback_review_update, overwrite = T)

    review_session <- data.frame(
      review_session_id = feedback_review_agree$review_session_id,
      reviewer_email = name,
      date_added = date
    )
    incProgress(0.6)

    dbWriteTable(con, "reviewer_session", review_session, append = T)

    # Update the datatable
    remove_study_refresh_trigger(remove_study_refresh_trigger() + 1)
    incProgress(0.8)

    })
    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Review Complete - Study Removed.<br><br>
          Changes will be made to the database and app in the next weekly update.<br><br>
          Thank you for reviewing!
        </div>'),
      easyClose = TRUE
    ))

  })


  # Observe Submit Button Click
  observeEvent(input$study_remove_reject, {

    withProgress(message = "Submitting review...", value = 0, {

      incProgress(0.2)

    date <- format(Sys.Date(), "%d%m%Y")
    study <- selected_study()
    name <- reviewer_name$name

    feedback_review_disagree <- dbReadTable(con, "feedback_review") %>%
      filter(suggestion_id == study$id) %>%
      mutate(review_status = "keep in db",
             reviewer_notes = input$remove_study_review_notes)

    incProgress(0.4)

    feedback_review_update <- dbReadTable(con, "feedback_review") %>%
      filter(!suggestion_id == study$id) %>%
      rbind(feedback_review_disagree)

    dbWriteTable(con, "feedback_review", feedback_review_update, overwrite = T)

    incProgress(0.6)

    review_session <- data.frame(
      review_session_id = feedback_review_disagree$review_session_id,
      reviewer_email = name,
      date_added = date
    )

    dbWriteTable(con, "reviewer_session", review_session, append = T)
    incProgress(0.8)

    # Update the datatable
    remove_study_refresh_trigger(remove_study_refresh_trigger() + 1)

    })
    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Review Complete - Study Not Removed.<br><br>
          Changes will be made to the database and app in the next weekly update.<br><br>
          Thank you for reviewing!
        </div>'),
      easyClose = TRUE
    ))

  })

  # Enable "Add Study" button when row is selected, ignoreNULL allows this to work for de-selection
  observeEvent(input$edit_study_table_rows_selected, ignoreNULL = FALSE, {

    if (length(input$edit_study_table_rows_selected) > 0) {

      updateActionButton(session, "review_edit_btn", disabled = FALSE)

    } else {

      updateActionButton(session, "review_edit_btn", disabled = TRUE)

    }
  })


  predicted_labels <- reactiveVal(NULL)  # Store selected study row

  # Capture selected study and show removal form
  observeEvent(input$review_edit_btn, {

    row_selected <- input$edit_study_table_rows_selected
    if (length(row_selected) == 0) return()

    study_uid <- edit_study_review_table()[row_selected, ]
    review_study <- edit_study_review()

    study_check <- edit_study_review() %>%
      filter(uid %in% study_uid) %>%
      distinct(session_id) %>%
      pull(session_id)

    # Check for when a study has been annotated more than once
    if (length(study_check) > 1) {

      study <- edit_study_review() %>%
        filter(uid %in% study_uid) %>%
        filter(session_id == min(study_check))

    } else {

      study <- edit_study_review() %>%
      filter(uid %in% study_uid)
    }
    selected_study(study)

    predict_labels <- edit_table() %>%
      filter(uid %in% study_uid)

    predicted_labels(predict_labels)
    review_edit_study_modal(study, predict_labels)

  })

  review_edit_study_modal <- function(study, predict_labels) {

    intervention_result <- create_review_comparison_box_ui(predict_labels, study, "intervention")
    outcome_result <- create_review_comparison_box_ui(predict_labels, study, "outcome")
    discipline_result <- create_review_comparison_box_ui(predict_labels, study, "discipline")
    provider_result <- create_review_comparison_box_ui(predict_labels, study, "provider")
    target_population_result <- create_review_comparison_box_ui(predict_labels, study, "target_population")
    research_stage_result <- create_review_comparison_box_ui(predict_labels, study, "research_stage")
    target_pop_location_result <- create_review_comparison_box_ui(predict_labels, study, "target_pop_location")


    create_matrix_row <- function(row_label, predicted, suggested, changes_ui_id, agree_id, has_changed = FALSE) {

      fluidRow(
        column(2,
               tags$strong(row_label, style = "line-height: 80px; display: block;")
        ),
        column(3,
               tags$div(predicted,
                        style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; height: 80px; overflow-y: auto;")
        ),
        column(3,
               tags$div(suggested,
                        style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; height: 80px; overflow-y: auto;")
        ),
        column(3,
               uiOutput(changes_ui_id)
        ),
        column(1,
               div(
                 style = "display: flex; justify-content: center; align-items: center; height: 80px;",
                 switchInput(agree_id, onLabel = "Yes", offLabel = "No", onStatus = "success", disabled = !has_changed, value = !has_changed)
               )        )
      )
    }

    showModal(
      modalDialog(
        title = tags$div(
          "Review Suggested Annotation",
          style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;"
        ),
        size = "xl",
        easyClose = FALSE,
        fade = TRUE,
        footer = tagList(
          br(),
          modalButton("Close"),
          actionButton("review_submit_edit_btn", "Submit Annotation Review")
        ),
        tags$div(style = "font-family: KohinoorBangla, sans-serif !important; color: #1A465F;",
                 fluidRow(
                   tags$head(
                     tags$style(HTML("
          #review_submit_edit_btn {
            background-color: #1A465F !important;
            color: white !important;
            border: none;
          }
          #review_submit_edit_btn:enabled {
            background-color: #1A465F !important;
          }
          #review_submit_edit_btn:disabled {
            opacity: 0.6;
            cursor: not-allowed;
          }
    .link-box {
      border: 1px solid #ccc;
      padding: 8px;
      background-color: #f8f9fa;
      width: 100%;
      border-radius: 8px;
      transition: background-color 0.3s ease, text-decoration 0.3s ease;
      text-decoration: none;
    }

    .link-box:hover {
      background-color: #e2e6ea;
      cursor: pointer;
      text-decoration: underline;
    }
  "))
                   ),
                   column(8,
                          tags$div(
                            tags$strong("Title:", style = "display: block; margin-bottom: 8px;"),
                            tags$div(
                              predict_labels$title,
                              style = "border: 1px solid #ccc; padding: 8px; background-color: #f8f9fa; width: 100%; border-radius: 8px;"
                            )
                          )
                   ),

                   column(4,
                          tags$div(
                            tags$strong("DOI:", style = "display: block; margin-bottom: 8px;"),
                            tags$a(
                              href = predict_labels$link,
                              target = "_blank",
                              style = "text-decoration: none; color: inherit;",
                              tags$div(
                                class = "link-box",
                                tags$span(predict_labels$doi),
                                tags$i(class = "fas fa-link", style = "margin-left: 8px;")
                              )
                            )
                          )
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(2, ""),

                   column(3,
                          div(class = "text-center",
                              icon("robot", class = "fa-2x"),
                              br(),
                              tags$strong("LLM Prediction")

                          )
                   ),

                   column(3,
                          div(class = "text-center",
                              icon("user-secret", class = "fa-2x"),
                              br(),
                              tags$strong("User Suggestion")
                          )
                   ),

                   column(3,
                          div(class = "text-center",
                              icon("user-pen", class = "fa-2x"),
                              br(),
                              tags$strong("Changes")
                          )
                   ),

                   column(1,
                          div(class = "text-center",
                              icon("house-circle-check", class = "fa-2x"),
                              br(),
                              tags$strong("Approve")
                          )
                   )
                 ),

                 hr(),
                 # Data rows
                 create_matrix_row("Intervention", predict_labels$intervention,
                                   study %>% filter(type == "intervention") %>% pull(suggestion),
                                   "intervention_review_display", "agree_intervention",
                                   has_changed = intervention_result$has_changed),
                 hr(),
                 create_matrix_row("Outcome Measure", predict_labels$outcome,
                                   study %>% filter(type == "outcome") %>% pull(suggestion),
                                   "outcome_review_display", "agree_outcome",
                                   has_changed = outcome_result$has_changed),
                 hr(),
                 create_matrix_row("Discipline", predict_labels$discipline,
                                   study %>% filter(type == "discipline") %>% pull(suggestion),
                                   "discipline_review_display", "agree_discipline",
                                   has_changed = discipline_result$has_changed),
                 hr(),
                 create_matrix_row("Intervention Provider", predict_labels$provider,
                                   study %>% filter(type == "provider") %>% pull(suggestion),
                                   "provider_review_display", "agree_provider",
                                   has_changed = provider_result$has_changed),
                 hr(),
                 create_matrix_row("Target Population", predict_labels$target_population,
                                   study %>% filter(type == "target_population") %>% pull(suggestion),
                                   "target_population_review_display", "agree_target_population",
                                   has_changed = target_population_result$has_changed),
                 hr(),
                 create_matrix_row("Research Stage", predict_labels$research_stage,
                                   study %>% filter(type == "research_stage") %>% pull(suggestion),
                                   "research_stage_review_display", "agree_research_stage",
                                   has_changed = research_stage_result$has_changed),
                 hr(),
                 create_matrix_row("Target Pop Location", predict_labels$target_pop_location,
                                   study %>% filter(type == "target_pop_location") %>% pull(suggestion),
                                   "target_pop_location_review_display", "agree_target_pop_location",
                                   has_changed = target_pop_location_result$has_changed)
        )
      )
    )
  }


  create_review_comparison_box_ui <- function(original_data, suggested_data, column) {


    original_split <- sort(trimws(unlist(strsplit(original_data[[column]], ";"))))
    original <- paste(original_split, collapse = "; ")

    #suggested_split <- sort(trimws(unlist(strsplit(suggested_data[[column]], ";"))))
    suggested_split <- sort(trimws(unlist(strsplit(suggested_data %>% filter(type == column) %>% pull(suggestion), ";"))))

    suggested <- paste(suggested_split, collapse = "; ")

    has_changed <- !setequal(original, suggested)

    ui <- tags$div(
      style = "
        padding: 12px;
        background-color: #ffffff;
        border: 1px solid #ccc;
        border-radius: 4px;
        width: 100%;
        font-family: KohinoorBangla, sans-serif !important;
        color: #1A465F;
      ",
      if (has_changed) {
        tagList(
          span("Was: ", style = "font-weight: bold; color: red;"),
          span(original, style = "color: red;"),
          tags$br(),
          span("Now: ", style = "font-weight: bold; color: #47B1A3;"),
          span(suggested, style = "color: #47B1A3;")
        )
      } else {
        span("No changes", style = "color: #47B1A3; font-weight: bold;")
      }
    )

    return(list(ui = ui, has_changed = has_changed))

  }



  output$intervention_review_display <- renderUI({

    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "intervention")
    result$ui
  })

  output$discipline_review_display <- renderUI({
    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "discipline")
    result$ui
  })

  output$outcome_review_display <- renderUI({
    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "outcome")
    result$ui
  })

  output$provider_review_display <- renderUI({
    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "provider")
    result$ui
  })

  output$target_population_review_display <- renderUI({
    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "target_population")
    result$ui
  })

  output$research_stage_review_display <- renderUI({
    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "research_stage")
    result$ui
  })

  output$target_pop_location_review_display <- renderUI({

    result <- create_review_comparison_box_ui(predicted_labels(), selected_study(), "target_pop_location")
    result$ui
  })

  observeEvent(input$review_submit_edit_btn, {


    withProgress(message = "Submitting annotation review...", value = 0, {

      incProgress(0.2)
      date <- format(Sys.Date(), "%d%m%Y")
      name <- reviewer_name$name

      suggested_changes <- selected_study()

      review_update <- function(element){

        update_id <- suggested_changes %>%
          filter(type == element) %>%
          pull(id)

        feedback_review <- dbReadTable(con, "feedback_review") %>%
          filter(suggestion_id == update_id) %>%
          mutate(review_status = ifelse(input[[paste0("agree_", element)]] == TRUE, "approve", "reject"),
                 reviewer_notes = "")

        return(feedback_review)

      }
      incProgress(0.4)

      # Elements to process
      elements <- c("intervention", "discipline", "outcome", "provider", "target_population", "research_stage", "target_pop_location")

      updates_list <- lapply(elements, review_update)
      full_update <- do.call(rbind, updates_list)

      update_feedback_review <- dbReadTable(con, "feedback_review") %>%
        filter(!suggestion_id %in% full_update$suggestion_id) %>%
        rbind(full_update)
      incProgress(0.6)

      dbWriteTable(con, "feedback_review", update_feedback_review, overwrite = T)

      reviewer_session_update <- data.frame(
        review_session_id = unique(full_update$review_session_id),
        reviewer_email = name,
        date_added = date
      )

      incProgress(0.8)

      dbWriteTable(con, "reviewer_session", reviewer_session_update, append = T)

      # Update the datatable
      edit_study_refresh_trigger(edit_study_refresh_trigger() + 1)
      edit_study_table_refresh_trigger(edit_study_table_refresh_trigger() + 1)
      incProgress(1)

    })

    # Confirmation message
    showModal(modalDialog(
      title = HTML('<i class="fa fa-thumbs-up fa-beat" style="color: #1A465F; margin-right: 10px;"></i> Success!'),
      fade = TRUE,
      HTML('<div style="color: #1A465F;">
          Review Complete.<br><br>
          Changes will be made to the database and app in the next weekly update.<br><br>
          Thank you for reviewing!
        </div>'),
      easyClose = TRUE
    ))

  })

}
# Run the application
shinyApp(ui = ui, server = server)

# suggestions <- dbReadTable(con, "suggestions_table")
#
# suggester_session <- dbReadTable(con, "suggester_session")
#
# feedback_review <- dbReadTable(con, "feedback_review")
#
# reviewer_session <- dbReadTable(con, "reviewer_session")




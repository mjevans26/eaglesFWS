#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
header <-  dashboardHeader(disable = TRUE)

sidebar <-  dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")),

  navbarPage(div(column(4,tags$a(href = "http://www.defenders.org", tags$img(src = "01_DOW_LOGO_COLOR_300-01.png", height = "80px"))), column(8, h4("Defenders of Widlife", br(), "Center for Conservation Innovation"))),
   tabPanel(h4("FWS Eagle Take Estimator"),id = "summary",
    fluidPage(
    fluidRow(
      column(2,br()),
      column(8,
             h4("Fish and Wildlife Services uses a Bayesian model to estimate the number of eagles likely to be killed by proposed wind projects.
               This approach combines prior information about eagle collision rates and exposure across wind farms, with
               observed estimates of eagle activity at proposed sites to estimate the likely number of fatalities.",br(),
                "Fatalities = Collision Rate * Exposure * Project Size")),
      column(2, br())
    ),
    fluidRow(
     column(4,
            h2("Collision Rate"),
            h4("Prior distributions represent the current knowledge about a system or parameter.  Priors are expressed as
               a statistical distribution, indicating the possible values and relative certainty a variable may take.  FWS
               has used the observed collision rates at wind energy sites to create a prior distribution for this part of the fatality equation.")),
     column(8,
            plotlyOutput("prior"),br())
     ),
    fluidRow(
      column(4,
             h2("In Action"),
             h4("Select one of the wind sites below and click 'Update Distributions' to see how eagle survey information at a given location
            is integrated with prior information about eagle exposure and collision rates to produce an estimate of fatality"),
             selectInput("sites", "Choose a Site", choices = c("", levels(Bay16$SITE)), selected = NULL),
             h4("Alternatively, enter your own survey information",br(), "(Note: the site selector must be empty)"),
             numericInput("time", label = "Eagle flight time (min)", value = mean(Bay16$FLIGHT_MIN), min = 0),
             numericInput("effort", label = "Survey Effort (km2*hr)", value = mean(Bay16$EFFORT), min = 0),
             actionButton("update", "Update Distributions")
             ),
      column(8, plotlyOutput("exposure"), br())
    ),
    fluidRow(
      column(4,
             h2("Estimating Eagle Fatality"),
             h4("Click the 'Calculate Fatalities' button to see the predicted fatalities for the currently selected site,
                or survey information.  Of particular interest is how the estimated values generated using prior information
                on exposure rates (purple), compare to estimates produced when only a given site's survey is considered."),
             actionButton("calculate", "Calculate Fatalities")
             ),
      column(8, plotlyOutput("fatal"))
    )
    )), fluid=TRUE
  ),
  br(),
 fluidRow(column(2),
  column(4, div(HTML('<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
    <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>
  <br />
  This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" rel="dct:type">work</span>
  by <a xmlns:cc="http://creativecommons.org/ns" href="http://defenders.org" property="cc:attributionName" rel="cc:attributionURL">Defenders of Wildlife</a>
  is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
  <br />'),
  style = "text-align: center")),
  column(1),
  column(2, div(HTML('<br /> Email questions or comments to <a href = "mailto:esa@defender.org"> esa@defenders.org </a>'), style = "text-align: center"))
 )
)

dashboardPage(header, sidebar, body, skin = "blue")


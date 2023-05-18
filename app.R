# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Introduce Sean", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Introducing_Sean_Burke")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Introducing Sean Burke"), # This should be the full name.
          p("This is a Shiny application to introduce Sean Burke."),
          h2("Instructions"),
          p("The instructions below provide guidence on how to navigate the app."),
          tags$ol(
            tags$li("Click on the 'Explore' button to learn about Sean."),
            tags$li("Click on the 'Game' button to go to the Game page."),
            tags$li("Answer the given multiple choice questions."),
            tags$li("Hit the 'Submit' button to receive score.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "explore1",
              label = "Explore!",
              size = "large",
              icon = icon("wpexplorer"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield  and Robert P. Carey, III.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 05/17/2023 by NJH.")
          )
        ),

        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          h2("About Sean Burke"),
          fluidPage(
            tabsetPanel(
              #First tab -
              tabPanel(
                title = "Basic Info",
                br(),
                h3("Information"),
                tags$figure(
                  align = "left",
                  tags$img(
                    src = "profile.png",
                    width = 200,
                    height = 200,
                    alt = "Picture of Sean"
                  ),
                  tags$img(
                    src = "piano.png",
                    width = 350,
                    height = 200,
                    alt = "picture of Piano"
                  )
                ),
                br(),
                p("Sean is an upcoming Second year at Penn State University 
                  majoring in Statistics. He is currently pursuing the Statistics 
                  and Computation route within the major. In his free time, Sean
                  often likes to play sports such as soccer and tennis. In 
                  addition to sports, he also enjoys playing piano. His favorite 
                  genre to play is Romantic Classcial.")
              ),
              tabPanel(
                title = "Data Visualization",
                br(),
                h3("Data Visualization"),
                p("[Insert Data Visualization]")
              ),
            )
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "challenge1",
              label = "Challenge!",
              size = "large",
              icon = icon("gears"),
              style = "default"
            )
          ),
        ),
      
        
   
            
        
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Quick Quiz"),
          p("The general intent of a Challenge page is to have the user take
            what they learned in an Exploration and apply that knowledge in new
            contexts/situations. In essence, to have them challenge their
            understanding by testing themselves."),
          p("What this page looks like will be up to you. Something you might
            consider is to re-create the tools of the Exploration page and then
            a list of questions for the user to then answer.")
        ),
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App will help you get to know Sean."
      )
    }
  )
  

  ### Explore button ----
  observeEvent(input$explore1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "explore"
    )
  })

  ### Challenge button ----
  observeEvent(input$challenge1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "challenge"
    )
  })
  
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

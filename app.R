# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

# Load additional dependencies and setup functions
# source("global.R")

# Activity data
activity <- data.frame(
  group = c("Gym", "Eating", "Sleep", "Piano", "Tennis", "TV", "Part-Time", "Research"),
  value = c(1.5,2.5,6,3,1,2,2.5,4)
)

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
            tags$li("Click on the 'Challenge' button to go to the Challenge page."),
            tags$li("Answer the given multiple choice questions."),
            tags$li("Hit the 'Submit' button to receive score.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToExplore",
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
                    alt = "Headshot of Sean Burke"
                  ),
               
                  tags$img(
                    src = "piano.png",
                    width = 350,
                    height = 200,
                    alt = "Picture of Sean on Piano"
                  )
                ),
                br(),
                p("Sean is an upcoming Second year at Penn State University 
                  majoring in Statistics. He is currently pursuing the Statistics 
                  and Computation route within the major. In his free time, Sean
                  often likes to play sports such as soccer and tennis. In 
                  addition to sports, he also enjoys playing piano. His favorite 
                  genre to play is Romantic Classical.")
              ),
              tabPanel(
                title = "Data Visualization",
                br(),
                h3("Data Visualization"),
                fluidRow(
                  column(
                    width = 4,
                    offset = 0,
                    wellPanel(
                      selectInput(
                        inputId = "plotType",
                        label = "Select a plot",
                        choices = c("Pie Chart", "Bar Chart")
                      ),
                      bsButton(
                        inputId = "createPlot",
                        label = "Plot!",
                        size = "large",
                        style = "default"
                      )
                    )
                  ),
                  column(
                    width = 8,
                    offset = 0,
                    plotOutput(outputId = "activityPlot")
                  )
                )
              ),
            )
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToChallenge",
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
          p("Answer the three multiple choice questions to the best of your ability!"),
          br(),
          fluidRow(
            column(
              width = 8,
              offset = 0,
              wellPanel(
                tags$figure(
                  align = "center",
                  tags$img(
                    src = "profile.png",
                    width = 200,
                    height = 200,
                    alt = "Headshot of Sean Burke"
                  )
                ),
                br(),
                p("Question?"),
                br(),
                checkboxGroupInput(
                  inputId = "answerChoice",
                  label = "Choose One",
                  choices = c("a","b","c")
                )
                
              )
            ),
          )
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
  observeEvent(
    eventExpr = input$goToExplore,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )
  
 ### Activity Pot ----
  observeEvent(
    eventExpr = input$createPlot,
    handlerExpr = {
      if (input$plotType == "Pie Chart") {
        output$activityPlot <- renderPlot(
          {
            ggplot(
              data = activity, 
              mapping = aes(x="", y=value, fill=group)
              ) +
            geom_bar(
              stat="identity", 
              width=1, 
              color="white"
              ) +
            coord_polar("y", start=0) +
            theme_void() + 
            scale_fill_manual(
              values = boastUtils::boastPalette
            )
          }
        )
      } else {
        output$activityPlot <- renderPlot(
          {
            ggplot(
              data = activity, 
              mapping = aes(x=group, y=value, fill = group)
              ) +
            geom_bar(
              stat="identity"
              ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
              )+
            scale_fill_manual(
              values = boastUtils::boastPalette
            )
          }
        )
      }
    }
  )
  
  ### Challenge button ----
  observeEvent(
    eventExpr = input$goToChallenge,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "challenge"
      )
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

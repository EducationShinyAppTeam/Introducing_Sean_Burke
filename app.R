# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

# Load additional dependencies and setup functions
# source("global.R")

# Activity data ----
activity <- data.frame(
  group = c("Gym", "Eating", "Sleep", "Piano", "Tennis", "TV", "Part-Time", "Research"),
  value = c(1.5,2.5,6,3,1,2,2.5,4)
)

# Define UI for App ----
ui <- list(
  ## app page ----
  dashboardPage(
    skin = "blue",
    ### app header ----
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
    ### Sidebar/left navigation menu ----
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
        #### Overview Page ----
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

        #### Explore Page ----
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
      
        
   
            
        
        #### Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Quick Quiz"),
          p("Answer the three multiple choice questions to the best of your ability!"),
          br(),
          fluidRow(
            column(
              width = 7,
              offset = 0,
              wellPanel(
                tabsetPanel(
                  id = "quiz",
                  type = "hidden",
                  tabPanel(
                    title = "First Question",
                    value = paste0("Q1"),
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "vis.png",
                        width = 300,
                        height = 200,
                        alt = "View of Visualization Tab"
                      )
                    ),
                    br(),
                    h3("Question 1"),
                    radioButtons(
                      inputId = "answerChoice1",
                      label = "Looking at the Data Visualization on the Explore 
                      page, what activity does Sean spend the most time on?",
                      br(),
                      choices = c("Tennis","Sleep","Piano")
                    )
                  ),
                  tabPanel(
                    title = "Second Question",
                    value = "Q2",
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "vis.png",
                        width = 300,
                        height = 200,
                        alt = "View of Visualization Tab"
                      )
                    ),
                    br(),
                    h3("Question 2"),
                    radioButtons(
                      inputId = "answerChoice2",
                      label = "Looking at the Data Visualization on the Explore 
                      page, what activity does Sean spend the second-to-least time on?",
                      br(),
                      choices = c("Gym","Tennis","Sleep")
                    )
                  ),
                  tabPanel(
                    title = "Third Question",
                    value = "Q3",
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "piano.png",
                        width = 300,
                        height = 200,
                        alt = "View of Piano"
                      )
                    ),
                    br(),
                    h3("Question 3"),
                    radioButtons(
                      inputId = "answerChoice3",
                      label = "According to the description on the Explore page,
                      what is Sean majoring in?",
                      br(),
                      choices = c("Mathematics","Piano Performance","Statistics")
                    )
                  )
                )
              )
            ),
            column(
              width = 5,
              offset = 0,
              div(
                style = "text-align: center;",
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                bsButton(
                  inputId = "nextPage",
                  label = "Next!",
                  size = "large",
                  style = "default"
                ),
                br(),
                br(),
                bsButton(
                  inputId = "prevPage",
                  label = "Previous!",
                  size = "large",
                  style = "default"
                 ),
                br(),
                br(),
                bsButton(
                  inputId = "submitQuiz",
                  label = "Submit!",
                  size = "large",
                  style = "default"
                )
                
              ),
              
            )
          )
        ),
        
        #### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield., N. J. (2023). boastUtils: BOAST utilities.
            (v0.1.11.2). [R Package]. Available from
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create dashboards
            with 'Shiny'. (v0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Availble from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            (v3.4.2). [R Package]. New York:Springer-Verlag. Available from
            https://ggplot2.tidyverse.org"
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
              values = boastUtils::boastPalette,
              guide = "none"
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
  
  # Current Question Counter----
  currentQuestion <- reactiveVal(1)
  
  ### Next Button ----
  
  observeEvent(
    eventExpr = input$nextPage,
    handlerExpr = {
      if (currentQuestion() != 3) {
        currentQuestion(currentQuestion()+1)
        updateTabsetPanel(
          session = session,
          inputId = "quiz",
          selected = paste0("Q",currentQuestion())
        )
      }
    }
  )
  
  ### Previous Button ----
  
  observeEvent(
    eventExpr = input$prevPage,
    handlerExpr = {
      if (currentQuestion() != 1) {
        currentQuestion(currentQuestion()-1)
        updateTabsetPanel(
          session = session,
          inputId = "quiz",
          selected = paste0("Q",currentQuestion())
        )
      }
    }
  )
  
  #Current Score for the Quiz----
  currentScore <- reactiveVal(0)
  
  ### Submit Button ----
  observeEvent(
    eventExpr = input$submitQuiz,
    handlerExpr = {
      if (!is.null(input$answerChoice1) && input$answerChoice1 == "Sleep") {
        currentScore(currentScore() + 1)
      }
      if (!is.null(input$answerChoice2) && input$answerChoice2 == "Gym") {
        currentScore(currentScore() + 1)
      }
      if (!is.null(input$answerChoice3) && input$answerChoice3 == "Statistics") {
        currentScore(currentScore() + 1)
      }
      
      sendSweetAlert(
        session = session,
        type = "success",
        title = "Quiz Complete!",
        text = paste0("Your Score: ", currentScore(), "/3")
      )
      
      currentScore(currentScore() - currentScore())
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

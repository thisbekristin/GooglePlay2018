library(DT)
library(shinydashboard)


shinyUI(dashboardPage(skin = "green",
  dashboardHeader(title = "Google Play Store EDA"),
  dashboardSidebar(
    sidebarUserPanel("Android Apps Analysis",
                     image = "https://i-cdn.phonearena.com/images/article/114429-two_lead/The-Google-app-hits-an-impressive-milestone-in-the-Play-Store-only-reached-by-two-other-Android-apps.jpg"),
    sidebarMenu(
      menuItem("Background", tabName = "background", icon = icon("question-circle")),
      menuItem("Overview and Ranking", tabName = "overview", icon= icon("chart-line")),
      menuItem("Category Details", tabName = "category", icon = icon("chart-bar")),
      menuItem("Data", tabName = "data", icon = icon("database"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "background",
              h2("At a Glance"),
              fluidRow(
                infoBoxOutput("totalapps"),
                infoBoxOutput("uniquecat"),
                infoBoxOutput("uniquegenre")
              ), 
              fluidRow(
                box(
                  h2("Google Play Store Background"),
                  hr(),
                  h4( "Google Play Store consists of numerous applications within varying categories.
          As a digital distribution service for Android, users have access to a number of 
          family, gaming, and tool apps, to name a few. There are approximately 2.7 million applications available
          as of February 2017. The 2018 scraped", 
          shiny::a(href= 'https://www.kaggle.com/lava18/google-play-store-apps?select=googleplaystore.csv', "data"),
          "within this dashboard reveals a sample of the state of the Google Play Store. Further
          exploration and analysis can be done to determine which categories contain the most or least amount of apps, installations,
          rating distributions, and more."),
                  h2("Future Work"),
                  h4("If given more time and data, I would like to explore what characteristics of an App lead to a higher number of Installs.\n
                     Based on the correlation matrix, high ratings, price, and number of reviews don't necessarily determine the number of Installs 
                     and vice versa.\n Since most apps are Free, revenue may be driven by other features of an App not given in the dataset. Therefore, I would like
                     to explore what leads to an increased number of Installs per Category, which can potentially lead to factors that increase revenue.\n
                     Further data exploration may involve recommender systems, which may help with customer segmentation and identifying which users 
                     are likely to download what kind of Apps. In addition, evaluating App reviews can help determine what features of an App per 
                     Category a particular user likes.
                     "),
                  hr(),
                 
          
          ),
              uiOutput("picture")
          )
      ),
      tabItem(tabName = "overview",
              h2("General Insights"),
              fluidRow(
                column(width = 6,
                       plotOutput("correlation")),
                column(width = 6,
                       plotOutput("ratinghist"))
                ),
              br(),
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         tabPanel("Top 10 Made", plotOutput("topcat")),
                         tabPanel("Bottom 10  Made", plotOutput("bottomcat")),
                         tabPanel("Top 10 Installs", plotOutput("topinstalls")),
                         tabPanel("Bottom 10 Installs", plotOutput("bottominstalls")),
                         tabPanel("Top Made and Installed", tableOutput("topboth"))
                       )
                ))
              ),
      tabItem(tabName = 'category',
              fluidRow(
                sidebarLayout(
                  sidebarPanel(width = 2,
                               checkboxGroupInput(inputId = "category",
                                                  label = h3("Select Categories:"),
                                                  choices = sort(unique(df$Category)),
                                                  selected = c("FAMILY","GAME","TOOLS"))
                  ),
                  mainPanel(width = 10,
                    infoBoxOutput("averate"),
                    infoBoxOutput("totcatinstalls"),
                    infoBoxOutput("medprice"),
                    br(),
                    tabsetPanel(
                      tabPanel("Free or Paid", plotOutput("categoryinfo")),
                      tabPanel("Ratings", plotOutput("categoryrating")),
                      tabPanel("Content Rating", plotOutput("categorycontent")),
                      tabPanel("Total Installs", plotOutput("categoryinstalls")),
                      tabPanel("Total Genres", plotOutput("genres"))
                    ),
                    br(),
                    tabsetPanel(
                      tabPanel("Most Installed", tableOutput("topinstall")),
                      tabPanel("Highest Rated", tableOutput("toprated"))
                    )
      )))),
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"),
                           width = 12)))
  ))))




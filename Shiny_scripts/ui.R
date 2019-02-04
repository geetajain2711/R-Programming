#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h5("use case - embed a pdf user guide in the app - embed as a local pdf or from web URL")
    ),
  
  # Application title
  titlePanel("Team 1: Exploration of skills before and after program"),
  
  # Sidebar with a slider input for number of bins 
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Reference", 
               tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                           src="Users/geetajain2711/Desktop/Hult/MBAN/TextAnalytics/project_final/PPT/Slide.PNG")),
      tabPanel("Summary"),
      tabPanel("Plot")
      tabPanel("Skills Before Program", plotOutput("skills_prior",  width = "100%", height = "500px")),
      tabPanel("Skills Acquired", plotOutput("skills_after",  width = "100%", height = "500px")),
      tabPanel("Internships Responsibilities", plotOutput("internship",  width = "100%", height = "500px")),
      tabPanel("Job Responsibilities", plotOutput("job",  width = "100%", height = "500px")),
      tabPanel("Word Cloud from people with job offer", plotOutput("jobyes",  width = "200%", height = "500px")),
      tabPanel("Word Cloud from people without job offer", plotOutput("jobno",  width = "250%", height = "500px")),
      tabPanel("Sentiment before program", plotOutput("sentimentbefore",  width = "100%", height = "1000px")),
      tabPanel("Sentiment after program", plotOutput("sentimentafter",  width = "100%", height = "1000px"))
      
    )
    
  )
))
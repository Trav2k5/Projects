#
# SDS 313 Shiny App Example - Films with variable choice
#

library(shiny)
library(scales)
library(tidyverse)
library(stringr)

#read in data
presals <- read_csv("ds_salaries.csv")
#change experience level abbreviations to full name
presals$experience_level[presals$experience_level=="EN"] <- 'Entry-level'
presals$experience_level[presals$experience_level=="MI"] <- 'Mid-level'
presals$experience_level[presals$experience_level=="SE"] <- 'Senior-level'
presals$experience_level[presals$experience_level=="EX"] <- 'Executive-level'
presals$company_size[presals$company_size=="S"] <- 'Small'
presals$company_size[presals$company_size=="M"] <- 'Medium'
presals$company_size[presals$company_size=="L"] <- 'Large'


#Create a new variable that is whether they live in the country their company is located in
presals$live_in_same[presals$employee_residence == presals$company_location] <- "Yes"
presals$live_in_same[presals$employee_residence != presals$company_location] <- "No"

#Make a subset of just the variables of interest
sals <- subset(presals, select = c(experience_level, remote_ratio, live_in_same, salary_in_usd, company_size, job_title))
common_positions <- subset(sals, sals$job_title == "Data Engineer" | sals$job_title == "Data Scientist" | sals$job_title == "Data Analyst")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Graphs for Data Science Job Salaries"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel("Choose 1 variable or 1 numeric and 1 categorical variable",
      
      #Select box for variable:
      selectInput("selectvar1", label = "Categorical",
                  choices=list("No Categorical Variable"=0,"Experience Level"="experience_level", "Remote Ratio"="remote_ratio", "Company Size"="company_size", "3 Most Common Job Titles"="job_title", "Live in same country as company" = "live_in_same"), 
                  selected = 0),
      
      selectInput("selectvar2", label = "Numeric",
                  choices=list("No Numeric Variable"= 0,"Salaries (Only works if no categorical variable is chosen)"=1,"Average Salary (Only works if a categorical variable is also chosen)"=2), 
                  selected = 0),
      
      # Slider input for number of bins
      sliderInput("bins",
                  "Number of bins:",min = 1,max = 50,value = 30),
      
      #option to show mean
      checkboxInput("Flip", label="Flip x and y axis", value=FALSE),
      
      sliderInput("Range","Only show salaries between",min = 1,max = 700000,value = c(0, 700000)),
      
      selectInput("Appearance", label = "Graph Color", choices = list("blue"= 'blue', "orange"='orange', "green" = 'green', "turquoise" = 'turquoise'),selected = 2),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      p("Description (Only works for single variable graphs): "),
      tableOutput("describe1"),
      textOutput("describe2"),
      img(src='moneyimage.png', align = "right", height="93%", width="93%"),
      print("Image obtained from: https://www.computerworld.com/article/3662128/it-salaries-arent-keeping-up-with-inflation-but-that-may-soon-change.html")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  comptomeansal <- function(t, d, c, e = FALSE){
    
    meansals <- group_by(t, t[d])
    l <- summarize(meansals, mean = mean(salary_in_usd))
    if(!input$Flip){
      print(ggplot(l) + geom_bar(aes(x = as.factor(l[[d]]), y = mean), stat = "identity", fill = c) + scale_y_continuous(labels = label_comma()) + labs(title = paste("Mean Salaries by", d), x = d, y = "Mean Salary in USD"))
      
    }
    else{
      print(ggplot(l) + geom_bar(aes(x = as.factor(l[[d]]), y = mean), stat = "identity", fill = c) + scale_y_continuous(labels = label_comma()) + labs(title = paste("Mean Salaries by", d), x = d, y = "Mean Salary in USD")) + coord_flip()
      
    }
  }
  singlecatvar <- function(t, d, c, e = FALSE){
    gg_sals <- ggplot(t)
    if(!input$Flip){
      gg_sals + geom_bar(aes(as.factor(t[[d]])), bins = input$bins, fill = c) + labs(title = paste("Graph of", input$selectvar1), x = input$selectvar1)
    }
    else{
      gg_sals + geom_bar(aes(as.factor(t[[d]])), bins = input$bins, fill = c) + labs(title = paste("Graph of", input$selectvar1), x = input$selectvar1) + coord_flip()
    }
  }
  
  output$distPlot <- renderPlot({
    
    if(input$selectvar1 == 0 & input$selectvar2 == 1){
      gg_sals <- ggplot(sals)
      if(!input$Flip){
        gg_sals + geom_histogram(aes(salary_in_usd), bins = input$bins, fill = input$Appearance) + lims(x = c(input$Range[1], input$Range[2])) + labs(title = "Graph of Salaries", x = "Salary in USD")
      }
      else{
        gg_sals + geom_histogram(aes(salary_in_usd), bins = input$bins, fill = input$Appearance) + lims(x = c(input$Range[1], input$Range[2])) + labs(title = "Graph of Salaries", x = "Salary in USD") + coord_flip()
      }
    }
    else if(input$selectvar1 != 0 & input$selectvar2 == 2 & input$selectvar1 != "job_title"){
      sals <- sals[sals$salary_in_usd >= input$Range[1] & sals$salary_in_usd <= input$Range[2],]
      gg_sals <- ggplot(sals)
      comptomeansal(sals, input$selectvar1, input$Appearance)
    }
    else if(input$selectvar1 != 0 & input$selectvar2 == 0 & input$selectvar1 != "job_title"){
      sals <- sals[sals$salary_in_usd >= input$Range[1] & sals$salary_in_usd <= input$Range[2],]
      gg_sals <- ggplot(sals)
      singlecatvar(sals, input$selectvar1, input$Appearance)
    }
    else if(input$selectvar1 == "job_title"){
      sals <- sals[sals$salary_in_usd >= input$Range[1] & sals$salary_in_usd <= input$Range[2],]
      gg_sals <- ggplot(common_positions)
      comptomeansal(common_positions, "job_title", input$Appearance)
    }

  })
  output$describe1 <- renderTable({
    if(input$selectvar1 != 0 & input$selectvar2 == 0){
      n <- ncol(sals)
      o <- 0
      if(input$selectvar1 == "job_title"){
        o <- common_positions[,6]
      }
      else{
        for(i in 1:n){
          if(colnames(sals[i]) == input$selectvar1){
            o <- sals[,i]
          }
        }
      }
      table(o)
    }
  })
  
  output$describe2 <- renderText({
    if(input$selectvar1 == 0 & input$selectvar2 == 1){
      paste("The standard deviation for salaries is", round(sd(sals$salary_in_usd), digits = 2), "and the mean is", round(mean(sals$salary_in_usd), digits = 2))
    }
  })
    
  }
  

# Run the application 
shinyApp(ui = ui, server = server)


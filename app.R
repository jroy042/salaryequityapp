## TO DO ###
## ADD Percent Women
## ADD Carnegie Classifications
## ADD Contract Data (Start with most recent and then go back.


library(shiny)
## ui.R ##
library(shinydashboard)


#You can quickly view it at the R console by using the shinyApp() function. (You can also use this code as a single-file app).

## app.R ##
library(DT)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Salary Equity Charts", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Salary Equity Tables", tabName = "widgets", icon = icon("th")),
    menuItem("Percent Women across Rank", tabName = "widgets2", icon = icon("dashboard"))
     )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
            # Boxes need to be put in a row (or column)
       fluidRow(box(width=5, background="light-blue",strong("Salary Equity"),p(" This measure is computed by the average salary of women divided by the average salary of men times 100 for each rank. 
                            Salary Equity below 100 indicates that women are paid, on average, less than men for that specific rank with the salary equity measure being the amount of cents earned per dollar of salary for men. 
                                    All salaries displayed are calculated according to a 9 month basis. Institutions who report less than 15 men or less than 15 women are excluded from the display. 
                                    Salaries which were less than 15,000 for a given rank are excluded and represent less than .5% of the original data."))),
       fluidRow(
             box(inputPanel(selectInput("state", "State:", choices = 
                                   c("Alabama", "Alaska", "Arizona","Arkansas", 
                                     "California", "Minnesota","Colorado","Connecticut",         
                                     "Delaware","District of Columbia","Florida","Georgia",             
                                     "Hawaii","Idaho","Illinois","Indiana",             
                                     "Iowa","Kansas","Kentucky","Louisiana",           
                                     "Maine","Maryland","Massachusetts","Michigan",            
                                     "Mississippi","Missouri","Montana","Nebraska",            
                                     "Nevada","New Hampshire","New Jersey","New Mexico",          
                                     "New York","North Carolina","North Dakota","Ohio",                
                                     "Oklahoma","Oregon","Pennsylvania","Rhode Island",        
                                     "South Carolina","South Dakota","Tennessee","Texas",               
                                     "Utah","Vermont" ,"Virginia","Washington",          
                                     "West Virginia","Wisconsin","Wyoming" )),
                     selectInput("control","Control:",choices =c("Public","Private not-for-profit","Private for-profit"))))),
            fluidRow(box(plotOutput("plot1", height = 250)))
       ),

      tabItem(tabName = "widgets",
              fluidRow(box(width=5, background="light-blue",strong("Salary Equity"),p(" This measure is computed by the average salary of women divided by the average salary of men times 100 for each rank. 
                            Salary Equity below 100 indicates that women are paid, on average, less than men for that specific rank. The salary equity measure is the amount of cents women earned per dollar of salary for men. 
                                    All salaries displayed are calculated according to a 9 month basis. Institutions who report less than 15 men or less than 15 women are excluded from the display. 
                                    Salaries which were less than 15,000 for a given rank are excluded and represent less than .5% of the original data."))),
              fluidRow(h2("Distribution Table of Salaries by Category and Rank"),
                box(inputPanel(selectInput("state", "State:", choices =
                                             c("Alabama", "Alaska", "Arizona","Arkansas",
                                               "California", "Minnesota","Colorado","Connecticut",
                                               "Delaware","District of Columbia","Florida","Georgia",
                                               "Hawaii","Idaho","Illinois","Indiana",
                                               "Iowa","Kansas","Kentucky","Louisiana",
                                               "Maine","Maryland","Massachusetts","Michigan",
                                               "Mississippi","Missouri","Montana","Nebraska",
                                               "Nevada","New Hampshire","New Jersey","New Mexico",
                                               "New York","North Carolina","North Dakota","Ohio",
                                               "Oklahoma","Oregon","Pennsylvania","Rhode Island",
                                               "South Carolina","South Dakota","Tennessee","Texas",
                                               "Utah","Vermont" ,"Virginia","Washington",
                                               "West Virginia","Wisconsin","Wyoming" )),
                               selectInput("control","Control:",choices =c("Public","Private not-for-profit","Private for-profit")))),

              fluidRow(column(7,dataTableOutput('dto')))
              )
      ),
      tabItem(tabName = "widgets2",
              # Boxes need to be put in a row (or column)
              fluidRow(box(width=5, background="light-blue",strong("Percent Women by Rank (Full Time)"),p("Institutions who report less than 15 men or less than 15 women are excluded from the display."))),
       fluidRow(
             box(inputPanel(selectInput("state", "State:", choices = 
                                   c("Alabama", "Alaska", "Arizona","Arkansas", 
                                     "California", "Minnesota","Colorado","Connecticut",         
                                     "Delaware","District of Columbia","Florida","Georgia",             
                                     "Hawaii","Idaho","Illinois","Indiana",             
                                     "Iowa","Kansas","Kentucky","Louisiana",           
                                     "Maine","Maryland","Massachusetts","Michigan",            
                                     "Mississippi","Missouri","Montana","Nebraska",            
                                     "Nevada","New Hampshire","New Jersey","New Mexico",          
                                     "New York","North Carolina","North Dakota","Ohio",                
                                     "Oklahoma","Oregon","Pennsylvania","Rhode Island",        
                                     "South Carolina","South Dakota","Tennessee","Texas",               
                                     "Utah","Vermont" ,"Virginia","Washington",          
                                     "West Virginia","Wisconsin","Wyoming" )),
                     selectInput("control","Control:",choices =c("Public","Private not-for-profit","Private for-profit"))))),
            fluidRow(box(plotOutput("plot2", height = 250)))
       )

    )#Tab Items End
  )
)

server <- function(input, output) { 
  library(ggplot2)
  library(tidyverse)
  
  equity = readRDS("newSal2.RDS")
  
  output$plot1 <- renderPlot({
    plName = paste0( input$control," Institutions in ", input$state)
    
    plotData = equity %>% filter(institutional_control == input$control & state==input$state) %>% 
    group_by(Year,rank) %>% summarise(AvgEquity = weighted.mean(equity,total_faculty,na.rm=TRUE),
                                              sd_equity = sd(equity, na.rm=TRUE), OverallPercentWom = weighted.mean(PercentWomen,total_faculty,na.rm = TRUE),
                                        N=length(unique(unitid)))
  
  
    ggplot(plotData,aes(x=Year, y=AvgEquity, color=rank, group=rank, shape=rank))+
    geom_point() + geom_line() + theme_bw() + ggtitle(plName, subtitle = "2005 to 2016 Salary Equity by Rank") +
    xlab("Year") + ylab("Salary Equity by Gender") + 
    geom_hline(yintercept =100, color="red") + guides(fill=guide_legend(title="Rank (Full Time)"))
   
  })
  output$plot2 <- renderPlot({
    plName = paste0( input$control," Institutions in ", input$state)
    
    plotData = equity %>% filter(institutional_control == input$control & state==input$state) %>% 
      group_by(Year,rank) %>% summarise(AvgEquity = weighted.mean(equity,total_faculty,na.rm=TRUE),
                                        sd_equity = sd(equity, na.rm=TRUE), OverallPercentWom = weighted.mean(PercentWomen,total_faculty,na.rm = TRUE),
                                        N=length(unique(unitid)))
    
    
    ggplot(plotData,aes(x=Year, y=OverallPercentWom, color=rank, group=rank, shape=rank))+
      geom_point() + geom_line() + theme_bw() + ggtitle(plName, subtitle = "2005 to 2016 Percent Women") +
      xlab("Year") + ylab("Percent Women by Faculty Rank") + 
      geom_hline(yintercept =50, color="red") + scale_fill_manual("Rank (Full Time)")
    
  })
  output$dto <- renderDataTable(equity %>% filter(institutional_control==input$control & state == input$state)%>%
                                select(Institution, Year, rank, num_women,avg_women,num_men,avg_men, equity),
                                  extensions = 'Buttons', 
                                options = list(dom = 'Blfrtip',
                                               buttons = list('copy', list(extend='csv',filename="StateGenderEquity"), 
                                                              list(extend='excel', filename="StateGenderEquity"), 
                                                              list(extend='pdf',filename="StateGenderEquity",orientation = 'landscape'), 'print'))
  )
  
  output$instructions <- renderText("Gender Equity is computed by the average salary of women divided by the average salary of men times 100 for each rank
                                    All salaries displayed are calculated according to a 9 month basis. Institutions who report less than 15 men or less than 15 women are excluded from the display. 
                                    Salaries which were less than 15,000 for a given rank are excluded (and represent less than .5% of the original data.")
                                
                                

  
}

shinyApp(ui, server)

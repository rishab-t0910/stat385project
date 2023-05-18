library(shiny)
library(tidyverse)

data = read_csv("cricket.csv")

team_list = unique(c(data$team1, data$team2))

ui <- navbarPage(
  title = 'ODI Games', 
  tabPanel(title = 'Input/Vizualization',
           titlePanel(title = 'Men\'s ODI Win Data'), 
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = 'team', 
                 label = 'Team:', 
                 choices = sort(unique(c(data$team1, data$team2)))
               ), 
               
               dateRangeInput(
                 inputId = 'date',
                 label = 'Date Range',
                 start = min(data$match_date),
                 end =  max(data$match_date),
                 min = min(data$match_date),
                 max = max(data$match_date),
                 startview = 'year',
                 format = 'mm-dd-yyyy'
               ),
               
               checkboxInput(inputId = 'select_ground', label = 'Select Ground', 
                             value = FALSE),
               
               selectInput(
                 inputId = 'ground', 
                 label = 'Country of the Ground:', 
                 choices = sort(unique(data$ground))
               ), 
               
             ),
             mainPanel(plotOutput('plot'))
           ),
  ),
  tabPanel(title = 'Table', dataTableOutput('table')), 
  tabPanel(title = 'About', includeMarkdown('about.Rmd'))
)

server <- function(input, output) {
  
  team_filter = reactive({
    data |>
      filter(team1 == input$team | team2 == input$team)
  }) 
  
  observeEvent(eventExpr = input$team, 
               handlerExpr = {
                 updateSelectInput(inputId = 'ground', 
                                   choices = sort(unique(team_filter()$ground)))
               }
  )
  
  output$plot <- renderPlot({
    
    if(input$select_ground) {
      data = data |>
        filter(ground == input$ground)
    }
    
    data |>
      filter(team1 == input$team | team2 == input$team )|>
      filter(match_date >=  input$date[1])|>
      filter(match_date <= input$date[2]) |>
      filter(winner == input$team) |>
      mutate(
        teamA = input$team,
        teamB = ifelse(team1 == input$team, team2, team1)
      ) |>
      group_by(teamB) |>
      summarise(Count = n()) |>
      ggplot() + 
      aes(x = teamB, y = Count , fill = teamB) |>
      geom_bar(stat = 'identity') +
      xlab("Opposition") + 
      ylab("Wins") + 
      guides(fill=guide_legend(title="Opposition")) + 
      theme_bw()
  })
  
  output$table <- renderDataTable({
    
    tab = team_filter() |>
      filter(match_date >=  input$date[1])|>
      filter(match_date <= input$date[2])
    
    if(input$select_ground) {
      tab = tab |>
        filter(ground == input$ground)
    }
    
    tab
    
  })
  
}

shinyApp(ui = ui, server = server)

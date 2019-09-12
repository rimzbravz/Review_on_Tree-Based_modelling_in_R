
#### R Shiny Tutorial ###

library(shiny)

server <- function(input, output, session){ } #The Server

#ui <- basicPage("This is a Real Shiny app") #The User Interface


ui <- fluidPage(
  
  titlePanel("App with simple layout"),
  
  sidebarLayout(
    
    sidebarPanel(
      "Sidebar"
    ), #end sidebarpanel
    
    mainPanel(
      "This is the main Panel"
    )# end mainpanel
  ) # end sidebar layout
  
  
  ) #The User Interface



shinyApp(ui = ui, server = server)


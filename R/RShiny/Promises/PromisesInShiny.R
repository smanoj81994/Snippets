# Loading required libraries
lib<-function(){
    library(shiny)
    library(dplyr)
    library(promises)
    library(future)
    library(shinydashboard)
    library(ggplot2)
    plan(multiprocess)
}
lib()
  

# Defining header, sidebar and body  
header<-dashboardHeader(title = "Promises & Future")
sidebar<-dashboardSidebar(actionButton("refresh_data","Click to load data :",width = "80%"))
# Body with two tabs containing simple plots
body<-dashboardBody(fluidPage(
    
    tabBox(id = "tabset1",
           width = "100%",
           height = "800px",
           tabPanel("fir",value=1,
                    plotOutput("plot2",width = "100%",height = "500px")
           ),
           tabPanel("sec",value=2,
                    plotOutput("plot1",width = "100%",height = "500px")
           )
   )
    
))

# Stiching up the ui  
ui<-dashboardPage(header,sidebar,body)

# Server side 
server<-shinyServer(function(input, output, session) {
    
    # Use this if you want to load based on the tab 
    # observeEvent(input$tabset1, {
    # print(input$tabset1)
    # if(input$tabset1=="1"){
    #   print("s")
    #   data<-future({
    #     print("started")
    #     Sys.sleep(5)
    #     print("ended")
    #     mt
    #   })
    #   #print(value(data)%>%head())
    #   dat<<-value(data)
    #   print(nrow(dat))
    # }
    # })
    
  # Creating a future object when clicked on the button
    data<-eventReactive(input$refresh_data, {
      print(input$tabset1)
      print("s")
      future({
        print("started")
        Sys.sleep(5)
        print("ended")
        mtcars
      })
      
    })
    
    
    # Directly plotting the dataframe : Expecting to see this load first while the data for other tab loads
    output$plot2 <- renderPlot({
      ggplot(mtcars,aes(x=mpg,y=cyl))+geom_line()
    })
    
    # Plot using future object : Expecting this would load and be ready when we go to the respective tab
    output$plot1 <- renderPlot({
      ggplot(value(data()),aes(x=mpg,y=cyl))+geom_line()
    })
    
  })

# Run the app
shinyApp(ui,server)

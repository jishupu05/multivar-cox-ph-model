library(shinythemes)
library(shinydashboard)
library(shiny)
library(survival)
library(data.table)
lung1=within(data = lung,{
  status=status-1
  sex=factor(sex,levels=c(1,2),labels = c(1,2))
  #ph.ecog=as.factor(ph.ecog)
  #ph.karno=as.factor(ph.karno)
  #pat.karno=as.factor(pat.karno)
  
})
lung1<-lung1[complete.cases(lung1),]
lung1=lung1[-1]
lung1
cox.model<-coxph(Surv(time,status)~.,data = lung1)
saveRDS(cox.model,"cox.model.rds")
cox.model<-readRDS("cox.model.rds")

ui<-fluidPage(theme = shinytheme("flatly"),
              title = titlePanel(title = "Multivariate Cox model",windowTitle = "Time independent cox ph model"),
              sidebarLayout( fluid = TRUE,
                             sidebarPanel = sidebarPanel(
                               tags$label(h3("Input parameters")),
                               numericInput("time",
                                            label = "Duration",
                                            min = 0,
                                            max = 500,
                                            value = 200),
                               numericInput("age",
                                            label = "Age",
                                            min=1,
                                            max = 100,
                                            value = 50),
                               selectInput("status",
                                           label = "Status of Event",
                                           choices = c("Died"=1,"Censored"=0)),
                               selectInput("sex",
                                           label = "Sex",
                                           choices = c("Male"=1,"Female"=2)),
                               sliderInput("ph.ecog",
                                           "ECOG performance",
                                           min = 0,
                                           max=2,
                                           value = 1),
                               sliderInput("ph.karno",
                                           label = "Karnofsky Performance",
                                           min=40,
                                           max=100,
                                           value = 60,
                                           step = 10),
                               sliderInput("pat.karno",
                                           label = "Karnofsky Performance by patient",
                                           min = 30,
                                           max = 100,
                                           value = 50,
                                           step = 10),
                               numericInput("meal.cal",
                                            "Calory per Meal",
                                            min = 0,
                                            max = 2000,
                                            value = 100),
                               numericInput("wt.loss",
                                            "Weight loss",
                                            min=-20,
                                            max=100,
                                            value = 0),
                               actionButton("submitbutton", "Go", class = "btn btn-primary")
                               
                             ),
                             mainPanel(
                               tags$label(h3("Status/Output")),
                               verbatimTextOutput("Contents"),
                               tableOutput("tabledata")
                               #plotOutput("Coxgraph")
                             )
                             
              )
              
)


server<-function(input,output,session){
  inpudataset<-reactive(
    {
      df<-data.frame(
        Survival<-c("time","age","status","sex","ph.ecog","ph.karno","pat.karno","meal.cal","wt.loss"),
        Value<-as.character(c(input$time,input$age,input$status,input$sex,input$ph.ecog,input$ph.karno,input$pat.karno,input$meal.cal,input$wt.loss)),
        stringsAsFactors = FALSE
      )
      input1<-transpose(df)
      write.table(input1,"input1.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      test <- read.csv(paste("input1", ".csv", sep=""), header = TRUE)
      test$sex<-factor(test$sex)
      Output <- data.frame(predicted_life=round(predict(cox.model,test,type="survival"), 4))
      print(Output)
      #plot<-plot(survfit(cox.model))      
      
      
    }
  )
  output$Contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  # Prediction results table
  output$tabledata <- renderTable({
    if(input$submitbutton>0) { 
      isolate(inpudataset()) 
    } 
  })
}


shinyApp(ui, server)

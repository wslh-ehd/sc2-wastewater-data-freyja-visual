library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(openxlsx)
library(plotly)
library(shinycssloaders)



load("InternalUseData.RData")


ui <-dashboardPage(
  
  
  dashboardHeader(),
  
  
  dashboardSidebar(  
    checkboxGroupInput("choice_sampletype", "Select a sample type: ", choices = levels(as.factor(freyja.raw$Sample.type)),  selected = "Control"),
    checkboxGroupInput("choice_QC_sample", "QC sample: ", choices = levels(as.factor(freyja.raw$QC_Sample)), selected = "qc_passed"),
    checkboxGroupInput("choice_QC_run", "QC run: ", choices = unique(freyja.raw$QC_Run), selected = "qc_passed"),
    pickerInput("choice_city", "Select a city: ", choices = levels(as.factor(freyja.raw$sites)), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search`=TRUE), selected = levels(as.factor(freyja.raw$sites))),
    pickerInput("choice_run", "Select a run: ", choices = levels(as.factor(freyja.raw$Run)), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search`=TRUE), selected = last.run),
    dateRangeInput("choice_DateRange", "Dates:", start = "2022-01-01", end = as.Date(Sys.Date(),"%Y-%m-%d"), format ="yyyy-mm-dd"),
    
    h3("Only for raw data"),
    pickerInput("choice_Variant1", "Select a variant group (i.e., covariants.org): ", choices = sort(levels(as.factor(freyja.raw$Lineage))), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search`=TRUE), selected = levels(as.factor(freyja.raw$Lineage))),
    pickerInput("choice_Variant2", "Select a Pango lineage (no alias): ", choices = sort(levels(as.factor(freyja.raw$Pango))), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search`=TRUE), selected = levels(as.factor(freyja.raw$Pango))),
    pickerInput("choice_Variant3", "Select a Pango lineage (raw freyja): ", choices = sort(unique(freyja.raw$OriginalLineages)), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search`=TRUE), selected = levels(as.factor(freyja.raw$OriginalLineages)))
    ),

  
  dashboardBody(
    withSpinner(plotlyOutput("freyja.summarized", height = 500), color="#c5050c"),
    hr(),
    withSpinner(plotlyOutput("freyja.raw", height = 500), color="#c5050c"),
    h4("Average of the selected variant(s) (%): ", textOutput("text.average", container = span))
    
    )
)







server <- function(input, output, session){
  

    freyja.summarized.select <- reactive({
      req(input$choice_city)
      req(input$choice_run)
      req(input$choice_sampletype)
      req(input$choice_QC_sample)
      req(input$choice_QC_run)
      req(input$choice_DateRange)
      
      freyja %>%
        filter(sites %in% input$choice_city) %>%
        filter(Run %in% input$choice_run) %>%
        filter(Sample.type %in% input$choice_sampletype) %>%
        filter(QC_Sample %in% input$choice_QC_sample) %>%
        filter(QC_Run %in% input$choice_QC_run) %>%
        filter(Date %in% c(input$choice_DateRange[1]:input$choice_DateRange[2]))
    })
    
    
    freyja.raw.select <- reactive({
      req(input$choice_city)
      req(input$choice_run)
      req(input$choice_sampletype)
      req(input$choice_QC_sample)
      req(input$choice_QC_run)
      req(input$choice_DateRange)
      req(input$choice_Variant1)
      req(input$choice_Variant2)
      req(input$choice_Variant3)
      
      freyja.raw %>%
        filter(sites %in% input$choice_city) %>%
        filter(Run %in% input$choice_run) %>%
        filter(Sample.type %in% input$choice_sampletype) %>%
        filter(QC_Sample %in% input$choice_QC_sample) %>%
        filter(QC_Run %in% input$choice_QC_run) %>%
        filter(Date %in% c(input$choice_DateRange[1]:input$choice_DateRange[2])) %>%
        filter(Lineage %in% input$choice_Variant1) %>%
        filter(Pango %in% input$choice_Variant2) %>%
        filter(OriginalLineages %in% input$choice_Variant3)
    })
    
    
    output$freyja.summarized <- renderPlotly({
      
      plot_ly(data = freyja.summarized.select(), x = ~Display, y = ~proportion*100, color = ~Lineage, type = "bar") %>% 
        layout(barmode = "stack", 
               xaxis = list(title = '', font.size = 1), 
               autosize = T,
               yaxis = list(title = 'Proportion (%)'), 
               legend = list(title=list(text='<b> Variants </b>')))
    })
      

    
    output$freyja.raw<- renderPlotly({
      
      plot_ly(data = freyja.raw.select(), x = ~Display, y = ~proportion, color = ~OriginalLineages, type = "bar") %>% 
        layout(barmode = "stack", 
               xaxis = list(title = ''),
               autosize = T,
               yaxis = list(title = 'Proportion (%)'), 
               legend = list(title=list(text='<b> Variants </b>')))
    })
    
    
    output$text.average <- renderText({
      sum(freyja.raw.select()$proportion, na.rm=TRUE)/length(unique(freyja.summarized.select()$samples))
    })
    
    

    

}

shinyApp(ui, server)

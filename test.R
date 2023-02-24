library(shiny)
library(plotly)
library(chron)
library(shinyWidgets)
library(readxl)
library(scales)

#df2 <- read_excel("/Users/glennp/Desktop/R/Shiny/Shiny_run/example_data.xlsx")

#df3 <- df2 %>% 
#  mutate(`Collection Date Time` = as.POSIXct(paste(as.character(df2$`Collection Date`),df2$`Collection Time`) , tz = ""))

ui <- fluidPage(
  
  fileInput('file', 'Upload data in xlsx file, time varialbes should be date/time format.'),
  uiOutput('dropdown0'),
  uiOutput('dropdown1'),
  uiOutput('dropdown2'),
  uiOutput('dropdown3'),
  uiOutput('dropdown4'),
  actionButton("goButton", "Update_Assay"),
  plotlyOutput('plot')
  #verbatimTextOutput("console_text")
  
)




server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  
  
  # All dropdowns become inputs after pickerInput, e.g. use input$All for dropdown1 
  output$dropdown0 <- renderUI({
    req(data()) 
    pickerInput("Which", "Which variable name in data if for assay?", multiple = F, choices = c(colnames(data())),
                options = list(`max-options` = 4,size = 10))
  })
  
  output$dropdown1 <- renderUI({
    req(data()) 
     pickerInput("All", "Assays", multiple = F, choices = (unique(data()[(input$Which)])),#c(unique(data()$get(input$Which))),
                options = list(`max-options` = 4,size = 10))
  })
  
  
  output$dropdown2 <- renderUI({
    req(data()) 
    dd2 <- pickerInput("Xaxis","Time variable", choices = c(colnames(data())),
                options = list(`max-options` = 4,size = 10))
  })
  
  output$dropdown3 <- renderUI({
    req(data()) 
    dd3 <- pickerInput("CBy","Colored by (at the same plot)", multiple = F, choices = c("",colnames(data())), selected = "",
              options = list(`max-options` = 4,size = 10))
  })   
  
  output$dropdown4 <- renderUI({
    dd4 <- pickerInput("By","Panel divided by (You can only select up to 2 variables from the list, recommend not overlaped with selected Colord by variable)", 
                multiple = T, choices = c("",colnames(data())), selected = "",
                options = list(`max-options` = 4,size = 10))
  }) 
  
 # print(paste("see here:"))
  
  trend <- eventReactive(input$goButton, {
 
    data() %>%
      filter(get(input$Which) %in% list(input$All) )%>% 
      mutate(time = as.Date(get(input$Xaxis), format = "%m/%d/%Y")) %>%
      arrange(input$Xaxis) %>% 
      droplevels()
    
  })
  
  
  output$plot <- renderPlotly({
    
    if(is.null(input$CBy) | is.null(input$By)){
        return()
    }else{
        if(input$CBy!=""){
          g <- ggplot(aes(x=time(), y=Value, colour = get(input$CBy)), 
                      data = trend())  
        }else if(input$CBy=="" & input$By[[1]]==""){
          g <-ggplot(aes(x=time, y=Value), 
                     data = trend())  
        }else if(input$CBy=="" & input$By[[1]]!=""){
          g <- ggplot(aes(x=time, y=Value, colour = get(input$By[[1]])),  
                      data = trend())  
        }
    }
    
    g <- g + geom_point() + 
      geom_line() + 
      theme(legend.position = "none") +
      scale_colour_discrete(name = '') + 
     scale_x_date(labels = date_format("%m-%Y")) +
      labs(x = paste0(input$Xaxis), 
           y = paste0(input$All, " (",unique(trend()$Unit),")")) 
    if(input$By[[1]]!=""){
      if(length(input$By)==1){
        g <- g + facet_grid(rows = vars(get(input$By[[1]]))) 
      }else if(length(input$By)==2){
        g <- g + facet_grid(get(input$By[[2]])~get(input$By[[1]])) 
      }
    }else if(input$By[[1]]==""){
      if(length(input$By)==2){
        g <- g + facet_grid(rows = vars(get(input$By[[2]])))  
      }
    }
    
    g
    
  })
  
  
}
shinyApp(ui = ui, server = server)



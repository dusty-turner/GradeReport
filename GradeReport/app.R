library(shiny) 
library(tidyverse)
library(lubridate)
library(shinyAce)
library(DT)
library(shinydashboard)
library(magrittr)
library(stringi)
library(shinyjs)
library(reshape2)

ui <- dashboardPage(skin = "yellow",
                     dashboardHeader(title = "Course Report"),
                     dashboardSidebar(useShinyjs(),
                      sidebarMenu(
                        menuItem("Data", tabName = "Data", icon = icon("dashboard"), startExpanded = TRUE, 
                                 fileInput('csvfile','Upload a CSV File',
                                   accept = c('text/csv',
                                              'text/comma-separated-values,text/plain',
                                              '.csv')
                                            )
                                 ),
                        menuItem("Selections", tabName = "Selections", icon = icon("dashboard"), startExpanded = TRUE,
                                 uiOutput("ui1")),
                        menuItem("Facets", tabName = "Facets", icon = icon("dashboard"), startExpanded = TRUE,
                                 uiOutput("ui2")),
                        menuItem("VOI", tabName = "Variable of Interest", icon = icon("dashboard"), startExpanded = TRUE,
                                 uiOutput("ui3"), uiOutput("ui4")),
                        menuItem("Average", tabName = "Take the Average", icon = icon("dashboard"), startExpanded = TRUE,
                                 radioButtons(
                                   "AVG",
                                   label = h4("Take the Average?"),
                                   choices = c("Yes","No"),
                                   selected = c("Yes")),
                                 radioButtons(
                                   "FacetOO",
                                   label = h4("Facet Plots"),
                                   choices = c("Yes","No"),
                                 selected = c("No"))
                                 )
                      )),
                    
                    
                    dashboardBody(
                      tabBox(
                        title = "Course Analytics",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        tabPanel("Data", 
                                 # tableOutput("text"), 
                                 DTOutput("datatable2")),
                        tabPanel("Histograms", plotOutput("ggplot"), plotOutput("ggplot2")),
                        tabPanel("Box Plots",plotOutput("ggplot3"),
                                 DTOutput("box")
                                 ),
                        width=12
                      )
                    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   

  output$ui1 <- renderUI({
    # if (is.null(input$dataset))
    #   return()
    checkboxGroupInput(
      "selections",
      label = h4("Columns of Interest"),
      choices = names(fulldatatable()[-c(1:4)]),
      # choices = names(fulldatatable()),
      selected = names(fulldatatable()[,c(6,8:9)])
      # selected = names(fulldatatable()[,c(1,6:9)])
    )
  })

  output$ui2 <- renderUI({
    # if (is.null(input$dataset))
    #   return()
    radioButtons(
      "radios",
      label = h4("Facets"),
      # choices = names(fulldatatable()[,c(2:4,6)]),
      choices = names(fulldatatable()[,c(2,3,4,6)]),
      selected = NULL)
    
  })

  output$ui3 <- renderUI({
    radioButtons(
      "VOI",
      label = h4("Variable of Interest"),
      choices = names(selecting2()),
      selected = names(selecting2())[1])
    
  })
  output$ui4 <- renderUI({
    checkboxGroupInput(
      "boxplotcheckbox",
      label = h4("Variable of Interest"),
      choices = names(selecting2()),
      selected = names(selecting2())[1])
    
  })
  
  maxeventpoints = reactive({
    maxhelper = read_csv(input$csvfile$datapath) 
    maxhelper = maxhelper[3,-c(1:10)]
    return(maxhelper)
  })
  
  fulldatatable = reactive({
    file = read_csv(input$csvfile$datapath)
    admin = file %>%
      select(1:10) %>% set_colnames(c("Name",  "Co","Year", "Major", "2nd Maj", "Section", "MaxPts", "Pts","Ave", "Grade")) %>%
      slice(-1:-3)%>%
      mutate(Name = stri_sub(Name, 1, -4)) %>%
      mutate(Section = stri_sub(Section, -2, -1)) %>%
      mutate(Grade = ifelse(stri_sub(Grade, -1, -1)=="-"|stri_sub(Grade, -1, -1)=="+",stri_sub(Grade, -2, -1),stri_sub(Grade, -1, -1))) %>%
      mutate(Pts = as.numeric(Pts), Ave = as.numeric(Ave)) 
    
    data =
      file %>%
      select(-1:-10) %>%
      set_colnames(paste0(file[1,-c(1:10)],file[2,-c(1:10)])) %>%
      slice(-1:-3) %>%
      set_colnames(make.names(names=names(.), unique=TRUE, allow_ = TRUE)) %>%
      mutate_all(funs(as.numeric), file[2,-c(1:10)])
    
  if(input$AVG=="Yes"){
    data =round(t(t(as.matrix(data))/as.numeric(maxeventpoints())),2)
}
    full = cbind(admin,data)
    return(full)
  })

  # partialdatatable = reactive({
  #   partial = fulldatatable() %>%
  #     # select(names())
  #     select_if(is.numeric)
  #   return(partial)
  # })
  
  
  selecting = reactive({
    selections = fulldatatable() %>%
      select(c("Name","Co","Major","Year",input$selections))
      # select_if(is.numeric)
    return(selections)
  })

  selecting2 = reactive({
    selections2 = fulldatatable() %>%
      select(input$selections)%>%
      select_if(is.numeric)

    return(selections2)
  })
  
  # text = reactive({
  #   test = partialdatatable()
  #   return(test)
  # })
  
  plots = reactive({
    if(input$FacetOO=="Yes"){
    plot = selecting() %>%
      ggplot(aes_string(input$VOI)) +
      geom_histogram() +
      facet_wrap(input$radios) +
      ggtitle(paste(input$VOI, "Faceted by", input$radios))
    return(plot)
    } else {
      plot = selecting() %>%
        ggplot(aes_string(input$VOI)) +
        geom_histogram() +
        # facet_wrap(input$radios) +
        ggtitle(paste("Histogram of", input$VOI))
      return(plot)
    }
  })

  plots2 = reactive({
    plot = box() %>%
      ggplot(aes(x=New,y=value)) +
      geom_boxplot() +
      coord_flip() +
      ggtitle(paste(input$VOI, "Faceted by", input$radios))
    return(plot)
  })
  
  box = reactive({
    # box = melt(fulldatatable(), id.vars=(c("Name","Co","Year","Section","Major", "2nd Maj","MaxPts")))
    box = melt(fulldatatable()[,-c(2:10)], id.vars=(c("Name")))
    box = box %>% mutate(New = variable) %>%
      filter(New == input$boxplotcheckbox)
    return(box)
  })
  
  
  
  output$ggplot = renderPlot(plots())
  output$ggplot3 = renderPlot(plots2())
  output$box = renderDT(box())
  output$datatable2 = renderDT(selecting(),options = list(pageLength = 10))
  output$text = renderTable(text())
  
}





# Run the application 
shinyApp(ui = ui, server = server)


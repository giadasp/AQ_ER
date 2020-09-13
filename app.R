
source('global.R', local = FALSE)
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu( # Create a sidebar menu
    menuItem(
      selectInput("prov_input", h3("Provincia"), 
                  choices = list("Bologna" =1,
                                 "Forlì Cesena" =2,
                                 "Ferrara" =3,
                                 "Modena" =4,
                                 "Parma" = 5,
                                 "Piacenza"=6,
                                 "Ravenna" = 7,
                                 "Reggio Emilia" = 8,
                                 "Rimini" = 9),
                  selected = 9)
    ),
    menuItem(
      selectInput("centr_input", 
                  "Centralina",
                  choices = NULL)
    ),
    
    menuItem( # Add a menu item
      text = "Periodo",
      tabName = "time_tab",
      # Insert your inputs within the menuItem
      # Range de Selecao de Data
      dateRangeInput(
        "time_input",
        label = ("Periodo"),
        start = today-as.difftime( 40, units="days" ) ,
        end = today,
        format = "dd/mm/yyyy",
        language = "it-IT",
        separator = "a"
        
      ) # /dateRangeInput
    ),
    menuItem(
      text = "Inquinante",
      tabName = "inq_tab",
      checkboxGroupInput(
        "inq_input", 
        h3("Inquinanti"), 
        choices = NULL 
      )
    )# /menuItem 
  ) # /sidebarMenu
)

ui<-dashboardPage(
  dashboardHeader(title = "Rimini Dati di qualità dell'aria",
                  titleWidth = 309
  ),
  sidebar,
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
    ,
    fluidRow(
      uiOutput("boxes")
    )
  )
)
#dati live
#https://dati.arpae.it/api/action/datastore_search_sql?sql=SELECT * from 'a1c46cfe-46e5-44b4-9231-7d9260a38e68' WHERE station_id = %276000031%27 and reftime=%272017-02-22T00:00:00%27and variable_id=5
#https://dati.arpae.it/api/action/datastore_search_sql?sql=SELECT reftime,value from "a1c46cfe-46e5-44b4-9231-7d9260a38e68" WHERE station_id = '6000031' and variable_id=5
#https://dati.arpae.it/datastore/dump/a1c46cfe-46e5-44b4-9231-7d9260a38e68
server <- function(input, output, session) {
  print("here4")
  # https://dati.arpae.it/api/action/datastore_search_sql?sql=SELECT reftime,value,station_id FROM "a1c46cfe-46e5-44b4-9231-7d9260a38e68" WHERE station_id='6000031' and reftime='2018-01-01T00:00:00' and variable_id=5  
  dft <-reactive({centraline %>% filter(prov_cod == as.numeric(input$prov_input))})
  observeEvent(input$prov_input,{ updateSelectInput(session,'centr_input', choices=setNames(dft()$centr_cod,dft()$centralina), label="centralina")})
  inq_reac<-reactive({stazioni %>% filter(Cod_staz == as.numeric(input$centr_input))})
  observeEvent(input$centr_input,{updateCheckboxGroupInput(session,'inq_input', choices=  setNames(as.numeric(inq_reac()$Id_Param), inq_reac()$PARAMETRO), label="inquinante")})
  
  
  #update inputs for centraline
  plotting <- function(x){
    if(is.null(x)){return()}else{
      #Create the main ggplot
      date_end_input=input$time_input[2]
      date_start_input=input$time_input[1]
      data_plot <- data[[toString(input$centr_input)]][[x]] %>% filter(reftime >= as.POSIXct(date_start_input,format="%Y-%m-%d"))
      data_plot <- data_plot %>% filter(reftime <= as.POSIXct(date_end_input,format="%Y-%m-%d"))
      data_plot$reftime<-as.Date(data_plot$reftime)
      p <- ggplot(data=data_plot,aes(x = as.Date(reftime), y = value)) +
        # aes(xmin = as.Date(today-as.difftime( 20, units="days" )), 
        #     xmax = as.Date(today)) +
        geom_line(stat="identity",position="identity",size=1) #+
        # theme_Publication() +
        scale_x_date(date_breaks = "days" ) #+
        #theme(axis.text.x = element_text(angle = 90))      
      #p <- p + ylab("Number Reported")+scale_color_brewer(palette="Set2",name="Weekly case counts")
      #p <- p + ggtitle(paste("MMWR",input$disease_name, "Reports"))+xlab(xlabel) + geom_point()
      #x gives the series
      return(p)
    }
  }
  
  observeEvent(input$inq_input,{
    
    if(is.null(input$inq_input)){
    }else{
      centr<-toString(input$centr_input)
      inqTot<-as.character(input$inq_input)
      for(inq in inqTot){
        #download required data
        last_date<-try(tail(data[[centr]][[inq]]$reftime)[6])
        if(is.null(last_date)){download<-TRUE}else{
        download<-try(as.Date(last_date,tz="CEST")+1 < as.Date(today,tz="CEST"))
        print(download)}
        if(is.na(download)){download<-TRUE}
        if(is(download,"try-error") || download){
          withProgress(message = 'Downloading data', value = 0, {
            #download data!
            url <- paste0("https://dati.arpae.it/api/action/datastore_search_sql?sql=SELECT%20*%20from%20%22a1c46cfe-46e5-44b4-9231-7d9260a38e68%22%20WHERE%20station_id%20=%20%27",centr,"%27%20and%20variable_id=",inq,"%20ORDER%20BY%20reftime")
            print(url)
            dati<- fromJSON(url)$result$records
            incProgress(1/3, detail = "Data downloaded")
            newtime<-try(as.POSIXct(dati$reftime, format="%Y-%m-%dT%H:%M:%S",tz="Europe/Berlin"))
            
            incProgress(2/3, detail = "Data checked")
            if(is.double(newtime)){
              dati$reftime<-newtime
              outdateddata<-data[[centr]][[inq]]
              if(length(outdateddata)>5){
                data[[centr]][[inq]]<<-rbind(outdateddata,dati[dati$reftime>as.POSIXct(tail(outdateddata$reftime)[6],tz="Europe/Berlin"),])
              }else{
                data[[centr]][[inq]]<<-dati
              }
            }else{
              data[[centr]][[inq]]<<-data.frame()
            }
            incProgress(1, detail = "Data updated")
          }
          )
          
        }else{print(as.POSIXct(tail(data[[centr]][[inq]]$reftime)[1]))
          print(as.POSIXct(today))}
      }
      output$boxes <- renderUI({ 
        lapply(as.numeric(input$inq_input[!is.null(input$inq_input)]), function(x) {
          x<-toString(x)
          box(width=12, title = inquinanti$inqui[inquinanti$inqui_cod==x], renderPlot(plotting(x)))
        })
      })
    }
    # 
    # output$boxes <- renderUI({
    #   lapply(as.numeric(input$inq_input[!is.null(input$inq_input)]), function(x) {
    #      #x is the inquinante
    #     box(title = inquinanti[,1][inquinanti[["inqui_cod"]]==as.numeric(x)], renderPlot(plotting(x)))
    #   })
    # })
  })
}

shinyApp( ui, server)


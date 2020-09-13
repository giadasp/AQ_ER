require(shiny)
require(shinydashboard)
require(ggplot2)
require(jsonlite)
require(curl)
require(dplyr)
#require(shinythemes)
require(ggthemes)
today <- as.POSIXct(Sys.time(),tz="Europe/Berlin")
print("here1")

print(today)
print("here2")

data <- vector("list", length=53)
stazioni <- read.csv(file="Stazioni.csv", header=TRUE, sep=",")
centraline <- read.csv(file="centraline.csv", header=TRUE, sep=";",col.names = c("provincia","prov_cod","centralina","centr_cod_input","centr_cod","date_Start","time_start"))
inquinanti <- read.csv(file="inquinanti.csv", header=TRUE, sep=";",encoding = 'UTF8',col.names = c("inqui_cod","inqui","UM","time","orari"))
# if(as.Date(Sys.Date()-1)>today){
#   today <- Sys.Date()-1
#   print(today)
#   for(centr in (centraline$centr_cod)){
#   centr<-toString(centr)
#   print(centr)
#     data[[centr]]<-vector("list",length=16)
#     for(inq in inquinanti$inqui_cod){
#       if(inq %in% stazioni$Id_Param[stazioni$Cod_staz==centr]){
#         url <- paste0("https://dati.arpae.it/api/action/datastore_search_sql?sql=SELECT%20reftime,value%20from%20%22a1c46cfe-46e5-44b4-9231-7d9260a38e68%22%20WHERE%20station_id%20=%20%27",centr,"%27%20and%20reftime%3E=%272018-01-01T00:00:00%27%20and%20variable_id=",inq,"%20ORDER%20BY%20reftime")
#         print(url)
#         #dati <- try(url %>%
#         #             xml2::read_html() %>%
#         #            html_node("table"))
#         dati<- fromJSON(url)$result$records
#         #dati<- dati%>%
#           #  html_table()
#           newtime<-try(as.POSIXct(dati$reftime, format="%Y-%m-%dT%H:%M:%S"))
#           if(is.double(newtime)){
#           dati$reftime<-newtime
#           data[[centr]][[toString(inq)]]<-dati
#         }else{
#           data[[centr]][[toString(inq)]]<-data.frame()
#         }
#       }
#     }
#   }
# }
# if(as.Date(Sys.Date()-1)>today){
#   today <- Sys.Date()-1
#   print(today)
#   j=1
#   for(centr in centraline$centr_cod){
#     data[[j]]<-vector("list",length=16)
#     i=1
#     for(inq in inquinanti$inqui_cod){
#       if(inq %in% stazioni$Id_Param[stazioni$Cod_staz==centr]){
#         url <- paste0("http://www.arpa.emr.it/aria/estraistaz.asp?q=",inq,"&i=","01.01.2018","&f=",
#                       strftime(today, format="%d.%m.%Y"),"&s=",centr)
#         print(url)
#         #dati <- try(url %>%
#          #             xml2::read_html() %>%
#           #            html_node("table"))
#         dati<- htmltab(doc = url, encoding='UTF-8',fillNA = NA,which = 1,rm_nodata_cols = F)
#         if(!is(dati, "xml_missing") & (!is(dati, "error"))){
#           #dati<- dati%>%
#           #  html_table()
#           
#           dati$Data_inizio<-as.POSIXct(dati[["Data inizio"]])
#           dati$Data_fine<-as.POSIXct(dati[["Data inizio"]])
#           if(inquinanti$orari[inq]==0){
#             for(k in 1:nrow(dati)){
#               originizio<-dati[["Data inizio"]][k]
#               origfine<-dati[["Data inizio"]][k]
#               datainizio<-as.POSIXct(originizio, format="%d/%m/%Y")
#               datafine <-as.POSIXct(origfine, format="%d/%m/%Y")
#               dati$Data_inizio[k]<-datainizio
#               dati$Data_fine[k]<-datafine
#             }
#           }else{
#             for(k in 1:nrow(dati)){
#               originizio<-dati[["Data inizio"]][k]
#               origfine<-dati[["Data inizio"]][k]
#               datainizio<-as.POSIXct(originizio, format="%d/%m/%Y %H.%M.%S")
#               datafine <-as.POSIXct(origfine, format="%d/%m/%Y %H.%M.%S")
#               if(is.na(datainizio)){
#               datainizio<- as.POSIXct(originizio, format="%d/%m/%Y")}
#               if(is.na(datainizio)){
#                 datafine<- as.POSIXct(origfine, format="%d/%m/%Y")}
#               dati$Data_inizio[k]<-datainizio
#               dati$Data_fine[k]<-datafine
#             }
#           }
#           data[[j]][[i]]<-dati
#         }else{
#           data[[j]][[i]]<-data.frame()
#         }
#       }
#       print(i)
#       i = i+1
#     }
#     j= j+1
#   }
# }
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

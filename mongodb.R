library(mongolite)
db <- mongo("pollutants", url ="mongodb://mognousername:mongopwd@cluster&authSource=admin&retryWrites=true&w=majority")

addSeries <- function(data) {
  db <- mongo("pollutants", url ="mongodb://mognousername:mongopwd@cluster&authSource=admin&retryWrites=true&w=majority")
  for (i in 1:length(data,loc)){
    query <- paste('{pollutant: "',data$pollutant[i],'", date: "',data$date[i],'", loc: "',loc,'"}')
    db$update(query, update= paste('{"$set":{value: ',data$value[i],'}}'), upsert = TRUE, multiple = FALSE)
  }
}



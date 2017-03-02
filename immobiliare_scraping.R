#===========================================
# @Project: Immobiliare little scraper
# @Name: immobiliare_scraping.R
# @author: jprimav
# @date: 2017/03
#===========================================


## --- packages
library(XML)
library(XLConnect) # if your Java and rJava architectures do not coincide this may fail


## --- function scraper
prezzi_immobiliare <- function(periodo,regione,provincia,export=0) {

  # url create
  url_prezzi <- paste0("http://www.immobiliare.it/prezzi-mq/",as.character(regione),"/",as.character(provincia),".html")
  tables_prezzi <- readHTMLTable(url_prezzi)
  
  # for zone
  zone <- tables_prezzi[[2]]
  newNamesZone <- c("zona","vendita","affitto")
  names(zone) <- newNamesZone
  zone$vendita <- as.numeric(sub(".","",zone$vendita,fixed=T))
  zone$affitto <- as.numeric(sub(",",".",sub(".","",zone$affitto,fixed=T)))
  zone$periodo <- rep(periodo,length(zone$vendita))
  zone$regione <- rep(regione,length(zone$vendita))
  zone$provincia <- rep(provincia,length(zone$vendita))
  zone <- zone[with(zone,order(-vendita)),c("periodo","regione","provincia","zona","vendita","affitto")]

  # x POSTCODE
  cap <- tables_prezzi[[3]]
  newNamesCap <- c("cap","vendita","affitto")
  names(cap) <- newNamesCap
  cap$vendita <- as.numeric(sub(".","",cap$vendita,fixed=T))
  cap$affitto <- as.numeric(sub(",",".",sub(".","",cap$affitto,fixed=T)))
  cap$periodo <- rep(periodo,length(cap$vendita))
  cap$regione <- rep(regione,length(cap$vendita))
  cap$provincia <- rep(provincia,length(cap$vendita))
  cap <- cap[with(cap,order(-vendita)),c("periodo","regione","provincia","cap","vendita","affitto")]

  rm(list=c("tables_prezzi","newNamesZone","newNamesCap"))
  prezzi <- list(zone=zone,cap=cap)
  
  if (export==1) 
    {
    nome_file_out <- paste0(provincia,"_",periodo,".xlsx")  
    wb <- loadWorkbook(nome_file_out,create=TRUE)
    createSheet(wb, name="zone")
    writeWorksheet(wb,prezzi$zone,sheet="zone")
    createSheet(wb, name="cap")
    writeWorksheet(wb,prezzi$cap,sheet="cap")
    saveWorkbook(wb)  
    } 

  return(prezzi)
  
}


## --- try it out
roma_201612 <- prezzi_immobiliare(201612,"Lazio","Roma",export=0)
str(roma_201612)
milano_201612 <- prezzi_immobiliare(201612,"Lombardia","Milano",export=0)
milano_201612

## --- export
wb <- loadWorkbook("Milano_201612.xlsx",create=TRUE)
createSheet(wb, name="zone")
writeWorksheet(wb,milano_201612$zone,sheet="zone")
createSheet(wb, name="cap")
writeWorksheet(wb,milano_201612$cap,sheet="cap")
saveWorkbook(wb)

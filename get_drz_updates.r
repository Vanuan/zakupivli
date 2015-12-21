library(XML)
url <- "http://drz.lanet.ua/updates/"
html <- xmlRoot(htmlTreeParse(url,useInternalNodes = TRUE))
files = xpathApply(html ,"//a", xmlGetAttr, "href")

am_files = files[grepl("AM.*xml", files)]

#ra_files = files[grepl("RA.*xml", files)]
#ra_tender <- function(name){
#  xml<- xmlRoot(xmlTreeParse(paste(url, name, sep=""), useInternalNodes = TRUE))
#  winners = xpathApply(xml ,"//edz:announces/edz:announce/edz:winners/edz:winner/edz:code", xmlValue)
#  return (winners)
#}
#ra_winners<-sapply(ra_files, ra_tender)

getCodesByName <- function(company_names, codename) {
  dict <- as.list(codename[1,])
  names(dict) <- codename[2,]
  return(sapply(company_names, FUN=function(company_name) { return(dict[company_name][[1]]) }))
}

getWinners <- function(winner) {
  winner <- xmlDoc(winner)
  winner_code <- xpathSApply(winner, "/edz:participant_winner/edz:code", xmlValue)
  winner_name <- xpathSApply(winner, "/edz:participant_winner/edz:name", xmlValue)
  if(length(winner_code) == 0) {
    winner_code <- list("")
  }
  return (c(winner_code, winner_name))
}

getLots <- function(lot) {
  lot <- xmlDoc(lot)
  lot_number <- xpathSApply(lot, "/edz:lot/edz:number", xmlValue)
  lot_desc <- xpathSApply(lot, "/edz:lot/edz:description", xmlValue)
  lot_quantity <- xpathSApply(lot, "/edz:lot/edz:quantity", xmlValue)
  if(length(lot_quantity) == 0) {
    lot_quantity <- list("N/A")
  }
  if(length(lot_desc) == 0) {
    lot_desc <- list("N/D")
  }
  return (c(lot_number, lot_desc, lot_quantity))
}

getLotDescsByNumbers <- function(lot_numbers, lots) {
  dict <- as.list(lots[2,])
  names(dict) <- lots[1,]
  return(sapply(lot_numbers, FUN=function(lot_number) { return(dict[lot_number][[1]]) }))
}


getLotQByNumbers <- function(lot_numbers, lots) {
  dict <- as.list(lots[3,])
  names(dict) <- lots[1,]
  return(sapply(lot_numbers, FUN=function(lot_number) { return(dict[lot_number][[1]]) }))
}


parseAnnounce <- function(announce) {
  announce <-xmlDoc(announce)

  full_id <- xpathSApply(announce, "/edz:announce/edz:startAnnounceInfo", xmlValue)
  short_id <- xpathSApply(announce, "/edz:announce/edz:announce_number", xmlValue)
  announce_date <- xpathSApply(announce, "/edz:announce/edz:publish_date", xmlValue)
  subject <- xpathSApply(announce, "/edz:announce/edz:purchaseSubjectInfo/edz:purchase_subject", xmlValue)
  customer_name <- xpathSApply(announce, "/edz:announce/edz:customer/edz:name", xmlValue)
  customer_code <- xpathSApply(announce, "/edz:announce/edz:customer/edz:code", xmlValue)
  url  <- xpathSApply(announce, "/edz:announce/edz:procedure_url", xmlValue)
  print(full_id)
  
  winners <- xpathSApply(announce, "/edz:announce/edz:participants_winners/edz:participant_winner", getWinners)

  propositions_names <- xpathSApply(announce, "/edz:announce/edz:propositions/edz:proposition/edz:name", xmlValue)
  propositions_codes <- getCodesByName(propositions_names, winners)

  lots <- xpathSApply(announce, "/edz:announce/edz:propositions/edz:proposition/edz:lots", xmlValue)
  lots_desc <- "N/A"
  lots_q <- "N/A"
  if(length(lots) == 0 || length(grep(',',lots[[1]])) > 0) {
    lots <- "N/A"
  } else {
    lots_obj <- xpathSApply(announce, "/edz:announce/edz:lots/edz:lot", getLots)
    if(length(lots_obj) > 0) {
      lots_desc <- getLotDescsByNumbers(lots, lots_obj)
      lots_q <- getLotQByNumbers(lots, lots_obj)
    }
  }
  finish_dates <- xpathSApply(announce, "/edz:announce/edz:propositions/edz:proposition/edz:dtAccept", xmlValue)
  
  prices <- xpathSApply(announce, "/edz:announce/edz:propositions/edz:proposition/edz:price", xmlValue)
  currencies <- xpathSApply(announce, "/edz:announce/edz:propositions/edz:proposition/edz:currency", xmlValue)

  propositions <- rbind(accept_date=finish_dates, lot_number=lots, lot_description=lots_desc, lot_quantity=lots_q,
                        winner_name=propositions_names,
                        winner_code=propositions_codes, price=prices, currency=currencies)
  announce_fields <- list(full_id, short_id, announce_date, subject,
                          customer_name, customer_code, url)
  names(announce_fields) <- c('full_id', 'short_id', 'announce_date', 'subject', 'customer_name', 'customer_code', 'url')
  announces <- merge(announce_fields, t(propositions))
  return(announces)
}

am_tender <- function(filename){
  xml<- xmlRoot(xmlTreeParse(paste(url, filename, sep=""), useInternalNodes = TRUE))
  announces <- xpathApply(xml ,"//edz:announces/edz:announce", parseAnnounce)
  announces <- Reduce(function(...) merge(..., all=T), announces)
  return (announces)
}

am_winners <- apply(as.matrix(am_files[1:length(am_files)]), MARGIN = 1, FUN=am_tender)
am_winners <- Reduce(function(...) merge(..., all=T), am_winners)

write.csv(file="drz_am.csv", x=am_winners)
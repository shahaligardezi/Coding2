library(tidyverse)
library(rvest)
library(stringr)
library('xml2')
library(httr)  
library(rvest) 
library(tidyr) 
library(reshape2)

# function to get links of first 520 companies
getLinks = function(){
  linksList = list()
  for (page in 1:26) {
    url = paste0("https://csimarket.com/markets/Stocks.php?days=yy&pageA=",page,"#tablecomp2")
    print(url)
    page <- read_html(url) #just read the html once
    web <- page %>%
      html_nodes("table") %>% html_nodes("tr") %>% html_nodes("a") %>%
      html_attr("href")
    print(web[238:257])
    linksList = (c(linksList,web[238:257]))
  }
  linksList
}
linksList = getLinks()
linksList = unlist(linksList)

# modifying link so that it can be used to acces each companies profile
templist = list()
for (i in 1:length(linksList)) {
  templist[i] = paste0("https://csimarket.com/",linksList[i])
}
templist = unlist(templist)

# function to return each companies data as a dataframe
getDataFrame = function(templist){
  
  
  url1 = templist
  
  df = read_html(url1)
  
  CompanyName = df %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Naziv", " " ))]')%>%html_text()
  Company = CompanyName
  
  tables_list = df %>%
    html_nodes("table")%>%
    html_table(fill= TRUE)
  
  t3 = tables_list[10]
  t3 = as.data.frame(t3)
  t3$X1 = str_replace_all(t3$X1, "[^[:alnum:]]", " ")
  Industry = unlist(strsplit(t3$X1[1] , "                 "))[2]
  Sector = unlist(strsplit(t3$X1[2] , "          "))[2]
  ColNames = c("Company","Industry","Sector")
  val = c(Company,Industry,Sector)
  t1 = tables_list[13]
  t1 = as.data.frame(t1)
  ColNames1 = gsub(":", "", t1$X1)
  ColNames2 = gsub(":", "", t1$X3)
  val1 =  t1$X2
  val2 =  t1$X4
  
  t2 = tables_list[15]
  t2 = as.data.frame(t2)
  t2$X1 = gsub("[\r\n]", "", t2$X1)
  t2$X1 = gsub("    ", "", t2$X1)
  ColNames3 = t2$X1
  val3 =  t2$X2
  
  Names = c(ColNames,ColNames1,ColNames2,ColNames3)
  Number = c(val,val1,val2,val3)
  
  datadf = data.frame(Names,Number)
  
  datadf = dcast(datadf, Number ~ Names)
  datadf = datadf[1,c(1:20)]
  names(datadf) = Names
  datadf[1,] = Number
  datadf
}

# looping to get data for each company
dataFrame = data.frame()
for (i in 1:length(templist)) {
  print(paste(i, "/" ,length(templist)))
  dfff = getDataFrame (templist[i])
  dataFrame = rbind(dataFrame,dfff)
}

dataFrame
# saving data as rds
saveRDS(dataFrame, file="DataFrame.rds")





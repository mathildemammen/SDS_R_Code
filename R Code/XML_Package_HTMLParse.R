
# Call libraries
library(XML)
library(stringr)

#Building URL
URL <- "http://news.bbc.co.uk/2/hi/uk_politics/8044207.stm" 

URL = htmlTreeParse("http://news.bbc.co.uk/2/hi/uk_politics/8044207.stm")
class(URL)
URL

# parse the document for R representation: 
URL.doc <- htmlParse(URL)


# get all the tables in mps.doc as data frames
URL.tabs <- readHTMLTable(URL.doc)
# loop to find relevant table:

first <- "Abbott, Ms Diane"
last <- "157,841"

for (i in 1:length(URL.tabs)) {
  
  lastrow <- nrow(URL.tabs[[i]]) # get number of rows
  lastcol <- ncol(URL.tabs[[i]])
  
  if (as.character(URL.tabs[[i]][1,1])==last & as.character(URL.tabs[[i]][lastcol,lastrow)==first) {
    
    tabi <- i
    
  }
}
 


head(URL.tabs[[tabi]])
  
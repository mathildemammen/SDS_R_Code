
######################################################## ASSIGNMENT 2 SDS ########################################################

### ATTENTION : Run code in numerated sequence - first [1] and then [2] ###

################################################################ [1] #############################################################
library("plyr")
library("rvest")
library("stringr") 

# 1.1) 
#Need to create values of 0:1000 by 10 in order construct list of viable URLS to scrape for the appropriate css.selector for each post.
x1000 <- c(0:1000)
n1000 <- length(x1000)
var1000 = x1000[seq(1, n1000, 10)]

#Looking at the structure of the URL's we see that /reports/paid?page= defines the span for subsets of 10 posts
# Thus, if we can insert 0:1000 by intervals of 10 into "LANK", we actually have all 100 pages, each consisting of 10 posts 
linksub.li = "http://www.ipaidabribe.com/reports/paid?page=LANK"


# 1.2) FUNCTION : 
#Create function that runs through all numbers in var1000 and replaces LANK with the 0:1000 by 10s

link_str_replace = function(var1000){
  link.sub = gsub("\\LANK", var1000, linksub.li)
}

# 1.3) LOOP : 
# Using plyr (ld) for simple, efficient looping - list-to-list, llply works, but not optimally as transformations has to be made afterwards
num.link.li = ldply(var1000, link_str_replace)
num.link.li2 = num.link.li$V1

# 1.4) FUNCTION : 
#Now, create function that fetches post links from each website, which only has 10 posts pr. page and coerce these links into list

#Link-fetching function based on list of 101 links in num.link.li - should create 1010 new links
#Scrape www.ipaidabribe.com - articles; n = 1000
#Define css-selector that is unique to the title href pointing at each specific post

as2.css.selector_1 = ".heading-3 a"   #URL and/or TITLE depends on 'html_attr(name = href/title)'


################################################################# [2] #############################################################

#2.1) CSS-Selectors
#Now we need to make the actual request of data we are to analyse : 
# date, title  ammount, name of dept., transaction detail, number of views, city

#Note: I did not succeed in identifying the css-selectors using selector-gadget. Instead, press CTRL+SHIFT+I ->
#Then, locate the relevant piece of html-code (the piece that corresponds to the "box" or string of interest) ->
#When located, right-click and select 'Copy CSS-Path' ->
# It will look something like this: 
# -> :  body > main > div > div.wrapper > div > div.col-md-8 > div.report-listing.details > article:nth-child(1) > section.ref-module-paid-bribe > ul.overview.clearfix > li.views
# --> pick the part, that is unique to the field, typically the last part.

## Let's identify the relevant css-selectors

as2.css.selector_1 = ".heading-3 a"       #URL and/or TITLE depends on 'html_attr(name = href/title)'
as2.css.selector_2 = "span.date"          #date
as2.css.selector_3 = "li.paid-amount"     #amount paid
as2.css.selector_4 = "div.key > a"        #location
as2.css.selector_5 = "li.name > a"        #department
as2.css.selector_6 = "li.transaction > a" #transaction details
as2.css.selector_7 = "li.views"           #number of views

#2.2) FUNCTION : 
## Then, we create a function that looks at all the css-selectors and returns data based on link, css-selector by evaluating argument:
## html_attr or html_text : if we want a reference, use html_attr and corresponding reference otherwise, for a string og text; html_text()
## Then return as dataframe bound by columns for each link fed into the function

scrape_post_bribe = function(num.link.li2){
  post.url = read_html(num.link.li2, encoding = "UTF-8")
  post.title = post.url %>%
    html_nodes(css = as2.css.selector_1) %>%
    html_attr(name = 'title')
  post.date = post.url %>%
    html_nodes(css = as2.css.selector_2) %>%
    html_text()
  post.paid = post.url %>%
    html_nodes(css = as2.css.selector_3) %>%
    html_text()
  post.location = post.url %>%
    html_nodes(css = as2.css.selector_4) %>%
    html_attr(name = 'title')
  post.dept = post.url %>%
    html_nodes(css = as2.css.selector_5) %>%
    html_attr(name = 'title')
  post.trans = post.url %>%
    html_nodes(css = as2.css.selector_6) %>%
    html_attr(name = 'title')
  post.views = post.url %>%
    html_nodes(css = as2.css.selector_7) %>%
    html_text()
  return(cbind(post.title, post.date, post.location, post.dept, post.trans, post.views, post.url))
}

# 2.3) LOOP : 
### The loop for the function defined above: nothing new here
### As we are requesting 8 variables on 1000 links, this takes a lot of time if we set 'Sys.sleep(1)' - cut the wait time in half.
### This should not cause the servers of www.ipaidabribe.com to overload - I hope...

post.bribe.df = list()
for(i in num.link.li2){
  print(paste("processing", i, sep = " "))
  post.bribe.df[[i]] = scrape_post_bribe(i)
  Sys.sleep(0.01)
  cat("done!\n")
}

### WARNING : This request took me approximately 20-30 minutes to complete - do not start the request if you do not intend to finish.
###           The requested data will not be saved in meta-environment if stopped. You will have to start over - and if this is done 
###           repeatedly or at a busy time, your IP may be blocked by the host. 

# 2.4) DATA :
# Now that the data has been gathered, we need to do a little cleaning - first step is to set up a dataframe and remove duplicate observations
# ldply(data, data.frame) fixes this for us
IN.Bribe.df = ldply(post.bribe.df, data.frame)
# We now have a dataframe of 4012 observations - this is due to the URL-structure, some are bound to duplicate(as they don't include unique codes)
# Removing duplicates can be done by sorting variable = '.id'
IN.Bribe.df = IN.Bribe.df[!duplicated(IN.Bribe.df$.id), ]

# I realize by now, that variable post.paid is not included in this data.frame - no worries; a simple gsub(... IN.Bribe.df$post.title)
# -> can extract the amount paid.  

################################################################# [3] #############################################################

# 3.1) Manupulation of IN.Bribe.df - preparation for analysis
IN.Bribe.df$post.views = gsub("\\views.*$", "", IN.Bribe.df$post.views)                     # Seperating numeric from views
IN.Bribe.df$city = word(IN.Bribe.df$post.location, +1)                                      # Seperating region and city into two distinct variables
IN.Bribe.df$city = gsub("\\,", "", IN.Bribe.df$city)                                        # ...
IN.Bribe.df$region = word(IN.Bribe.df$post.location, -1)                                    # ...
IN.Bribe.df$bribe.paid = as.numeric(gsub("[^\\d]+", "", IN.Bribe.df$post.title, perl=TRUE)) # Extracting numeric value of bribe from title using PERL-type regular expression

# 3.2) Deleting obsolete variables/columns
IN.Bribe2.df = subset(IN.Bribe.df, , -c(.id, post.title))
IN.Bribe3.df = data.frame(lapply(IN.Bribe2.df, as.character), stringsAsFactors=FALSE)
IN.Bribe3.df$post.date = gsub("\\,", "", IN.Bribe3.df$post.date)
IN.Bribe3.df$month = word(IN.Bribe3.df$post.date)
IN.Bribe3.df$day = word(IN.Bribe3.df$post.date, -2)
IN.Bribe3.df$year = word(IN.Bribe3.df$post.date, -1)

IN.Bribe3.df$post.url = NULL
IN.Bribe3.df$post.location = NULL
IN.Bribe3.df$post.date = NULL

library("lubridate")

?lubridate


head(IN.Bribe3.df, 5)

write.table(IN.Bribe3.df, "C:/Users/Adam/Desktop/Bribe.csv", sep = "\t")

?write.csv

library("readr")
bribe.csv = read.csv("https://raw.githubusercontent.com/adamingwersen/Data.for.ass2_SDS/master/bribe.csv", sep = "\t")
?read.table




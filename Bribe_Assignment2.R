
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


# I realize by now, that variable post.paid is not included in this data.frame - no worries; a simple gsub(... IN.Bribe.df$post.title)
# -> can extract the amount paid.  

################################################################# [3] #############################################################

# 3.1) Manupulation of IN.Bribe.df - preparation for analysis
IN.Bribe.df$post.views = gsub("\\views.*$", "", IN.Bribe.df$post.views)                           # Seperating numeric from views
IN.Bribe.df$region = gsub(".*,", "", IN.Bribe.df$post.location)                                   # Using regex with gsub to seperate words by comma
IN.Bribe.df$post.city = gsub("\\,.*", "", IN.Bribe.df$post.location)                              # ...
IN.Bribe.df$bribe.paid.INR = as.numeric(gsub("[^\\d]+", "", IN.Bribe.df$post.title, perl=TRUE))   # Extracting numeric value of bribe from title using PERL-type regular expression

# Dates, using simple as.Date function

IN.Bribe.df$post.date = gsub("\\,", "", IN.Bribe.df$post.date)
IN.Bribe.df$post.date = gsub("\\November", "11", IN.Bribe.df$post.date)
IN.Bribe.df$post.date = gsub("\\October", "10", IN.Bribe.df$post.date)
IN.Bribe.df$num.date = strptime(IN.Bribe.df$post.date, "%m %d %Y")

# 3.2) Deleting obsolete variables/columns
IN.Bribe3.df = data.frame(lapply(IN.Bribe.df, as.character), stringsAsFactors=FALSE)
IN.Bribe3.df$post.url = NULL
IN.Bribe3.df$post.location = NULL
IN.Bribe3.df$.id = NULL
IN.Bribe3.df$post.region = NULL
IN.Bribe3.df$town = IN.Bribe3.df$post.city

################################################################# [4] #############################################################
Ext1.list = read.csv("https://raw.githubusercontent.com/adamingwersen/Data.for.ass2_SDS/master/India.Region.Literacy.csv.csv", sep = ";")

# Join
IN.Bribe3.df$region = gsub("^\\s+|\\s+$", "", IN.Bribe3.df$region)  #Trim trailing/leading whitespace
Ext1.list$region = as.character(Ext1.list$region)

library("dplyr")
combi.df = right_join(Ext1.list, IN.Bribe3.df, by = "region", copy = TRUE, all.x = TRUE)

################################################################# [5] #############################################################
#MAP DATA
library("maps")
data(world.cities)
map("world", "India")
map.cities(country = "India")

world.cities$name = gsub("\\'", "", world.cities$name)
world.cities$town = world.cities$name
world.cities1 = world.cities[world.cities$country.etc == "India",]

library("dplyr")
india.spatial.df = inner_join(world.cities1, combi.df, by = "town", all.y = TRUE)
india.spatial.df$town <- as.character(india.spatial.df$town)
india.spatial.df$pop = as.numeric(india.spatial.df$pop) 
india.spatial.df$bribe.paid.INR = as.numeric(india.spatial.df$bribe.paid.INR)
india.spatial.df$post.views = as.numeric(india.spatial.df$post.views)

# GGMAP-PLOT 
library("ggmap")
map <- get_map("India", zoom = 5, maptype = "terrain")
p <- ggmap(map)
print(p)
ggsave(p, file = "map1.png", width = 5, height = 5, type = "cairo-png")


ind = ggmap(map) + geom_point(aes(x=long, y=lat), data=india.spatial.df, col="orange", alpha=0.4, size = log(india.spatial.df$pop)) +  
  geom_density(aes(x = long, y = lat), data = india.spatial.df, col = "blue", alpha = 0.4, size = india.spatial.df$post.views) + scale_size(name = "Population") +
  labs(x = "Longitude", y = "Latitude") + ggtitle("Population and post views in Indian cities")
plot(ind)

top10=india.spatial.df %>% 
  group_by(town, ratio)  %>% 
  summarise(bribe.paid=n())
top10=arrange(top10, desc(bribe.paid))
head(top10, 10)

# Regs

reg1 = lm(india.spatial.df$post.views~india.spatial.df$citiziens)
res1 = reg1$residuals
vcov(reg1)
install.packages("lmtest")
library("lmtest")
bptest(reg1)
abline(reg1)
plot(india.spatial.df$post.views~india.spatial.df$citiziens)
abline(a = reg1$coeff[1], b = reg1$coeff[2])

reg2 = lm(india.spatial.df$citiziens ~ india.spatial.df$literacy)
plot(india.spatial.df$citiziens ~ india.spatial.df$literacy)
abline(a = reg2$coeff[1], b = reg2$coeff[2])

reg3 = lm(as.numeric(spat2$post.views)~as.numeric(spat2$ratio))
plot(as.numeric(spat2$post.views)~as.numeric(spat2$ratio), main = "Post Views ~ Literacy Rate", xlab = "Literacy Rate", ylab = "Post Views")
abline(reg3$coefficients)

combi.df$logbribe = log(as.numeric(combi.df$bribe.paid.INR))
reg4 = lm(as.numeric(combi.df$logbribe)~as.numeric(combi.df$post.views))
plot(as.numeric(combi.df$logbribe)~as.numeric(combi.df$post.views), main = "Post Views ~ Bribe amt.", xlab = "Bribe", ylab = "Post Views")
abline(reg4$coefficients)
res4 = residuals(reg4)
hist(res4, freq = FALSE)
curve(dnorm, add = TRUE)

res3 = resid(reg3)
plot(as.numeric(spat2$ratio), res3)

# Dates
india.spatial.df$POSIXct = as.POSIXct(india.spatial.df$num.date)
Sys.setlocale("LC_TIME","English")
india.spatial.df$wday = weekdays(india.spatial.df$POSIXct)
india.spatial.df$wday <- factor(india.spatial.df$wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plots
library("ggplot2")
library("scales")
wp = ggplot(india.spatial.df, aes(x = wday))
wp = wp + geom_density(aes(group = wday, colour = wday, fill = wday), alpha = 0.4)
wp = wp + labs(x = "Weekday", y = "Posts", title = "Number of posts on weekdays")
wp = wp + theme_minimal()
plot(wp)

wp = ggplot(india.spatial.df, aes(x = wday))
wp = wp + geom_density(aes(group = wday, colour = wday, fill = wday), alpha = 0.4)
wp = wp + labs(x = "Weekday", y = "Posts", title = "Density: Posts on weekdays by region")
wp = wp + theme(panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank(), 
                                      panel.background = element_blank(), 
                                      axis.line = element_line(colour = "blue"),
                                      axis.text.x = element_text(angle = 90))
wp = wp + facet_wrap(~region, scales = "free_y")
plot(wp)

wp = ggplot(india.spatial.df, aes(x = wday))
wp = wp + geom_density(aes(group = wday, colour = wday, fill = wday), alpha = 0.4)
wp = wp + labs(x = "Weekday", y = "Posts", title = "Density: Posts on weekdays by region")
wp = wp + theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "blue"),
                axis.text.x = element_text(angle = 90))
wp = wp + facet_wrap(~post.dept, scales = "free_y")
plot(wp)

(india.spatial.df$citiziens)

g = ggplot(india.spatial.df, aes(x = POSIXct, y = log(bribe.paid.INR)))
g = g +geom_jitter() 
g = g + facet_wrap(~wday, scales = "free_y")
plot(g)

gg = ggplot(combi.df, aes(x = town, y = log(bribe.paid.INR), rm.na = TRUE))
gg = gg + geom_jitter()
plot(gg)

install.packages("doBy")
library("doBy")

summaryBy(citiziens ~ region, data = india.spatial.df,
            FUN = list(mean, median))

spat2 = subset(india.spatial.df, citiziens > 50000000)

pl = ggplot(spat2, aes(x = region, y = as.factor(citiziens)))
pl = pl + geom_bar(stat="identity", colour = "#FF9933")
pl = pl + theme_minimal()
pl = pl + labs(x = "Region", y = "Inhabitants", title = "Population distribution")
pl = pl + scale_fill_manual(values = c("#FF9933", "#138808"))
pl = pl + theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "blue"),
                axis.text.x = element_text(angle = 90))
plot(pl)

india.spatial.df$nregion = india.spatial.df$region[c("Maharashtra", "Delhi", "Karnataka", "Telangana", "Tamil Nadu", "Gujarat"),]
india.spatial.df$nregion = india.spatial.df[(india.spatial.df$region = c("Maharashtra", "Delhi", "Karnataka", "Telangana", "Tamil Nadu", "Gujarat")),]
india.spatial.df$nregion = india.spatial.df[grepl(c("Maharashtra", "Delhi", "Karnataka", "Telangana", "Tamil Nadu", "Gujarat"), india.spatial.df$region),]

pl = ggplot(spat2, aes(x = region, y = citiziens/100000000))
pl = pl + geom_bar(stat="identity")
pl = pl + theme_minimal()
pl = pl + labs(x = "Region", y = "Inhabitants", title = "Population distribution in millions")
pl = pl + theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "blue"),
                axis.text.x = element_text(angle = 90))
plot(pl)







  

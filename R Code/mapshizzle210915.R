library("knitr")
install.packages("purl")
library("purl")


df = read_csv("https://raw.githubusercontent.com/sebastianbarfort/sds/master/data/marijuana-street-price-clean.csv")

library("lubridate")
library("dplyr")
df$year = year(df$date)

df = df %>% 
  group_by(State, year) %>%
  summarise(
    m.price = mean(HighQ, na.rm = TRUE)
  ) %>%
  mutate(
    region = tolower(State)
  )

library("maps")
library("ggplot2")

us.states = map_data("state")
df.merge = left_join(df, us.states)

df.bench = read_csv("http://wfs-kbhkort.kk.dk/k101/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=k101:baenk&outputFormat=csv&SRSNAME=EPSG:4326")
names(df.bench)

library("dplyr")
library("stringr")
df.bench = df.bench %>%
  select(wkb_geometry, baenk_tilstand) 

# cleaning  
df.bench$wkb_geometry = gsub("\\(|\\)", "", df.bench$wkb_geometry) 
df.bench$wkb_geometry = str_extract(df.bench$wkb_geometry, "[0-9].+")
x = str_split(df.bench$wkb_geometry, pattern  = " ")
x = do.call(rbind.data.frame, x)
df.bench = bind_cols(df, x)
names(df.bench) = c("wbk_geometry", "baenk_tilstand", "lat", "lon")
df.bench$lon = as.numeric(as.character(df.bench$lon))
df.bench$lat = as.numeric(as.character(df.bench$lat))

install.packages("ggmap")
library("ggmap")
qmplot(lat, lon, zoom = 15, data = df.bench, 
       maptype = "toner-background", color = I("red"))

install.packages("mapDK")
library(mapDK)
args(getID)

mapDK()
mapDK(detail = "zip")

mapDK(values = "stemmer", id = "id", 
      data = subset(votes, navn == "socialdemokratiet"),
      detail = "polling", show_missing = FALSE,
      guide.label = "Stemmer \nSocialdemokratiet (pct)")


library("mapproj")
library("ggmap")
df = mapDK::polling
df.votes = mapDK::votes
df = df %>% filter(KommuneNav == "koebenhavn")
df.t = left_join(df, df.votes)
cph.map = ggmap(get_map(location = c(12.57, 55.68), 
                        source = "stamen", 
                        maptype = "toner", crop = TRUE,
                        zoom = 13))
p = cph.map + 
  geom_polygon(data = subset(df.t, navn == "socialdemokratiet"), 
               aes(x = long, y = lat,
                   group = group, fill = stemmer),
               alpha = .75)

plot(p)


### -------


install.packages("WDI")
library("WDI")
library("dplyr")
df = WDI(indicator = "NY.GDP.PCAP.KN" ,
         start = 2010, end = 2010, extra = F)
df = df %>% filter(!is.na(NY.GDP.PCAP.KN))

install.packages("maps")
library("maps")
require("maps")
df.world = map_data("world")



library("dplyr")
df.merge = left_join(df, df.world, by = c("iso2c" = "region"), copy = TRUE)
df.merge.s = full_join(df, df.world, by = c("country" = "region"))

p = ggplot(df.merge, aes(x = long, y = lat, group = iso2c)) + 
  geom_polygon(aes(fill = NY.GDP.PCAP.KN)) + 
  theme_minimal()
plot(p)

getwd()


library("maps")
ggplot(df.merge, aes(x = long, y = lat, group = iso2c)) + 
  geom_polygon(aes(fill = NY.GDP.PCAP.KN)) + 
  expand_limits(x = df.merge$long, y = df.merge$lat) + 
  theme_minimal()
plot(p)

p = qplot(long, lat, data = df.merge, group = group,fill=NY.GDP.PCAP.KN,geom ="polygon",ylab="",xlab="")
p = p + scale_fill_continuous(name="GDP", trans = "log", low = "black", high = "pink") + theme_minimal() + theme(panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
plot(p)

## ---------------------

df.dk = mapDK::votes

df.dkp = mapDK()
mapDK(detail = "polling")

mapDK(values = "stemmer", id = "id", 
      data = subset(votes, navn == "detkonservativefolkeparti"),
      detail = "polling", show_missing = FALSE,
      guide.label = "Stemmer dkonsfp (pct)")








table((df$country & df.world$region) = FALSE)

install.packages("countrycode")
library("countrycode")
countrycode(df.world, "region", "iso2c", warn = FALSE)
library("countrycode")
df.world$region = countrycode(df.world$region, origin = "country.name", destination = "iso2c")

header(df$iso2c)

library("mapproj")
library("ggmap")
df = mapDK::polling
df.votes = mapDK::votes
df = df %>% filter(KommuneNav == "koebenhavn")
df.t = left_join(df.votes, df.dk)
cph.map = ggmap(get_map(location = c(12.57, 55.68), 
                        source = "stamen", 
                        maptype = "toner", crop = TRUE,
                        zoom = 13))
p = cph.map + 
  geom_polygon(data = subset(df.t, navn == "detkonservativefolkeparti"), 
               aes(x = long, y = lat,
                   group = group, fill = m.vote),
               alpha = .75)

plot(p)


df.dk = df.dk %>% 
  group_by(navn, id) %>%
  summarise(
    m.vote = mean(stemmer, na.rm = TRUE)
  )
  


















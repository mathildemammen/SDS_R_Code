# Loading .csv file

library("readr")
moma = read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")

###############################################   Part 1    ###

# Using lubridate to translate dates into months/years by some standard

library("lubridate")
moma.df = moma[-which(is.na(moma$DateAcquired)),]
moma.df$mnyr = ymd(moma.df$DateAcquired) # Translating AquiredDate into PROSIX1t format
moma.df$mon = month(moma.df$mnyr, label =TRUE, abbr = TRUE) # Reading mnyr and translating only month into new variable, "mon", abbr is abbreviation and TRUE -> Jan instead of January


###############################################   Part 2    ###

library("ggplot2")

p = ggplot(data = moma.df, aes(x = mon))
p = p + geom_histogram(fill = "red", alpha = 0.9) + labs(x = "Month", y = "Number of Painting in holding", title = "MoMA Paintings by Month")
p = p + theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "blue"))
plot(p)

# Nothing new here - themes are only to colour axis and make background blank

###############################################   Part 3    ###

p = ggplot(data = moma.df, aes(x = mon, fill = CuratorApproved))
p = p + geom_histogram(alpha = 0.9) + labs(x = "Month", y = "Number of Painting in holding", title = "MoMA Paintings by Month")
p = p + theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
plot(p)

###############################################   Part 4    ###

moma.dep = moma.df %>%
  filter(!is.na(Department)) %>%
  group_by(Department)

###############################################   Part 5    ###

p = ggplot(data = moma.df, aes(x = Department))
p = p + geom_histogram(alpha = 0.9) + labs(x = "Department", y = "Number of Painting in holding", title = "MoMA Paintings by Department")
p = p + theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90))
plot(p)

# or

p = ggplot(data = moma.df, aes(x = Department))
p = p + geom_histogram(alpha = 0.9) + labs(x = "Department", y = "Number of Painting in holding", title = "MoMA Paintings by Department")
p = p + coord_flip()
p = p + theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
plot(p)

  # Another, more colourful representation using facet-wrap
p = ggplot(data = moma.df, aes(x = DateAcquired, fill = Department))
p = p + geom_bar() + labs(x = "Yearr", y = "# Paintings", title = "MoMA Paintings over time by Department")
p = p + facet_wrap(~Department, scales = "free_y")
plot(p)


###############################################   Part 6    ###
 # May not be applicable #
moma.hp = moma %>%
  filter(!is.na(Artist)) %>%
  group_by(Artist) %>%
  
  #Entire code for making the table#  
sort(table(moma$Artist), decreasing = TRUE)[1:10]


###############################################   Part 7    ###

  # Firstly, using gsub and word for extracting nationalities from the ArtistBio-variable
library("stringr")
moma$Nationality = gsub("\\(", "", moma$ArtistBio) # Replace ( with "blank"
moma$Nationality = word(moma$Nationality, +1) #Pick the 1st word 
moma$Nationality = gsub(",", "", moma$Nationality)  # Replace , with "blank"

  # Read external files from Adam's GitHub repository 

    # Data on Countries and Nationalities, tab-separated from (https://www.englishclub.com/vocabulary/world-countries-nationality.htm) and more.
Nat1 <- read.csv("https://raw.githubusercontent.com/adamingwersen/Data_for_assignment1_SDS/master/Nationalities2.txt", sep="\t", header = FALSE)

    # Data on population-size by iso3c codes from (http://data.worldbank.org/indicator/SP.POP.TOTL) 
Pop1 <- read.csv("https://raw.githubusercontent.com/adamingwersen/Data_for_assignment1_SDS/master/Countrypop.txt", sep = "\t", header = TRUE)


  # Next, we clean up the Nat1 Dataframe by renaming variables and adding an iso3c countrycode
library("plyr")
Nat1 = rename(Nat1, c("V1"="Country", "V2"="Nationality"))


  # Creating a joined dataframe by nationalities, so that Country.names based on the artists birthplace are linked to each artwork
    # Plyr to dplyr issue - disable plyr
library("dplyr")
combi.df = inner_join(moma, Nat1, by = "Nationality")


  # In order to create a map of the world, the ggmap-package is used to create a new dataframe
library("ggmap")
map.df = map_data("world")


  # Adding countrycode (iso2c) to each of our dataframes:
library("countrycode")
map.df$iso2c = countrycode(map.df$region, origin = "country.name", destination = "iso2c")
combi.df$iso2c = countrycode(combi.df$Country, origin = "country.name", destination = "iso2c")
Pop1$iso2c = countrycode(Pop1$iso3c, origin = "iso3c", destination = "iso2c")


  # Summarising over the total number of paintings attributed to each country in order to reduce final dataframe significantly
library("dplyr")
momamap.df = combi.df %>%
  filter(!is.na(iso2c)) %>%
  select(iso2c) %>%
  group_by(iso2c) %>%
  summarise(number = n())

  # Joining dataframes:
library("dplyr")
  # Adding long/lat to the summarised dataframe created above
momamap.df = inner_join(map.df, momamap.df, by = "iso2c")
  # Matching with 2014 population in each country
momamap.df2 = inner_join(momamap.df, Pop1, by = "iso2c")
  # Finally, create new variable using mutate to obtain a pr. capita number of paintings for each country
momamutate.df = mutate(momamap.df2, paint.by.pop = number/Population)

  # Due to the fact that American artists can be attributed the majority of paintings held in the MoMA, we can use log - however the meaning of this plot is somewhat nonexistent.
library("ggplot2")
library("ggmap")
p = ggplot(momamutate.df, aes(x = long, y = lat, group = group, fill = log(number)))
p = p + geom_polygon()
p = p + theme(panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), 
             panel.background = element_blank(), 
             axis.line = element_blank(), 
             axis.text.x = element_blank(), 
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.title.x = element_blank(), 
             axis.title.y = element_blank())
p = p + labs(title = "Number of MoMA Paintings around the world")
p = p + scale_fill_gradient(low = "#00b3b3", high = "#cccc00", guide = "colorbar")
plot(p)

  # Instead we could use the pr. capita paintings by country to obtain a more meaningful map-plot
p = ggplot(momamutate.df, aes(x = long, y = lat, group = group, fill = paint.by.pop))
p = p + geom_polygon()
p = p + theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_blank(), 
              axis.text.x = element_blank(), 
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(), 
              axis.title.y = element_blank()) # Removing exessive lines, background and text  
p = p + labs(title = "Number of MoMA Paintings around the world pr. capita (2014)")
p = p + scale_fill_gradient(low = "#ffa500", high = "#228b22", guide = "colorbar") # Using different colours for the two plots
plot(p)

###############################################   Part 8    ###

library("stringr")
  # Extract the 1st word within "Dimensions" and call this "Dimensions1"
moma$Dimensions1 = str_extract_all(moma$Dimensions, "\\([^()]+\\)"[[1]])
  # Remove parentheses
moma$Dimensions1 = gsub("\\(", "", moma$Dimensions1)
moma$Dimensions1 = gsub("\\)", "", moma$Dimensions1)
  # Remove "cm"
moma$Dimensions1 = gsub("\\cm", "", moma$Dimensions1)
  # str_split could have been used - this was easier to do, however
    ## First chr
moma$H = word(moma$Dimensions1, +1)
    ## Second chr
moma$L = word(moma$Dimensions1, -2)
  # We need to transform chr to num in order to perform math-expressions as below
moma$H = as.numeric(moma$H)
moma$L = as.numeric(moma$L)
  # A = H * L, if i remember correctly
moma$PicArea = (moma$L * moma$H)

library("dplyr")
# Multiple approaches 
top5 = moma %>%
  arrange_(~ desc(PicArea)) %>%
  slice(1:5)

bot5 = moma %>%
  arrange_(~ (PicArea)) %>%
  slice(1:5)

mix10 <- rbind(top5, bot5)







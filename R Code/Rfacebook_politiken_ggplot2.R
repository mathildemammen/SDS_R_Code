
 ############################################ Facebook scrape of Politiken ###############################################

## -------------------- PACKAGES --------------------------------- 
library("Rfacebook")
library("readr")
library("stringr")
library("lubridate")
library("hexbin")
library("dplyr")
 
## -------------------- RETRIEVE DATA -----------------------------

 # 2hr token via developers.facebook.com/GraphAPI - be aware of settings.. 
token <- "CAACEdEose0cBAKc5ey00ryyqBZBSnm3iSAYkn8ZBOqQ3wjbkOY8lZBMgXLN4mc160CYwB6ibxN5dFI8nwupZAuOZBhK82tRCwXy98qnNXY1ZCPZCl5Jj2gy0PXMZCZBCWgUO63asnUPZAyxxbQwXDV4frDKZAPyJgWSUw8uYf2q6nX0WENJW2FzD12NKuuRVTWqUeh0rNVYA51fUgZDZD"
# Define the page and number of observations, here n = 1.000
page <- getPage("Politiken", token, n = 1000)
# Save and load as CSV
read.csv(page, "~C:\\Users\\Adam.csv", rows.names = FALSE)

## --------------------- OR READ CSV DIRECTLY FROM GIT/SDS --------

table2 = read.csv("https://raw.githubusercontent.com/sebastianbarfort/sds/master/data/politiken.csv")

##---------------------- TRANSFORMATIONS --------------------------
			### @ PLOTTING vol.1 ###
# Parse //time
table2$created_time = parse_datetime(table2$created_time)

			### @ PLOTTING vol.2 ###
# Use "stringr" package to read text-string; here politiken's URL 
	# Telling R to read xxx from (www.politiken.dk)/..xxx../ <- STOP!
table2$section = str_extract(table2$link, ".dk/[a-z]*")
table2$section = gsub(".dk/", "", table2$section)

# Use "dplyr" package to arrange the average of likes pr. post by section
table2.section = table2 %>%
  filter(!is.na(section)) %>%
  filter(section !="") %>% # Disregard all NA's and BLANKS
group_by(section) %>%
  summarise(
    likes.pr.post = mean(likes_count, na.rm = TRUE)
  ) %>%
  arrange(-likes.pr.post)

# Use "lubridate" package to understand the date-codes and create weekday variables instead of e.g. created_time = "2015-03-19 10:32:11"
table2$weekday = wday(table2$created_time, label = TRUE)

# Use "dplyr" package to create a dataframe, categorized into two subsection: "indland" & "udland"
table2.subset = table2 %>%
  filter(section %in% c("indland", "udland"))

# Create new dataframe including only 3 variables
tab = data.frame(table(table2$section, table2$weekday))
names(tab) = c("section", "weekday", "count")


##-------------------------- PLOTTING vol.1 -----------------------------

# 1.1 - Histogram of likes/count
p = ggplot(data=table2, aes(x = likes_count))
p = p + geom_histogram()
p = p + scale_x_log10()
plot(p)

# 1.2 - Density Plot of likes/count
p = ggplot(data=table2, aes(x = likes_count))
p = p + geom_density()
p = p + scale_x_log10()
plot(p)

# 1.3 - Area plot of likes/count
p = ggplot(data = table2, aes(x = likes_count))
p = p + geom_area(stat = "bin")
p = p + scale_x_log10()
plot(p)

# 1.4 - Combined Density/Histogram on likes_count
p = ggplot(data = table2, aes(x = likes_count))
p = p + geom_histogram(aes(y =..density..), colour = "black", fill = "#FFB366") + geom_density(alpha = 0.5, fill = "#556B2F") + scale_x_log10()
plot(p)

# 1.5 - Two-variable jitter-plot
p = ggplot(data = table2, aes(x = likes_count, y = comments_count))
p = p + geom_jitter()
p = p + scale_x_log10()
p = p + geom_abline(intercept = 0, slope = 50, color = "red")
plot(p)

# 1.6 - Two-variable point-plot
p = ggplot(data = table2, aes(x = likes_count, y = comments_count))
p = p + geom_point(aes(colour = likes_count), alpha = 0.2)
p = p + scale_x_log10()
p = p + scale_y_log10()
plot(p)

# 1.7 - Add geom_smooth - default
p = ggplot(data = table2, aes(x = likes_count, y = comments_count))
p = p + geom_point(aes(colour = likes_count), alpha = 0.2)
p = p + geom_smooth(na.rm = TRUE, data = table2[table2$likes_count>0 & table2$comments_count>0,])
p = p + scale_x_log10()
p = p + scale_y_log10()
plot(p)

# 1.8 - Add geom_smooth - extra "lm" smooth
p = ggplot(data = table2, aes(x = likes_count, y = comments_count))
p = p + geom_point(aes(colour = likes_count), alpha = 0.2)
p = p + geom_smooth(na.rm = TRUE, 
                    data = table2[table2$likes_count>0 & table2$comments_count>0,])
p = p + geom_smooth(na.rm = TRUE, 
                    data = table2[table2$likes_count>0 & table2$comments_count>0,], method = "lm", colour = "orange")
p = p + scale_x_log10()
p = p + scale_y_log10()
plot(p)

	# 1.8.1 - Or more simply

p = ggplot(data = table2, aes(x = likes_count, y = comments_count))
p = p + geom_point() + 
  geom_smooth(na.rm = TRUE, data = table2[table2$likes_count>0 & table2$comments_count>0,]) + 
  geom_smooth(na.rm = TRUE, data = table2[table2$likes_count>0 & table2$comments_count>0,],method = "lm", colour = "magenta") +
    scale_x_log10() + scale_y_log10()
plot(p)

# 1.9 - Likes over time (two var)
p = ggplot(table2, aes(x = as.Date(created_time), y = likes_count))
p = p + geom_line()
plot(p)

## -------------------------- PLOTTING vol.2 -----------------------------

# 2.1 - Attempting to do a geom_bar plot - does not work; "Error in reorder(section, likes.pr.post) : object 'section' not found"
p = ggplot(table2.section, aes(x = reorder(section, likes.pr.post), 
      y = likes.pr.post))
p = p + geom_bar(stat = "identity") + coord_flip()
plot(p)

	# Using the lubridated data for weekdays 
# 2.2 - Plot of distribtution of likes by weekday - 3rd. line removes grids and colours axis
p = ggplot(table2, aes(x = likes_count, colour = weekday))
p = p + geom_density() + scale_x_log10()
p = p + theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "blue"))
plot(p)

# 2.3 - Facet-wrapped (weekdays) comments_count * likes_count plot with smoothers
p = ggplot(table2, aes(x = likes_count, y = comments_count))
p = p + geom_point() + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE,
              data = table2[table2$likes_count>0 & table2$comments_count>0,])
p = p + facet_wrap(~ weekday, scales = "free")
plot(p)

# 2.4 - Facet-wrapped(weekdays & section) comments_count * likes_count plot with smoothers
p = ggplot(table2.subset, aes(x = likes_count, y = comments_count))
p = p + geom_point() + 
  scale_x_log10() + scale_y_log10() +
  geom_smooth(na.rm = TRUE,
              data = table2.subset[table2.subset$likes_count>0 & table2.subset$comments_count>0,]) + 
  facet_grid(section ~ weekday, scales = "free")
plot(p)

# 2.5 - Tile plot on weekdays by section <- count of obs
p = ggplot(tab, aes(x = section, y = weekday))
p = p + geom_tile(aes(fill = count))
plot(p)

# 2.6 - Hexogonal bins using "hexbin" package
p = ggplot(data = table2, aes(x = likes_count, y = comments_count))
p = p + geom_hex(data = table2[table2$likes_count>0 & table2$comments_count>0,], bins = 78)
p = p + scale_x_log10() + scale_y_log10() + scale_fill_continuous(trans = "log10")
plot(p)

# 2.7 - Lineplot, likes over time: grids removed, axes defined & labs
p = ggplot(data =table2, aes(x = as.Date(created_time), y = likes_count))
p = p + geom_line()
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))
p = p + labs( y= "number of likes @ politiken", x = "Time")
plot(p)






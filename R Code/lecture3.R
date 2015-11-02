## ---- message = FALSE, warning = FALSE-----------------------------------
library("readr")

df = read_csv("https://raw.githubusercontent.com/sebastianbarfort/sds/master/data/politiken.csv", 
  col_types = list(created_time = col_character()))

df$created_time = parse_datetime(df$created_time)

## ---- message = FALSE, warning = FALSE-----------------------------------
library("ggplot2")
p = ggplot(data = df, aes(x = likes_count)) # data & aesthetics
p + geom_histogram() # add geom

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(data = df, aes(x = likes_count)) # data & aesthetics
p = p + geom_histogram() # add geom
p + scale_x_log10() # add log scale 

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(data = df, aes(x = likes_count)) # data & aesthetics
p = p + geom_density() # add geom
p + scale_x_log10() # add log scale 

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(data = df, aes(x = likes_count)) # data & aesthetics
p = p + geom_area(stat = "bin") # add geom
p + scale_x_log10() # add log scale 

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(data = df, aes(x = likes_count)) # data & aesthetics
p + geom_histogram(aes(y=..density..), colour="black", fill="white") + 
    geom_density(alpha=.2, fill="#FF6666") + scale_x_log10() 

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(data = df, aes(x = likes_count, y = comments_count))
p + geom_point() + scale_x_log10() + scale_y_log10() # add log scales 

## ---- message = FALSE, warning = FALSE-----------------------------------
p + geom_point() + geom_smooth(na.rm = TRUE, 
  data = df[df$likes_count>0 & df$comments_count>0,]) + 
  geom_smooth(na.rm = TRUE, 
    data = df[df$likes_count>0 & df$comments_count>0,], 
    method = "lm", colour = "red") + 
  scale_x_log10() + scale_y_log10() # add log scales 

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(df, aes(x = as.Date(created_time), y = likes_count))
p + geom_line()

## ------------------------------------------------------------------------
head(df$link, 3)

## ---- message = FALSE, warning = FALSE-----------------------------------
library("stringr")
df$section = str_extract(df$link, ".dk/[a-z]*")
df$section = gsub(".dk/", "", df$section)
head(df$section, 5)

## ---- message = FALSE, warning = FALSE-----------------------------------
library("dplyr")
df.section = df %>%
  filter(!is.na(section)) %>%
  filter(section != "") %>%
group_by(section) %>%
  summarise(
    likes.pr.post = mean(likes_count, na.rm = TRUE)
) %>%
  arrange(-likes.pr.post)

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(df.section, aes(x = reorder(section, likes.pr.post), 
  y = likes.pr.post))
p + geom_bar(stat = "identity") + coord_flip()

## ---- message = FALSE, warning = FALSE-----------------------------------
library("lubridate")
df$weekday = wday(df$created_time, label = TRUE)

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(df, aes(x = likes_count, colour = weekday))
p = p + geom_density() + scale_x_log10()

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df, aes(x = likes_count, colour = weekday))
p + geom_density() + scale_x_log10()

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(df, aes(x = likes_count, y = comments_count))
p = p + geom_point() + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE, 
    data = df[df$likes_count>0 & df$comments_count>0,]) +
  facet_wrap(~ weekday, scales = "free")

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df, aes(x = likes_count, y = comments_count))
p + geom_point() + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE, 
    data = df[df$likes_count>0 & df$comments_count>0,]) +
  facet_wrap(~ weekday, scales = "free")

## ---- message = FALSE, warning = FALSE-----------------------------------
df.subset = df %>%
  filter(section %in% c("indland", "udland"))

## ---- message = FALSE, warning = FALSE, results="hide"-------------------
p = ggplot(df.subset, aes(x = likes_count, y = comments_count))
p = p + geom_point() + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE, 
  data = df.subset[df.subset$likes_count>0 & 
      df.subset$comments_count>0,]) +
  facet_grid(section~ weekday, scales = "free")

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df.subset, aes(x = likes_count, y = comments_count))
p + geom_point() + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE, 
  data = df.subset[df.subset$likes_count>0 & df.subset$comments_count>0,]) +
  facet_grid(section~ weekday, scales = "free")

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df.subset, aes(x = likes_count, y = comments_count, 
  size = shares_count, colour = weekday, shape = section))
p + geom_point() + scale_x_log10() + scale_y_log10() 

## ---- message = FALSE, warning = FALSE-----------------------------------
tab = data.frame(table(df$section, df$weekday))
names(tab) = c("section", "weekday", "count")

## ---- message = FALSE, warning = FALSE-----------------------------------
p = ggplot(tab, aes(x = section, y = weekday))
p = p + geom_tile(aes(fill = count))

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(tab, aes(x = section, y = weekday))
p = p + geom_tile(aes(fill = count)) +
  theme(axis.text.x = element_text(angle=90)) 
p

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df, aes(x = section))
p + geom_histogram()

## ---- message = FALSE, warning = FALSE, echo = FALSE, echo = FALSE-------
p = ggplot(df, aes(x = likes_count, y = comments_count))
p + geom_point(alpha = .35) + scale_x_log10() + scale_y_log10() 

## ---- message = FALSE, warning = FALSE, echo = FALSE, echo = FALSE-------
p = ggplot(df[df$likes_count>0 & df$comments_count>0,], 
  aes(x = likes_count, y = comments_count))
p + geom_hex() + scale_x_log10() + scale_y_log10() + 
  scale_fill_continuous(trans="log10")

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df, aes(x = as.Date(created_time), y = likes_count))
p + geom_line() + labs(title = "Number of likes over time", 
  y = "number of likes", x = "time")

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df, aes(x = as.Date(created_time), y = likes_count))
p + geom_line() + labs(title = "Number of likes over time", 
  y = "number of likes", x = "time") + theme_minimal()

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df, aes(x = likes_count, y = comments_count))
p + geom_point(alpha = .25) + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE, 
  data = df[df$likes_count>0 & 
      df$comments_count>0,],
    colour = "green") +
      geom_rug() + theme_minimal() +
  labs(x = "likes (log scale)", y = "comments (log scale)",
    title = "Relationship between likes and comments on Politiken's Facebook page")

## ---- message = FALSE, warning = FALSE, echo = FALSE---------------------
p = ggplot(df.subset, aes(x = likes_count, 
  y = comments_count, colour = section))
p + geom_point(alpha = .25) + scale_x_log10() + scale_y_log10() + 
  geom_smooth(na.rm = TRUE, 
  data = df.subset[df.subset$likes_count>0 & 
      df.subset$comments_count>0,],
    method = "lm") +
      geom_rug() + theme_minimal() +
  labs(x = "likes (log scale)", y = "comments (log scale)",
    title = "Relationship between likes and comments on Politiken's Facebook page")


# Loading READR Lib for easier reading of .csv filen via URL
library(readr)

# Accessing githubuserdata GoT data
got.df = read.csv("https://raw.githubusercontent.com/chrisalbon/war_of_the_five_kings_dataset/master/5kings_battles_v1.csv")

# What are the dimensions of the dataset? - The dim-command tells us; 38 by 25 - We could've also just looked at the Data window..
dim(got.df)

# Suppose we also want ALL names in the dataset?
names(got.df)

# the lenght command provides an overview of no. of variables in this dataset
length(got.df)

# Summary of the dataframe dependent on attacker_size presents stats for attacker_size, mean, quantiles etc.
summary(got.df$attacker_size)

# Tables can be used to summarize non-numerical data
table(got.df$attacker_king)

table(got.df$attacker_king, got.df$attacker_outcome)

# Acessing the ggplot2 lib
library(ggplot2)

# Copied from SDS - presents distrubution of attacker kings army sizes
p = ggplot(got.df, aes(x =attacker_size, fill =attacker_king))
  p = p + geom_density(alpha = 0.6)
  p = p + scale_x_log10()
  p = p + labs(x = "size of attacking army (logscaled)", title = "Attacker king sizes")
plot(p)

# Created by AFIL - presents distr. of atk sizes over regions
p = ggplot(got.df, aes(x =attacker_size, fill = location))
  p = p + geom_density(alpha = 0.4)
  p = p + labs(x = "size of attacking army ", title = "Battle location")
plot(p)

# Created by AFIL - Histogram on battle types
p = ggplot(subset(got.df, battle_type != "" ), aes(x = battle_type))
  p = p + geom_histogram()
  p = p + labs (x = "Battle Type", title = "Histogram of density / battle type")
plot(p)

# Copied from SDS - Histogram on attacker_king army sizes
p = ggplot(subset(got.df, attacker_king != ""), aes( x = attacker_king))
  p = p + geom_histogram()
  p = p + labs(x = "king", title = "King activity in war")
plot(p)

# Created by AFIL - Histogram of battle activity on Houses
p = ggplot(subset(got.df, attacker_1 != ""), aes( x = attacker_1))
  p = p + geom_histogram()
  p = p + labs(x = "Faction", title = "House activity in war")
plot(p)


help("ggplot2-package")

p = ggplot(got.df, aes(x = attacker_size, y = defender_size, fill = defender_1, color = defender_1))
  p = p + geom_jitter(size=8)
  p = p + geom_abline(intercept = 0, slope = 1, color = "magenta")
  p = p + labs(x = "Attacker Size", y = "Defender Size", title = "Fairness of Battles")
plot(p)

str(got.df$defender_size)

?ggplot






  




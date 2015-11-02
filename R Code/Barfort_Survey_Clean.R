library("readr")
library("dplyr")

surv.df = read.csv("https://raw.githubusercontent.com/sebastianbarfort/sds/master/data/sds-survey-1.csv")


names(surv.df) = gsub("What.is.your.", "", names(surv.df))
View(surv.df)

names(surv.df)

ssurv.df = surv.df %>%
  mutate(
    field.of.study = ifelse(field.of.study == "", Other, field.of.study),
    degree = ifelse(degree == "", Other.1, degree)
  )

 
library("ggplot2")

p = ggplot(subset(surv.df, gender !=""), aes(x = field.of.study))
  p = p + geom_histogram()
  p = p +labs(x = "Study", title = "Number of students")
plot(p)

p = ggplot(surv.df, aes(x = time.spent, fill = field.of.study))
  p = p + geom_density(alpha = 0.4)
  p = p + scale_x_log10()
  p = p + labs(x = "Study", title = "Age of students")
plot(p)

p = ggplot(subset(surv.df, system !=""), aes(x = system))
    p = p + geom_histogram()
    p = p + labs(x= "System", title = "System usage distribution/ gender")
plot(p)



dim(surv.df)



?ifelse

install.packages("rvest")
library("rvest")
library("dplyr")


ku.lec.links = "http://www.econ.ku.dk/ansatte/vip/"
css.selector = "td:nth-child(1) a"
css.selector2 = "td:nth-child(2)"

ku.personel_1 = read_html(ku.lec.links) %>%
  html_nodes(css = css.selector) %>%
  html_text()
ku.personel_2 = read_html(ku.lec.links) %>%
  html_nodes(css = css.selector2) %>%
  html_text()

ku.links = read_html(ku.lec.links, encoding = "UTF-8") %>%
  html_nodes(css = css.selector) %>%
  html_attr(name = 'href')
head(ku.links, 5)

longlink = paste(ku.lec.links, ku.links, sep = "")
head(longlink,5)

scrape_person_ku = function(longlink){
  long.link = read_html(longlink)
  long.link.id = long.link %>%
    html_nodes("href") %>% html_text()
  long.link.title = long.link %>%
    html_nodes(".type") %>%
    html_text()
  return(cbind(long.link.id, long.link.title, longlink))
}



ku.data.df = list()
for(i in longlink){
  print(paste("processing", i, sep = " "))
  ku.data.df[[i]] = scrape_person_ku(i)
  #wait
  Sys.sleep(1)
  cat("done!\n")
}

library("plyr")
ku.dat.df = ldply(ku.data.df)





  ################# HELPERS #################
firstlink = longlink[1]
scrape_person_ku(firstlink)



ku.personel = cbind(ku.personel_1, ku.personel_2)

ku.personel







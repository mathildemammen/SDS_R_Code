## Twitter test
  

install.packages("twitteR")
library("twitteR")
install.packages("streamR")
library("streamR")


key = "ephYT3K5vLoTxkATlmUgL0beL"
secret = "cVQif36uKHjuzuQLbIwxXm67CSQ46UzLbrV6kBrqnvmY9JWET4"
token = "270261031-VgRdEcqPyXMzNh9dlDMTa1xQH3WaesQymG1S8XSK"
access.secret = "0gI4BDkkpxgStjPYg4iSGZWzQkF76q9Hkjg1WgLtSmjnK"

setup_twitter_oauth(key, secret, token, access.secret)

searchTwitter("#NDP", n=100)

#### NYT test

install.packages("rtimes")
library("rtimes")

article.key = "96614b9765e5f369105f25a3800e168c:19:73304880"

out = as_search(q = "Donald Trump", begin_date = "20150801", end_date = "20150901", key =  "96614b9765e5f369105f25a3800e168c:19:73304880")
out$data[1:4]

######## STATDK

install.packages("devtools")
library("devtools")
install.packages("Rtools")
install_github("rOpenGov/dkstat")


  
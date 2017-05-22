# the sentiment and Rstem packages are no longer available so we need to fetch them with devtools 


install.packages("devtools")
require(devtools)
install_url("http://www.omegahat.net/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")





#Another alternative is the tm.plugin.sentiment


### all is broken as fuck ####

#install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org") 
# ^ not working on Windows like this; it says it can't find binaries, but that is a lie...


# when installing from github cant find xts package

install_github("mannau/tm.plugin.sentiment")

#install.packages("tm.plugin.webmining")


#library(tm.plugin.sentiment)

# retrieve corpus
#require(tm.plugin.webmining)
corp = WebCorpus(GoogleFinanceSource("AAPL"))

# score corpus
corp <- score(corp)
sentixts <- metaXTS(corp)

# chart sentiment scores
chartSentiment(sentixts)
## Creates a png word frequency cloud for each of the corpus demographic groups.
## Use after running corpusBuilder in your current R session.

####### read a stop word list #######

palabrasParo <-read.table("~/hablaMty/stopwords/palabrasParo.txt", 
                          encoding = "UTF-8", colClasses = "character")


###### Create term-document matrix and plot wordcloud ######

for (i in seq_along(grupos)){
    
    tdm <- TermDocumentMatrix(grupos[[i]],
                              control = list(removePunctuation = TRUE,
                                             stopwords = c("pos","allá","digo","nomás","mjm", palabrasParo[,1]),
                                             removeNumbers = TRUE, tolower = TRUE,
                                             content_transformer(stripWhitespace)))
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE) 
    dm <- data.frame(word = names(word_freqs), freq = word_freqs)
    top <- dm[1:100,]
    png(paste0("~/hablaMty/img/",nombres[i],".png"), width=12, height=8, 
        units="in", res = 300)
    wordcloud(top$word, top$freq, random.order=FALSE, 
              colors = brewer.pal(8, "Dark2"))
    dev.off()
}
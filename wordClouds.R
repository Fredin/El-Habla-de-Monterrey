#install.packages("tm")
#install.packages("stringi")
#install.packages("wordcloud")

library(tm)
library(stringi)
library(wordcloud)

##### Create cropus ######
hablaMty <- Corpus(DirSource("~/hablaMty/TEXT"),
                   readerControl = list(reader = readPlain,
                                        language = "spa",
                                        load = FALSE,
                                        encoding = "UTF-8"))

##### Setting metadata tags ######
for (i in seq_along(hablaMty)) {
    splitID <- strsplit(meta(hablaMty[[i]], "id"), "")
    meta(hablaMty[[i]], tag = "sexo") <- splitID[[1]][6]
    meta(hablaMty[[i]], tag = "grupo_edad") <- splitID[[1]][7]
    meta(hablaMty[[i]], tag = "nivel_edu") <- splitID[[1]][8]
}

##### Unicode fixing #####
fixUnicode<- function (x){
    x<-stri_escape_unicode(x)
    x<-gsub("\\u00c3\\u00a1", "\\u00e1", x, fixed = TRUE) # a w acute accent
    x<-gsub("\\u00c3\\u00a9", "\\u00e9", x, fixed = TRUE) # e w acute accent
    x<-gsub("\\u00c3\\u00ad", "\\u00ed", x, fixed = TRUE) # i w acute accent
    x<-gsub("\\u00c3\\u00b3", "\\u00f3", x, fixed = TRUE) # o w acute accent
    x<-gsub("\\u00c3\\u00ba", "\\u00fa", x, fixed = TRUE) # u w acute accent 
    x<-gsub("\\u00c3\\u00b1", "\\u00f1", x, fixed = TRUE) # n w tilde
    x<-gsub("\\u00c2\\u00bf", "\\u00bf", x, fixed = TRUE) # inverted questionm.
    x<-gsub("\\u00c2\\u00a1", "\\u00a1", x, fixed = TRUE) # inverted bang
    x<-gsub("\\u00e2\\u20ac\\u0153", "\\u201c", x, fixed = TRUE) # o.d.quot. m.
    x<-gsub("\\u00e2\\u20ac\\u009d", "\\u201d", x, fixed = TRUE) # c.d.quot. m.
    x<-gsub("\\u00c3\\u00bc", "\\u00fc", x, fixed = TRUE) # u w dieresis
    x<-gsub("\\u00e2\\u20ac\\u2122", "'", x, fixed = TRUE) # u w dieresis
    x<-stri_unescape_unicode(x)
    return(x)
}

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))

###### Transformations #######
hablaMty <- tm_map(hablaMty, f, "^E:.*") # get rid of interviewer
hablaMty <- tm_map(hablaMty, f, "<.*>") # get rid of annotations
hablaMty <- tm_map(hablaMty, content_transformer(fixUnicode))

####### Subsetting by groups #######
h11 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "1" &
                    meta(hablaMty, tag = "nivel_edu") == "1"]

h12 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "1" &
                    meta(hablaMty, tag = "nivel_edu") == "2"]

h13 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "1" &
                    meta(hablaMty, tag = "nivel_edu") == "3"]

h21 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "2" &
                    meta(hablaMty, tag = "nivel_edu") == "1"]

h22 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "2" &
                    meta(hablaMty, tag = "nivel_edu") == "2"]

h23 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "2" &
                    meta(hablaMty, tag = "nivel_edu") == "3"]

h31 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "3" &
                    meta(hablaMty, tag = "nivel_edu") == "1"]

h32 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "3" &
                    meta(hablaMty, tag = "nivel_edu") == "2"]

h33 <- hablaMty[meta(hablaMty, tag = "sexo") == "H" & 
                    meta(hablaMty, tag = "grupo_edad") == "3" &
                    meta(hablaMty, tag = "nivel_edu") == "3"]

m11 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "1" &
                    meta(hablaMty, tag = "nivel_edu") == "1"]

m12 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "1" &
                    meta(hablaMty, tag = "nivel_edu") == "2"]

m13 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "1" &
                    meta(hablaMty, tag = "nivel_edu") == "3"]

m21 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "2" &
                    meta(hablaMty, tag = "nivel_edu") == "1"]

m22 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "2" &
                    meta(hablaMty, tag = "nivel_edu") == "2"]

m23 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "2" &
                    meta(hablaMty, tag = "nivel_edu") == "3"]

m31 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "3" &
                    meta(hablaMty, tag = "nivel_edu") == "1"]

m32 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "3" &
                    meta(hablaMty, tag = "nivel_edu") == "2"]

m33 <- hablaMty[meta(hablaMty, tag = "sexo") == "M" & 
                    meta(hablaMty, tag = "grupo_edad") == "3" &
                    meta(hablaMty, tag = "nivel_edu") == "3"]

###### Create term-document matrix and plot wordcloud ######
grupos <- list(h11, h12, h13, h21, h22, h23, h31, h32, h33, m11, m12, m13, m21, 
               m22, m23, m31, m32, m33)
nombres <- c("h11", "h12", "h13", "h21", "h22", "h23", "h31", "h32", "h33", 
              "m11", "m12", "m13", "m21", "m22", "m23", "m31", "m32", "m33")
palabrasParo <-read.table("~/hablaMty/stopwords/palabrasParo.txt", 
                          encoding = "UTF-8", colClasses = "character")

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

##### Pruebas ######

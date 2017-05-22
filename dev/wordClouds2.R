install.packages("tm")
install.packages("stringi")
install.packages("wordcloud")

library(tm)
library(stringi)
library(wordcloud)

##### Create cropus ######
hablaMty <- Corpus(DirSource("./TEXT"),
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

###### Transformations #######
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
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
palabrasParo <-read.table("stopwords/palabrasParo.txt",
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

getText <- function (x, lema, ignore.case = TRUE) {
    mydf <- data.frame(NULL)
    for (i in seq_along(x)){
        linea <-grep(lema, as.character(x[[i]]), ignore.case = ignore.case)
        texto <- grep(lema, as.character(x[[i]]), ignore.case = ignore.case,
                     value = TRUE)
        id <- rep(deparse(substitute(x)), length(linea))
        numDoc <- rep(i, length(linea))
        tmpdf <- data.frame(texto, id, numDoc, linea)
        mydf <- rbind(mydf, tmpdf)
    }
    return(mydf)
}

#### muerte #####

mH11 <- getText(h11, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH12 <- getText(h12, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH13 <- getText(h13, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH21 <- getText(h21, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH22 <- getText(h22, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH23 <- getText(h23, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH31 <- getText(h31, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH32 <- getText(h32, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mH33 <- getText(h33, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM11 <- getText(m11, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM12 <- getText(m12, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM13 <- getText(m13, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM21 <- getText(m21, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM22 <- getText(m22, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM23 <- getText(m23, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM31 <- getText(m31, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM32 <- getText(m32, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")
mM33 <- getText(m33, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")

laMuerte <- rbind(mH11,
    mH12,
    mH13,
    mH21,
    mH22,
    mH23,
    mH31,
    mH32,
    mH33,
    mM11,
    mM12,
    mM13,
    mM21,
    mM22,
    mM23,
    mM31,
    mM32,
    mM33)
write.csv(laMuerte,"muerteHablaMty.csv")

muerte <- Corpus(VectorSource(laMuerte[,1]))

muerte <- tm_map(muerte, f, "(\\<mur|\\<muert|\\<muer|\\<mori|\\<morí)")

tdm <- TermDocumentMatrix(muerte,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("pos","allá","digo","nomás","mjm", palabrasParo[,1]),
                                         removeNumbers = TRUE, tolower = TRUE,
                                         content_transformer(stripWhitespace)))
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
dm <- data.frame(word = names(word_freqs), freq = word_freqs)
top <- dm[1:100,]
png("muerte.png", width=12, height=8,
    units="in", res = 300)
wordcloud(top$word, top$freq, random.order=FALSE,
          colors = brewer.pal(8, "Dark2"))
dev.off()

### sacar ####

mH11 <- getText(h11, "(\\<sac|\\<saqu)")
mH12 <- getText(h12, "(\\<sac|\\<saqu)")
mH13 <- getText(h13, "(\\<sac|\\<saqu)")
mH21 <- getText(h21, "(\\<sac|\\<saqu)")
mH22 <- getText(h22, "(\\<sac|\\<saqu)")
mH23 <- getText(h23, "(\\<sac|\\<saqu)")
mH31 <- getText(h31, "(\\<sac|\\<saqu)")
mH32 <- getText(h32, "(\\<sac|\\<saqu)")
mH33 <- getText(h33, "(\\<sac|\\<saqu)")
mM11 <- getText(m11, "(\\<sac|\\<saqu)")
mM12 <- getText(m12, "(\\<sac|\\<saqu)")
mM13 <- getText(m13, "(\\<sac|\\<saqu)")
mM21 <- getText(m21, "(\\<sac|\\<saqu)")
mM22 <- getText(m22, "(\\<sac|\\<saqu)")
mM23 <- getText(m23, "(\\<sac|\\<saqu)")
mM31 <- getText(m31, "(\\<sac|\\<saqu)")
mM32 <- getText(m32, "(\\<sac|\\<saqu)")
mM33 <- getText(m33, "(\\<sac|\\<saqu)")

sacar <- rbind(mH11,
                  mH12,
                  mH13,
                  mH21,
                  mH22,
                  mH23,
                  mH31,
                  mH32,
                  mH33,
                  mM11,
                  mM12,
                  mM13,
                  mM21,
                  mM22,
                  mM23,
                  mM31,
                  mM32,
                  mM33)
write.csv(sacar,"sacarHablaMty.csv")

sacar <- Corpus(VectorSource(sacar[,1]))

sacar <- tm_map(sacar, f, "(\\<sac|\\<saqu)")

tdm <- TermDocumentMatrix(sacar,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("pos","allá","digo","nomás","mjm", palabrasParo[,1]),
                                         removeNumbers = TRUE, tolower = TRUE,
                                         content_transformer(stripWhitespace)))
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
dm <- data.frame(word = names(word_freqs), freq = word_freqs)
top <- dm[1:100,]
png("sacar.png", width=12, height=8,
    units="in", res = 300)
wordcloud(top$word, top$freq, random.order=FALSE,
          colors = brewer.pal(8, "Dark2"))
dev.off()

### meter ####

mH11 <- getText(h11, "(\\<met)")
mH12 <- getText(h12, "(\\<met)")
mH13 <- getText(h13, "(\\<met)")
mH21 <- getText(h21, "(\\<met)")
mH22 <- getText(h22, "(\\<met)")
mH23 <- getText(h23, "(\\<met)")
mH31 <- getText(h31, "(\\<met)")
mH32 <- getText(h32, "(\\<met)")
mH33 <- getText(h33, "(\\<met)")
mM11 <- getText(m11, "(\\<met)")
mM12 <- getText(m12, "(\\<met)")
mM13 <- getText(m13, "(\\<met)")
mM21 <- getText(m21, "(\\<met)")
mM22 <- getText(m22, "(\\<met)")
mM23 <- getText(m23, "(\\<met)")
mM31 <- getText(m31, "(\\<met)")
mM32 <- getText(m32, "(\\<met)")
mM33 <- getText(m33, "(\\<met)")

meter <- rbind(mH11,
                  mH12,
                  mH13,
                  mH21,
                  mH22,
                  mH23,
                  mH31,
                  mH32,
                  mH33,
                  mM11,
                  mM12,
                  mM13,
                  mM21,
                  mM22,
                  mM23,
                  mM31,
                  mM32,
                  mM33)
write.csv(meter,"meterHablaMty.csv")


meter <- Corpus(VectorSource(meter[,1]))

meter <- tm_map(meter, f, "(\\<met)")

tdm <- TermDocumentMatrix(meter,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("pos","allá","digo","nomás","mjm", palabrasParo[,1]),
                                         removeNumbers = TRUE, tolower = TRUE,
                                         content_transformer(stripWhitespace)))
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
dm <- data.frame(word = names(word_freqs), freq = word_freqs)
top <- dm[1:100,]
png("meter.png", width=12, height=8,
    units="in", res = 300)
wordcloud(top$word, top$freq, random.order=FALSE,
          colors = brewer.pal(8, "Dark2"))
dev.off()

### onda ####

mH11 <- getText(h11, "(de onda)", FALSE)
mH12 <- getText(h12, "(de onda)", FALSE)
mH13 <- getText(h13, "(de onda)", FALSE)
mH21 <- getText(h21, "(de onda)", FALSE)
mH22 <- getText(h22, "(de onda)", FALSE)
mH23 <- getText(h23, "(de onda)", FALSE)
mH31 <- getText(h31, "(de onda)", FALSE)
mH32 <- getText(h32, "(de onda)", FALSE)
mH33 <- getText(h33, "(de onda)", FALSE)
mM11 <- getText(m11, "(de onda)", FALSE)
mM12 <- getText(m12, "(de onda)", FALSE)
mM13 <- getText(m13, "(de onda)", FALSE)
mM21 <- getText(m21, "(de onda)", FALSE)
mM22 <- getText(m22, "(de onda)", FALSE)
mM23 <- getText(m23, "(de onda)", FALSE)
mM31 <- getText(m31, "(de onda)", FALSE)
mM32 <- getText(m32, "(de onda)", FALSE)
mM33 <- getText(m33, "(de onda)", FALSE)

onda <- rbind(mH11,
                  mH12,
                  mH13,
                  mH21,
                  mH22,
                  mH23,
                  mH31,
                  mH32,
                  mH33,
                  mM11,
                  mM12,
                  mM13,
                  mM21,
                  mM22,
                  mM23,
                  mM31,
                  mM32,
                  mM33)
write.csv(onda,"ondaHablaMty.csv")

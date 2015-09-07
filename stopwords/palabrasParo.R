#Tomamos una lista nueva de palabras paro de https://code.google.com/p/stop-words/
# Como hay dos listas, las consolidamos en una sola. 


palabrasParo1 <- read.table("~/hablaMty/stopwords/stop-words_spanish_1_es.txt", 
                            encoding = "UTF-8", colClasses = "character")
palabrasParo1 <- enc2utf8(palabrasParo1[,1])
palabrasParo2 <- read.table("~/hablaMty/stopwords/stop-words_spanish_2_es.txt", 
                            encoding = "UTF-8", colClasses = "character")
palabrasParo2 <- enc2utf8(palabrasParo2[,1])
palabrasParo <- c(palabrasParo1, palabrasParo2)
palabrasParo <- unique(palabrasParo)
write.table(palabrasParo, "~/hablaMty/stopwords/palabrasParo.txt", 
            fileEncoding = "UTF-8")
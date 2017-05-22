# un comment these if not installed
#install.packages("tm")
#install.packages("stringi")
#install.packages("wordcloud")

library(tm)
library(stringi)
library(wordcloud)

##### Create cropus ######
hablaMty <- Corpus(DirSource("~/hablaMty/text"),
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

##### Unicode fixing functions#####
fixUnicode<- content_transformer(function (x){
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
})

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))

###### Transformations #######
hablaMty <- tm_map(hablaMty, f, "^E:.*") # get rid of interviewer
hablaMty <- tm_map(hablaMty, f, "<.*>") # get rid of annotations
hablaMty <- tm_map(hablaMty, fixUnicode)

####### Subsetting by groups #######

subsetter <- function(sexo, edad, edu){
    x<-hablaMty[meta(hablaMty, tag = "sexo") == sexo & 
                 meta(hablaMty, tag = "grupo_edad") == edad &
                 meta(hablaMty, tag = "nivel_edu") == edu]
    return(x)
}

nombres <- c("H11", "H12", "H13", "H21", "H22", "H23", "H31", "H32", "H33", 
             "M11", "M12", "M13", "M21", "M22", "M23", "M31", "M32", "M33")

grupos<- list()

for (i in seq_along(nombres)){
    sexo<-substr(nombres[i],1,1)
    edad<-substr(nombres[i],2,2)
    edu<-substr(nombres[i],3,3)
    
    grupos[[i]] <- assign(nombres[i], subsetter(sexo,edad,edu))
    
}

##### getText ######
### This little function lets you recover text from a corpus. 
### "x" is a corpus object,"lema" is the regex for the strings you are trying
### to match, e.g. getText(H11,"ching+[a-z]")

getText <- function (x, lema, ignore.case = TRUE) {
    mydf <- data.frame(NULL)
    for (i in seq_along(x)){
        linea <-grep(lema, as.character(x[[i]]), ignore.case = ignore.case)
        texto <-grep(lema, as.character(x[[i]]), ignore.case = ignore.case,
                      value = T)
        grupo <- rep(deparse(substitute(x)), length(linea))
        numDoc <- rep(i, length(linea))
        tmpdf <- data.frame(as.character(texto), grupo, numDoc, linea)
        mydf <- rbind(mydf, tmpdf)
    }
    return(mydf)
}
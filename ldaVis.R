library(tm)
library(lda)
library(LDAvis)
library(stringi)

hablaMty <- Corpus(DirSource("~/hablaMty/TEXT"),
                   readerControl = list(reader = readPlain,
                                        language = "spa",
                                        load = FALSE,
                                        encoding = "UTF-8"))

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

hablaMty <- tm_map(hablaMty, content_transformer(fixUnicode))

###### Transformations #######
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
hablaMty <- tm_map(hablaMty, f, "<.*>") # get rid of annotations

###### Stopwords #####
palabrasParo <-read.table("~/hablaMty/stopwords/palabrasParo.txt", 
                          encoding = "UTF-8", colClasses = "character")

tdm <- TermDocumentMatrix(hablaMty,
            control = list(removePunctuation = TRUE,
                    stopwords = c("pos","allá","digo","nomás","mjm", palabrasParo[,1]),
                    removeNumbers = TRUE, tolower = TRUE,
                    content_transformer(stripWhitespace)))


tdm <- removeSparseTerms(tdm, .25)

m <- as.matrix(tdm)



##### Pruebas ######
writeLines(as.character(hablaMty[[1]]))



# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = variable.names(m), K = K, vocab = row.names(m), 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)

serVis(json, out.dir = 'vis', open.browser = FALSE)

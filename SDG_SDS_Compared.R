# Script developed for TU-1701 Sustainable Development
# Developed by Lars Schulz

# Daten einlesen ----
sdg <- read.csv("data/sdg_raw.csv", header=TRUE, na.strings="NA", strip.white=TRUE, stringsAsFactors = FALSE)
sds <- read.csv("data/sds_raw.csv", header=TRUE, na.strings="NA", strip.white=TRUE, stringsAsFactors = FALSE)

sdg_goals <- read.csv("data/sdg_goals.csv", header=TRUE, na.strings="NA", strip.white=TRUE, stringsAsFactors = FALSE)

str(sdg)
str(sds)

# Daten transformieren ----

# sdg Tabelle transformieren
sdg$text <- do.call("rbind", strsplit(sub(" ", ";", sdg$indicator_sdg), ";"))
sdg$indicator_sdg <- sub(".*? (.+)", "\\1", sdg[,1]) #the regular expression that contains terms to be removed:
# sdg[,2] <- NULL # drop second column (not needed)

# SDS tabelle transformieren
# @TODO: keep code part
sds$id <- paste("(", sub(".*? \\((.+)", "\\1", sds[,1]) ) #the regular expression that contains terms to be removed:
sds$indicator_sds <- do.call("rbind", strsplit(sub(" \\(", ";", sds$indicator_sds), ";"))

# SDG Goals Tabelle transformieren
sdg_goals$text <- do.call("rbind", strsplit(sub(" ", ";", sdg_goals$goals_sdg), ";"))
sdg_goals$goals_sdg <- sub(".*? (.+)", "\\1", sdg_goals[,1]) #the regular expression that contains terms to be removed:
# sdg[,2] <- NULL # drop second column (not needed)

# Prepare for kumu import ----
# https://docs.kumu.io/guides/import.html

# install.packages("NLP")
# install.packages("openNLP")
library(NLP) 
library(openNLP)
options(java.parameters = "- Xmx1024m") # increase memory

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  # list(POStagged = POStagged, POStags = POStags)
}

# @TODO: Funktion auf jede Reihe des Vektors anwenden
test <-  lapply( sds$indicator_sds, tagPOS )
test_text <- lapply( c("Hallo", "Vater"), tagPOS )
paste ( test_text[grepl("NN", test_text)] )

# Testing lapply
x <- c("A","B","C")
lapply (sds$indicator_sds[2:4],tagPOS)

colnames(sdg)[1] <- "Label"



# Output ----

write.csv(sdg, file = "results/sdg_clean.csv")
write.csv(sds, file = "results/sds_clean.csv")

#install.packages("xlsx")
library(xlsx)
sdg$text <- sdg$id
write.xlsx(sdg$text, file = "results/sdg_clean.xlsx")
write.xlsx(sdg_goals$text, file = "results/sdg_goals.xlsx")
write.xlsx(sds, file = "results/sds_clean.xlsx")




# Wordcloud ----
# https://georeferenced.wordpress.com/2013/01/15/rwordcloud/

library(tm)
library(wordcloud)

wordcloud(sdg, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))




# Method 1: using the native R adist
source1.devices<-sdg
source2.devices<-sds

str(sds)
# To make sure we are dealing with charts
source1.devices$name<-as.character(source1.devices$indicator_sdg)
source2.devices$name<-source2.devices$indicator_sds

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name<-adist(source1.devices$name,source2.devices$name, partial = TRUE, ignore.case = TRUE)

# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)

match.s1.s2<-NULL  
for(i in 1:nrow(dist.name))
{
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name[i]),match.s1.s2)
}
# and we then can have a look at the results
View(match.s1.s2)

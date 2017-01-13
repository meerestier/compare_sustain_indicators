# TU-1701 Sustainable Development

# Daten einlesen ----
sdg <- read.csv("data/sdg_raw.csv", header=TRUE, na.strings="NA", strip.white=TRUE)
sds <- read.csv("data/sds_raw.csv", header=TRUE, na.strings="NA", strip.white=TRUE)

str(sdg)
str(sds)

# Daten transformieren ----

# sdg Tabelle transformieren
sdg$id <- do.call("rbind", strsplit(sub(" ", ";", sdg$indicator_sdg), ";"))
sdg$indicator_sdg <- sub(".*? (.+)", "\\1", sdg[,1]) #the regular expression that contains terms to be removed:
sdg[,2] <- NULL # drop second column (not needed)

# SDS tabelle transformieren
# @TODO: keep code part
# sds$id <- sub(".*? (.+)", "\\1", sdg[,1]) #the regular expression that contains terms to be removed

# drop rows with "(sd"
sds <- data.frame (sds[!grepl("\\(sd", sds$indicator_sds),])
sds$indicator_sds <- sds[,1]
sds[,1] <- NULL

sds$indicator_sds <- do.call("rbind", strsplit(sub("\\(", ";", sds$indicator_sds), ";")) #remove ()


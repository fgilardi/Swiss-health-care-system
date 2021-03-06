rm(list = ls())

library(rio)
library(reshape)
library(ggplot2)

setwd("~/Documents/Github/Swiss-health-care-system/")

d <- import_list("Monatliche-Standardprämien-nach-Kanton-in-Franken.xlsx")

kpr <- import("Landesindex-der-Konsumentenpreise.xlsx")

names(d)

d_19_25 <- d[[1]]

d_19_25 <- melt(d_19_25)
colnames(d_19_25) <- c("Kanton", "Jahr", "Prämien")
d_19_25$Jahr <- as.numeric(levels(d_19_25$Jahr))[d_19_25$Jahr]

d_19_25$prämien_kpi <- NA

yr <- unique(d_19_25$Jahr)

for(i in 1:length(yr)){

	d_19_25$prämien_kpi[d_19_25$Jahr == yr[i]] <- d_19_25$Prämie[d_19_25$Jahr == 2000] * kpr$Index[kpr$Jahr == yr[i]]/100

}

d_19_25 <- melt(d_19_25, id.vars = c("Kanton", "Jahr"))

colnames(d_19_25) <- c("Kanton", "Jahr", "Inflation", "Prämien")

d_19_25$Inflation <- as.character(levels(d_19_25$Inflation))[d_19_25$Inflation]

d_19_25$Inflation[d_19_25$Inflation == "Prämien"] <- "Tatsächliche Entwicklung"
d_19_25$Inflation[d_19_25$Inflation == "prämien_kpi"] <- "Entwicklung gemäss Teuerung (2000 = 100)"

head(d_19_25)

ggplot(data = subset(d_19_25, Kanton == "CH" & Jahr >= 2000), aes(x = Jahr, y = Prämien, group = Inflation, color = Inflation)) +
	geom_line() +
	labs(y = "Fr./Monat", title = "Monatliche Standardprämien, Schweizer Durchschnitt") +
	theme_light() +
	NULL

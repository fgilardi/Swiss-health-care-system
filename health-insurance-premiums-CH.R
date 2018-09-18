rm(list = ls())
library(ggplot2)

setwd("~/Documents/Github/misc/Swiss health insurance premiums/")
d <- read.csv2("health-insurance-premiums-CH.csv", sep = ";", stringsAsFactors = FALSE)
head(d)
names(d)
dim(d)

d <- subset(d, (Altersklasse == "AKL-JUG") & (Unfalleinschluss == "MIT-UNF") & (isBaseF == 1) & (isBaseP == 1) & !(Kanton %in% c("ZE", "ZR")))

d$Prämie <- as.numeric(d$Prämie)
d$Kanton <- factor(d$Kanton)
kt <- unique(d$Kanton)

pr <- c()
d$Prämie_median <- NA
for(i in 1:length(kt)){

	d$Prämie_median[d$Kanton == kt[i]] <- median(d$Prämie[d$Kanton == kt[i]], na.rm = TRUE)
	pr[i] <- median(d$Prämie[d$Kanton == kt[i]])

}

d$Kanton <- factor(d$Kanton, levels = unique(d$Kanton[order(d$Prämie_median, decreasing = FALSE)]))

d2 <- data.frame(Kanton = kt, Prämien = pr)
d2$Ärzte_pro100k_2016 <- NA
d2$Ärzte_pro100k_2016[d2$Kanton == "VD"] <- 244
d2$Ärzte_pro100k_2016[d2$Kanton == "VS"] <- 156
d2$Ärzte_pro100k_2016[d2$Kanton == "GE"] <- 376
d2$Ärzte_pro100k_2016[d2$Kanton == "BE"] <- 220
d2$Ärzte_pro100k_2016[d2$Kanton == "FR"] <- 138
d2$Ärzte_pro100k_2016[d2$Kanton == "SO"] <- 164
d2$Ärzte_pro100k_2016[d2$Kanton == "NE"] <- 220
d2$Ärzte_pro100k_2016[d2$Kanton == "JU"] <- 144
d2$Ärzte_pro100k_2016[d2$Kanton == "GE"] <- 376
d2$Ärzte_pro100k_2016[d2$Kanton == "BS"] <- 435
d2$Ärzte_pro100k_2016[d2$Kanton == "BL"] <- 234
d2$Ärzte_pro100k_2016[d2$Kanton == "AG"] <- 167
d2$Ärzte_pro100k_2016[d2$Kanton == "ZH"] <- 257
d2$Ärzte_pro100k_2016[d2$Kanton == "GL"] <- 152
d2$Ärzte_pro100k_2016[d2$Kanton == "SH"] <- 188
d2$Ärzte_pro100k_2016[d2$Kanton == "AR"] <- 171
d2$Ärzte_pro100k_2016[d2$Kanton == "AI"] <- 137
d2$Ärzte_pro100k_2016[d2$Kanton == "SG"] <- 191
d2$Ärzte_pro100k_2016[d2$Kanton == "GR"] <- 175
d2$Ärzte_pro100k_2016[d2$Kanton == "TG"] <- 161
d2$Ärzte_pro100k_2016[d2$Kanton == "LU"] <- 166
d2$Ärzte_pro100k_2016[d2$Kanton == "UR"] <- 94
d2$Ärzte_pro100k_2016[d2$Kanton == "SZ"] <- 147
d2$Ärzte_pro100k_2016[d2$Kanton == "OW"] <- 120
d2$Ärzte_pro100k_2016[d2$Kanton == "NW"] <- 129
d2$Ärzte_pro100k_2016[d2$Kanton == "ZG"] <- 206
d2$Ärzte_pro100k_2016[d2$Kanton == "TI"] <- 219

d2$Kanton <- factor(d2$Kanton, levels = unique(d2$Kanton[order(d2$Prämien, decreasing = FALSE)]))


#png("premiums-2018-boxplot.png", width=7, height=5.5, units="in", res=400)
#pdf(file="premiums-2018-boxplot.pdf", paper="special", width=7, height=5.5)
ggplot(data = d, aes(y = Prämie, x = Kanton)) + 
	geom_boxplot(notch = FALSE, outlier.shape = NA, color = "red") +
	labs(y = "", x = "Fr./Monat", title = "Krankenversicherungsprämien 2018", subtitle = "Junge Erwachsene (19-25), ordentliche Franchise (300 Fr.), mit Unfallversicherung", caption = "Daten: https://opendata.swiss/de/dataset/health-insurance-premiums/.") +
	theme_light() +
	theme(axis.text = element_text(size = 10))
	NULL
#dev.off()



#png("premiums-2018-dotplot.png", width=7, height=5.5, units="in", res=400)
#pdf(file="premiums-2018-dotplot.pdf", paper="special", width=7, height=5.5)
ggplot(data = d2, aes(x = Prämien, y = Kanton)) + 
	geom_point(color = "red", size = 2) +
	labs(y = "", x = "Fr./Monat", title = "Krankenversicherungsprämien 2018: Medianwerte", subtitle = "Junge Erwachsene (19-25), ordentliche Franchise (300 Fr.), mit Unfallversicherung", caption = "Daten: BAG.") +
	theme_light() +
	theme(axis.text = element_text(size = 10))
	NULL
#dev.off()


#png("premiums-doctors-2018.png", width=7, height=5.5, units="in", res=400)
#pdf(file="premiums-doctors-2018.pdf", paper="special", width=7, height=5.5)
ggplot(data = d2, aes(x = Ärzte_pro100k_2016, y = Prämien)) + 
geom_smooth(method = "lm", se = FALSE, color = "red", linetype = 1, size = 0.5) +
	geom_text(label = d2$Kanton, color = "red", size = 4) +
	labs(y = "Prämien (Fr./Monat, 2018)", x = "Anzahl Ärzte pro 100'000 Einwohner (2016)", title = "Korrelation zwischen Ärztedichte und Krankenversicherungsprämien", caption = "Daten: BAG, BFS.") +
	theme_light() +
	theme(axis.text = element_text(size = 10))
	NULL
#dev.off()


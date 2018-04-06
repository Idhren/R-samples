# HD2 2. első ránézések
# 2017.08.08.
# Limbek Zsófi

# ez a honlap jónak tűnik: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4466856/

library(data.table)
library(ggplot2)
library(xlsx)
library(party)

# ábrázolások

temp <- long[tag == "R_SURM302.POR" & 
                 (date_ext >= as.POSIXct("2016-07-11") & 
                      date_ext <= as.POSIXct("2016-07-15"))]
setorder(temp, date_ext)

ggplot(temp) + 
    geom_point(aes(x = date_ext, y = val, color = bent, 
                   shape = termek), size = 1.5, alpha = .5) +
    scale_color_manual(values = c("FALSE" = "darkred","TRUE" = "turquoise3")) +
    geom_col(aes(x = extr_kezd, y = as.numeric(bent)), fill = "turquoise3", 
             width = 1000) +
    geom_col(aes(x = extr_veg, y = as.numeric(bent)), fill = "darkred", 
             width = 1000, alpha = 0.5) +
    coord_cartesian(ylim = c(0.951, 0.955)) +
    scale_x_datetime(date_breaks = "12 hours",
                     date_minor_breaks = "1 hour",
                     date_labels = "%m.%d. %H:%M") +
    labs(title = "A SURM302.POR taghez tartozó adatpontok") +
    theme(axis.title.y = element_blank())

ggsave("plots/eltunes_SURM302.POR_07_11-15.png")

temp <- long[tag == "R_SURTK404.GRAN" & 
                 (date_ext >= as.POSIXct("2016-07-11") & 
                      date_ext <= as.POSIXct("2016-07-15"))]
setorder(temp, date_ext)

ggplot(temp) + 
    geom_point(aes(x = date_ext, y = val, color = bent, 
                   shape = termek), size = 1.5, alpha = 0.5) +
    scale_color_manual(values = c("FALSE" = "darkred","TRUE" = "turquoise3")) +
    geom_col(aes(x = extr_kezd, y = as.numeric(bent)), fill = "turquoise3", 
             width = 1000) +
    geom_col(aes(x = extr_veg, y = as.numeric(bent)), fill = "darkred", 
             width = 1000, alpha = 0.5) +
    coord_cartesian(ylim = c(0.9513, 0.9552)) +
    scale_x_datetime(date_breaks = "12 hours",
                     date_minor_breaks = "1 hour",
                     date_labels = "%m.%d. %H:%M") +
    labs(title = "A SURTK404.GRAN taghez tartozó adatpontok") +
    theme(axis.title.y = element_blank())

ggsave("plots/eltunes_SURTK404.GRAN_07_11-15.png")

temp <- long[tag == "R_MFID201.ZAGY" & 
                 (date_ext >= as.POSIXct("2016-07-11") & 
                      date_ext <= as.POSIXct("2016-07-15"))]
setorder(temp, date_ext)

ggplot(temp) + 
    geom_point(aes(x = date_ext, y = val, color = bent, 
                   shape = termek), size = 1.5, alpha = 0.5) +
    scale_color_manual(values = c("FALSE" = "darkred","TRUE" = "turquoise3")) +
    geom_col(aes(x = extr_kezd, y = as.numeric(bent)*650), fill = "turquoise3", 
             width = 1000) +
    geom_col(aes(x = extr_veg, y = as.numeric(bent)*650), fill = "darkred", 
             width = 1000, alpha = 0.5) +
    coord_cartesian(ylim = c(200, 600)) +
    scale_x_datetime(date_breaks = "12 hours",
                     date_minor_breaks = "1 hour",
                     date_labels = "%m.%d. %H:%M") +
    labs(title = "A MFID201.ZAGY taghez tartozó adatpontok") +
    theme(axis.title.y = element_blank())

ggsave("plots/eltunes_MFID201.ZAGY_07_11-15.png")

# az adagnak és a megfeleltnek is faktornak kellene lennie

# kapcsolatvizsgálatok

# numerikusok között korreláció

cor(wide[, -(1:4)], use = "pairwise.complete.obs")
# de persze most még nem ez az izgi, hanem inkább az eredményváltozók

# beírom a tagek közé a szórásokat
# TERMÉKENKÉNT KELL
temp <- as.data.table(sapply(wide[, -(1:4)], sd, na.rm = T), keep.rownames = T)
names(temp) <- c("tag", "sd")
tag <- merge(tag, temp, by = "tag", all.x = T)

temp <- as.data.table(sapply(wide[termek == "7000F", -(1:4)], sd, na.rm = T), 
                      keep.rownames = T)
names(temp) <- c("tag", "sd_70")
tag <- merge(tag, temp, by = "tag", all.x = T)

temp <- as.data.table(sapply(wide[termek == "7500F", -(1:4)], sd, na.rm = T), 
                      keep.rownames = T)
names(temp) <- c("tag", "sd_75")
tag <- merge(tag, temp, by = "tag", all.x = T)

# normalitási tesztet csinálok azokra a tagekre, amiknek a szórása nem 0
# TERMÉKENKÉNT KELL
temp <- lapply(wide[, -c(names(wide)[1:4], tag[sd == 0][[1]]), with = F], 
               shapiro.test)

shap <- data.table(tag = names(temp), shapiro.p = -2.45)
    # azért írtam bele a 2.45-öt, mert különben logical lesz az oszlop (??)

for (k in shap$tag) shap[tag == k, 2] <- temp[[k]][["p.value"]]

tag <- merge(tag, shap, by = "tag", all.x = T)

tag[shapiro.p > 0.01, 1]
ggplot(wide[, c("date_ext", "megfelelt", "FR231.PV")], aes(x = date_ext, y = FR231.PV)) +
    geom_point(aes(color = megfelelt), alpha = 0.8)

# érdekel a H^2 mutató termékenként minden tagre a megfeleléssel kapcsolatban
# TERMÉKENKÉNT KELL
shap$megf_H2 <- -2.45

for (k in shap$tag) {
    form <- paste(k, " ~ megfelelt", sep = "")
    shap[tag == k, "megf_H2"] <- summary(lm(form, data = wide))$r.squared
}

shap$megf_70_H2 <- -2.45

for (k in shap$tag) {
    form <- paste(k, " ~ megfelelt", sep = "")
    shap[tag == k, "megf_70_H2"] <- summary(lm(form, 
                                            data = wide[termek == "7000F"])
                                         )$r.squared
}

shap$megf_75_H2 <- -2.45

for (k in shap$tag) {
    form <- paste(k, " ~ megfelelt", sep = "")
    shap[tag == k, "megf_75_H2"] <- summary(lm(form, 
                                               data = wide[termek == "7500F"])
    )$r.squared
}

tag <- merge(tag, shap[, -2], by = "tag", all.x = T)

write.xlsx(tag, "data/tag_proc.xlsx",
           sheetName = "tag_ANOVA2",
           row.names = F,
           showNA = F,
           append = T)

### JAVÍTANDÓ
# és akkor döntési fák

# kiinduló formula:
magy <- tag[megf_H2 >= 0.13][["tag"]]

form <- paste("megfelelt ~", paste(magy, collapse = " + "))

# ctree
install.packages("party")
library("party")

magy70 <- tag[megf_70_H2 >= 0.13][["tag"]]
form <- paste("megfelelt ~", paste(magy70, collapse = " + "))

ctree_1_70 <- ctree(as.formula(form), wide[termek == "7000F"])

png("plots/ctree_1.png", width = 1200, height = 600)
plot(ctree_1_70, type = "simple")
dev.off()

magy75 <- tag[megf_75_H2 >= 0.13][["tag"]]
form <- paste("megfelelt ~", paste(magy75, collapse = " + "))

ctree_1_75 <- ctree(as.formula(form), wide[termek == "7500F"])

png("plots/ctree_1.png", width = 1200, height = 600)
plot(ctree_1_75, type = "simple")
dev.off()

# szétszedem az adatbázist termék szerint, mert mindig elfelejtem, hogy mindent külön kell számolni
t70 <- wide[termek == "7000F"]
t75 <- wide[termek == "7500F"]

# újra csinálhatnám a szórásokat és a shapiro teszteket is, de most nincs rá időm
# és úgysem veszem figyelembe őket

# fa minden változóval

form <- paste("megfelelt ~", paste(names(t70)[-(1:4)], collapse = "+"))
ctree70_mind <- ctree(as.formula(form), t70)
png("plots/ctree70_mind.png", width = 1200, height = 600)
plot(ctree70_mind, type = "simple", 
     main = "7000F, magyarázó változók: mind")
dev.off()


form <- paste("megfelelt ~", paste(names(t75)[-(1:4)], collapse = "+"))
ctree75_mind <- ctree(as.formula(form), t75)
png("plots/ctree75_mind.png", width = 1200, height = 600)
plot(ctree75_mind, type = "simple", 
     main = "7500F, magyarázó változók: mind")
dev.off()

# átállítom a tesztstatisztikát quadról maxra
form <- paste("megfelelt ~", paste(names(t70)[-(1:4)], collapse = "+"))
ctree70_mind_max <- ctree(as.formula(form), t70, 
                      controls = ctree_control(teststat = "max"))
png("plots/ctree70_mind_max.png", width = 1200, height = 600)
plot(ctree70_mind_max, type = "simple", 
     main = "7000F, magyarázó változók: mind, teszt: max")
dev.off()
# nincs különbség az előzőhöz képest, úgyhogy nem írom ki

# teszek bele random szelekciót
ctree70_mind_rnd <- ctree(as.formula(form), t70, 
                          controls = ctree_control(mtry = 20))
png("plots/ctree70_mind_rnd.png", width = 1200, height = 600)
plot(ctree70_mind_rnd, type = "simple", 
     main = "7000F, magyarázó változók: mind, random szelekció")
dev.off()

ctree75_mind_rnd <- ctree(as.formula(form), t75, 
                          controls = ctree_control(mtry = 20))
png("plots/ctree75_mind_rnd.png", width = 1200, height = 600)
plot(ctree75_mind_rnd, type = "simple", 
     main = "7500F, magyarázó változók: mind, random szelekció")
dev.off()

# feljebb veszem a szignifikancia szintet
ctree70_mind_crit <- ctree(as.formula(form), t70, 
                          controls = ctree_control(mincriterion = 0.9))
png("plots/ctree70_mind_crit.png", width = 1200, height = 600)
plot(ctree70_mind_crit, type = "simple", 
     main = "7000F, magyarázó változók: mind, szignifikancia: 0,9")
dev.off()

ctree75_mind_crit <- ctree(as.formula(form), t75, 
                           controls = ctree_control(mincriterion = 0.9))
png("plots/ctree75_mind_crit.png", width = 1200, height = 600)
plot(ctree75_mind_crit, type = "simple", 
     main = "7500F, magyarázó változók: mind, szignifikancia: 0,9")
dev.off()

# rajzolok pár ROC görbét, de úgy igazán nem értek hozzájuk :/
plot(roc(t70$megfelelt, predict(ctree70_mind_rnd, t70)), col="darkred")
lines.roc(roc(t70$megfelelt, predict(ctree70_mind, t70)),col="turquoise3")
lines.roc(roc(t70$megfelelt, predict(ctree70_mind_crit, t70)),col="darkorange3")

plot(roc(t75$megfelelt, predict(ctree75_mind_rnd, t75)), col="darkred")
lines.roc(roc(t75$megfelelt, predict(ctree75_mind, t75)),col="turquoise3")
lines.roc(roc(t75$megfelelt, predict(ctree75_mind_crit, t75)),col="darkorange3")

# a leginkább releváns magyarázó változók korrelációi egymás közt
mat <- cor(t70[, names(t70) %in% tag[megf_70_H2 > 0.13]$tag, with = F], 
           use = "pairwise.complete.obs")
write.xlsx(mat, "data/tag_proc.xlsx",
           sheetName = "cor70",
           showNA = F,
           append = T)

mat <- cor(t75[, names(t75) %in% tag[megf_75_H2 > 0.13]$tag, with = F], 
           use = "pairwise.complete.obs")
write.xlsx(mat, "data/tag_proc.xlsx",
           sheetName = "cor75",
           showNA = F,
           append = T)

# korrelációk az összes változóra, nem csak a narancssárgákra
mat <- cor(t70[, -(1:4)], 
           use = "pairwise.complete.obs")
mat[upper.tri(mat)] <- NA
write.xlsx(mat, "data/tag_proc.xlsx",
           sheetName = "cor70_mind",
           showNA = F,
           append = T)

mat <- cor(t75[, -(1:4)], 
           use = "pairwise.complete.obs")
mat[upper.tri(mat)] <- NA
write.xlsx(mat, "data/tag_proc.xlsx",
           sheetName = "cor75_mind",
           showNA = F,
           append = T)

temp <- t70[, -"F235.PV"]
form <- paste("megfelelt ~", paste(names(temp)[-(1:4)], collapse = "+"))
ctree70_sz1 <- ctree(as.formula(form), t70)
png("plots/ctree70_sz1.png", width = 1200, height = 600)
plot(ctree70_sz1, type = "simple", 
     main = "7000F, magyarázó változók: mind, kivéve F235.PV")
dev.off()

# regresszió?
temp <- t70[, -c("R_MFID201.ZAGY", "R_SURM302.POR", "R_SURTK404.GRAN")]
# KI KÉNE VENNI BELŐLE A TELJESEN EGYÜTT MOZGÓ VÁLTOZÓKAT
form <- paste("megfelelt ~", paste(names(temp)[-(1:4)], collapse = "+"))
summary(lm(as.formula(form), temp))
lin70_st <- step(lm(as.formula(form), temp))
summary(lin70_st)
lin70_69_var <- as.data.table(summary(lin70_st)$coefficients, keep.rownames = T)
temp <- merge(tag[, c(1, 2, 4, 5, 9, 11, 15)], lin70_69_var, 
              by.x = "tag", by.y = "rn")

write.xlsx(temp, "comp/lin70.xlsx",
           sheetName = "st_69",
           row.names = F,
           showNA = F)

# ezt a szűkítés után újracsinálom a 03-ban!

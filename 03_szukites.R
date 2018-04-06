# HD2 3.
# 2017.08.14.
# Limbek Zsófi

library(data.table)
library(xlsx)
library(party)

# hogy szűröm ki azt az 50 változót, amiknek egymással 1,0 a korrelációja?
# FC221.PV fog maradni, akkor dobjunk ki mindenkit aki vele együtt mozog

mat <- cor(t70[, -(1:4)], use = "pairwise.complete.obs")
colnames(mat)[35]

temp <- as.data.table(mat[, 35], keep.rownames = T)
names(temp) <- c("tag", "cor_FC221.PV")

# ezeket veszem ki a t70-ből:
ki <- temp[cor_FC221.PV >= 0.89][[1]][-21]

t70 <- t70[, !(names(t70) %in% ki), with = F]

# most t75-re
mat <- cor(t75[, -(1:4)], use = "pairwise.complete.obs")
colnames(mat)[35]

temp <- as.data.table(mat[, 35], keep.rownames = T)
names(temp) <- c("tag", "cor_FC221.PV")

# ezeket veszem ki a t75-ből:
ki <- temp[cor_FC221.PV >= 0.89 | cor_FC221.PV <= -0.89][[1]][-14]

t75 <- t75[, !(names(t75) %in% ki), with = F]

# így van 70 változóm a t70-ben és 77 a t75-ben

write.xlsx(t70, "data/szukitett.xlsx",
           sheetName = "t70",
           row.names = F,
           showNA = F)

write.xlsx(t75, "data/szukitett.xlsx",
           sheetName = "t75",
           row.names = F,
           showNA = F,
           append = T)

# ciklusok jelölése

c70 <- c(630593, 630594, 630596, 630597)
c75 <- c(630663, 630664, 630666)

dim(t70[adag %in% c70]) # van 240 sor, ez jó

# először 7000F

temp <- t70[adag %in% c70]
dim(t70[adag %in% c70])

form <- paste("megfelelt ~", paste(names(temp)[-(1:4)], collapse = "+"))
ctree70_szurt_cikl <- ctree(as.formula(form), temp)
png("plots/ctree70_szurt_cikl.png", width = 1200, height = 600)
plot(ctree70_szurt_cikl, type = "simple", 
     main = "7000F, szűrt, egy ciklus")
dev.off()

# random szelekció?
ctree70_szurt_cikl_rnd <- ctree(as.formula(form), temp, 
                            controls = ctree_control(mtry = 20))
png("plots/ctree70_szurt_cikl_rnd.png", width = 1200, height = 600)
plot(ctree70_szurt_cikl_rnd, type = "simple", 
     main = "7000F, szűrt, egy ciklus, random szelekció")
dev.off()

# ezt lefuttattam 25-ször
plot(ctree(as.formula(form), temp, 
           controls = ctree_control(mtry = 30)), type = "simple")

# több random szelekciós fa után ezek a tagek jönnek elő sokszor első vágásnak:
# T417 (12), T435(8), T430(4), T416(1), FC415(1), T231(1) 
# az összeg 27, valahol elszámolhattam


# 7500F termék

temp <- t75[adag %in% c75]
dim(t75[adag %in% c75])

form <- paste("megfelelt ~", paste(names(temp)[-(1:4)], collapse = "+"))
ctree75_szurt_cikl <- ctree(as.formula(form), temp)
png("plots/ctree75_szurt_cikl.png", width = 1200, height = 600)
plot(ctree75_szurt_cikl, type = "simple", 
     main = "7500F, szűrt, egy ciklus")
dev.off()

# random szelekció?
ctree75_szurt_cikl_rnd <- ctree(as.formula(form), temp, 
                                controls = ctree_control(mtry = 20))
png("plots/ctree75_szurt_cikl_rnd.png", width = 1200, height = 600)
plot(ctree75_szurt_cikl_rnd, type = "simple", 
     main = "7500F, szűrt, egy ciklus, random szelekció")
dev.off()


plot(ctree(as.formula(form), temp, 
           controls = ctree_control(mtry = 30)), type = "simple")
# A202_C21 (6), T323(9), 


# negatív értékek törlése
# vagy: 2017.07.12 06:01 - minden tag itt bolondul meg

for (i in 5:ncol(t70)) {
    temp <- t70[[i]]
    print(length(temp[temp < 0 & !is.na(temp)]))
    temp[temp < 0 & !is.na(temp)] <- NA
}

for (i in 5:ncol(t75)) {
    temp <- t75[[i]]
    print(length(temp[temp < 0 & !is.na(temp)]))
    temp[temp < 0 & !is.na(temp)] <- NA
}

t70[adag %in% c70]

# H^2 a szűrt táblákra
tag$H2_70_szurt <- -2.45
for (k in names(t70)[-(1:4)]) {
    form <- paste(k, " ~ megfelelt", sep = "")
    tag[tag == k, "H2_70_szurt"] <- summary(lm(form, data = t70[adag %in% c70]))$r.squared
}
tag[H2_70_szurt == -2.45]$H2_70_szurt <- NA

tag$H2_75_szurt <- -2.45
for (k in names(t75)[-(1:4)]) {
    form <- paste(k, " ~ megfelelt", sep = "")
    tag[tag == k, "H2_75_szurt"] <- summary(lm(form, data = t75[adag %in% c75]))$r.squared
}
tag[H2_75_szurt == -2.45]$H2_75_szurt <- NA

write.xlsx(tag, "data/tag_proc.xlsx",
           sheetName = "ANOVA4",
           row.names = F,
           showNA = F,
           append = T)

# korrelációk egymás közt

mat <- cor(t70[adag %in% c70, -(1:4)], use = "pairwise.complete.obs")

write.xlsx(mat, "data/tag_proc.xlsx",
           sheetName = "cor_t70_szurt",
           showNA = F,
           append = T)

mat <- cor(t75[adag %in% c75, -(1:4)], use = "pairwise.complete.obs")

write.xlsx(mat, "data/tag_proc.xlsx",
           sheetName = "cor_t75_szurt",
           showNA = F,
           append = T)

# regressziók az összehasonlítás kedvéért
temp <- t70[, -c("R_MFID201.ZAGY", "R_SURM302.POR", "R_SURTK404.GRAN")]

form <- paste("megfelelt ~", paste(names(temp)[-(1:4)], collapse = "+"))
summary(lm(as.formula(form), temp))
lin70_st <- step(lm(as.formula(form), temp))
summary(lin70_st)
lin7000F_var <- as.data.table(summary(lin70_st)$coefficients, keep.rownames = T)
temp <- merge(tag[, c(1, 2, 4, 5, 9, 11, 15)], lin7000F_var, 
              by.x = "tag", by.y = "rn")
temp$`|est|` <- abs(temp$Estimate)

write.xlsx(temp, "comp/lm.xlsx",
           sheetName = "7000F_st",
           row.names = F,
           showNA = F)


temp <- t75[, -c("R_MFID201.ZAGY", "R_SURM302.POR", "R_SURTK404.GRAN")]

form <- paste("megfelelt ~", paste(names(temp)[-(1:4)], collapse = "+"))
summary(lm(as.formula(form), temp))
lin75_st <- step(lm(as.formula(form), na.omit(temp)))
summary(lin75_st)
lin7500F_var <- as.data.table(summary(lin75_st)$coefficients, keep.rownames = T)
temp <- merge(tag[, c(1, 2, 4, 5, 9, 11, 15)], lin7500F_var, 
              by.x = "tag", by.y = "rn")
temp$`|est|` <- abs(temp$Estimate)

write.xlsx(temp, "comp/lm.xlsx",
           sheetName = "7500F_st",
           row.names = F,
           showNA = F,
           append = T)

# regressziók standardizált magyarázó változókkal
temp <- data.table(megfelelt = t70$megfelelt, 
                   scale(t70[, -c(names(t70)[1:4], "R_MFID201.ZAGY", 
                                  "R_SURM302.POR", "R_SURTK404.GRAN", "FC243_1.PV",
                                  "FC243_2.PV"), with = F]))

form <- paste("megfelelt ~", paste(names(temp)[-1], collapse = "+"))
summary(lm(as.formula(form), temp))
lin70_sc_st <- step(lm(as.formula(form), temp))
summary(lin70_sc_st)
lin7000F_sc_var <- as.data.table(summary(lin70_sc_st)$coefficients, keep.rownames = T)
temp <- merge(tag[, c(1, 2, 4, 5, 9, 11, 15)], lin7000F_sc_var, 
              by.x = "tag", by.y = "rn")
temp$`|est|` <- abs(temp$Estimate)

write.xlsx(temp, "comp/lm.xlsx",
           sheetName = "7000F_sc_st",
           row.names = F,
           showNA = F,
           append = T)

temp <- data.table(megfelelt = t75$megfelelt, 
                   scale(t75[, -c(names(t75)[c(1:4, 9:10, 16:17, 29:32, 55:57)]), 
                             with = F]))

form <- paste("megfelelt ~", paste(names(temp)[-1], collapse = "+"))
summary(lm(as.formula(form), temp))
lin75_sc_st <- step(lm(as.formula(form), na.omit(temp)))
summary(lin75_sc_st)
lin7500F_sc_var <- as.data.table(summary(lin75_sc_st)$coefficients, keep.rownames = T)
temp <- merge(tag[, c(1, 2, 4, 5, 9, 11, 15)], lin7500F_sc_var, 
              by.x = "tag", by.y = "rn")
temp$`|est|` <- abs(temp$Estimate)

write.xlsx(temp, "comp/lm.xlsx",
           sheetName = "7500F_sc_st",
           row.names = F,
           showNA = F,
           append = T)

# outlierek kiszedése, mert elmásznak tőlük a regressziós együtthatók

sd70 <- sapply(t70[, -(1:4)], sd, na.rm = T)
sd70 <- as.data.table(sd70, keep.rownames = T)
sd70$fancy <- format(sd70$sd70, scientific = F, digits = 1)

# hm mégsem tűnnek olyan óriásinak a varianciák
ggplot(t70, aes(x = as.factor(adag), y = jitter(scale(A202_C32.PV)))) + 
    geom_point(aes(color = megfelelt), shape = 1, size = 3) +
    scale_shape_discrete(solid = F)

# elemszámok aktualizálása
# termékenként
# a döntési fákhoz és a regressziókhoz
# az összeset varianciával légyszi

temp <- as.data.table(colSums(!is.na(t70)), keep.rownames = T)
names(temp) <- c("tag", "t70_oszlop_szures_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(t70_oszlop_szures_utan), t70_oszlop_szures_utan := 0]

temp <- as.data.table(sapply(t70[, -(1:4)], sd, na.rm = T), keep.rownames = T)
names(temp) <- c("tag", "sd70_oszlop_szures_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)

temp <- as.data.table(colSums(!is.na(t70[adag %in% c70, -(1:4)])), keep.rownames = T)
names(temp) <- c("tag", "t70_egy_ciklus")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(t70_egy_ciklus), t70_egy_ciklus := 0]

temp <- as.data.table(sapply(t70[adag %in% c70, -(1:4)], sd, na.rm = T), 
                      keep.rownames = T)
names(temp) <- c("tag", "sd70_egy_ciklus")
tag <- merge(tag, temp, by = "tag", all.x = T)

temp <- as.data.table(colSums(!is.na(t75)), keep.rownames = T)
names(temp) <- c("tag", "t75_oszlop_szures_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(t75_oszlop_szures_utan), t75_oszlop_szures_utan := 0]

temp <- as.data.table(sapply(t75[, -(1:4)], sd, na.rm = T), keep.rownames = T)
names(temp) <- c("tag", "sd75_oszlop_szures_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)

temp <- as.data.table(colSums(!is.na(t75[adag %in% c75, -(1:4)])), keep.rownames = T)
names(temp) <- c("tag", "t75_egy_ciklus")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(t75_egy_ciklus), t75_egy_ciklus := 0]

temp <- as.data.table(sapply(t75[adag %in% c75, -(1:4)], sd, na.rm = T), 
                      keep.rownames = T)
names(temp) <- c("tag", "sd75_egy_ciklus")
tag <- merge(tag, temp, by = "tag", all.x = T)

write.xlsx(tag, "comp/tag_temp.xlsx", showNA = F)

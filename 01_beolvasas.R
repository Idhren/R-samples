# HDPE-2 hozzákezdés
# 2017.08.04.
# Limbek Zsófi

# setwd("S:/Limbek Zsófia/170802_HD2")

library(xlsx)
library(data.table)

# tag lista

tag <- read.xlsx("data/tagek.xlsx", "tag_R",
                 encoding = "UTF-8",
                 stringsAsFactors = F)
tag$egyseg <- factor(tag$egyseg, 
                     levels = c("reg", "D-201", "D-221", "porvonal", "extr"),
                     ordered = T)
setDT(tag)

tag$tag <- gsub("PE4.", "", tag$tag)
tag$tag <- gsub("Y.QC.HD2.", "R_", tag$tag)

ido_kul <- data.table(egyseg = levels(tag$egyseg),
                      ido = c(-150, -150, -90, -30, 0))

tag <- merge(tag, ido_kul, by = "egyseg")

save(tag, file = "data/RData/tagek.RData")
rm(ido_kul)

# adagok listája

adag <- read.csv2("data/adagok.csv",
                  colClasses = c("numeric", "factor", "numeric", "character", 
                                 "character", "character", "character", 
                                 "character", "character", "character", 
                                 "character", "factor", "factor"))
# eredetileg excelből akartam beolvasni, de a dátumokat sehogy sem volt hajlandó GMT-től különböző időzónában kezelni
# úgyhogy az excelből csináltam egy csv-t, és így már mennek a dolgok
setDT(adag)

adag[, 4:11] <- as.data.table(lapply(adag[, 4:11], 
                                     as.POSIXct, 
                                     format = "%Y.%m.%d %H:%M"))

## beolvasás

# legenerálom a fájlneveket

fajlok <- data.table(adag = adag$szam, kezd_str = NA, veg_str = NA, file_1 = NA,
                     file_2 = NA)

for (k in 1:nrow(adag)) {

    fajlok$kezd_str[k] <- strftime(adag[k]$D.201_kezd, format = "%Y.%m.%d %H%M")
    fajlok$kezd_str[k] <- gsub(" 0", " ", fajlok$kezd_str[k])
    
    fajlok$veg_str[k] <- strftime(adag[k]$extr_veg, format = "%Y.%m.%d %H%M")
    fajlok$veg_str[k] <- gsub(" 0", " ", fajlok$veg_str[k])
}
# ezt azért internal reffel csinálom, mert így rövidebb egy sor :D
fajlok[, file_1 := paste("data", kezd_str, "-", veg_str, ".csv", sep = "")]
fajlok[, file_2 := paste("data2", kezd_str, "-", veg_str, ".csv", sep = "")]

fajlok <- merge(fajlok, adag[, 1:2], by.x = "adag", by.y = "szam")

# és a beolvasás

long <- data.table(tag = factor(), date = character(), val = numeric(), 
                   conf = integer(), adag = integer(), termek = character())

for (i in t(fajlok[, 4:5])) {
#    print(i)
    
    path <- paste("data/raw/", i, sep = "")
    temp <- read.csv(path, 
                     sep = ";", 
                     header = F,
                     colClasses = c("factor", "character", "numeric", "integer"),
                     col.names = c("tag", "date", "val", "conf"))
    # wowwwww, működik a colClasses
    
    sor <- which(fajlok == i, arr.ind = T)[1]
    temp$adag <- fajlok$adag[sor]
    temp$termek <- fajlok$termek[sor]
    
    long <- rbindlist(list(long, temp))
}

# átalakítások
long$tag <- gsub("QC.HD2.", "R_", long$tag)
long$tag <- factor(gsub("PE4.", "", long$tag))
long$date <- as.POSIXct(long$date, format = "%Y.%m.%d. %H:%M:%S")

# dokumentálom, hogy melyik taghez hány értéket olvastunk be
temp <- as.data.table(table(long$tag))
names(temp) <- c("tag", "beolvasas_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)

## sortörlések

# hiányzó értéket tartalmazó sorok törlése
long <- na.omit(long, cols = "val")

temp <- as.data.table(table(long$tag))
names(temp) <- c("tag", "na.omit_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(na.omit_utan), na.omit_utan := 0]

# 0 megbízhatóságú sorok törlése
long <- long[conf != 0]

temp <- as.data.table(table(long$tag))
names(temp) <- c("tag", "conf0_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(conf0_utan), conf0_utan := 0]

rm(i, k, path, sor)

# idők egyeztetése
# először megcsinálom az eltolásokat, utána pedig levágom, ami kilóg az időkből

# a vágás alapjául szolgáló extruder dátum hozzáadása
long <- merge(long, tag[, c(1, 5)], by = "tag", all.x = T)
names(long)[2] <- "date_org"
long$date_ext <- long$date_org - long$ido*60 
# azért mínusz, mert az extruderhez képest, tehát visszafelé írtam fel az elcsúszásokat

long <- merge(long, adag[, c(1, 10, 11, 3)], by.x = "adag", by.y = "szam")

long$bent <- long$date_ext >= long$extr_kezd & long$date_ext <= long$extr_veg
# JAJ DE SZÉP VEKTORIZÁLT MEGOLDÁS!!

temp <- as.data.table(table(long[bent == T]$tag))
names(temp) <- c("tag", "eltolas_utan")
tag <- merge(tag, temp, by = "tag", all.x = T)
tag[is.na(eltolas_utan), eltolas_utan := 0]

long <- long[, c(1, 11, 2:10, 12)]
setorder(long, ido, adag, tag, date_org)

save(long, file = "data/RData/long.RData")

wide <- dcast(long[bent == T], date_ext + adag + megfelelt + termek ~ tag, value.var = "val")
# uh ez elég kicsi lett

write.xlsx(wide, "data/HD2_por.xlsx",
           row.names = F,
           showNA = F)

write.xlsx(tag, "data/tag_proc.xlsx",
           sheetName = "tag_lista",
           row.names = F,
           showNA = F)

write.xlsx(adag, "data/tag_proc.xlsx",
           sheetName = "adagok",
           row.names = F,
           showNA = F,
           append = T)

save.image()

# itt még elférne, hogy leíró statisztikák alapján is kezdjek valamit a változóimmal
# pl. 0 és túl nagy variancákat megnézni
# kiugró értékeket megnézni
# negatív értékek ott, ahol ez nem értelmes (pl. arányok, mennyiségek egyirányú rendszerben)

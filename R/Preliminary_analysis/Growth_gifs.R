# experimenting with gifs of tree ring growth:

library(dplR)
COR <-read.rwl("cleanrwl/CORww.rwl")

dplR::plotRings(year = row.names(COR[!is.na(COR$COR978ac),]), trwN = COR[!is.na(COR$COR978ac),]$COR978ac, trwS = COR[!is.na(COR$COR978ac),]$COR978bc, COR[!is.na(COR$COR978ac),]$COR978cc, animation = TRUE, year.labels = TRUE, sys.sleep = 0.15, saveGIF = TRUE, fname = "COR_978.gif")
dplR::plotRings(year = row.names(COR[!is.na(COR$COR995bc),]),  trwN = COR[!is.na(COR$COR995bc),]$COR995bc, COR[!is.na(COR$COR995bc),]$COR995bc, animation = TRUE, year.labels = TRUE, sys.sleep = 0.15, saveGIF = TRUE, fname = "COR_978.gif")

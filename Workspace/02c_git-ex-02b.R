# Thomas Kiefer, 15.06.2022
# IRT Analyse von Rasch-Daten

source("00_grundscript.R")
schreiben <- TRUE

# 1 Einlesen ----
file <- list.files(here::here("roh"))
resp <- read.csv2(here::here("roh", file)) 

# 2 Vorbereiten ----
head(resp)
apply(resp, 2, table)
colMeans(resp)

# 3 Analyse ----
mod1 <- TAM::tam.mml(resp = resp)
wle1 <- TAM::tam.mml.wle2(mod1)

# 4 Schreiben ----
if(schreiben){
  write.csv2(wle1, here::here("ergebnis", paste0("personen_", Sys.Date(), ".csv")), 
             row.names = FALSE, na = "")
}

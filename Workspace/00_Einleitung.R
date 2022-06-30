####
#
# Innsbrucker Summer Seminar zu Methoden der empirischen Sozial- und 
# Bildungsforschung 2022
#
# Workshop 5: Advanced R - Prozesse, Performanz und Pakete
#
# Thomas Kiefer
# IQS – Institut des Bundes für Qualitätssicherung im österreichischen 
# Schulwesen 
# 07.-08. Juli 2022
#
# Script 00: Einleitung
# 
#
####

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(here,
               rmarkdown,
               microbenchmark,
               devtools,
               roxygen2,
               lme4,
               mvtnorm,
               Rcpp,
               TAM,
               RXKCD,
               tensor)

# ______________________ ---- 
# Organisatorisches ----

 ## Nützliche Settings:
 ##   -> Tools 
 ##      -> Global Options 
 ##         -> Restore Project no
 ##         -> Restore .Rdata no
 ##         -> Save Workspace no
 ##         -> Save history no
 ##      -> Code -> Display
 ##         -> Show margin
 ##      -> Appearance 
 ##         -> Modern, 100%, Lucida Console, 10, Idle Fingers
 ##      -> Spelling
 ##         -> Main Dictionary language 
 ##         -> install more languages/German
 ##   -> File Reopen with Encoding

# ______________________ ---- 
# Warm Up ----

##
recent.packages.rds <- function(){
  mytemp <- tempfile()
  download.file(paste0(options("repos")$repos,"/web/packages/packages.rds"),
                mytemp)
  mydata <- as.data.frame(readRDS(mytemp), row.names=NA)
  mydata$Published <- as.Date(mydata[["Published"]])
  mydata
}
mydata <- recent.packages.rds()
unname(as.character(mydata$Package[grep("Hadley Wickham", mydata$Maintainer)]))
unname(as.character(mydata$Package[grep("Hadley Wickham", mydata$Author)]))

##
base::rowsum   
tensor::tensor

##
stats::reshape 
base::with     
by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
               
##
utils::help    

##

# ______________________ ---- 
# Programm ----

# ______________________ ---- 
# Ressourcen ----

  ## _> Read the Source Luke ----
stats::lm

  ##
stats::lm.fit
getAnywhere("%%")
`%%`
rowsum

 ##
methods(rowsum)
rowsum.default 

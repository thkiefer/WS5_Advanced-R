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
# Script 02: Workflow
# 
#
####

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

# __________________________________________ ---- 
# Wickham's Vocabulary ----

  ## character manipulation
  ## ?grep
  ## ?regex 
  ## see https://bookdown.org/rdpeng/rprogdatascience/regular-expressions.html
  ## cheat sheet https://raw.githubusercontent.com/rstudio/cheatsheets/main/regex.pdf
  ##
  ## Fragen zu for/if/while/switch?
  ## https://twitter.com/data_question/status/1518205827223638017
  for(i in 0:0) print("y") ## length(0:0) 

# __________________________________________ ---- 
# Googles Style Guide ----

# __________________________________________ ---- 
# Jenny Brian [@Rstudio] on Project Workflow ----

 ## https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

 ##_ > R-Project ----

 ## _> here ----

##
file.path(getwd(), "Subverzeichnis", "Da ist eine datei.wev")
here("Subverzeichnis", "Da ist eine datei.wev")

 
  ## _> str+shift+F10 ----

  ## _> .Rprofile ----
  
  ## _> git ----
  
  ## https://docs.github.com/en/get-started
  ## Tools -> Version Control -> Project Setup 
  ## (oder tools -> Project Options Git/SVN)
  ## Version Control System -> Git -> yes -> yes
  ## new tab "git" on top right pane

  ## _> Syntax-Folding und Abschnittsberschriften ----

  ## [https://twitter.com/hi_im_alise/status/1539746290564997121]

  ## _> Dateibezeichnungen ----

  ## [https://speakerdeck.com/jennybc/how-to-name-files]

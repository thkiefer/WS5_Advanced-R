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
# VOLL-Version
#
####

if(!"pacman" %in% installed.packages()) install.packages("pacman")

# installed via renv to /renv
# use to install/library/require -> p_load
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

  ## Welche R-Version/Rstudio-Version hast Du?
  ## Lade bitte das R-Projekt/Verzeichnis zum Workshop herunter
  ##   von https://github.com/thkiefer/WS5_Advanced-R
  ##   bzw. von USB-Sticks
  ## Timeline
  ## Nutze renv -> vorher das Projekt öffnen und 
  ##   -> install.packages("pacman"); pacman::p_load(…) ausführen
  ##   -> renv verwenden wir nicht
  ## Settings:
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

  ## Zu meiner Person: Studium Mathematik/Informatik, Schwerpunkt Statistik 
  ##   bis 2010; Unter Verwendung von R
  ##   Arbeit in Sozial- und Bildungswissenschaftlichen Bereich in 
  ##   Dortmund und Salzburg; einige (u.a. eigene) R-Pakete, deren 
  ##   Weiterentwicklung und Maintenance nicht in meiner Hand liegt
  ##   -> fechner, TAM, CDM, LSAmitR
  ## Arme hoch: wer arbeitet seit mindesten 1 Jahr mit R? 2 Jahre? 5 Jahre? 10 Jahre?
  ## Wer weiß, was das Hadley-Verse ist?
  ##   -> https://stackoverflow.com/questions/10081796/search-for-packages-by-a-particular-author
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
  ##   -> auch tidyverse
  ##   -> Data Import/Manipulation/Visualization/Output
  ##   -> Package-Framework für vollständigen Data-Science-Prozess 
  ##   -> u.a. dplyr, ggplot2, tidyverse, (abgesehen von Statistischen Methoden)
  ##   -> Autor von https://r4ds.had.co.nz/index.html
  ##   -> Darüber hinaus Developer Tools wie devtools und dessen Framework, assertthat, testthat
  ##   -> Autor von https://adv-r.hadley.nz/ und https://r-pkgs.org/ 
  ## * Und wer ist Yihui Xi?
  ##   -> https://yihui.org/
  ##   -> Software Engineer @Rstudio
  ##   -> Wie Wickham auch aus der Schule um Di Cook und Heike Hofmann
  ##   -> Spezialisiert auf Tex/Pandoc; Pakete knitr, bookdown
  ## Welche (base-)R Funktion bekommt viel zu wenig Beachtung?
base::rowsum   # extrem schnelle gruppenweise Berechnung von Summen
               # auch für Matrizen
               # habe ich sehr spät kennen gelernt. Vorher habe ich rowSums, 
               # aggregate und tapply benutzt, die alle langsamer sind.
tensor::tensor # ich bin ein Fan der R-Matrix-Algebra, die durch Arrays und 
               # Tensor-Produkten verallgemeinert wird.
               # Mit hochdimensionalen Arrays und Tensor-Produkten kann man sich 
               # einen richtigen Knoten ins Gehirn machen.
  ## Welche (base-)R Funktion findest Du in der Anwendung unangenehm?
stats::reshape # je nachdem, welche Richtung gewählt wird, sind unterschiedliche 
               # Argumente notwendig und die Argumentenbezeichnung ist unklar
base::with     # ist enorm schwierig zu Debuggen, weil man auf den Ausdruck nicht 
               # ohne weiteres zugreifen kann 
by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
               # aufgrund ihrer vielfältigen Anwendbarkeit, ist der Output von 
               # by sehr allgemein (von der print-Funktion ganz abgesehen).
               # In der Produktion ist das schwierig hieraus schöne 
               # Ausgabetabellen zu erzeugen (z.B. gruppenweise Summen)
  ## Welche (base-)R Funktion beschreibt Dich am besten?
utils::help    # ich weiß vieles nicht, und kann mir auch nicht alles merken, 
               # aber ich weiß, wo ich Hilfe bekomme.
               # https://win-vector.com/2019/02/26/if-you-were-an-r-function-what-function-would-you-be/
               # John Chambers auf useR!2014 über S und R
               #   -> Alles was existiert ist ein Objekt
               #   -> Alles was passiert ist ein Funktionsaufruf
               # andererseits hilft das Manual kaum, wenn man nicht weiß, wie die
               # Funktion heißt, nachder man sucht -- hier sind Vignetten besser
               # geeignet.

# ______________________ ---- 
# Programm ----

 ## In jeder Disziplin, die es zu Meistern gilt, führt eine Professionalisierung 
 ## über das Festigen von Grundlagen. So auch in R. Damit beschäftigen wir uns 
 ## im ersten Teil. 
 ## Dazu gibt es ein paar Demonstrationen zu RStudio und idealtypischen Workflows.
 ## 
 ## Im zweiten Teil des Workshops (heute Nachmittag und morgen) 
 ## beschäftigen wir uns mit dem Vertiefen: 
 ## Programmieren, der Performanz und Pakete. Nicht notwendigerweise in dieser 
 ## Reihenfolge. 
 ## Welcher Teil davon interessiert Sie am meisten? Dann würde ich die Reihenfolge 
 ## ändern. Einige Elemente, die in den einzelnen Einheiten angerissen werden,
 ## werden in einer anderen Einheit detaillierter besprochen; dennoch sollten 
 ## die Module weitestgehend unabhängig voneinander funktionieren.

 ## Hier ist nicht wichtig alles im Detail zu durchdringen, sondern den 
 ## Anwendungsfall verstanden zu haben und ein Selbstvertrauen zu entwickeln, 
 ## die Beispiele für die eigenen Fälle zu adaptieren.

 ## Ich werde nicht groß auf spezielle Statistik- und Data-Science-Pakete 
 ## eingehen. Ebenso wenig, wie mit Funktionen und sogen. Expressions als 
 ## Objekten programmiert wird (weil ich das in meiner Arbeit selbst kaum 
 ## verwende). Wir verwenden zudem nur ein einziges Reporting-Setup; andere  
 ## Möglichkeiten mit denen ich gearbeitet habe, verwenden Latex-Umgebungen und  
 ## sprengen hier m.U. den Rahmen (markdown hat eine Latex-Schnittstelle)

# ______________________ ---- 
# Ressourcen ----

 ## AdvancedR kann vorbehaltlos empfohlen werden
 ## bei Programmier-Problemen ist stackoverflow oft eine gute 
 ## Anlaufstelle 
 ##   -> erst nach Stichworten suchen (ggf. auch aus google heraus)
 ##   -> bei speziellen Probleme einzelner Pakete 
 ##      -> Autor anschreiben 
 ##      -> MWE (minimal, workable example) mitsenden, bei dem der Adressat 
 ##         das problem möglichst gut nachstellen kann
 ##         (https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example,
 ##          https://stackoverflow.com/help/minimal-reproducible-example)
 ## google/Rseek (https://rseek.org/, verwende ich gar nichtmehr so häufig, weil 
 ##   google meist die seiten selbst findet) 
 ## 
 ## es biete sich an, eigene Funktionsbibliotheken aufzusetzen oder 
 ## spezielles Wissensmanagement zu betreiben, um wiederkehrende Tasks dort 
 ## nachschlagen zu können
 ##   -> Wickham: if you need to copy more than twice, build a function 
 ##   -> oft landen diese Funktionen zusammen in einem Custom-R-Paket 
 ##      (IQS intern, 
 ##       IQB: https://r-forge.r-project.org/projects/eat/, 
 ##       https://github.com/beckerbenj, https://github.com/beckerbenj/eatATA)
 ## 
 ## Neben Advanced R
 ## Patrick Burns R-Inferno – 9 circles of R-Hell and how to get out
 ## efficient R by Colin Gillespie & Robin Lovelace
 ## Folien, Workshops und Präsentationen auf useR!-Konferenzen werden seit 2017 
 ## aufgezeichnet; nach Genehmigung durch die Autorinnen und Autoren werden diese
 ## dann z.B. auf Youtube oder anderen Kanälen veröffentlicht
 ## 
 ## Für R-Packaging und Report-Generation bediene ich mich zweier weiterer 
 ## Ressourcen, von denen ich annehme, dass Hadley W. sie absichtlich aus 
 ## seinen Bücher außen vor gelassen hat. Dafür werde ich einige Dinge, auf die 
 ## er größeren Fokus legt nur anreißen

    ## _> Read the Source Luke ----

 ## Es ist ein sehr nützlicher Skill den Source-Code anderer Nutzer lesen zu 
 ## können. Einerseits in der Anwendung, um zu verstehen, wie die Argumente 
 ## der Funktionen weiterverwendet werden (wenn die Dokumentation hier nicht 
 ## genau genug ist). Weiter in der Anwendung, um die dahinter liegenden 
 ## Methoden im Detail zu verstehen (die -- einmal verstanden -- viel von Ihrer 
 ## "Magie" verlieren). Außerdem um Anwendungsfälle bestimmter Programmiertools 
 ## zu sehen und diese für die eigenen Fälle entleihen zu können. R ist 
 ## open source -- nutzt das!
 ## Viele Funktionen sind in base-R-Code geschrieben und veröffentlicht. 
 ##   -> RStudio Funktion anklicken und F2 drücken oder STRG+Klick öffnet neuen 
 ##      Tab mit R-Source
stats::lm
 ## Manchmal ist es nicht ganz einfach, an die letztliche Implementierung der 
 ## Methoden zu kommen. 
stats::lm.fit
getAnywhere("%%")
`%%`
rowsum
 ## für einen Überblick, siehe auch 
 ## https://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function
 ## https://cran.r-project.org/doc/Rnews/Rnews_2006-4.pdf (Uwe Ligges, S. 43 ff.)
methods(rowsum)
rowsum.default 
 ##   -> https://github.com/wch/r-source Read-only mirror of R source code
 ##      suche nach rowsum_matrix -- entrypoint do_rowsum; suche nach do_rowsum
 ##      suche nach rowsum
 ##
 ## Ein zentrales Ziel, das v.a. der zweite Teil dieses Kurses erreichen will, 
 ## ist genau diese Lücke zu schließen

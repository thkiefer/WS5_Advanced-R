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
# VOLL-Version
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

  ## zur fortgeschrittenen Programmierung gehört ein stabiles Vokabular 
  ## von Basis-Funktionen zu haben. 
  ## Hadley ist für die erste Version seines Buchs die Base/Stats und 
  ## Utils-Bibliothek durchgegangen 
  ## und hat die für ihn wichtigsten Funktionen aufgelistet. 
  ## Ich hatte auch angefangen, die für mich zentralen Funktionen in einem 
  ## Dokument zu sammeln und die meisten davon hat Hadley mit seiner Arbeit hier 
  ## abgedeckt. 

  ## Ich hatte noch immer recht viel mit Matrix-Algebra zu tun; daher finde ich 
  ## gehören colSums, rowSums und rowsum auch dazu.

  ## Wichtig ist auch nicht die Funktionen im Detail zu kennen; sondern zu 
  ## wissen, dass es sie gibt. 
 
  ## Ein paar von Hadleys Liste waren für mich auch neu und ich verwende nicht 
  ## alle regelmäßig, aber ich werde die ein oder andere Funktion in Zukunft mal 
  ## gezielter verwenden

  ## Geht die Liste durch.

  ## [optional] Vielleicht nehmen wir uns 2 oder 3 Methoden raus und sehen uns 
  ## an, wie sie funktionieren.
  ## Vorschläge?

  ## character manipulation
  ## ?grep
  ## ?regex 
  ## see https://bookdown.org/rdpeng/rprogdatascience/regular-expressions.html
  ## cheat sheet https://raw.githubusercontent.com/rstudio/cheatsheets/main/regex.pdf
  ##
  ## Fragen zu for/if/while/switch?
  ## https://twitter.com/data_question/status/1518205827223638017
  for(i in 0:0) print("y") ## body wird ausgewertet so oft, der Vektor der Sequenz 
                           ## (nach in) Elemente hat (hier: length(0:0))
                           ## body wird nicht ausgewertet, wenn seq keine Elemente hat

  ## Wie schon angesprochen -- learn to read the source
  ## man lernt irrsinnig viel, wenn man sich die R-Programme andere Entwickler 
  ## anschaut. 
  ## Auch wenn man deren Stil oder deren Programmierform nicht zustimmt, man 
  ## denkt immer wieder über Performanz, Stil und Funktionen nach und erkennt, 
  ## wie andere über die gleichen Themen nachdenken und wie sie damit umgehen
  ## -- und es ist fast immer eine neue Funktion dabei, die man bis dato noch 
  ##    nicht in Aktion kannte

# __________________________________________ ---- 
# Googles Style Guide ----

  ## Ein klar definierter Programmierstil ist nicht unbedingt notwendig; 
  ## aber er erleichtert die Kommunikation mit Kolleginnen und Kollegen; 
  ## er erspart ihnen und sich selbst Zeit beim Lesen/Debuggen und hat mit der 
  ## Zeit auch etwas ästhetisches, wenn man einen Coding-Style erkennt. 

  ## Style Guide war ebenfalls ein eigenes Kapitel in der ersten Edition von 
  ## Hadleys Buch, in welchem er die Google Style Guides erweiterte.
  ## Inzwischen sind die Style Guides im tidyverse zu finden, wobei diese von 
  ## Google referenziert werden.
 
  ## Wichtig ist vielleicht auch nicht die spezifische Wahl eines Coding-Styles 
  ## sondern eher, dass man einen hat, dem man konsistent folgen kann
  ## das ist schwer -- und (für mich kann ich sagen): der wandelt sich

  ## zum Beispiel fange ich erst nach und nach an die geschweiften Klammern oder 
  ## um die Ausdrücke innerhalb for und if Leerzeichen zu setzen
 
  ## ich bin auch bei der Verwendung von Namens-Trennung nicht konsistent
  ## Namen sind generell sehr schwer gut zu wählen
  ## die .-Notation hatte ich früher verwendet; ich mache ich jetzt aber 
  ## nicht mehr gerne, weil die zu nah an der S3-Notation ist (da komme ich 
  ## nachher nochmal dazu)
  ## camelCase habe ich eine Zeitlang probiert; das fand ich nie wirklich 
  ## ästhetisch
  ## ich werde es jetzt doch mal mit _ probieren
 
  ## vielleicht noch eine Sache, die mir recht wichtig ist -- vor allem, wenn man
  ## in einem Team oder als Auftragnehmer arbeitet.
  ## Wenn Skripte in einer Reihenfolge laufen sollen, setzte ich in der 
  ## Dateinamensbezeichnung in der Regel Ziffern vorne dran 
  ## (das erhöht mindestens die Transparenz und Reproduzierbarkeit)

  ## [optional]
  ## Style-Guide durchgehen
  ## : und :: und ::: sind infix-Operatoren, die keine Leerzeichen drum rum brauchen

?? select

# __________________________________________ ---- 
# Jenny Brian [@Rstudio] on Project Workflow ----

  ## https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

  ## Die Zentrale Idee dabei ist, in einem abgeschlossenen Projekt
  ## zu arbeiten, dass beliebig transportierbar und dessen Ergebnisse und 
  ## Zwischenschritte transparent reproduzierbar gemacht werden

  ## versteht mich nicht falsch: Wir haben diese Skripte mit rm(list = ls()) am 
  ## Anfang.. Viele davon.


  ##_ > R-Project ----
 
  ## new Project -> 02c_workflow  

  ## setwd()-Befehl setzt ein Verzeichnis voraus, dass sich ändern kann, oder 
  ## wenn man mit anderen zusammenarbeitet, bei denen so nicht vorliegt.
  ## R-projekte setzen das Arbeitsverzeichnis (setwd()) auf den Verzeichnispfad 
  ## des Projekts

  ## R-Projekt-Optionen .RData zeigen und deaktivieren!
  ## verwendet standardmäßig R-Studio-Global-Default (der am Anfang bereits 
  ## deaktiviert ist)

  ## _> here ----

  ## mit dem Paket here() kann man wie mit file.path() davon ausgehend 
  ## in die Unterverzeichnisse reinsehen
file.path(getwd(), "Subverzeichnis", "Da ist eine datei.wev")
here("Subverzeichnis", "Da ist eine datei.wev")

  ## _> str+shift+F10 ----

  ## was ist mit dem rm(list = ls())?
  ## Das Problem ist, dass nur Objekte entfernt werden. Es werden keine 
  ## Optionen auf den Projekt-Default zurückgesetzt, keine 
  ## überarbeiteten Umgebungsvariablen und keine Parameter zurückgesetzt
  ## (options(), par())

  ## _> .Rprofile ----

  ## -> new .Rprofile -> copy from git-ex-_Rprof.R

  ## mit einen .Rprofile-Dokument (entweder im Projekt-Verzeichnis; oder, 
  ## wenn es kein Projekt ist, im User-Verzeichnis), kann man den Startup des 
  ## Projekts bzw. von R/R-Studio sich selbst einrichten.

  ## hier sollten keine Befehle rein, die die Funktionalität von R systematisch 
  ## ändern (das ist vor allem dann schwerwiegend, wenn das .Rprofile-Dokument 
  ## nur lokal liegt und nicht mit einem Projekt übergeben wird -- darunter 
  ## leidet die Reproduzierbarkeit): 
  ## -> keine library-Aufrufe (die sollten in die scripte rein und nicht im 
  ##    .Rprofile verstecken)
  ## -> keine Optionen setzen, die R-Funktionalität ändern (wie options(digits = 3), 
  ##    options(stringsAsFactors = FALSE) [letzteres ist veraltet, aber früher 
  ##    Standard für: so geht's nicht])

  ## -> ich verwende ganz gerne das .Rprofile, um Funktionen-Shortcuts, 
  ##    default-objekte festzulegen oder eigene Funktionen anzulegen. 
  ## => hier scheiden sich die Geister
  ##    (1) das könnte/sollte (gemeinsam mit den library()-Aufrufen) in ein 
  ##        Grundscript, das am Anfang eines jeden scripts eingelesen wird
  ##    => ich finde hier aber die Projekt-Umgebung mit STRG+SHIFT+F10 neu zu 
  ##       laden angenehmer.
  ##    (2) oder solche Funktionen (v.a. wenn sie länger sind und häufig 
  ##        verwendet werden) sollten in ein R-Paket gesteckt werden. 

  ## _> git ----
  
  ## benötigt git auf dem Rechner
  ## https://docs.github.com/en/get-started
  ## Tools -> Version Control -> Project Setup 
  ## (oder tools -> Project Options Git/SVN)
  ## Version Control System -> Git -> yes -> yes
  ## new tab "git" ob top right pane
  ## -> 01_raschanalyse.R -> copy from git-ex_01 -> commit 
  ## -> new folder roh
  ##    new folder ergebnis
  ##    new script -> library(TAM); schreiben <- FALSE 00_grundscript.R
  ##    Konsole -> data(data.sim.rasch)
  ##    write.csv2(data.sim.rasch, here::here("roh", "rasch_itemresponses.csv"), row.names = FALSE, na = "")
  ##    edit 01_raschanalyse
  ##    ausführen (strg+shift+s)
  ##    commit
  ## -> see version history 01_raschanalyse.R

  ## _> Syntax-Folding und Abschnittsberschriften ----

  ## [von https://twitter.com/hi_im_alise/status/1539746290564997121]
  ## ALT+o, ALT+SHIFT+O

  ## _> Dateibezeichnungen ----

  ## https://speakerdeck.com/jennybc/how-to-name-files

  ## roh => data_cleaning 
  ## -> aufbereitet => modellierung/analyse/prediktion/verifikation/visualisierung
  ## -> deskriptives/ergebnis/uebergabe => Ablage von Ergebnis-Files
  ## -> reporting/bericht => Bericht

# __________________________________________----
# Reminder: ----
# Aussuchen, womit machen wir weiter? ----
# Lieber Unterbrechen/Fragen/Verstehen, ----
# als alles vollständig durchgehen ----

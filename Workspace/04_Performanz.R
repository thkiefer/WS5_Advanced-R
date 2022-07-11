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
# Script 04: Performanz
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

where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if(exists(name, envir = env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))
  }
}

  ## R ist per Default nicht die schnellste Programmiersprache, die es gibt; 
  ## aber sie ist für die meisten Dinge schnell genug.
  ## Das ist zum Teil per Design: einige Entscheidungen am Design der Sprache 
  ## wurden zugunsten der Flexibilität bzw. der Einfachheit mit der Statistik 
  ## ausgeführt werden kann, getroffen.

  ## R wird in der Regel verwendet, um einen einzelnen Datensatz zu 
  ## analysieren -- und das soll schnell gehen -- da möchte man sich nicht 
  ## Stundenlang mit den Einzelheiten der Sprache beschäftigen, sondern 
  ## man möchte schnell und unkompliziert Ergebnisse; und das ist in Ordnung

  ## Wenn man aber Code produziert, den andere verwenden sollen, oder der 
  ## häufig ausgeführt wird, wird man sich schon Gedanken um die Performanz
  ## machen; 
  ## aber eben auch um Lesbarkeit, Verständlichkeit, Debugging und Useability

  ## nicht zu unterschätzen ist der Geschwindigkeitszuwachs, den größere 
  ## Computer mit sich bringen
  ## > Gordon Moore sagte 1965 -- frei interpretiert -- voraus, dass die 
  ##   Rechengeschwindigkeit der Prozessoren sich alle paar (etwa 2) Jahre 
  ##   verdoppelt und wir nur noch die Hälfte für zahlen werden.
  ## > das hat sich inzwischen (in der Corona-Zeit) etwas verlangsamt, 
  ##   aber generell gilt dennoch, dass der Leistungszuwachs von Rechenprozessen 
  ##   allein schon auf Hardwareebene relevant ist
  ## Moore's Law refers to Gordon Moore's perception that the number of 
  ## transistors on a microchip doubles every two years, though the cost of 
  ## computers is halved. Moore's Law states that we can expect the speed and 
  ## capability of our computers to increase every couple of years, and we will 
  ## pay less for them.
  ##

  ## Trotzdem wollen wir uns hier ein paar Tools widmen, die in erster Linie 
  ## die Performanz von R erhöhen, aber auch dazu beitragen sollen das 
  ## Verständnis dafür zu schärfen, wie R funktioniert. Das ist ein Wechselspiel
  ## 

# ______________________________ ----
# Performanz vs. Flexibilität ----

  ## R hat die Eigenschaft, dass man sich bei vielen Anwendungen keine Gedanken 
  ## über den zu verwendenden Datentyp von Variablen machen muss. R macht das 
  ## für uns. Das ist anders bspw. bei C/C++; das sehen wir später noch.
 
  ## Um explizit anzugeben, dass x ein Integer ist
x <- 0L 

  ## Der Haken: R weiß nicht, dass in einer Anwendung x immer ein Integer ist 
  ## und muss daher jedes mal nachsehen, welche Implementierung bspw. für "+" 
  ## verwendet wird. Das kostet zeit
for(i in 1:1e6) x <- x + 1 

  ## Ebenso kostet das dynamic lookup (hatten wir im Rahmen des Programmierens 
  ## angesprochen) Zeit. Für jeden Funktionsaufruf oder variablen-anweisung 
  ## geht R den Suchpfad durch das Objekt/die Funktion gefunden wurde.
a <- 1
f <- function() {
  g <- function() {
    print(a)
    assign("a", 2, envir = parent.frame())
    print(a)
    a <- 3
    print(a)
  }
  g()
}
f()

  ## Das gilt nicht nur für Objekte oder geladene Funktionen, sondern auch für 
  ## alle Base-Funktionen. Sogar für Primitives; weil es sehr wohl möglich ist
  ## sogar diese zu überschreiben (das gilt sogar für `{` und `(`)
`{`
where("(")
search()

  ## Zudem kostet die Verwendung von lazy evaluation Zeit. 
  ## Argumente werden bei Funktionsaufruf erzeugt (nicht das Argument, aber die 
  ## Zuweisung) und sobald die Argumente im Body verwendet werden, wird die 
  ## Zuweisung ausgeführt.
  ## Das Erzeugen solcher Zuweisungen kostet Zeit; unnötige Zeit, wenn sie nicht 
  ## verwendet werden; macht es dem Entwickler aber einfacher, weil er sich 
  ## nicht darum kümmern muss, ob das Argument verwendet wird, oder nicht.
  ## Andere Programmiersprachen warnen davor oder geben Fehler, wenn objekte 
  ## erzeugt oder Argumente gesetzt werden, die im Funktions-Body nicht 
  ## vorkommen.

# ______________________________ ----
# Profiling ----

  ## system.time ist ein erster einfacher Schritt die Geschwindigkeit von 
  ## Implementierungen zu untersuchen.
x <- runif(100)
  ## zum Beispiel für verschiedene Möglichkeiten zur Wurzel-Berechnung.
sqrt(x)
exp(log(x) / 2)
x^0.5
 
  ## welche ist schneller?
system.time(sqrt(x))
system.time(x^0.5)

  ## Die Computer sind da inzwischen zu schnell, als das system.time das für 
  ## derart kleine Anforderungen schätzen könnte. Aber. man kann das ja oft genug
  ## wiederholen.

n <- 1e6
system.time(for(ii in 1:n) sqrt(x))
system.time(for(ii in 1:n) x^0.5) 
  ## das ist in Sekunden. 
  ## Durch n dividiert ergibt das die Laufzeit _einer_ Iteration

  ## Alternative: proc.time oder Sys.time().
ptm <- proc.time()
for(ii in 1:n) sqrt(x)
proc.time() - ptm

  ## microbenchmark; Alternatives Standard-tool, das auch für Stackoverflow-Zwecke 
  ## ausreicht
## library(microbenchmark)
x <- runif(100)
microbenchmark(sqrt(x))
microbenchmark(sqrt(x), 
               x^0.5) 
 ## neval = jeder Prozess wird 100 mal durchgeführt und der Mittelwert für eine
 ## Iteration berechnet.
 ## (milli heißt: 1 000 Iterationen pro Sekunde
 ##  micro heißt: 1 000 000 Iterationen pro Sekunde
 ##  nano heißt: 1e-9 Iterationen pro Sekunde)

microbenchmark(sqrt(x), x^0.5, times = 1000) ## Fokus auf Median und untere und obere Quartile

  ## Für 100 Berechnungen von ^0.5 braucht R ca. 3100 Nanosekunden (bei mir); 
  ## sqrt braucht 600, das sind 2500 Nanosekunden weniger. 
  ## Das sind 25 Nanosekunden je Berechnung, oder 25 Sekunden für eine Milliarde 
  ## Berechnungen von sqrt 
  ## (
  ## -> ich hab' einen recht leistungsstarken Laptop mit einem i7-Prozessor
  ## -> auf unseren Rechenservern, die auf parallele Berechnung optimiert sind, 
  ##    sind das 0,08 Sekunden je 1 Millionen Berechnungen
  ## -> wie ist das bei euch? 
  ## -> vor ein paar Jahren noch 5 Sekunden für eine Millionen Berechnungen 
  ## -> R.3.6.1 aufmachen -- Unterschiede v.a. für ^.5; im Vergleich zum 
  ##    Prozessor eher gering
  ## )
 
  ## Das soll auch helfen wirklich über euer Bottleneck nachzudenken. 
  ## Andererseits ist das eine Gelegenheit sich selbst herauszufordern.

# Rprof
  
  ## Um das Bottlenecks zu identifizieren, eignet sich Rprof. 
  ## Dabei sollten realistische Szenarien für den Input entworfen werden und 
  ## dann die Laufzeit gemessen werden. 
  ## Und wie gesagt: pragmatisch sein; nicht zu viel zeit investieren zu 
  ## versuchen alle Bottlenecks zu eliminieren.

?Rprof ## Funktioniert für Quelldateien

  ## output
tmp <- tempfile()
# require(here)
source(here::here("04a_profiling-source-01.R")) ## 3 Geschachtelte Funktionen mit sleep()

Rprof(tmp, interval = .01)
f()
Rprof(NULL)
summaryRprof(tmp)

  ## [optional] Fortgeschritten bspw. 
  ## Hadley Wickham: devtools::install_github("hadley/lineprof") -- deprecated
  ## Jenny Bryan: devtools::install_github("rstudio/profvis") 
  ## https://support.rstudio.com/hc/en-us/articles/218221837-Profiling-R-code-with-the-RStudio-IDE
library(profvis)
library(ggplot2)
profvis({
  g <- ggplot(diamonds, aes(carat, price)) + geom_point(size = 1, alpha = 0.2)
  print(g)
})

# ______________________________ ----
# Optimieren ----
  
  ## Für R-Code optimieren gibt es einige Ansätze, von denen wir ein paar hier 
  ## -- teilweise etwas ausführlicher -- besprechen.

  ## _> Vorhandene Lösungen? ----

  ## Ein gern übersehener Ansatz ist sich die Frage zu stellen, ob das vielleicht
  ## schon mal wer gemacht hat. Vielleicht gibt es schon schnelle Lösungen für 
  ## das Problem, vor dem man steht.
  ## Auf CRAN, stackoverflow, 
  ## reverse dependencies von Rcpp

  ## _> Defensiv/Sparsam programmieren ----

  ## Indem man sich über den möglichen Input Gedanken macht, kann man auch 
  ## auf Funktionen zugreifen, die auf Datentypen zugeschneidert und damit
  ## schneller sind: rowSums, colSums, rowMeans, colMeans, rowsum
  ## eher als: apply-Familie, aggregate, ...
  ## vapply ist schneller als sapply, weil der ouptut festgelegt ist
  ## names von data.frames, anstatt colnames

x <- 1:100
  ## Gleichheit vs. Mengeninklusion
microbenchmark(any(x == 10), 10 %in% x)

  ## Dafür braucht man ein stabiles Vokabular, um zu wissen, dass solche
  ## Funktionen existieren. Manche Funktionen transformieren den Input in den 
  ## benötigten Datentyp. Wenn man den Datentyp oder die Datenstruktur kennt, 
  ## kann man auch die spezifischen Methoden direkt ansprechen

df <- data.frame(x = runif(100), y = runif(100))
microbenchmark({
  sub <- df[sample(nrow(df), 10), ] ## Subset von data.frame
  cor(sub$x, sub$y) 
},{
  i <- sample(nrow(df), 10) 
  cor(df$x[i], df$y[i]) ## Subset von vektor
})

 ## Beispiele: apply transformiert den Input in eine Matrix

 ## Default-Optionen von Funktionen können aufwendig sein; wenn das nicht 
 ## benötigt, oder vorab bekannt ist, sollte man die Argumente entsprechend setzen
x <- rep(letters[1:3], each = 50)
microbenchmark(factor(x), factor(x, levels = c("a", "b", "c")))

 ## as.data.frame vs. class(l) <- "data.frame"

 ## Die meisten Funktionen werden so geschrieben, dass sie möglichst flexibel 
 ## mit verschiedensten Eingabe-Datentypen umgehen können.
 ## Diese auf die spezifischen Datentypen, die in deinem Code verwendet werden, 
 ## umzuschreiben, kann hilfreich sein.
 ## Dafür muss man sich mit dem Quell-Code der Funktion auseinandersetzen und 
 ## Schritt für Schritt prüfen, ob man die Eigenschaften des Datentyps/der 
 ## Datenstruktur im Einzelnen wirklich braucht. 
 
  ## _> Vektorisieren ----

  ## R ist eine Vektorbasierte Programmiersprache mit mächtiger Matrixalgebra. 
  ## Vektorisieren bedeutet von dieser Eigenschaft und den Tools Gebraucht 
  ## machen. D.h. die Funktionen möglichst so schreiben, dass sie alle Einträge 
  ## eines Vektors gleichzeitig verarbeiten.

  ## Vektorisieren bedeutet nicht unbedingt Schleifen auf Teufel-komm-raus zu 
  ## vermeiden, sondern einen Ansatz zu verfolgen, der das gesamte Objekt und
  ## nicht nur Skalare Einträge davon verwendet

  ## Ein erster Reflex mag sein, for-Schleifen gegen apply zu ersetzen; 
  ## das ist zwar manchmal auch schneller, aber das ist nicht unter vektorisiert 
  ## zu verstehen.

  ## Außerdem sind für komplexe Statements for-Schleifen deutlich einfacher 
  ## lesbar und zu debuggen -- wobei man die Statements ja auch in Funktionen 
  ## auslagern kann

  ## vektor-fähige Funktionen sind häufig in C geschrieben. Typische 
  ## Vektorisierte Funktionen sind rowSums, colSums, rowMeans, colMeans, ... 
  ## Mangmal ist Matrixalgebra hilfreich -- wie %*%, outer, oder tensor für 
  ## höherdimensionale Algebra
x <- matrix(runif(100), ncol = 5)
r <- rep(1, nrow(x))

microbenchmark(
  "for" = {
    res <- numeric(ncol(x))
    for(i in 1:ncol(x)) res[i] <- sum(x[, i])
  },
  apply(x, 2, sum),
  colSums(x),
  rowsum(x, r),
  r %*% x
)

# all.equal(c(r %*% x), colSums(x))

  ## rowsum
x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
microbenchmark(
  aggregate(x, list(group), sum),
  rowsum(x, group)# ,
  # rowsum.default(x, group)
)
rowsum
rowsum.default # internal rowsum_matrix

  ## Index-Matrix
mx <- matrix(runif(1e6), ncol = 2)
bmx <- cmx <- matrix(NA, nrow(mx) / 2, ncol(mx))
microbenchmark( ## multipliziere jede Zeile eintragsweise mit der darauffolgenden.
  "for" = for(ii in 1:nrow(bmx)) bmx[ii, ] <- mx[2 * ii - 1, ] * mx[2 * ii, ],
  "index" = cmx <- mx[seq(1, nrow(mx), by = 2), ] * mx[seq(2, nrow(mx), by = 2), ],
  "index2" = {
    ind <- seq(1, nrow(mx), by = 2)
    cmx <- mx[ind, ] * mx[ind + 1, ]
  }
)

all.equal(bmx, cmx)

  ## vectorized if
hit <- vector(length = 1e4)
microbenchmark(
  "hit" = for(i in 1:1e4) if(runif(1) < .3) hit[i] <- TRUE,
  "fasthit" = fasthit <- ifelse(runif(1e4) < .3, TRUE, NA)
)

  ## Vektorisieren ist oft gar nicht möglich; bspw. wenn die Iterationen von 
  ## der Vorgänger-Iteration abhängen (wie das bei Schätzfunktionen (EM, Bayes)
  ## oft der FAll ist)
  ## Kennt ihr filter, cumsum, diff?

  ## Wenn man schon eine Schleife braucht, sollte man darauf achten, soweit wie 
  ## möglich alle Operationen, die nicht in der Schleife gebraucht werden, 
  ## auszulagern (sparsam programmieren)

  ## Wenn man die Wahl hat, kann man auf die notwendige Zahl an Iterationen 
  ## achten: oft weniger Spalten im data.frame, als Fälle; oder über die Levels 
  ## eines Faktors.

  ## Manchmal kann Vektorisierung auch nach hinten los gehen; bspw. wenn man 
  ## umfangreiche Integer-Matritzen erzeugen will, die lange dauern oder viel 
  ## Arbeitsspeicher brauchen.

  ## _> Kopieren vermeiden ----
 
  ## Immer wieder gerne gesehen und in der interaktiven Datenanalyse vollkommen 
  ## in Ordnung ist, dass Objekte im laufenden Prozess größer werden. 
  ## Das verbraucht Ressourcen, da die Objekte dabei neu angelegt (i.e. kopiert)
  ## werden. 
  ## Bei der Entwicklung von Funktionen kann man mit etwas Planung diese 
  ## Ressourcen einsparen. 
  ## Man sollte sich die Fragen stellen:
  ## [1] legt man mit einem Aufruf eine Kopie?
  ## [2] ist das ein Aufruf einer weiteren Funktion?
n <- 1000
microbenchmark(
  {
    vec <- numeric(0) # oder c(), oder NULL
    for(i in 1:n) vec <- c(vec, i) ## der Vektor wächst
  },
  {
    vec <- numeric(n)
    for(i in 1:n) vec[i] <- i ## der Vektor ist vorinitialisiert
  },
  vec <- 1:n # vektorisiert
)

  ## cbind, rbind und paste weisen hier einen größeren Overhead auf. 
n <- 10
microbenchmark(
  "wachsen" = {
    df <- data.frame(a = character(0), 
                     b = numeric(0))
    for(i in 1:n){
      df<- rbind(df, data.frame(a = sample(letters, 1, replace = TRUE),
                                b = runif(1)))
    }
  },
  ## Wenn man weiß, wie groß die Objekte werden, kann man sie entsprechend 
  ## vorinitialisieren
  "initialisieren" = {
    df <- data.frame(a = character(n), 
                     b = numeric(n))
    for(i in 1:n){
      df$a <- sample(letters, 1, replace = TRUE)
      df$b <- runif(1)
    }
  })


  ## Es kann passieren, dass am Anfang nicht klar ist, wie viele Zeilen ein 
  ## data.frame letztlich hat. 

  ## wachsen
f1 <- function(n) {
  my.df <- data.frame(a = character(0), 
                      b = numeric(0))
  for(i in 1:n){
    this.N <- rpois(1, 10) ## zufälliges Wachstum
    my.df<- rbind(my.df, data.frame(a = sample(letters, this.N, replace = TRUE),
                                    b = runif(this.N)))
  }
  my.df
}

  ## dann kann man evtl. eine sinnvolle obere Grenze angeben und leere Zeilen 
  ## hinterher löschen, oder 
  ## man kann den data.frame dynamisch erweitern um mehr als ein Element pro 
  ## Schleife.
f2 <- function(n) {
  current.N <- 10 * n
  my.df<- data.frame(a = character(current.N),
                     b = numeric(current.N))
  count <- 0
  for(i in 1:n) {
    this.N <- rpois(1,10)
    if(count + this.N > current.N) { 
      old.df <- my.df
      current.N <- round(1.5 * (current.N + this.N))
      my.df<- data.frame(a = character(current.N), b = numeric(current.N))
      my.df[1:count, ] <- old.df[1:count,]
    }
    my.df[count + 1:this.N, ] <- data.frame(a = sample(letters, this.N, replace = TRUE),
                                            b = runif(this.N))
    count<- count + this.N
  }
  my.df <- my.df[1:count,]
  my.df
}

  ## Schließlich kann das rbind auch ausgelagert werden. 
f3 <- function(n) {
  my.list<- vector('list', n)
  for(i in 1:n) { 
    this.N <- rpois(1, 10)
    my.list[[i]] <- data.frame(a = sample(letters, this.N, replace = TRUE),
                               b = runif(this.N))
  }
  my.df<- do.call('rbind', my.list) # Reduce('rbind', my.list)
  my.df
}
microbenchmark(f1(10), f2(10), f3(10))

  ## _> Paralleles programmieren ----

  ## Paralleles Programmierung ist in R noch nicht intrinsisch. Auch wenn der 
  ## Computer mehrere Rechenkerne hat, belegt die R-Konsole zu jeder Zeit nur 
  ## immer einen Kern (ggf. gleichzeitig mehrere -- aber das macht Windows, nicht 
  ## R; aber auch dann kann in Summe nur 1 Kern belastet werden).

  ## Ein Dummy-Weg für paralleles Rechnen ist einen Prozess aufzusetzen (bspw. 
  ## bei einer Simulationsstudie) und den mit bspw. verschiedenen Startwerten 
  ## oder Startparametern in mehreren Konsolen anzustarten; und die Ergebnisse 
  ## hinterher per hand zu poolen

  ## Für "echte" Parallelisierung gibt es inzwischen verschiedene R-Pakete
  ## z.B.: das älteste mir bekannte R-Paket dürfte snow sein. 
  ##  Darauf basieren einige Weiterentwicklungen (snowFT, vgl. Slides)
  ## Außerdem gibt es parallel (seit 2.14. in R-Core), doParallel und forEach
  ## Das parallel Package enthält Implementierungen von snow (Luke Tierney at al)
  ## und multicore (Simon Urbanek); letzteres bietet Funktionalitäten für 
  ## Mac/Unix/Linux OS

  ## Ich möchte hier auf ein (sehr einfaches Beispiel) eingehen, damit das 
  ## prinzipielle Vorgehen klar wird. Ich kann mich hier nur auf Windows 
  ## beschränken, weil ich das noch nicht mit Mac oder Linux probiert habe.

  ## Untersuche wieviel _physikalische_ Kerne zur Verfügung stehen 
library(parallel)
detectCores()
(phys_cores <- detectCores(logical = FALSE)) ## physical vs. logical cores


  ## Starte einen Cluster 
cl <- makeCluster(phys_cores - 1, type = "PSOCK") ## wir wollen den Computer nicht 
                                                  ## auslasten; lassen einen Kern 
                                                  ## für das Betriebssystem

  ## type PSOCK: Workers starten mit einer leeren Umgebung
  ## type FORK: Workers sind vollständige Kopien des Start-Prozess (gibt's in 
  ##           Windows nicht)
  ## Auf mac/unix/linux verwende mclapply anstatt lapply 
  ## -> mclapply erbt wie FORK den kompletten Workspace vom Start-Prozess
cl
typeof(cl)
(n_cl <- length(cl))
cl[[1]]
names(cl[[1]])
# stopCluster(cl)

  ## Objekte auf den Cluster zu kopieren kostet Zeit, unabhängig vom 
  ## Cluster-Type. Es wird immer einen Overhead der Kommunikation zwischen 
  ## Workers und Start-Prozess geben. Daher darf man nicht davon ausgehen, 
  ## dass sich die Laufzeit halbiert bei 2 Clustern.
x <- 1:10
  ## mit clusterEvalQ(cl, expr) wird auf den Clustern von cl der Ausdruck expr 
  ## verarbeitet.
clusterEvalQ(cl, x) ## x ist noch nicht drauf
clusterExport(cl, "x")
clusterEvalQ(cl, x) ## jetzt ist es da.

  ## das gleiche gilt für Pakete, optionen, parameter, usw.
library(mvtnorm)
clusterEvalQ(cl, exists("dmvnorm"))
clusterEvalQ(cl, {
  library(mvtnorm)
  exists("dmvnorm")
})

  ## Eine typische Verwendung von parallel ist clusterApply
  ## clusterApply(cl = NULL, x, fun, ...)
clusterApply(cl, rep(1000, n_cl), fun = function(x) mean(rnorm(x, mean = 5)))

  ## Wenn ein Ausdruck öfter ausgeführt werden soll, als Cluster verfügbar sind, 
  ## warten alle Worker bis der letzte fertig ist, dann wird das nächste Set 
  ## in der Länge verfügbarer Workers verarbeitet.
clusterApply(cl, rep(1000, 25), fun = function(x) mean(rnorm(x, mean = 5))) 

stopCluster(cl)

# foreach
library(doParallel)
library(foreach)
  ## Verwendung von foreach-Package für intuitive Überführung von for-Schleifen 
  ## in parallele Programmierung
cl <- makeCluster(phys_cores - 1, type = "PSOCK") # Starte Cluster
doParallel::registerDoParallel(cl) # registriere cluster für foreach-Pakage

res <- vector(mode = "list", length = 10)
x <- rnorm(1000)

res <- foreach(ii = 1:length(res), 
               .combine = c, ## oft list 
               .packages = NULL) %dopar% { ## ersetze %dopar% durch %do%  
                                           ## für nicht-parallel
                 seq_length <- length(x)/length(res) ## greife auf Objekte 
                                                     ## im Startprozess zu
                 index <- (ii - 1) * seq_length + (1:seq_length) 
                                                     ## greife auf Objekte
                                                     ## des Aufrufs zu
                 list(mean(rnorm(x[index])))
               }

res
stopCluster(cl)

  ## _> Auslagern ----

  ## Manchmal reicht R-Code einfach nicht, um das Paket/die Funktion 
  ## schnell genug zu machen. 

  ## R hat Schnittstellen externe schnellere Programmiersprachen wie C, Fortran, 
  ## C++ einzubinden. 
  ## Wir konzentrieren uns hier auf C++, weil die Schnittstelle mit dem R-Paket 
  ## Rcpp von Dirk Eddelbüttel und Romain Francois am weitesten entwickelt ist.

  ## Dennoch: externer C++-Code lässt sich deutlich schwerer debuggen als R-Code
  ## (die browser-/traceback-Methoden gehen nicht in den C++-Code rein)
  ## und für Anwender noch schwerer lesen, weil man da schwerer rankommt.

  ## R bietet die Möglichkeit mittels dem Paket Rcpp expliziten C++-Code in R 
  ## auszuführen. 
  ## Wir sehen später, wie Pakete C++ (oder C-Code) einbinden, aber das hier 
  ## ist eine geeignete Möglichkeit das schon mal vorzubereiten.

  ## Ich gehe nicht zu sehr in die Tiefe von C++; das heißt, ihr müsst kein C++ 
  ## können, um diesem Teil folgen zu können -- der Kern der Syntax dürfte klar 
  ## werden.

  ## Typische Engpässe, die in C++ umgesetzt werden können, sind 
  ## [1] Schleifen, die nicht leicht vektorisiert werden können
  ## [2] rekursive Funktionen oder welche die hoch-frequente Aufrufe benötigen
  ## [3] weitere Datenstrukturen und Algorithmen, für die spezielle C++-
  ##     Programmbibliotheken existieren.

  ## install.packages("Rcpp") -- häufig schon installiert sein, da extrem viele 
  ## Pakete inzwischen davon abhängen
library(Rcpp)

  ## wir schreiben uns eine einfache Addition
  ## Wir brauchen dafür Rtools, um diese Funktion kompilieren zu können. R
  ## kann keinen C++-Code importieren, aber die daraus kompilierte dynamic-link-
  ## library. Das C++-Code-Kompilieren übernimmt eines der Tools aus RTools.

  ## Kommentare starten mit //
cppFunction('int add(int x, int y, int z) { // Die Input- (wenn vorhanden) und 
                                            // Output-Typen müssen angegeben 
                                            // werden
               int sum = x + y + z; // Befehle enden mit ;
               return sum;
             }') 

add
add(1, 2, 3)

  ## vektor-input
cppFunction('int sumC(NumericVector x) { // Es gibt NumericVector, IntegerVector, 
                                         // CharacterVector, LogicalVector
                                         // double, int, String, bool
               int n = x.size();         // jede Variable muss deklariert und 
                                         // initialisiert werden (auch ein Laufindex)
                                         // [object][dot][method]-Syntax (x.size())
                                         //
               double tot = 0;           // eigentlich int tot -- Ausgabe ist ja auf int gesetzt
                                         //
               for(int i = 0; i < n; i++){ // if ist genau wie in R; for ist leicht anders
                                           // VEKTOR INDEX STARTET BEI 0
                 tot += x[i];              // a += b ist Abkürzung für a = a + b
               }
               return tot;
             }')  

sumR <- function(x) {
  tot <- 0
  for(i in 1:length(x)) tot <- tot + x[i]
  tot
}

x <- 1:100
microbenchmark(sumR(x), sumC(x), sum(x))

# SourceCpp

  ## cppFunction ist nett als Spielerei bzw. für Gimmicks oder um etwas 
  ## Die tatsächliche Entwicklung passiert aber in C++-Quell-Dateien (i.d.R. im 
  ## Rahmen von R-Paketen) aus denen DLLs gebunden werden. 
  ## Dieser Vorgang kann mit SourceCpp simuliert werden.

  ## file -> new -> c++
  ## bespreche default-Input 
  ## header nötig
  ## // [[Rcpp::export]] für export in R (space is notwendig)
  ## R-code in C++-Command--Blocks for Tests

sourceCpp(here("04b_sourceCpp.cpp"))
x <- matrix(sample(100), nrow = 10)
microbenchmark(rowSumsC(x), rowSums(x))

  ## Weitere Objekt-Typen sind NA_INTEGER, NA_STRING, NA_LOGICAL, NA_REAL
  ## List input, input.size(), input[i]
  ## return List::create(int_s, chr_s, lgl_s, num_s)
  ## create also for .*Vector

  ## weitergehende Zusammenfassungen über die Möglichkeiten und verfügbaren 
  ## Funktionalitäten mit Rcpp gibt es unter http://www.rcpp.org/ und 
  ## https://adv-r.hadley.nz/rcpp.html?q=Rcpp

# sugar

  ## Rcpps sugar bietet einige Funktionen, die es in base-R gibt direkt in C an
  ## all, any, boolean operators, arithmetic operators 

cppFunction('int sumC2(NumericVector x) {
               return sum(x);
             }')
microbenchmark(sumR(x), sumC2(x), sumC(x), sum(x))

  ## Über die reine Geschwindigkeit von Schleifen in C++ hinaus haben wir 
  ## hiermit die Möglichkeit die umfangreichen C++-Bibliotheken 
  ## wie die standard template library (STL) einzubinden
  ## oder Rcpp-Armadillo zu verwenden: das ist die spezielle optimierte 
  ## Matrix-Algebra-Bibliothek Armadillo im Rcpp-Look-And-Feel bereitgestellt

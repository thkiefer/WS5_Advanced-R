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
# Script 03: Programmieren
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

  ## jede Funktion hat drei zentrale Komponenten
f <- function(x) x^2 ## und mit geschweiften Klammern...
formals(f)
body(f)
environment(f)

  ## Ausnahmen von der Regel, sind sogenannte Primitives: Funktionen, die 
  ## aus Performance-Gründen direkt C-code ansteuern. 
  ##  die gibt es nur in base
sum
formals(sum)

`[`

# _______________________________________ ----
# Suchpfad ----

  ## _> lexical scoping ----

    ## (ist die Definition von Aufgaben- oder Untersuchungsumfängen in komplexen 
    ## Planungs-, Management- und Herstellungsprozessen)

    ## In R ist lexical scoping der Such-Prozess, den R durchläuft, wenn 
    ## ein Element aufgerufen wird.
    ## Also der ganz grundlegende Prozess, das Objekt zu suchen, auf das man 
    ## gerade zugreifen will
x <- 10
x
    ## R geht dabei den "Suchpfad" durch und schaut sequenziell nach, ob 
    ## in der jeweiligen Umgebungen das Objekt (hier x) definiert ist. 
    ## Das erste Auftreten von x, das gefunden wird, wird zurückgeworfen.
    ## Der Suchpfad ist
search() ## in der Reihenfolge

  ##
rm(x)

  ## Lexical Scoping ist nach 4 Regeln definiert: 
  ## -> Name masking / Namenüberlagerung
  ## -> Functions vs. variables / Funktionen gegen Variablen
  ## -> A fresh start / Neustart
  ## -> Dynamic lookup / Dynamisches Nachschlagen
 
# Name masking
  ## wenn R ein Objekt nicht findet, geht R den suchpfad nach oben hin durch
f <- function() {
  x <- 1
  y <- 1
  c(x, y)
}

f()   # x und y werden innerhalb von f gefunden
rm(f) # entfernen wir f

x <- 2
f <- function() {

  y <- 1
  c(x, y)
}

f() # x gibt es innerhalb von f nicht. Der nächste Ort, wo gesucht wird, ist 
    # .GlobelEnv

  ## das gilt auch bei geschachtelten Funktionen
  ## R schaut zuerst in die jeweilige Funktion, dann von dort aus nach "oben", 
  ## und sucht wo die jeweiligen Objekte definiert sind.
x <- 2 
f <- function() {
  y <- 3
  g <- function() {
    z <- 4
    c(x, y, z)
  }
  g()
}
f()

x <- 4
f()
rm(f, x)

  ## gilt auch bei closures (Funktionen die von andere Funktionen erzeugt werden)
  ## hier greifen wir etwas voraus. Auf die komme ich gleich nochmal

z <- 4
f <- function(x) {
  y <- 3
  function() { ## letztes Statement in einer Funktion wird als Ergebnis 
    c(x, y, z) ## zurückgegeben. hier: eine Funktion
  }
}

g <- f(1) ## erzeuge Funktion g für die gilt, dass x = 1 und y = 3 ist; und z?
g()

  ## Woher kennt R die Objekte z und y?
  ## R erzeugt eine Umgebung für g (kommen wir im nächsten Abschnitt nochmal 
  ## dazu)
  ## bei der Erstellung von g wird der Inhalt von f (y) dort gespeichert. 
  ## z existiert da aber nicht. Das wird aus einer darüber liegenden Umgebung 
  ## geholt
ls(envir = environment(g)) 

rm(f, g)

# Functions vs. variables

  ## das name masking gilt auch für Funktionen. Die erste im Suchpfad gefundene 
  ## Funktion, die den Namen trägt, wird verwendet 
  ## -> suchpfad beachten 
  ## -> d.h. auch, es kann von Bedeutung sein, in welcher Reihenfolge R-pakete 
  ##    geladen werden, die mit Unter Funktionen implementieren, die den gleichen 
  ##    Namen tragen (wird standardmäßig als meldung beim Laden von Paketen 
  ##    ausgegeben). 
  ##    => der Google-Style empfiehlt hier für Funktionen aus Paketen die 
  ##       Explizite Bezeichnung des Pakets, aus dem die Funktion ist: 
  ##       bsp here::here(), plyr::rbind.fill()
f <- function(x) x + 1
g <- function() {
  f <- function(x) x * 2
  f(10)
}
f(10) # die Funktion f liegt im .GlobalEnv vor
g()   # zuerst wird innerhalb der Funktion g() gesucht. Dort liegt f vor

rm(f, g)

  ## R unterscheidet 'f(' und 'f' und ignoriert bei der Suche nach 'f(' alles, 
  ## was keine Funktion ist (und umgekehrt)
f <- function(x) x + 1
g <- function() {
  f <- 3
  f(f) 
}

g() # 'f(' ist nicht innerhalb von g() aber im .GlobalEnv implementiert
rm(f, g)

# Neustart

  ## bei jedem Aufruf einer Funktion wird eine neue aufrufende Umgebung erzeugt
  ## (in der a noch nicht existiert); beim ersten mal nicht.. beim zweiten Mal 
  ## wurde a zwar schon mal angelegt, aber wieder verworfen.
f <- function() {
  if(!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
f()
f()

rm(f)

# Dynamisches Nachschlagen

  ## R sucht nach einem Objekt im Suchpfad zum Zeitpunkt des Aufrufs, nicht zum 
  ## Zeitpunkt der Funktionsdefinition
  ## Es sollte vermieden werden, dass Funktionen auf Werte im .GlobalEnv 
  ## zurückgreifen (das ist die Arbeitsumgebung); da kommen wir beim 
  ## debugging nochmal dazu
f <- function() x + 1

x <- 3 
f() 

x <- 10 
f() ## der Wert von f ist abhängig von dem Wert den x zum Zeitpunkt des 
    ## Funktionsaufrufs innehat; wenn x sich zur Laufzeit ändert, ändert sich auch 
    ## f()

  ## Das kann ganz schön lästig werden zu debuggen: wenn ihr in Eurer Funktion 
  ## einen Flüchtigkeitsfehler drin habt, kann das zum Zeitpunkt der Erstellung 
  ## funktionieren (weil die Funktion nicht-intendiert auf Objekte aus einer 
  ## anderen Umgebung (der .GlobalEnv) zugreift, anstatt auf ein ähnliches Objekt 
  ## aus der Funktion)
  ## Je nachdem ob das Objekt aus der .GlobalEnv zu einem späteren Zeitpunkt (
  ## z.B. wenn ihr die Funktion debuggen wollt) auch noch existiert, kann 
  ## es passieren, dass ihr den Bug gar nicht findet.
  ## Deswegen sind MWE sinnvoll.

codetools::findGlobals(f) ## listet alle Objekte auf, die außerhalb der Funktion 
                          ## existieren und von dort referenziert werden.

  ## Beachtet, dass "+" auch als solches Element von außerhalb der Funktion 
  ## referenziert wird. Das ist genau der Grund für Lexical Scoping, dass 
  ## es eine klare Regel gibt, wo R nach Objekten, selbst nach Funktionen sucht 
  ## (incl. sowas, wie "+")


rm(f, x)

  ## _> environment [optional] ----

  ## Wir haben schon viel über Umgebungen gesprochen. Ich möchte hier nur 
  ## kurz darauf eingehen, wie mit Umgebungen hantiert werden kann. In der  
  ## praktischen Arbeit verwende ich das wenig. Aber nachdem das so einen 
  ## zentralen Part beim Verständnis vom Programmieren einnimmt, sprechen 
  ## wir das an.

  ## Umgebungen sind im wesentlichen Strukturen in denen Objekte gebunden 
  ## werden können
  ## sie sind der Weg über den lexical scoping ermöglicht wird

# Grundlegendes
e <- new.env() ## neue Umgebung anlegen

e$a <- 1 ## Objekte in der Umgebung anlegen
e$b <- "a"
e$c <- FALSE
e$d <- 1:3

# Referenzsemantik
  ## Im grundlegenden Unterschied zu Listen oder anderen Datenstrukturen gilt bei 
  ## Umgebungen, dass, wenn ein Objekt in einer Umgebung verändert wird, 
  ## die Objekte nicht kopiert sondern direkt verändert werden.
  ## Das erlaubt Umgebungen innerhalb von Funktionen bzw. genauer: deren 
  ## Objekte anzusprechen und zu verändern, ohne die Umgebung als returnobjekt 
  ## übergeben zu müssen (wobei das objekt kopiert wird)
f <- function(x) {
  x$d <- 4:6
  invisible()
  return(5) ## man kann irgendetwas anderes übergeben
            ## wie sähe das aus, wenn x eine Liste und keine Umgebung wäre?
}
e$d
f(e)
e$d

# Eigenschaften
  ## Die Namen der Objekte in einer Umgebung sind eindeutig (im Unterschied zu 
  ## Listenelementen)
ls(name = e) 
  ## Es gibt keine Ordnung der Elemente innerhalb einer Umgebung (im Unterschied 
  ## zu Listen)
e[[1]]       
e$a
ls.str(e)

# Parent-Umgebungen
  ## eine Umgebung hat eine referenzierende "parent"-Umgebung (im Unterschied 
  ## zu Listen)
  ## diese Parent-Umgebung sind es, die lexical Scoping ausmachen
parent.env(e) 
identical(parent.env(e), globalenv())

search() ## der Suchpfad
  ## das heißt, die Parent-Umgebung, von .GlobalEnv ist (bei mir) tools::rstudio
parent.env(globalenv())

 ## parent.frame im Gegensatz zu parent.env gibt die Umgebung zurück, aus der 
 ## der Aufruf gestartet wurde
parent.frame() 

  ## einige wichtige Umgebungen
identical(globalenv(), environment())
identical(baseenv(), as.environment("package:base"))
identical(emptyenv(), parent.env(as.environment("package:base"))) ## danach 
  ## kommt nichts mehr. Über base ist nur mehr ein leeres environment. D.h. 
  ## wenn ein Objekt oder eine Funktion auhc in base nicht gefunden wird, 
  ## existiert sie nicht. (Es sei denn, man legt sie im emptyenv ab).

  ## Was passiert, wenn man eine Funktion im emptyenv ablegt?
f <- function() x + 1

x <- 3 
f() ## die hatten wir gerade unter dynamic lookup
codetools::findGlobals(f)

environment(f) <- emptyenv() ## legen wir sie im emptyenv ab
f() ## jetzt findet R "+" nichtmehr, weil "+" in base definiert ist.

# Zugriffsoperatoren
  ## $, [[ funktionieren nur innerhalb der Umgebung
  ## verwende get() um normales Scoping aufzurufen
get("d", e)
x <- 1
get("x", e) ## damit greift man eben auch auf Objekte im Suchpfad zu

# Entfernen
  ## Entfernen via rm; nicht <- NULL (im unterschied zu Listen)
rm("a", envir = e)
ls.str(e)

# exists 
  ## funktioniert ähnlich wie get(), enthält aber nur logische Rückgabe; erlaubt 
  ## aber auch nur in e nachzuschaun
exists("x", envir = e)
exists("x", envir = e, inherits = FALSE)

# Zusammenfassung
  ## diese Informationen können wir benutzen um uns eine Funktion zu schreiben,
  ## die zurückgibt, wo ein Objekt definiert ist
where <- function(name, env = parent.frame()) { ## parent.frame gibt die 
                                                ## Umgebung zurück, aus der der 
                                                ## Aufruf gestartet wird
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if(exists(name, envir = env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))                ## rekursiv
  }
}

where("x")
where("b", e) ## starte bei e
where("x", e) ## starte bei e
try(where("a", e)) # wurde vorhin gelöscht
where("+")
  
  ## _> Funktions-Umgebungen / closures [optional] ---- 

# Einschließende Umgebungen
environment(where)
ls(environment(where))
rm("x")
ls(environment(where))

# Die bindende Umgebungen 
  ## ... legt fest, wo der Name festgelegt ist, der die Methode aufruft?
e$f <- function() 1
environment(e$f)
where("f", e)

# Einschließende vs. Bindende Umgebungen 
  ## Eine einschließende Umgebung gehört zur Funktion und ändert sich nicht.
  ## Sie legt fest, wie eine Funktion werte findet

  ## Die bindende Umgebung legt fest, wie wir die Funktion finden

  ## Das ist wichtig für Pakete und Überlagerungen
  ## Daher bietet R Namespaces als einschließende Umgebungen
environment(sd) 
where("var")
  ## sd verwendet die Funktion var
sd(1:10) 
  ## wenn wir jetzt lokal var überschreiben
var <- function(x, na.rm = TRUE) 100 
  ## greift sd trotzdem auf die richtige Var-Methode zu, nämlich die, die 
  ## in der gleichen Umgebung gebunden (oder dort verfügbar gemacht wurde)
sd(1:10)

  ## Die ausführende Umgebung wird zum Zeitpunkt der Ausführung immer neu erzeugt
g <- function(x) {
  if(!exists("a", inherits = FALSE)) {
    a <- 1
  } else { 
    a <- a + 1
  }
  a
}
g(10)
g(10)

f <- function(x) {
  a <- 2
  x + a
}
x <- 2
f() ## hier ist x immer noch nicht festgelegt
f(4) ## hier wird x festgelegt

plus <- function(x) {
  function(y) x + y
}
f <- plus(1) ## hier wird x festgelegt
new_e <- environment(f)
ls(new_e)
new_e$x

# Aufrufende Umgebung
  ## dynamic lookup; wie sieht die Umgebung aus zu dem Zeitpunkt, an dem x 
  ## festgelegt ist
  ## das ist der parent.frame()
f <- function() {
  x <- 10
  function() {
    x
  }
}

i <- f()
x <- 20
i()

  ## zum krönenden Abschluss
f <- function() {
  x <- 10
  function() {
    e1 <- get("x", environment())
    e2 <- get("x", parent.frame())
    list("defined" = e1, "called" = e2)
  }
}
i <- f()
x <- 20
i()
x <- 11
i()

# ___________________________________ ----
# Elemente von Funktionen ----

  ## _> Argumente ----

  ## Hier geht es um formelle und tatsächliche Argumente von Funktionen und wie 
  ## sie übergeben werden.

  ## Definieren wir uns eine Funktion, die 3 Objekte übernimmt und diese als 
  ## Vektor zurückgibt.
f <- function(abcdef, bcde1, bcde2) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
} 

  ## Die Funktion kann die Objekte nach der Position, in der sie eingegeben 
  ## werden, den Argumenten der Funktion zuordnet
f(1, 2, 3)

  ## Wenn ein Argumenten-Name angegeben wird, dann wird das Objekt dem Argument, 
  ## zugeordnet, das den Namen hat; die anderen werden nach ihrer Position 
  ## zugeordnent
f(1, 2, "abcdef" = 3)

  ## Diese logik gilt auch, wenn der Argumentenname partiell gefunden wird 
  ## (so-genanntes partial Matching).
f(1, 2, "a" = 3)

  ## Das geht aber nur, wenn das Argument eindeutig gefunden wird
f(1, 2, "b" = 3)
formals(f)

  ## Man sollte Positions-Matching aus Lesbarkeitsgründen nur für die ersten ein 
  ## oder zwei Argumente verwenden 
  ## das sind in der Regel auch die Argument, die ohne Standardwerte kommen, 
  ## und daher notwendige Eingaben sind (gleich)

  ## Hinweis auf do.call
do.call(f, list(1, 2, 3))

  ## Viele Funktionen haben default-Werte, die dementsprechend beim 
  ## Funktionsaufruf angegeben werden können, aber nicht müssen.
f <- function(abcdef, bcde1, bcde2 = 3) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

# Lazy Evaluation 

  ## Lazy Evaluation ist das Prinzip, das es ermöglicht Ausdrücke innerhalb von 
  ## Argumenten im Funktionsaufruf anzugeben.
  ## Diese Ausdrücke werden erst zu dem Zeitpunkt ausgewertet, an dem das 
  ## Objekt innerhalb der Funktion aufgerufen wird.

?lm ## -> Print

  ## Zum Beispiel legen wir diesen einfachen Zusammenhang vom 3ten zum ersten 
  ## Argument fest
f <- function(abcdef, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

  ## Nicht empfehlenswert, aber (aufgrund der Lazy-Evaluation) auch möglich ist, 
  ## zuerst auftretende Argumente von späteren Argumenten abhängig zu machen
f <- function(abcdef = bcde1 / 4, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(bcde1 = 2)

  ## Nicht empfehlenswert, aber (aufgrund der Lazy-Evaluation) auch möglich ist, 
  ## Argumente von Objekten abhängig zu machen, die erst im body der Funktion 
  ## erzeugt werden

  ## Dadurch wird aber intransparent, was der default-Wert eigentlich ist.
  ## Wenn der default erst später kompliziert erzeugt wird, kann man ihn auch 
  ## einfach mit NULL initialisieren
f <- function(abcdef, bcde1, bcde2 = efg) {
  efg <- abcdef + bcde1^2
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(1, 2)

  ## Was passiert, wenn man ein Argument vergisst, was keinen Default-Wert hat, 
  ## haben wir alle schon gesehen
f(1)
  ## R versucht zum Zeitpunkt des Aufrufs von efg <- ... bcde1 zu initialisieren, 
  ## findet das aber nicht.
  ## Was aber, wenn bcde1 im Funktionsbody gar nicht gebraucht würde?
  
  ## Nehmen wir folgende Funktion, in der der Input x nie ausgewertet wird 
f <- function(x) {
  10
}
  ## und geben wir recht rabiat ein stop-Befehl rein
f(stop("give me an error")) ## der stop-Befehl wird nie ausgewertet

 ## Lazy evaluation wird mit force umgangen (oder dem im Body Explizit machen 
 ## von x). Der Aufruf von force ist schön, weil klar wird, wozu man das macht
f <- function(x) {
  force(x)
  10
}
f(stop("give me an error"))

 ## [optional] Die Ausführende Umgebung ist für Default-Werte eine leicht andere, 
 ## als für Nutzer-spezifizierte. Wann wird ls ausgewertet? 
f <- function(x = ls()) {
  a <- 10
  x
}
f()
f(ls())

 ## Lazy evaluation ist für logischen Operationen interessant, die 
 ## lazyness ist n?tzlich bei logischen Operatoren
x <- NULL
if(x > 0){}else{message("else")}
if(!is.null(x) && x > 0){} ## nachdem das erste schon FALSE wirft, wird das 
                           ## zweite gar nichtmehr ausgewertet (Beachte: 
                           ## FALSE && ... ergibt immer FALSE)
if(!is.null(x)) if(x > 0){} ## das ist das gleiche

# ... 
  ## Das Dot-Dot-Dot-Argument wird verwendet, um eine vorab nicht festgelegte
  ## Liste von Argumente an Funktionen weiterzuwerfen, die innerhalb des 
  ## Bodys aufgerufen werden. 
  ## Darin werden alle Objekte reingesteckt, die durch die benannten Argumente 
  ## nicht gematcht wurden. 
  ## Das wird oft in S3-methoden verwendet
?base::plot
  ## Das ist flexible aber nicht transparent
?plot.default 

 ## Die Argumente in ... kann im Funktionsbody am einfachsten mit list(...) 
 ## abgegriffen werden.
f <- function(...) {
  names(list(...))
}
f("a" = 1, "b" = 2)
 
  ## Achtung: falsch geschriebene Argumente fallen auch unter ...
sum(1, 2, 3, NA, na.mr = TRUE) 
sum(1, 2, 3, NA, na.rm = TRUE)
  
  ## _> Infix- und Ersetzungs-Funktionen [optional] ----

# Infix
  ## muss ein % haben
'%+%' <- function(a, b) paste0(a, b)
"new " %+% "string"

# Ersetzung
'second<-' <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:5
  ## das ist keine .primitive-Funktion, daher mit Kopier-semantic (d.h. das 
  ## Objekt wird im Funktionsbody kopiert; erhöht den Speicherbedarf)
second(x) <- 4 
x
`[<-`

  ## weitere Argumente gehen zwischen objekt und Ersetzungs-Wert
'modify<-' <- function(x, index, value) {
  x[index] <- value
  x
}
modify(x, 1) <- 10
x
  
  ## _> return, on.exit, closures ----

# return
  ## der letzte ausgewertete Ausdruck im Body wird zurückgegeben
f <- function(x) {
  if(x < 10) 0 else 10
}
f(5)
f(12)

  ## die invisible-Funktionalität sorgt dafür, dass das zurückgegebene Objekt 
  ## nicht in der Konsole geprintet wird (es sei denn der Print-Aufruf wird 
  ## explizit gemacht)
f <- function(x) {
  if(x < 10) invisible(0) else invisible(10)
}
f(5)
a <- f(5)
a
(f(5)) ## identisch zu print(f(5))

  ## Funktionen geben nur ein einziges Objekt zurück. Wenn man mehrere 
  ## zurückgegeben will, macht man eine Liste daraus.
  ## Man sollte return() v.a. für frühe returns explizit machen

# on.exit
 ## erzeuge einen Auslöser-Aufruf, der durchgeführt wird, wenn die 
 ## funktion stoppt (unabhängig vom Ergebnis)
 ## das ist hilfreich um Nebeneffekte (wie par, setwd, ...) zu reduzieren
par("pch")
my_plot <- function() { 
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(pch = 19)
  plot(cars)
}
my_plot()
par("pch")
 ## hier besser ... verwenden
 ## verwende add = TRUE-argument wenn on.exit mehrfach verwendet wird

# closures
 ## Funktionen können Rückgabeobjekte von Funktionen sein. Wir haben schon 
 ## Beispiele gesehen.

 ## Speichere Objekte/Einheiten in eigener Umgebung. Nützlich bspw. für counter 
 ## (funktioniert wegen fresh start)
f <- function() {
  x <- 0
  function() {
    x <<- x + 1 ## x in einschließender Umgebung wird um 1 erhöht.
    x
  }
}
i <- f()
j <- f()
i()
j()
i()

  ## Nützlich beim Erstellen vieler ähnlicher Funktionen
power <- function(exponent) {
  function(x) {
    x^exponent
  }
}
zero <- power(0)
ident <- power(1)
square <- power(2)
zero(5)
ident(5)
square(5)

# anonyme Aufrufe
  ## Funktionen können anonym (d.h. ohne Objektzuweisung) aufgerufen werden, 
  ## und sind dabei das Rückgabeobjekt von (
(function(x) x + 3)(10) 

   ## In der Verwendung v.a. bei apply
x <- 1:10
l <- list("mean" = mean, "median" = median, "sd" = sd)
lapply(l, function(f, ...) f(x), na.rm = TRUE)

# Funktionale Programmierung und Metaprogramming
  ## Das programmieren von und mit Funktionen und R-Code ist ein breites Gebiet, 
  ## das hier den Rahmen sprengt
  ## Wickham gibt den Möglichkeiten in seinem Buch mehr Gewicht, als ich das 
  ## hier tue; auch, weil das Grundlage seines tidyverse ist. 
  ## https://adv-r.hadley.nz/fp.html und 
  ## https://adv-r.hadley.nz/metaprogramming.html

  ## Gemeint sind Funktionen wie Negate, Reduce, Filter, Map, ...
Reduce
?Reduce
  ## bzw. eval, expr, parse, quote
?do.call
?eval

# ___________________________________ ----
# Objektorientierung (v.a. S3) ----

  ## Ich möchte hier einen kurzen Überblick geben, wie Objektorientierung i.S.v. 
  ## S3 in R funktioniert und mit welchen Hilfsmitteln man sich da zurechtfindet. 
  ## Objektorientierung heißt hier, dass Objekte eine Klasse haben, für die 
  ## spezielle Methoden existieren (wie print, plot oder summary), die ggf. 
  ## von allgemeineren Klassen geerbt werden.

  ## Ich konzentriere mich auf S3 und verliere ein paar Worte zu S4
  ## Zwei andere Typen: Reference classes und R6, die näher an der 
  ## Objektorientierung von Java und C++ sind, finden in R weniger breit 
  ## verwendung

  ## Im Zentrum von OO stehen Objektklassen, Klassenmethoden und vererbung

  ## C++/Java: Methoden sind Teil der Klassenspezifikation: 
  ##    klassenobjekt.methode(argument)
  ## R S3/S4: Methoden gehören zu Funktionen: so-genannte generische Funktionen 
  ##  entscheiden welche Methode verwendet wird: method(klassenobjekt, argument) 
  ##  Vererbung und Klassen-Strukturen müssen für S3 nicht formal festgelegt werden 
  ##  (anders als in S4)
  
# base-typen
f <- function() {}
typeof(f)
is.function(f)
typeof(sum)
  ## andere: is.logical, is.numeric, is.character
is.primitive(sum) 
typeof("a")

  ## Die meisten Objekte sind S3-Objekte. 
  ## wenige folgen komplexeren OO-Klassen.
  ## base-typen werden nur ganz selten vom R-Core hinzugefügt. 
  ## Alles andere sind S3-Objekte, auch wenn keine explizite Vererbung oder 
  ## Methoden definiert werden
df <- data.frame("x" = 1:10, y = letters[1:10], stringsAsFactors = TRUE)
class(df)
is.object(df)
is.object("a") # base-Typen sind keine Objekte
is.object(df$x) # base-Typen sind keine Objekte (vektor)
is.object(df["x"]) # base-Typen sind keine Objekte (Liste)
is.object(df$y) # Faktoren sind keine base-Typen
isS4(df) # data.frame ist kein S4-Objekt

# generische Funktionen
  ## Um herauszufinden, ob eine Funktion eine generische Funktion ist, sehen wir 
  ## uns den quell-code an und finden bei generischen Funktionen dort den 
  ## Aufruf zu UseMethod()
mean
sum
?"internal generic"
`[`
`[.data.frame`

  ## Es ist die Aufgabe der generischen Funktion die richtige Methode für 
  ## die Funktion zu finden, die für die jeweilige Klasse angewendet wird.
  ## S3-methoden folgen dem Schema: generic-function.class -- mean.Date, 
  ## print.factor, [.data.frame.

  ## -> Aus dem Grund empfiehlt es sich vielleicht von der .-Notation für 
  ##    Funktionen, Objekte und Klassen abzuweichen
  ## --> ist t.test() die t-generic für ein test-Objekt?
  ## --> ist print.data.frame die print.data-methode für ein frame-Objekt?

  ## Welche methoden es für eine generic gibt, findet man mit methods()
  ## Ebenso, welche generic für eine klasse implementiert sind
methods("plot")
methods(class = "data.frame")

# Klasse festlegen
x <- structure(list(), class = "my_class")
  ## oder
x <- list()
class(x) <- "my_class"

  ## Klassen werden häufig Objekten zugewiesen, die darüber hinaus noch weitere 
  ## komplexe Strukturen haben
  ## z.B. mehrelementige Listen mit Attributen -- Ergebnisse aus Modellanpassungen
class(x)
inherits(x, "my_class")

 ## Es können mehrere Klassen vorgegeben werden, wodurch eine Vererbungsstruktur 
 ## definiert wird.
 ## glm-Objekte haben (u.a.) die Klassen "glm" und "lm"
glm ## lower-case-Klassenname ist üblich und '.' sollte vermieden werden
 
# Konstruktor
  ## Häufig existiert eine Konstruktor-Funktion (z.B. aus Modellanpassung)
my_class <- function(x) {
  if(!is.numeric(x)) stop("x must be numeric")
  structure(list(x), class = "my_class")
} 
  ## oder siehe glm

  ## Darüber hinaus gibt es keinen Check, ob die Klassenzuweisung sinnvoll ist
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
class(df) <- "lm"
print(df) ## kann nicht ausgegeben werden, da die print-Methode der lm-Klasse 
          ## hier keinen Sinn ergibt, aber die Informationen sind noch alle da
str(df)   


  ## Eigene Generische Funktionen werden mit UseMethod() aufgesetzt
my_generic <- function(x) UseMethod("my_generic")

x <- my_class(1:5) ## Konstruktor von oben
my_generic
my_generic.my_class <- function(x) "meine Klasse" ## Implementation der Generic
                                                  ## für my_class.
my_generic(x) ## die Implementierung wird gefunden.

  ## UseMethod sucht nach Funktionen der Art 
  ## paste0(generic, ".", c(class(x), "default"))

  ## hier ist class(df) "lm" -- haben wir oben so gesetzt
  ## für "lm" gibt's die generic my_generic noch nicht. 
my_generic(df) 

  ## dafür können wir eine default-generic erstellen
print.default
plot.default
my_generic.default <- function(x) "eine andere Klasse" 
class(df)
my_generic(df)

  ## oder speziell für data.frame
my_generic.data.frame <- function(x) "Data Frame" 
class(df) <- "data.frame" ## setzen wir die Klasse wieder zurück
my_generic(df)

class(df) <- c("lm", "data.frame") ## und wenn wir 2 Klassen zuweisen? 
                                   ## die erste wird als Unterklasse (oder 
                                   ## Instanz) der allgemeineren zweiten Klasse
                                   ## aufgefasst.
print(df) ## verwendet print.lm                           
my_generic(df) ## my_generic.lm gibt's nicht. Gäb's my_generic.data.frame auch 
               ## nicht, würde my_generic.default verwendet werden.
print.data.frame(df) ## kann auch explizit angesprochen werden.
stats:::print.lm(df)

## [optional]
f <- function() 1
g <- function() 2
class(g) <- "function"

class(f)
class(g)

length.function <- function(x) "function" ## man kann keine internal generics 
                                          ## überschreiben
length(f)
length(g)

print(f)
print.function <- function(x) "print function"
print(f)

# S4
  ## S4-objekte haben einen Namen, Slots (felder, Funktionieren wie listen) und 
  ## Klassen von denen sie erbt
library("lme4")
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
isS4(fm1)
is(fm1)
  ## man muss mit setClass eine solche Klasse erstellen und mit new() 
  ## ein Objekt initialisieren
class?merMod 
             
  ## slots funktionieren wie listen
slotNames(fm1) 
fm1@call 
slot(fm1, "call")

  ## mehr möchte ich auf S4 auch gar nicht eingehen und auf die anderen OO-Typen 
  ## erst recht nicht -- sprengt hier jeden Rahmen und ich habe noch nicht 
  ## viel mit denen gearbeitet. Wenn man sich dafür entscheidet ein Paket mit 
  ## S4-OO zu implementieren, muss man sich eh neu eindenken. Hier nützlich v.a. 
  ## dass man mit @ auf die Slots zugreifen kann (aber nicht unbedingt sollte), 
  ## wenn man mal versuchen muss in derartige Funktionen einzutauchen. 

# ___________________________________ ----
# Debugging ----

  ## _> debugging ----
 
  ## Was sind die Schritte zum Debuggen?
  
  ## 1) Merke, dass du einen Bug hast.
  ## 2) Mach ihn reproduzierbar (stackoverflow minimum working example).
    ## - Erzeuge ihn auf Kommando.
    ## Oft startet man mit einem großen Block, der den Fehler produziert, und 
    ## reduziert den Block Schritt für Schritt (oder Hälfte für Hälfte), um zu 
    ## sehen in welchem Schritt oder welchem Schritt/Segment/welcher Hälfte der 
    ## Fehler geworfen wird.
    ## Vielleicht muss man sich Gedanken machen, wie man den Bug schneller 
    ## erzeugt.
    ## Dabei erzeugt man ein MWE (minimal working example), das den Bug immer 
    ## noch erzeugt.
    ## (https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example,
    ##  https://stackoverflow.com/help/minimal-reproducible-example)
    ## Dabei entdeckt man Eingaben, die den Bug _nicht_ erzeugen.
  ## 3) Finde heraus, wo der Bug ist.
    ## oft hilft ein gut platzierter Print-Befehl, der die Umgebung ausspuckt 
    ## oder Annahmen über die Umgebung und die dort vorhandenen Datenstrukturen
    ## zu diesem Zeitpunkt überprüft (getreu dem Wissenschaftlichen Ansatz: 
    ## versuche Hypothesen zu erstellen und die zu falsifizieren).
    ## ggf. unter Zuhilfenahme von google/stackoverflow
  ## 4) Behebe den Bug

  ## Vor allem Schritt 3) kann sehr aufwendig werden. Da ist zusätzliche 
  ## Hilfe sehr willkommen

  ## Einfaches erstes Tool: Schleifenparameter sind Variablen in der globalen 
  ## Umgebung
ii <- 0
ii
for (ii in 1:100) if (ii == 35) message("text")
ii
  ## Das heißt auch, dass der Schleifen-zyklus, in dem ein Fehler auftritt 
  ## post hoc bekannt ist.
for (ii in 1:100) if (ii == 35) stop()
ii

# error inspector / traceback() 
  ## Der Error-Inspektor listet die Reihenfolge der Befehle auf, die zu dem 
  ## Fehler führen.
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)
traceback()

  ## Rstudio Menü "Debug" -> On Error -> Error Inspector -> Show traceback
f(10)
 
  ##
## library(here)
source(here::here("03a_error-source-01.R"))
f(10) ## Zeigt auch, in welchem Script das passiert.
 
# rerun with debug / options(error = browser)
  ## Öffnet interkative Session, in der der Fehler auftritt. 
  ## Rstudio menu Debug -> On Error -> Break in Code
  ##         oder traceback -> Rerun with Debug
  ## [n]ext step in the function, 
  ## [s]tep into function to work line by line, 
  ## [f]inish current loop
  ## [Q]uit
f(10) ## hier v.a. traceback, source und environment zeigen.
      ## n/s/f/Q führen alle zum Abbruch, weil man sofort beim Fehler ankommt.

# [optional] ohne RStudio
options("error")
options("error" = browser) 
f(10)
options("error" = NULL) # reset in console

# breakpoints / browser() 
  ## [do:] erzeuge breakpoint in Script "03a_error-source-01.R 
  ##   (LINKS VON ZEILENNUMMER)
  ## source neu source(here("03a_error-source-01.R"))

  ## öffnet inteaktive Session am breakpoint
  ## Achtung, wo der Breakpoint zu setzen ist: _vor_ die Stelle an der man 
  ## ankommen möchte
source(here("03b_error-source-02.R"))
f(10) ## hier n/s

# debug
  ## setze breakpoint an den Beginn einer Funktion
debug(f)
f(10)
undebug(f)

  ## _> Condition handling ----

# conditions
  ## Neben Fehlern werden von Funktionen oft noch warnings oder messages 
  ## ausgeworfen.
f <- function(x) if(!is.numeric(x)) stop("x muss numerisch sein")
g <- function(x) if(!is.numeric(x)) warning("x sollte numerisch sein; ich mach das")
h <- function(x) if(!is.numeric(x)) message("x ist ", typeof(x))
i <- function(x) if(!is.numeric(x)) cat("x ist", typeof(x)) # ggf. print

f("a") 
g("a")
h("a")
i("a")

  ## Wenn man die warnings im Debug-Modus untersuchen will, kann man Warnungen 
  ## als Fehler interpretieren
options("warn" = 2) ## default 0
g("a")
options("warn" = 0)

# Handling
  ## Fehler werden mit dem try({Statement})-Befehl behandelt.
try(f("a"))

  ## wenn man bspw. Modelle für eine größere Liste an Datensätzen anpasst und 
  ## vorher schon weiß, dass die Modell-Schätzung für ein paar davon Fehler 
  ## werfen können, möchte man die Schleife dennoch durchlaufen und nicht die 
  ## Anpassung für die anderen Fälle verwerfen.
  
try(f("a"), silent = TRUE)

try({
  a <- 1
  b <- "a"
  d <- a + b
})
a ## wird ausgeführt
b ## wird ausgeführt
d 

  ## Fehler können auch zur Laufzeit behandelt werden
res <- try(f("a"), silent = TRUE)
class(res) ## im Fall eines Fehlers wird ein Objekt der Klasse "try-error" 
           ## erzeugt
class(try(1 + 2))
if(inherits(res, "try-error")) message("do something else")

  ## Es lohnt sich ggf. default-werte für Objekte zu setzen, die Resultate 
  ## von Fehlern werden können. Dann wird mit den Default-Werten weitergerechnet.
res <- NULL
try(res <- f("a"), silent = TRUE)
res

  ## tryCatch(expr, message = expr, warning = expr, error = expr, finally = expr)
  ## ist ein weiterer Mechanismus, der noch mehr Fokus auf die Behandlung von 
  ## Fehlern legt, indem explizit verlangt wird, welches Vorgehen im Falle von 
  ## Ereignissen durchgeführt werden soll.

# Unterdrücken von Ausgabe
try(f("a"), silent = TRUE)
suppressWarnings(g("a"))
suppressMessages(h("a"))
suppressMessages(i("a")) ## Hier muss man auf ein verbose-Argument des 
                         ## Entwicklers hoffen
quiet <- function(x) {   ## eine rabiate Methode ist, all diese Ausgaben in 
    sink(tempfile())     ## ein temfile zu schreiben
    on.exit(sink())
    invisible(force(x))
  }
quiet(i("a"))

  ## _> defensiv programming ----

  ## Sei genau mit den Datentypen/-Strukturen, die du als Input an eine Funktion 
  ## erlaubst. 
  ## Vermeide Funktionen, die abhängig vom Eingabetyp unterschiedliche 
  ## Ausgabetypen erzeugen 
  ## bspw: [ und sapply; Verwende [, drop = FALSE] und 
  ##       sapply(, simplify = FALSE)/vapply()




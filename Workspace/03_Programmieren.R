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

##
f <- function(x)  x^2 
formals(f)
body(f)
environment(f)

##
sum
formals(sum)

`[`

# _______________________________________ ----
# Suchpfad ----

## _> lexical scoping ----

##
x <- 10
x

##
search() ## in der Reihenfolge

##
rm(x)

## -> Name masking / Namenüberlagerung
## -> Functions vs. variables / Funktionen gegen Variablen
## -> A fresh start / Neustart
## -> Dynamic lookup / Dynamisches Nachschlagen

# Name masking
f <- function() {
  x <- 1
  y <- 1
  c(x, y)
}

f()
rm(f)

x <- 2
f <- function() {
  
  y <- 1
  c(x, y)
}

f()

##
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

##
z <- 4
f <- function(x) {
  y <- 3
  function() {
    c(x, y, z)
  }
}

g <- f(1) 
g()

##
ls(envir = environment(g)) 

rm(f, g)

# Functions vs. variables

f <- function(x) x + 1
g <- function() {
  f <- function(x) x * 2
  f(10)
}
f(10) 
g()   

rm(f, g)

##
f <- function(x) x + 1
g <- function() {
  f <- 3
  f(f) 
}

g() 
rm(f, g)

# Neustart

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

f <- function() x + 1

x <- 3 
f() 

x <- 10 
f() 
codetools::findGlobals(f) 

rm(f, x)

## _> environment ----

# Grundlegendes
e <- new.env()

e$a <- 1
e$b <- "a"
e$c <- FALSE
e$d <- 1:3

# Referenzsemantik
f <- function(x) {
  x$d <- 4:6
  return(5)
}
e$d
f(e)
e$d

##
l <- list(a = 1, b = "a", c = FALSE, d = 1:3, a = 2)
g <- function(x) {
  x$d <- 4:6
  return(x)
}
l <- g(l)

# Eigenschaften
ls(name = e) 

e[[1]]       
e$a
ls.str(e)

# Parent-Umgebungen
parent.env(e) 
identical(parent.env(e), globalenv())

search() 
parent.env(globalenv())

##
parent.frame() 

##
identical(globalenv(), environment())
identical(baseenv(), as.environment("package:base"))
identical(emptyenv(), parent.env(as.environment("package:base"))) 

##
f <- function() x + 1

x <- 3 
f()
codetools::findGlobals(f)

environment(f) <- emptyenv()
f()

# Zugriffsoperatoren
get("d", e)
x <- 1
get("x", e)

# Entfernen
rm("a", envir = e)
ls.str(e)

# exists 
exists("x", envir = e)
exists("x", envir = e, inherits = FALSE)

# Zusammenfassung
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
try(where("a", e)) # wurde weiter oben gelöscht
where("+")

## _> Funktions-Umgebungen / closures [optional] ---- 

# Einschließende Umgebungen
environment(where)
ls(environment(where))
rm("x")
ls(environment(where))

# Die bindende Umgebungen 
e$f <- function() 1
environment(e$f)
where("f", e)

# Einschließende vs. Bindende Umgebungen 
environment(sd) 
where("var")
##
sd(1:10) 
##
var <- function(x, na.rm = TRUE) 100 
##
sd(1:10)

##
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
f() 
f(4)

plus <- function(x) {
  function(y) x + y
}
f <- plus(1)
new_e <- environment(f)
ls(new_e)
new_e$x

# Aufrufende Umgebung
f <- function() {
  x <- 10
  function() {
    x
  }
}

i <- f()
x <- 20
i()

##
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

f <- function(abcdef, bcde1, bcde2) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
} 

##
f(1, 2, 3)

##
f(1, 2, "abcdef" = 3)

##
f(1, 2, "a" = 3)

##
f(1, 2, "b" = 3)
formals(f)

##
do.call(f, list(1, 2, 3))

##
f <- function(abcdef, bcde1, bcde2 = 3) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

# Lazy Evaluation 

?lm ## -> Print

##
f <- function(abcdef, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

##
f <- function(abcdef = bcde1 / 4, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(bcde1 = 2)

##
f <- function(abcdef, bcde1, bcde2 = efg) {
  efg <- abcdef + bcde1^2
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(1, 2)

##
f(1)

##
f <- function(x) {
  10
}
##
f(stop("give me an error"))

##
f <- function(x) {
  force(x)
  10
}
f(stop("give me an error"))

## [optional]
f <- function(x = ls()) {
  a <- 10
  x
}
f()
f(ls())

##
x <- NULL
if(x > 0){}else{message("else")}
if(!is.null(x) && x > 0){}
if(!is.null(x)) if(x > 0){}

# ... 
?base::plot
##
?plot.default 

##
f <- function(...) {
  names(list(...))
}
f("a" = 1, "b" = 2)

##
sum(1, 2, 3, NA, na.mr = TRUE) 
sum(1, 2, 3, NA, na.rm = TRUE)

## _> Infix- und Ersetzungs-Funktionen [optional] ----

# Infix
##
'%+%' <- function(a, b) paste0(a, b)
"new " %+% "string"

# Ersetzung
'second<-' <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:5
##
second(x) <- 4 
x
`[<-`

'third' <- function(x, value) {
  x[3] <- value
  x
}
x <- third(x, 3)


##
'modify<-' <- function(x, index, value) {
  x[index] <- value
  x
}
modify(x, 1) <- 10
x

## _> return, on.exit, closures ----

# return
f <- function(x) {
  if(x < 10) 0 else 10
}
f(5)
f(12)

##
f <- function(x) {
  if(x < 10) invisible(0) else invisible(10)
}
f(5)
a <- f(5)
a
(f(5)) ## identisch zu print(f(5))

# on.exit
par("pch")
my_plot <- function() { 
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(pch = 19)
  plot(cars)
}
my_plot()
par("pch")

# closures
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

##
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
(function(x) x + 3)(10) 

##
x <- 1:10
l <- list("mean" = mean, "median" = median, "sd" = sd)
lapply(l, function(f, ...) f(x), na.rm = TRUE)

# Funktionale Programmierung und Metaprogrammierung
## https://adv-r.hadley.nz/fp.html und 
## https://adv-r.hadley.nz/metaprogramming.html
## funktional
Reduce
?Reduce
## Metaprogrammierung
?do.call 
?eval

# ___________________________________ ----
# Objektorientierung (v.a. S3) ----

# base-typen
f <- function() {}
typeof(f)
is.function(f)
typeof(sum)
## is.logical, is.numeric, is.character
is.primitive(sum) 
typeof("a")

##
df <- data.frame("x" = 1:10, y = letters[1:10], stringsAsFactors = TRUE)
class(df)
is.object(df)
is.object("a") # base-Typen sind keine Objekte
is.object(df["x"]) # base-Typen sind keine Objekte
is.object(df$y) # Faktoren sind keine base-Typen
isS4(df) # data.frame ist kein S4-Objekt

# generische Funktionen
mean
sum
?"internal generic"
`[`
`[.data.frame`

##
methods("plot")
methods(class = "data.frame")

# Klasse festlegen
x <- structure(list(), class = "my_class")
x <- list()
class(x) <- "my_class"

##
class(x)
inherits(x, "my_class")

##
glm ## lower-case-Klassenname ist üblich und '.' sollte vermieden werden

# Konstruktor
my_class <- function(x) {
  if(!is.numeric(x)) stop("x must be numeric")
  structure(list(x), class = "my_class")
} 
## oder siehe glm

##
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
class(df) <- "lm"
print(df)
str(df)   


##
my_generic <- function(x) UseMethod("my_generic")

x <- my_class(1:5)
my_generic(x)
my_generic.my_class <- function(x) "meine Klasse"
my_generic(x)

##
my_generic(df) 

##
print.default
plot.default
my_generic.default <- function(x) "eine andere Klasse" 
my_generic(df)

##
my_generic.data.frame <- function(x) "Data Frame" 
class(df) <- "data.frame" ## setzen wir die Klasse wieder zurück
my_generic(df)

## [optional]
class(df) <- c("lm", "data.frame") ## und wenn wir 2 Klassen zuweisen? 
print(df)
my_generic(df)
print.data.frame(df)
stats:::print.lm(df)

f <- function() 1
g <- function() 2
class(g) <- "function"

class(f)
class(g)

length.function <- function(x) "function" 
length(f)
length(g)

print(f)
print.function <- function(x) "print function"
print(f)

# S4
library("lme4")
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
isS4(fm1)
is(fm1)
##
class?merMod 
##             
slotNames(fm1) 
fm1@call 
slot(fm1, "call")

# ___________________________________ ----
# Debugging ----

## https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-RStudio

## _> debugging ----

## Was sind die Schritte zum Debuggen?

## 1) Merke, dass du einen Bug hast.
## 2) Mach ihn reproduzierbar (stackoverflow minimum working example).
## (https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example,
##  https://stackoverflow.com/help/minimal-reproducible-example)
## 3) Finde heraus, wo der Bug ist.
## 4) Behebe den Bug

##
ii <- 0
ii
for (ii in 1:100) if (ii == 35) message("text")
ii

##
for (ii in 1:100) if (ii == 35) stop()
ii

# error inspector / traceback() 
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)
traceback()

## Rstudio Menü "Debug" -> On Error -> Error Inspector -> Show traceback
f(10)

##
source(here::here("03a_error-source-01.R"))
f(10) 

# rerun with debug / options(error = browser)
## Öffnet interkative Session, in der der Fehler auftritt. 
## Rstudio menu Debug -> On Error -> Break in Code
##         oder traceback -> Rerun with Debug
## [n]ext step in the function, 
## [s]tep into function to work line by line, 
## [f]inish current loop
## [Q]uit
f(10)

# [optional] ohne RStudio
options("error")
options("error" = browser) 
f(10)
options("error" = NULL) # reset in console

# breakpoints / browser() 
## [do:] erzeuge breakpoint in Script "03a_error-source-01.R
## source neu source(here("03a_error-source-01.R"))
source(here::here("03b_error-source-02.R"))
f(10) 

# debug
debug(f)
f(10)
undebug(f)

## _> Condition handling ----

# conditions
f <- function(x) if(!is.numeric(x)) stop("x muss numerisch sein")
g <- function(x) if(!is.numeric(x)) warning("x sollte numerisch sein; ich mach das")
h <- function(x) if(!is.numeric(x)) message("x ist ", typeof(x))
i <- function(x) if(!is.numeric(x)) cat("x ist", typeof(x)) # ggf. print

f("a") 
g("a")
h("a")
i("a")

##
options("warn" = 2) ## default 0
g("a")
options("warn" = 0)

# Handling
try(f("a"))

##  
try(f("a"), silent = TRUE)

try({
  a <- 1
  b <- "a"
  d <- a + b
})
a 
b 
d 

## 
res <- try(f("a"), silent = TRUE)
class(res) 
class(try(1 + 2))
if(inherits(res, "try-error")) message("do something else")

##
res <- NULL
try(res <- f("a"), silent = TRUE)
res

# Unterdrücken von Ausgabe
try(f("a"), silent = TRUE)
suppressWarnings(g("a"))
suppressMessages(h("a"))
suppressMessages(i("a")) ## Hier muss man auf ein verbose-Argument des 
## Entwicklers hoffen
quiet <- function(x) {   ## eine rabiate Methode ist, all diese Ausgaben in 
  sink(tempfile())     ## ein tempfile zu schreiben
  on.exit(sink())
  invisible(force(x))
}
quiet(i("a"))

## _> defensiv programming ----

## sapply(, simplify = FALSE)/vapply()
## [, drop = FALSE]


##
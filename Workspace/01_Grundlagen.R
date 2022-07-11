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
# Script 01: Grundlagen
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

# ____________________________________ ---- 
# Datenformate ----


  ## Welche atomaren Datentypen kennen wir? (Folien hier leer!) Welche "Werte" 
  ## kann ich in die Konsole schreiben, ohne, dass R den Fehler 
  ## "Objekt nicht gefunden" zurückwirft
TRUE
T # don't!!
2
3.2
str(3.2); is.double(3.2)
a
"a"
0i # der Vollständigkeit
as.raw(40) # raw bytes, hexadecimal - 0x28 ## 2*16+8; ?charToRaw; der Vollständigkeit

  ## Was ist mit Datum/Uhrzeit? -> Recodierungen von Integer/Double (wie in Excel)
x <- as.Date("07-07-2022", "%d-%m-%Y")
str(x)
class(x)
unclass(x) 


  ## c steht für combine (?c)
dbl <- c(1, 2.5, 4.5) 
int <- c(1L, 6L, 10L)
log <- c(TRUE, FALSE, T, F)
chr <- c("mein", "string", "vector")
 
  ## Vektoren sind immer Flach
c(1, c(2, 3)) 

  ## Typ-Abfrage
typeof(int)
is.integer(int)
is.numeric(int)
is.atomic(int)
is.double(int)

  ## NA
str(NA)
str(c(NA, 3.4))
str(c(NA_real_, 3.4)) # NA, NA_integer_, NA_real_, NA_character_; achtung, nicht NA_double_
str(NA_character_)

  ## Verbindungslogic (en: Coercion): logical -> integer -> double -> character
c(TRUE, "a")
as.numeric(c(TRUE, TRUE, FALSE))
sum(c(TRUE, TRUE, FALSE))
as.logical(c(0, 1, 2.4))
as.logical(c(0, 1, 2.4, -1))
as.logical(c("a", "1", "b"))
as.logical(c("0", "1", "2.4"))
str(c(NA_character_, 3.4))

  ## welche Datenstrukturen kennen wir?

# ____________________________________ ----
# Klassenmethoden ----

  ## _> Listen ----

# Erstellen
x <- list(dbl, int, log, chr)
str(x)
typeof(x)
is.list(x)

  ## Listen können geschachtelt sein
y <- list(list(list(1))) 
str(y)
is.recursive(y)
length(y)

# Kombinieren von Listen
  ## Wenn eine Kombination von Listen und Vektoren an c() übergeben wirden, wird 
  ## der Vektor zu einer Liste gewandelt -> == c(list(1, 2), as.list(c(3, 4)))
  ## implizite Konvertierungen sollten wenn möglich vermieden werden.
  ##
  ## Bei list, wird der Vektor als eigenes Listenelement betrachtet
list(list(1, 2), c(3, 4))
c(list(1, 2), c(3, 4))
c(list(1, 2), list(c(3, 4))) ## besser; keine konvertierung

list(1, 2, c(3, 4))

  ## _> Attribute ----

x <- 1:10 # Man kann Vektoren Attribute zuweisen

attr(x, "my_attr") <- "mein Attributtext" # mittels attr kann man eigene 
                                          # Attribute definieren oder andere 
                                          # ersetzen
attr(x, "my_attr2") <- "mein Attributtext2"

attributes(x) # attributes gibt die Attribute aus
 
attributes(x)[["my_attr"]]
attr(x, "my_attr") # attr gibt ein spezielles Attribut aus

x <- structure(1:10, "my_attr" = "mein Attributtext") # Attribute können auch 
                                                      # mittels structure 
                                                      # erzeugt werden

attributes(x[1]) # Subsetting oder Funktionsaufruf entfernen Attribute

# Element-Namen
  ## sind auch attribute 
  ## sind aufgrund der Referenz-By-Name-Option zentral 
  ## können auf verschiedene Arten gesetzt werden
x <- c(a = 1, "b" = 2)

x <- 1:3
names(x) <- c("a") # wenn nur 1 Name angegeben wird, wird nur für das erste 
                   # Element der Name gesetzt
x

names(x) <- c("a", "b", "c")
x

setNames(x, nm = c("d", "e", "f"))

unname(x) # oder wieder gelöscht werden
names(x) <- NULL

  ## das würde auch über attributes gehen
  ## man sollte aber auf die implementierten Setter-Methoden zugreifen
  ## das gilt auch für die anderen vordefinierten Attribute (wie dim und class)

# Faktoren
  ## sind eine wichtige Anwendung von Attributen
x <- factor(c("a", "b", "c", "a"))
x
class(x)
levels(x) # weiteres attribut: levels

x[2] <- "d" # werte können nicht hinzugefügt werden, wenn dafür das level nicht 
            # definiert ist
x

  ## was passiert, wenn man via c() zwei Faktoren kombinieren möchte?
c(x, factor("d")) # vor 4.0 ging das levels-Attribut verloren

  ## ein interessantes feature der table-Funktion 
  ## mit faktoren und levels hat die Tabelle eine Spalte für alle levels, auch  
  ## wenn der vector keine Einträge hat
table(factor(c("f", "f", "f"), levels = c("m", "f", "d")))

# stringsAsFactors
  ## https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/
  ## für alle, die schon länger mit R arbeiten, ist das eine Riesen Sache gewesen
  ## Dieser Änderung gingen jahrzente lange Verhandlungen innerhalb des 
  ## R-Entwickler-Cores voraus. 
  ## Mit dieser Änderung werden viele ältere Scripte nicht mehr mit den neuen 
  ## R-versionen laufen -- Abwärtskompatibilität war und ist für R-Core  
  ## eigentlich ein hohes Gut.
  ##

  ## Unschön war vor 4.0 die standardmäßige Konvertierung von Text in Faktoren. 
  ## Wenn man bspw. ein "." als NA-Zeichen im Dokument hat, wird die ganze 
  ## Variable als Character verstanden und als Faktor eingelese
z <- read.csv(text = "value\n12\n1\n.\n9",
              stringsAsFactors = TRUE) # das war damals der default
                                       # ggf. in anderer R-Console zeigen
z # in der Print-Ausgabe merkte man das gar nicht
str(z)
as.numeric(z$value) # Bei Konvertierung erhielt man nicht die Werte, die man 
                    # haben wollte
as.numeric(as.character(z$value)) # man musste die zuerst in Character zurück 
                                  # wandeln

z <- read.csv(text = "value\n12\n1\n.\n9", 
              stringsAsFactors = FALSE) # alternativ konnte man das Argument 
                                        # stringsAsFactors setzen
str(z)

  ## oder, wenn man weiß, dass der Auslöser ein "." als NA-Repräsentant ist,
  ## setzt man das Argument na.strings
  ## Allgemein ist es nützlich so viele wie mögliche Vorab-Informationen an die 
  ## Funktionen zu geben
z <- read.csv(text = "value\n12\n1\n.\n9", na.strings = ".")
str(z)

  ## _> matrix/array ----

# Erstellen
a <- matrix(1:6, ncol = 3, nrow = 2)  # Matrizen sind Spaltendominant (werden 
                                      # spaltenweise befüllt)
                                      # Argument: byrow 
b <- array(1:12, dim = c(2, 3, 2))

c <- 1:6 # man kann auch einen Vektor definieren und das dim-Attribut setzen
         # das tun wir in Zukunft nicht mehr, ein objekt "c" nennen.
cc <- 1:6
dim(cc) <- c(3, 2)

d <- structure(1:6, dim = c(3, 2)) # oder das attribut im structure-Aufrufs
d <- structure(1:6, ncol = 3, nrow = 2) # so nicht

  ## was ist mit Missmatch von Objekt-Länge und Dim-Attribut?
dim(cc) <- c(3, 3) # bei Attribut-Setzung _müssen_ beide passen

matrix(1:6, ncol = 3, nrow = 3) # beim Matrix-Befehl nicht unbedingt
                                # empfiehlt sich aber ohnehin -- seit 4.0 gibt 
                                # es hier eine Warning, die gab es vorher nicht

# Attribute
a

length(a) # Länge des zugrundliegenden Vektors

ncol(a) # weitere
ncol(a) <- 4
nrow(a)
`dim<-`

  ## names werden verallgemeinert zu row-/col- und dimnames
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")

dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))
b

  ## names können aber dem zugrundeliegenden Vektor zugeordnet werden
names(b)
names(b) <- c("A", "b")
c(b)
b
 
  ## Klasseneigenschaften
class(a)
class(b)
is.array(a)
is.matrix(b)

attributes(a)

  ## Es gibt eine wichtige Unterscheidung zwischen eindimensionalen Vektoren 
  ## und eindimensionalen Matrizen, was vor allem im dimensions-Attribut  
  ## gekennzeichnet ist
  ## Das ist dann lästig, wenn R bspw. eindimensionale Spalten-Matrizen in einen 
  ## Vektor konvertiert (sehen wir gleich noch)
str(1:3)
str(matrix(1:3, ncol = 1))
str(matrix(1:3, nrow = 1))
str(array(1:3, dim = 3))

  ## Auch Listen können mehrdimensional sein; 
  ## Das macht vor allem dann Sinn, wenn man unterschiedliche Datentypen in einer 
  ## mehrdimensionalen Systematik auffassen will (bspw. unterschiedliche 
  ## Eigenschaften eines Objekts zu verschiedenen Zeitpunkten oder Längen- und 
  ## Breiten-Graden)
l <- structure(list(1:3, "a", TRUE, 1.0), dim = c(2, 2))
l 
l[1, 2]
l[[1]]

  ## _> data.frame ----

  ## ... ist eine zweite 2-dimensionale Datenstruktur. Sie beruht auf Listen, 
  ## wobei die Listen die gleiche Länge haben müssen. Das heißt das data.frames 
  ## haben die gleichen Eigenschaften, wie Listen
  ## data.frame ist eine Liste von Vektoren gleicher Länge

# Erstellung 
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)
is.list(df)
  ## ggf. ist das Argument stringsAsFactors interessant
df <- data.frame(x = 1:3, y = c("a", "b", "c"),
                 stringsAsFactors = TRUE) # das war vor 4.0 default
str(df)

# Attribute von data.frame
attributes(df)
  ## die Zugriffs-Methoden greifen auch hier
names(df) 
class(df)
row.names(df) # das ist der eigentliche Zugriffsname für data.frame

rownames(df) # es gibt auch die Funktion rownames, die eigentlich für Matrizen  
             # vorgesehen ist. Aber warum klappt die auch für data.frame? 
 
rownames # Das liegt daran, dass sie über die S3-Generic "dimnames" definiert 
         # ist, für welche eine data.frame-eigene Methode implementiert ist

dimnames(df)[[2]]
dimnames.data.frame # die ihrerseits wieder auf row.names-Methode zugreift
  ## zu S3-Methoden kommen wir im zweiten Teil.


# Kombinieren von data.frame

cbind(df, data.frame(z = 1:3)) # via cbind und rbind (diese Methode gibt es für 
                               # Matrizen auch)
c(df, data.frame(z = 1:3))
c(list(1:3, factor(c("a", "b", "c"))), list(1:3))

str(cbind(df, z = 1:3)) # vektoren werden dabei in data.frames konvertiert

  ## Bei rbind ist die Reihenfolge der Elemente in den data.frames nicht wichtig.
  ## Der Output wird an die Reihenfolge des ersten data.frames angepasst.
  ## wichtig ist, dass die Elemente gleich heißen und in beiden vorhanden sind.
rbind(df, data.frame(y = "z", x = 10))
  ## plyr stellt die bequeme Funktion rbind.fill bereit, die fehlende Elemente 
  ## in den jeweiligen data.frames mit NA-Spalten befüllt

  ## Achtung
str(data.frame(cbind(a = 1:2, b = c("a", "b")))) 
                                                 # Was ist das Problem? 
                                                 # Wie ist das passiert?
                                                 # mit stringsAsFactors = TRUE sieht man's nichtmal
  ## 1) cbind verwandelt vektoren in Matrizen
  ## 2) dabei Verwandlung aller Elemente in den flexibelsten Datentyp character
  ## vor 4.0 hat dann der data.frame-Aufruf auch noch alles in Faktoren verwandelt

  ## das sauberere Vorgehen ist den DF element-weise aufzubauen
x <- data.frame(a = 1:2, b = c("a", "b")) # stringsAsFactors = FALSE ist default
str(x)

# Erweitern von data.frame
df$z <- list(1:2, 1:3, 1:4) # Man kann einem data.frame eine Liste als 
                            # Listenelement übergeben. Die muss nur/v.a. die 
                            # gleiche Länge haben, wie die anderen Listen
df
  ## aber...
data.frame(x = 1:3, y = c("a", "b", "c"), z = list(1:2, 1:3, 1:4)) 
  ## das geht mit der I()-Methode; das steht für "as is" (?I)
data.frame(x = 1:3, y = c("a", "b", "c"), z = I(list(1:2, 1:3, 1:4))) 


# ____________________________________ ----
# Zugriffslogik ----

  ## welche 3 Operatoren für das Subsetting oder den Zugriff auf Objekte oder 
  ## deren Elemente gibt es/kennen wir? 
  ## Von welchem Typ können der Inhalt dieser Operatoren sein?
  
  ## _> vector ----
x <- c(2.1, 4.2, 3.3, 5.4)

x[c(3L, 1L)]   # der einfachste sind positive Integer
x[c(3, 1)]     # der einfachste sind positive Integer
x[c(2.1, 2.9)] # Double-Indizes werden abgeschnitten
x[order(x)]    # dabei kann die Reihenfolge vertauscht werden
sort(x)
sort.default   # die sort-Methode beruht auf diesem Prinzip
x[c(1, 1)]     # R kann Elemente mehrfach ausgeben; R merkt das nicht..
x[c(1, 5)]

x[-c(1, 2)]    # negative Indizes sorgen dafür, dass Einträge entfernt werden
x[c(-1L, 2L)]  # negative und positive Integer können nicht kombiniert werden

x[c(TRUE, TRUE, FALSE, FALSE)] # logische Indizes
x[x > 3]       # nützlich vor allem durch logische Abfragen
x[c(TRUE, FALSE)] # falls zu kurz, werden sie zyklisch wieder referenziert
x[c(TRUE, FALSE, TRUE)] # auch wenn ihre Länge nicht durch die Vektorlänge 
                        # teilbar ist
x[c(TRUE, FALSE, NA, TRUE)] # NA werden als NA ("weiß nicht" ersetzt)

  ## Ein leerer Zugriff gibt den vollständigen Vektor wieder; das ist v.a. dann 
  ## nützlich, wenn das Objekt incl. seiner Attribute bestehen bleiben, aber 
  ## dessen Einträge ersetzt werden sollen (s. später im Rahmen von data.frame)
  ## oder allgemein bei abfrage höher-dimensionaler Datenstrukturen (s. bei 
  ## Matrizen)
x[]
x[0] # Der 0-Zugriff gibt ein Vektor der Länge 0 zurück
str(0) # das ist ein Unterschied zum integer 0, der die Länge 1 hat.

y <- setNames(x, c("a", "b", "c", "d")) # Zugriff über den Namen ist besonders 
                                        # wertvoll
y[c("a", "b")]
y[c("a", "a")] # auch hier können werte doppelt ausgegeben werden

y <- setNames(x, c("abc", "def", "geh", "ijk"))
y[c("a", "d")]  # beim Zugreifen über Character muss der Namen exakt verwendet 
                # verweden; Partielle Verwendung von Namen gibt es bei anderen 
                # Funktionalitäten (bei Argumenten von Funktionen)

  ## _> matrix ----

  ## der Zugriff erfolgt mit zwei Vektoren, die sich aus den 
  ## zuvorgenannten Optionen beliebig zusammensetzen können

mx <- matrix(1:9, nrow = 3, dimnames = list(NULL, c("A", "B", "C")))
 
mx[1:2, ] # bspw. leerer Vektor an der zweiten Stelle für Abfrage aller Spalten
mx[c(TRUE, FALSE, TRUE), c("B", "A")]  
mx[0, -2]
 
dim(mx[1, -2]) # Achtung: eine Matrix wird per default auf die niedrigst-
               # mögliche Dimension reduziert (auch für Array)
dim(mx[1, -2, drop = FALSE]) # es sei denn das drop-Argument wird gesetzt

 ## Wenn der Zugriff nicht die Dimension berücksichtigt, also keine Komma-
 ## trennung vorsieht, greift der Zugriffsoperator auf den der Matrix zugrunde-
 ## liegenden Vektoren zu.
vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals
vals[c(4, 15)] # dabei gilt wieder die Spaltendominanz bei Matrizen

 ## Interessant ist der Zugriff über eine Matrix mit dimensionsweisen Indizees
sel_mx <- rbind(c(1, 1),
                c(3, 1),
                c(2, 4))
vals[sel_mx]
 ## vgl. Argument arr.ind der which-Funktion
which(vals == "4,1")
which(vals == "4,1", arr.ind = TRUE)
 
  ## _> data.frame & list ----

  ## Der '['-Operator funktioniert für Listen (und daher auch für data.frame) wie 
  ## für Vektoren. Dabei wird eine Liste zurückgegeben
  ## Die weiteren Operatoren [[ und $ ziehen einzelne Einträge heraus
  ## Vgl. das Zug-Analogon von @RLangTip

df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"))

df[df$x == 2, ] # wie für Matrizen - mit zwei Vektoren, wobei der zweite leer ist 
str(mx[1, -2])
str(df[df$x == 2, ])
str(df[, c("x", "z")]) 

  ## Mit einem Vektor wie für listen: ziehe Listenelemente x und z heraus
  ## anders als bei Matrizen (dort: ziehe elemente x und z aus dem zugrunde 
  ## liegenden Vektor heraus)
str(df[c("x", "z")]) # anders als für Matrizen: eindimensionaler Vektor-Zugriff
                     # greift auf Listenelemente x und z des data.frame zu
                     # bei Matrix (ziehe Elemente x und z des zugrundeliegenden 
                     # Vektors heraus)
                     # matrix = Vektor mit Dim-Struktur
                     # data.frame = Liste von Listen gleicher Länge mit Dim-Struktur

  ## Achtung wieder: 
  ## wenn der Zugriffsvektor einelementig wird (oder nach logischen Abfragen 
  ## werden kann) wird unterschieden, ob der data.frame mit [] als Liste 
  ## oder mit [,] als matrix (ie. Dim-Struktur) interpretiert wird.
  ## -> Im ersten Fall wird das Listenelement als list/data.frame zurückgegeben 
  ## -> Im zweitem Fall wird dimensionssparsam das zugrundeliegende Listenelement als 
  ##    Vektor zurückgegeben
str(df["x"])
str(df[, "x"])
str(df[, "x", drop = FALSE]) 

# Zugriffe mit [[]] und $
l <- list(a = 1, b = 2)
l[1]
l[[1]]
df[[1]]
l[["a"]]
df[["x"]]

l[[c("a", "b")]] # Achtung: [[ und $ gehen nur mit einelementigen Aufrufen


 ## außer bei geschachtelten Listen, da ist das eine Kurzschreibweise 
 ## für Substruktur-Zugriffe
l <- list(a = 1, b = list(c = list(d = 1)))
l[[c("b", "c")]] #  same as l[["b"]][["c"]]

 ## $ ermöglicht Partial-Erkennung, [ und [[ nicht (vgl. Zugriffe auf Vektoren)
l <- list(abc = 1, def = 2)
l$a
l[["a"]]
l["a"]

 ## Aber: nicht in Kombination mit Zuweisung; 
 ## hier wird ein da wird ein neues Listenelement erzeugt
l$a <- 3
l["d"] <- 3 # das geht natürlich auch mit [
l

  ## _> Struktur-vereinfachend/-erhaltend ----

  ## wie bei Matrizen und data.frames gesehen, verliert nach gewissen Zugriffs-
  ## Operationen das Objekt Attribute (z.B. das Dimensions-Attribut)
  ## Wir legen jetzt den Fokus nochmal auf die Frage, wie die verschiedenen 
  ## Zugriffs-operatoren die Attribute erhalten oder entfernen (also die Struktur 
  ## vereinfachen)

# Vektor
x <- setNames(1:4, letters[1:4])
x
attributes(x)
x[1]
x[[1]]
x$a

# Faktor
f <- factor(letters[1:4])
f
attributes(f)
f[1]
f[[1]]
 ## attribute bleiben in jedem Fall erhalten
 ## man kann aber levels verwerfen
f[1, drop = TRUE]
f$a

# Matrix
mx <- matrix(1:9, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:3]))
mx
attributes(mx)
str(mx[1, ])
str(mx[1, , drop = FALSE])
mx$a

# Liste
l <- list(a = 1, b = 2, c = 3, d = 4)
l
attributes(l)
l[1]
l[[1]]
l$a

# Data Frame
df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
attributes(df)
str(df[1])
str(df[[1]])
str(df$a)

str(df[, 1])
str(df[, 1, drop = FALSE])

# combine
x <- 1:4
l <- list(a = 5, b = 6, c = 7, d = 8)
c(x, l) ## das gleiche wie c(as.list(x), l)
c(x, l, recursive = TRUE) ## https://twitter.com/data_question/status/1526178359776010242
c(x, l, recursive = TRUE, use.names = FALSE)

c(list(x), l) ## anders als c(as.list(x), l)
list(x, l)
list(x, c(l, recursive = TRUE, use.names = FALSE))
list(x, unname(unlist(l)))

rep(list(x), 5) ## https://stackoverflow.com/questions/8406307/repeat-list-object-n-times

  ## _> Zugriff und Zuweisung ----

  ## im praktischen Arbeiten mit R kann es vorkommen, dass solche Abfragen in 
  ## einer langen Reihe von R-Befehlen irgendwo vorkommen.
  ## Oft sind diese unterschiedlichen Fälle Quellen für Bugs in den eigenen 
  ## Programmzeilen (das gilt auch für die Zuweisung ohne Zugriff); daher ist 
  ## es nützlich, die Fälle gesehen zu haben

x <- 1:5

x[c(1, 2)] <- 2:3 # integer
x

x[-1] <- 3:1 # die linke Seite sollte genauso lange sein, wie die rechte Seite
             # sonst zyklisch erweiternd 
x

x[c(1, 1)] <- 2:3 # kein Check auf doppelten Zugriff 
x

x[c(1, NA)] <- c(1, 2) # bei Zuweisung keine fehlenden Integer möglich 
x[c(TRUE, FALSE, NA)] <- 1 # bei logischen Zugriff/Zuweisungen schon; 
                           # hier wird (1) zyklisch aufgefüllt und (2) NA als 
                           # FALSE aufgefasst 
x[c(TRUE, FALSE, NA)]
x[c(TRUE, FALSE, NA, TRUE, FALSE)]

x[7] <- 7 # https://twitter.com/data_question/status/1544297752976400384
x


  ## das ist nützlich für das Ersetzen bspw. bei data.frames mit logischen 
  ## Abfragen, die NA werfen können
df <- data.frame(a = c(1, 10, NA))
df
df$a < 5
df$a[df$a < 5] <- 0 ## which(df$a < 5)
df

 ## leerer Zugriff mit Zuweisung ist strukturerhaltend. 
 ## Per default ist das Ergebnis eines lapply-Aufrufs eine Liste
str(lapply(df, '*', 2))
 ## was ist jetzt der Unterschied zwischen den folgenden beiden Aufrufen?
df[] <- lapply(df, '*', 2) # Zuweisung mit leerem Zugriff: wird strukturerhaltend
                           # verändert
df <- lapply(df, '*', 2)   # Zuweisung ohne Zugriff: objekt wird überschrieben

  ## _> Anwendung ----

  ## [optional]
  ## Es gibt jede Menge Anwendungen mit dieser Zugriffslogik.
  ## Auf ein paar davon können wir hier eingehen

# Rekodierung
x <- c("m", "f", "u", "f", "f", "m", "m")
table(c("m" = "bekannt", "f" = "bekannt", "u" = "unbekannt")[x])
table(c("m" = "male", "f" = "female", "u" = NA)[x], useNA = "always")

# Matching
grades <- c(1, 2, 2, 3, 1)
info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)
info[match(grades, info$grade), ]

rownames(info) <- info$grade
info[as.character(grades), ]

# Bootstrap-Stichproben ziehen
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
set.seed(10)
df[sample(nrow(df), replace = TRUE), ]

# Sortieren
x <- c("b", "c", "a")
order(x)
x[order(x)]

# Ausweiten eines Datensatzes nach Gewichten
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), weight = c(3, 5, 1))
df
rep(1:nrow(df), df$weight)
df[rep(1:nrow(df), df$weight), ]

# Entfernen von Elementen
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]
df
df$z <- NULL
df

# mehrstufige Logische Operatoren
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ] # |, &, ! (don't use &&, ||)
                                             # logic laws: !(X & Y) == !X | !Y
                                             #             !(X | Y) == !X & !Y
                                             #             X | TRUE == TRUE
                                             #             X & FALSE == FALSE
                                             # Aufgabe: simplify !((X & Y) | !Z)
                                             # !(X & Y) & !!Z == !X | !Y & Z





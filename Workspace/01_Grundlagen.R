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

# ____________________________________ ---- 
# Datenformate ----


##
TRUE
T # don't!!
2
3.2
str(3.2); is.double(3.2)
a
"a"
0i # der Vollständigkeit
as.raw(40) # raw bytes, hexadecimal - 0x28 ## 2*16+8; ?charToRaw; der Vollständigkeit

##
x <- as.Date("07-07-2022", "%d-%m-%Y")
str(x)
class(x)
unclass(x) 


##
dbl <- c(1, 2.5, 4.5) 
int <- c(1L, 6L, 10L)
log <- c(TRUE, FALSE, T, F)
chr <- c("mein", "string", "vector")
 
##
c(1, c(2, 3)) 

##
typeof(int)
is.integer(int)
is.numeric(int)
is.atomic(int)
is.double(int)

##
str(NA)
str(c(NA, 3.4))
str(c(NA_real_, 3.4)) # NA, NA_integer_, NA_real_, NA_character_
str(NA_character_)

##
c(TRUE, "a")
as.numeric(c(TRUE, TRUE, FALSE))
sum(c(TRUE, TRUE, FALSE))
as.logical(c(0, 1, 2.4))
as.logical(c("a", "1", "b"))

# ____________________________________ ----
# Klassenmethoden ----

  ## _> Listen ----

##
x <- list(dbl, int, log, chr)
str(x)
typeof(x)
is.list(x)

##
y <- list(list(list(1))) 
str(y)
is.recursive(y)
length(y)

##
list(list(1, 2), c(3, 4))
c(list(1, 2), c(3, 4))
O

  ## _> Attribute ----

x <- 1:10
attr(x, "my_attr") <- "mein Attributtext"

attributes(x) 
attr(x, "my_attr") 
x <- structure(1:10, "my_attr" = "mein Attributtext") 

##
attributes(x[1]) 

# Element-Namen
x <- c(a = 1, "b" = 2)

x <- 1:3
names(x) <- c("a") 
x

names(x) <- c("a", "b", "c")
x

setNames(x, c("d", "e", "f"))

unname(x) 
names(x) <- NULL

# Faktoren
x <- factor(c("a", "b", "c", "a"))
x
class(x)
levels(x) 
x[2] <- "d" 
x

##
c(x, factor("d")) 

##
table(factor(c("f", "f", "f"), levels = c("m", "f")))

## stringsAsFactors
z <- read.csv(text = "value\n12\n1\n.\n9",
              stringsAsFactors = TRUE) 
z 
str(z)
as.numeric(z$value) 
as.numeric(as.character(z$value)) 
z <- read.csv(text = "value\n12\n1\n.\n9", 
              stringsAsFactors = FALSE) 
str(z)

z <- read.csv(text = "value\n12\n1\n.\n9", na.strings = ".")
str(z)

  ## _> matrix/array ----

# Erstellen
a <- matrix(1:6, ncol = 3, nrow = 2)  # Spaltendominant
b <- array(1:12, dim = c(2, 3, 2))

c <- 1:6 
dim(c) <- c(3, 2)

d <- structure(1:6, dim = c(3, 2))

##
dim(c) <- c(3, 3)
matrix(1:6, ncol = 3, nrow = 3)

# Attribute
a

length(a) 
ncol(a) 
nrow(a)

##
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")

dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))
b

##
names(b)
names(b) <- c("A", "b")
c(b)
b
 
##
class(a)
class(b)
is.array(a)
is.matrix(b)

attributes(a)

##
str(1:3)
str(matrix(1:3, ncol = 1))
str(matrix(1:3, nrow = 1))
str(array(1:3, dim = 3))

##
l <- structure(list(1:3, "a", TRUE, 1.0), dim = c(2, 2))
l 
l[1, 2]
l[[1]]

  ## _> data.frame ----

# Erstellung 
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)

##
df <- data.frame(x = 1:3, y = c("a", "b", "c"),
                 stringsAsFactors = TRUE) # das war vor 4.0 default
str(df)

# Attribute
attributes(df)

##
names(df) 
class(df)

##
row.names(df) 
rownames(df) 
rownames
dimnames.data.frame

# Kombinieren
cbind(df, data.frame(z = 1:3))
str(cbind(df, z = 1:3)) 

##
rbind(df, data.frame(y = "z", x = 10))

## Achtung
str(data.frame(cbind(a = 1:2, b = c("a", "b")))) 

##
x <- data.frame(a = 1:2, b = c("a", "b")) # stringsAsFactors = FALSE ist default
str(x)

# Erweitern
df$z <- list(1:2, 1:3, 1:4)

df
## aber...
data.frame(x = 1:3, y = c("a", "b", "c"), z = list(1:2, 1:3, 1:4)) 
##
data.frame(x = 1:3, y = c("a", "b", "c"), z = I(list(1:2, 1:3, 1:4))) 


# ____________________________________ ----
# Zugriffslogik ----

  ## _> vector ----
x <- c(2.1, 4.2, 3.3, 5.4)

x[c(3, 1)]
x[order(x)]
sort.default
x[c(1, 1)]
x[c(2.1, 2.9)]

x[-c(1, 2)]
x[c(-1, 2)]

x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]
x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE)] 
x[c(TRUE, FALSE, NA, TRUE)]

##
x[]
x[0]
str(0)

##
y <- setNames(x, c("a", "b", "c", "d")) 
y[c("a", "b")]
y[c("a", "a")]

y <- setNames(x, c("abc", "def", "geh", "ijk"))
y[c("a", "d")]

  ## _> matrix ----

mx <- matrix(1:9, nrow = 3, dimnames = list(NULL, c("A", "B", "C")))
 
mx[1:2, ]
mx[c(TRUE, FALSE, TRUE), c("B", "A")]  
mx[0, -2]
 
##
dim(mx[1, -2])
dim(mx[1, -2, drop = FALSE])

##
vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals
vals[c(4, 15)]

##
sel_mx <- rbind(c(1, 1),
                c(3, 1),
                c(2, 4))
vals[sel_mx]
which(vals == "4,1")
which(vals == "4,1", arr.ind = TRUE)
 
  ## _> data.frame & list ----

df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"))

df[df$x == 2, ]
str(df[df$x == 2, ])
str(df[, c("x", "z")]) 

##
str(df[c("x", "z")])

##
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

##
l <- list(a = 1, b = list(c = list(d = 1)))
l[[c("b", "c")]] #  same as l[["b"]][["c"]]

##
l <- list(abc = 1, def = 2)
l$a
l[["a"]]
l["a"]

## aber
l$a <- 3
l["d"] <- 3 
l

  ## _> Struktur-vereinfachend/-erhaltend ----

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

##
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
str(df[, 1])
str(df[, 1, drop = FALSE])
str(df[[1]])
str(df$a)

  ## _> Zugriff und Zuweisung ----

x <- 1:5

x[c(1, 2)] <- 2:3 # integer
x

x[-1] <- 3:1 # die linke Seite sollte genauso lange sein, wie die rechte Seite
             # sonst zyklisch erweiternd 
x

x[c(1, 1)] <- 2:3 # kein Check auf doppelten Zugriff 
x

x[c(1, NA)] <- c(1, 2) # bei Zuweisung keine fehlenden Integer möglich 
x[c(TRUE, FALSE, NA)] <- 1 
x[c(TRUE, FALSE, NA)]
x[c(TRUE, FALSE, NA, TRUE, FALSE)]

##
df <- data.frame(a = c(1, 10, NA))
df
df$a < 5
df$a[df$a < 5] <- 0 
df

##
str(lapply(df, '*', 2))
##
df[] <- lapply(df, '*', 2)
df <- lapply(df, '*', 2)

  ## _> Anwendung ----

  ## [optional]

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
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]



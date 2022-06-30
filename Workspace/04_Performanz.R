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

where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if(exists(name, envir = env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))
  }
}

# ______________________________ ----
# Performanz vs. Flexibilität ----

##
x <- 0L 

##
for(i in 1:1e6) x <- x + 1 
##
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

##
`{`
where("(")
search()

# ______________________________ ----
# Profiling ----

x <- runif(100)
##
sqrt(x)
exp(log(x) / 2)
 
##
system.time(sqrt(x))
system.time(x^0.5)

##
n <- 1e6
system.time(for(ii in 1:n) sqrt(x))
system.time(for(ii in 1:n) x^0.5) 

##
ptm <- proc.time()
for(ii in 1:n) sqrt(x)
proc.time() - ptm

##
library(microbenchmark)
x <- runif(100)
microbenchmark(sqrt(x))
microbenchmark(sqrt(x), 
               x^0.5) 
##
microbenchmark(sqrt(x), x^0.5, times = 1000) ## Fokus auf Median und untere und obere Quartile

# Rprof
  
##
?Rprof ## Funktioniert für Quelldateien

##
tmp <- tempfile()
require(here)
source(here("04a_profiling-source-01.R")) ## 3 Geschachtelte Funktionen mit sleep()

Rprof(tmp, interval = .01)
f()
Rprof(NULL)
summaryRprof(tmp)

## Fortgeschritten bspw. 
## Hadley Wickham: devtools::install_github("hadley/lineprof") -- deprecated
## Jenny Bryan: devtools::install_github("rstudio/profvis") 
library(profvis)
library(ggplot2)
profvis({
  g <- ggplot(diamonds, aes(carat, price)) + geom_point(size = 1, alpha = 0.2)
  print(g)
})

# ______________________________ ----
# Optimieren ----
  
  ## _> Vorhandene Lösungen? ----

  ## _> Sparsam programmieren ----

##
x <- 1:100
##
microbenchmark(any(x == 10), 10 %in% x)

##
df <- data.frame(x = runif(100), y = runif(100))
microbenchmark({
  sub <- df[sample(nrow(df), 10), ] ## Subset von data.frame
  cor(sub$x, sub$y) 
},{
  i <- sample(nrow(df), 10) 
  cor(df$x[i], df$y[i]) ## Subset von vektor
})

##
x <- rep(letters[1:3], each = 50)
microbenchmark(factor(x), factor(x, levels = c("a", "b", "c")))

  ## _> Vektorisieren ----

##
x <- matrix(runif(100), ncol = 5)
r <- rep(1, nrow(x))

microbenchmark(
  "for" = {
    res <- numeric(ncol(x))
    for(i in 1:ncol(x)) res[i] <- sum(x[, i])
  },
  apply(x, 2, sum),
  colSums(x),
  r %*% x
)

all.equal(c(r %*% x), colSums(x))

## rowsum
x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
microbenchmark(
  aggregate(x, list(group), sum),
  rowsum(x, group)
)
rowsum
rowsum.default # internal rowsum_matrix

## Index-Matrix
mx <- matrix(runif(1e6), ncol = 2)
bmx <- cmx <- matrix(NA, nrow(mx) / 2, ncol(mx))
microbenchmark( ## multipliziere jede Zeile eintragsweise mit der darauffolgenden.
  "for" = for(ii in 1:nrow(bmx)) bmx[ii, ] <- mx[2 * ii - 1, ] * mx[2 * ii, ],
  "index" = cmx <- mx[seq(1, nrow(mx), by = 2), ] * mx[seq(2, nrow(mx), by = 2), ]
)

all.equal(bmx, cmx)

## vectorized if
hit <- NA 
microbenchmark(
  "hit" = for(i in 1:1e4) if(runif(1) < .3) hit[i] <- TRUE,
  "fasthit" = fasthit <- ifelse(runif(1e4) < .3, TRUE, NA)
)

  ## _> Kopieren vermeiden ----

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

##
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
  "initialisieren" = {
    df <- data.frame(a = character(n), 
                        b = numeric(n))
    for(i in 1:n){
      df$a <- sample(letters, 1, replace = TRUE)
      df$b <- runif(1)
    }
  })


##
f1 <- function(n) {
  my.df <- data.frame(a = character(0), 
                      b = numeric(0))
  for(i in 1:n){
    this.N <- rpois(1, 10)
    my.df<- rbind(my.df, data.frame(a = sample(letters, this.N, replace = TRUE),
                                    b = runif(this.N)))
  }
  my.df
}

##
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

##
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

##
library(parallel)
detectCores()
(phys_cores <- detectCores(logical = FALSE)) ## physical vs. logical cores


##
cl <- makeCluster(phys_cores - 1, type = "PSOCK") 
cl
typeof(cl)
(n_cl <- length(cl))
cl[[1]]
names(cl[[1]])
stopCluster(cl)

##
x <- 1:10
##
clusterEvalQ(cl, x)
clusterExport(cl, "x")
clusterEvalQ(cl, x)

##
library(mvtnorm)
clusterEvalQ(cl, exists("dmvnorm"))
clusterEvalQ(cl, {
  library(mvtnorm)
  exists("dmvnorm")
})

##
clusterApply(cl, rep(1000, n_cl), fun = function(x) mean(rnorm(x, mean = 5)))

##
clusterApply(cl, rep(1000, 25), fun = function(x) mean(rnorm(x, mean = 5))) 

stopCluster(cl)

# foreach
library(doParallel)
library(foreach)
##
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

library(Rcpp)

##
cppFunction('int add(int x, int y, int z) { // Die Input- (wenn vorhanden) und 
                                            // Output-Typen müssen angegeben 
                                            // werden
               int sum = x + y + z; // Befehle enden mit ;
               return sum;
             }') 

add
add(1, 2, 3)

##
cppFunction('int sumC(NumericVector x) { // Es gibt NumericVector, IntegerVector, 
                                         // CharacterVector, LogicalVector
                                         // double, int, String, bool
               int n = x.size();         // jede Variable muss deklariert und 
                                         // initialisiert werden (auch ein Laufindex)
                                         // [object][dot][method]-Syntax (x.size())
               double tot = 0;
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

  ## file -> new -> c++
  ## bespreche default-Input 
  ## header nötig
  ## // [[Rcpp::export]] für export in R (space is notwendig)
  ## R-code in C++-Command--Blocks for Tests

sourceCpp(here("04b_sourceCpp.cpp"))

# sugar

cppFunction('int sumC2(NumericVector x) {
               return sum(x);
             }')
microbenchmark(sumR(x), sumC2(x), sumC(x), sum(x))

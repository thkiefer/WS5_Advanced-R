# first/last ----
.First <- function(){
  cat("\nWelcome at", date(), "\n")
}

.Last <- function(){
  cat("\nGoodbye at ", date(), "\n")
}

# settings ----

# braucht man 10000 Einträge Listenoutput?
options(max.print = 100)

## https://kevinushey.github.io/blog/2015/02/02/rprofile-essentials/
# warn on partial matches
options(warnPartialMatchAttr = TRUE,
        warnPartialMatchDollar = TRUE,
        warnPartialMatchArgs = TRUE)

# enable autocompletions for package names in
# `require()`, `library()`
utils::rc.settings(ipck = TRUE)

# # warnings are errors
# options(warn = 2)

# fancy quotes are annoying and lead to
# 'copy + paste' bugs / frustrations
options(useFancyQuotes = FALSE)

# functions/shortcuts ----
lf <- function(x, pattern, full.names = TRUE, ...) 
  list.files(x, pattern, full.names = full.names, ...)

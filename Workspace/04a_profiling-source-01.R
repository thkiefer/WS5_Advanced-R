f <- function() {
  Sys.sleep(.1)
  g()
  h()
}
g <- function() {
  Sys.sleep(.1)
  h()
}
h <- function() {
  Sys.sleep(.1)
}
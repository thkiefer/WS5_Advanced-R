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
# Script 05: Pakete
# 
#
####

# ________________________________ ----
# Paket01 ----

## https://cran.r-project.org/doc/manuals/r-release/R-exts.html
## https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
## https://cran.r-project.org/web/licenses/
## https://www.rstudio.com/resources/cheatsheets/#package
## https://www.rstudio.com/resources/cheatsheets/#RMarkdown

## http://r-pkgs.had.co.nz/

## 
## [str+shift+F10]
##

##
source(here::here("05a_funktionen.R"))
data(cats, package = "MASS")
linmodEst(cbind(1, cats$Bwt), cats$Hwt)
my_lm <- linmod(cbind(1, cats$Bwt), cats$Hwt)
class(my_lm)
print(my_lm)

## 
## [str+shift+F10]
## 
source(here::here("05a_funktionen.R"))

##
package.skeleton(name = "linmod", 
                 list = ls(), 
                 path = here::here("05_Pakete", "Paket01")) # working-dir/r-package


## Gehe zum Verzeichnis

## Notwendig sind man/, R/, DESCRIPTION, NAMESPACE
## Optional sind 
## -> data/
## -> src/ for C/C++/Fortran-Code
## -> inst/ for NEWS/CITATION, inst/doc
## -> vignettes/
## -> tests/

## -> Gehe zu R/

## -> Gehe zu Read-and-delete-me

## -> Gehe zu https://cran.r-project.org/web/packages/TAM/
##    source herunterladen (R ist open source; auch base R)
##    vorbereitet unter here::here("05_Pakete")
##    unzip/unzip/

## -> Gehe zu DESCRIPTION

## Öffnet aus Projekt heraus TAM-DESCRIPTION

## -> Gehe zu NAMESPACE

## -> Gehe zu man/

## https://cran.r-project.org/bin/windows/base/rdevel.html
##  https://cran.r-project.org/submit.html

# ________________________________ ----
# Paket01_Rcpp ----

library(Rcpp) 
Rcpp.package.skeleton(name = "linmod",
                      path = here::here("05_Pakete", "Paket02"),
                      list = character(),
                      code_files = here::here("05b_rcpp-funktionen.R"),
                      cpp_files = here::here("05b_rcpp-funktionen.cpp"),
                      
                      author = "Thomas Kiefer", 
                      email = "thomas.kiefer@iqs.gv.at", 
                      license = "GPL (>= 2)",
                      
                      example_code = FALSE, attributes = FALSE, module = FALSE
)

## -> Gehe zu DESCRIPTION
## -> Gehe zu NAMESPACE
## -> Gehe zu /src

# ________________________________ ----
# Paket02 ----

##  https://www.youtube.com/watch?v=9PyQlbAEujY

## _> Rstudio-Projekte und devtools ---- 

pacman::p_load(c("devtools", "roxygen2")) 

## die neueste Verson von devtools (für R 4.2 noch nicht auf CRAN) gibt's auf 
## github

devtools::install_github("hadley/devtools")  

## Erzeuge neues Projekt in einem neuen Verzeichnis
## (File -> New Project -> New Directory -> R Package 
## (oder R package using devtools)
## -> Package name linmod
## -> source 05c_funktionen.R
## -> Subdirectory of ../05_Pakete/Paket02
## -> git/rev optional
## -> open in new session

## -> gehe zu Verzeichnis (automatisch)
## -- R script ist reinkopiert
## -- man leer
## -- DESCRIPTION/NAMESPACE durch package.skeleton

## Jedenfalls sehen wir jetzt rechts oben das Build-Tab
## Gehe zu Menü Build-> Configure Build tools -> Build Tools -> Package
## -> use devtools if available (vorausgewählt)
## -> use Roxygen -> entable alles außer Vignette (machen wir später) -> ok
## Install/Check/Build Kommandos adaptierbar

pkgload::load_all() 
devtools::find_rtools()
pkgbuild::rtools_path()

## _> Erste Hilfeseiten mit Roxygen ---- 

## Roxygen fängt mit # und ' und einem Leerzeichen an. Darauf folgen die 
## Roxygen-Tags. 
## -> die erste Zeile ist der Titel.
## -> es folgt eine Leerzeile und eine weitere Zeile mit der Kurzbeschreibung.
## -> dann kann man die Parameter (die unter Arguments aufgelistet werden) 
##    beschreiben [-> die usage-Sektion wird automatisch erzeugt]
## -> \itemize/\item sind RD-Syntax
## -> @export legt fest, dass diese Methode durch das Paket exportiert wird. 
## -> Weitere Tags sind verfügbar; Rstudio bietet hier Autovervollständigung 
##    an. (Vgl. @example)

## Build-tab: Install and Restart -> writing linmodEst.Rd/writing NAMESPACE
## -> Help -> Reload Help page -> linmodEST
## -> gehe zu man/linmodEst.Rd
## -> gehe zu NAMESPACE (=> export)

## _> Weitere nützliche Roxygen-Tags ---- 

## Ersetze Beschreibung von R -> 05c_funktionen.R linmodEst mit 
## 05c_funktionen-roxygen-02.R

## linmodEst
## linmod
## linmod.default
## print.linmod 

## Build-tab: Install and Restart -> writing linmodEst.Rd/linmod-internal.Rd/
## NAMESPACE
## -> Help -> Reload Help page -> linmodEST
## -> Index -> print
## -> gehe zu NAMESPACE (=> S3method)

## _> CHECK zur Veröffentlichung ----

## https://cran.r-project.org/web/licenses/

# ________________________________ ----
# Vignette ----

pacman::p_load("rmarkdown")
usethis::use_vignette() 
## YAML

## Message von usethis::use_vignette()
## gehe zu linmod/vignettes

## gehe zu linmod/vignettes/linmod.Rmd

## 
## [STRG+Shift+K] 
## 

## 
## [STRG+Shift+K] 
## 

## Gehe zu Files (/vignettes)

## Build-Tab->More->Build Source, wodurch ein Paket erzeugt wird.
## -> gehe zu ../; unzip, unzip-To-
## -> gehe zu source../inst/doc

##
browseVignettes("devtools")

## Öffne 05d_vignette_linmod.Rmd

## Knit -> drop-down -> knit pdf vs. knit html_vignette.
## Section 3.1, https://bookdown.org/yihui/rmarkdown/html-document.html

## date: `r expr`

## 
## str(knitr::opts_chunk$get())
## https://yihui.name/knitr/options/#chunk_options
## 

## -> kopiere nach Vignette, 
## -> kopiere/schneide Namen ab von 05d_vignette_linmod__apa6 und references
## -> knit
## - in RMD machen Listen

## wMarkdown-Cheat-Sheet 
## https://www.rstudio.com/resources/cheatsheets/ bzw. 
## https://raw.githubusercontent.com/rstudio/cheatsheets/main/rmarkdown.pdf

## 

## 

## 

## https://bookdown.org/yihui/bookdown/cross-references.html

## https://livefreeordichotomize.com/2018/09/14/one-year-to-dissertate/

## 
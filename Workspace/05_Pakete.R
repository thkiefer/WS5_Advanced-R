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
# VOLL-Version
#
####

# ________________________________ ----
# Paket01 ----

  ## Ich möchte in dieser Einheit zeigen, wie R-Pakete erstellt werden. Dazu 
  ## gehen wir in einem ersten Teil Schritt für Schritt vor, um die Paket-
  ## Struktur aufzusetzen und zu befüllen. In einem zweiten Teil bedienen wir 
  ## uns dann mordernen Tools, die vor allem die manuellen Schritte erleichtern 
  ## und dabei Fehlerquellen minimieren. Ganz am Ende ist hoffentlich noch Zeit 
  ## um kurz über Markdown zu sprechen.

  ## Ich bediene mich hierbei vor allem einiger offizieller Manuale von CRAN, 
  ## die erstmal abschreckend wirken, aber in der letzten Konsequenz die 
  ## relevante Referenz sind. Um das CRAN-Team zu entlasten, sollten v.a. die 
  ## erste Ressource jedenfalls berücksichtigt werden.
  ## https://cran.r-project.org/doc/manuals/r-release/R-exts.html
  ## https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
  ## https://cran.r-project.org/web/licenses/
  ## https://www.rstudio.com/resources/cheatsheets/#package
  ## https://www.rstudio.com/resources/cheatsheets/#RMarkdown

  ## Instruktivere Refernzen zum Erstellen von R-Paketen gibt es auch (z.B. 
  ## http://r-pkgs.had.co.nz/)

  ## Fangen wir mit einem leeren Workspace an
  ## [str+shift+F10]
  ## und füllen ihn mit einem Satz von Funktionen, die wir zu einem Paket 
  ## binden wollen.
  
  ## Sehen wir uns zunächst an, was die Funktionen machen
source(here::here("05a_funktionen.R"))
data(cats, package = "MASS")
linmodEst(cbind(1, cats$Bwt), cats$Hwt)
my_lm <- linmod(cbind(1, cats$Bwt), cats$Hwt)
class(my_lm)
print(my_lm)

  ## Jetzt haben wir wieder den Workspace voll. Leeren wir ihn nochmal, um 
  ## das R-Paket aufzusetzen...
  ## [str+shift+F10]
  ## ... und laden die Funktionen, die in ein R-Paket sollen
source(here::here("05a_funktionen.R"))

  ## mit package.skeleton setzen wir die Verzeichnisstruktur für ein R-Paket auf, 
  ## das wir "linmod" nennen. 
  ## Wir schicken dort alles hinein, was im Workspace ist. Wir könnten das auch 
  ## händisch machen, oder das argument code_files verwenden, um nicht für jede 
  ## Funktion ein eigenes Source-File erzeugt zu haben.
  ## Das Paket legen wir unter here::here("05_Pakete", "Paket01") ab.
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
  ## R-Files sind einfache Dumps (ohne Kommentare usw.) und 1 File je Funktion 
  ##   der Implementierungen.
  ##   Ersetze durch 05a_funktionen.R

  ## -> Gehe zu Read-and-delete-me

  ## Für das Befüllen der anderen unterschiedlichen Dateien kann man sich, wenn 
  ## man die Manuale im Blick behält, am einfachsten an Vorlagen bedienen. 
  ## Ich ziehe hier TAM heran (früher hatte ich stats genommen), weil in TAM 
  ## viele Elemente inkludiert sind und ich weiß, wo ich suchen muss.

  ## -> Gehe zu https://cran.r-project.org/web/packages/TAM/
  ##    source herunterladen (R ist open source; auch base R)
  ##    vorbereitet unter here::here("05_Pakete")
  ##    unzip/unzip/

  ## -> Gehe zu DESCRIPTION
  ## Das sind alles Felder mit bestimmten Tags, die zum Beispiel 
  ##   bei der Installation oder dem Check Auswirkungen haben 
  ##   (siehe TAM; einige werden von CRAN erzeugt)
  ##   Hier auch Language: de und Encoding: UTF-8, wenn es irgendwo Umlaute gibt
  ##   (Die müssen dann auch in utf-umcodiert werden mittels ?iconv())

  ## Öffnet aus Projekt heraus TAM-DESCRIPTION

  ## -> Gehe zu NAMESPACE
  ## Hier wird festgelegt, welche Methoden wo sichtbar sind: 
  ##   export, S3method, ggf. import
  ##   Gegenwärtig sind alle Funktionen im Suchpfad sichtbar. Unsichtbare sind 
  ##   nicht für den Nutzer gedacht; können mit ::: angesteuert werden.
  ##   Hier wird später C++-Code mit useDynLib() eingelesen
 
  ## -> Gehe zu man/
  ##    RD = Dateiendung steht für R-Documentary
  ## linmod-package ist der Einstieg in die Manual-Seiten des R-Pakets (vgl. 
  ##    package?TAM)
  ## linmod.default eher mit linmod zusammen in eines. Dann \alias und \usage
  ##   in TAM auch print dort -> S3 eher mit \method{} zu dokumentieren.
  ##   (package.skeleton veraltete Funktionalität)
  ## ?TAM::`TAM-utilities` -- internal functions

  ## R CMD CHECK/INSTALL
  ## shift-rechtsklick auf Verzeichnis -> PowerShell (oder Kommandozeile) hier 
  ## öffnen (else: [windows]+cmd -> cd und hinnavigieren)
  ## PS: & 'C:\Program Files\R\R-4.2.0\bin\R'
  ## "C:\Program Files\R\R-3.5.2\bin\x64\R" CMD check linmod
  ## ... check/INSTALL [--timings --as-cran]/build/Rd2pdf
   
  ## Behebe die Fehler
 
  ## Für CRAN muss der Check mit --timings und --as-cran ohne Fehler, besser 
  ## ohne Warnings oder Notes unter der aktuellsten devel-version laufen
  ## https://cran.r-project.org/bin/windows/base/rdevel.html

  ## Wenn das alles geklappt hat, kann das Paket unter 
  ## https://cran.r-project.org/submit.html submitted werden.
  ## Die CRAN-Policies https://cran.r-project.org/web/packages/policies.html 
  ## sollten dabei im Detail berücksichtigt werden.

# ________________________________ ----
# Paket01_Rcpp ----
 
  ## Wenn wir bereits eine C++-Funktion haben, die wir dem Paket hinzufügen 
  ## wollen (bspw. sumC), verwenden wir die Erweiterung Rcpp.package.skeleton.
  ## sumC ist in der .cpp-Datei implementiert und in der .R-Datei referenziert.

library(Rcpp) 
Rcpp.package.skeleton(name = "linmod",
                      path = here::here("First_Rcpp_Package"),
                      list = character(),
                      code_files = here::here("05b_rcpp-funktionen.R"),
                      cpp_files = here::here("05b_rcpp-funktionen.cpp"),
                      
                      author = "Thomas Kiefer", email = "thomas.kiefer@iqs.gv.at", 
                      license = "GPL (>= 2)",
                      
                      example_code = FALSE, attributes = FALSE, module = FALSE
)

  ## -> Gehe zu DESCRIPTION
  ## Das DESCRIPTION-Dokument erhält einen Imports-Tag, weil das Paket von Rcpp 
  ## abhängig ist (nochmal: das ginge prinzipiell auch vollständig ohne Rcpp; 
  ## vgl. tam_caux.c)... 
  ## und ein LinkingTo-Tag, damit das Paket die Rcpp-Header-Files (die 
  ## Cpp-Klassen und Objekt-Deklarationen aus dem Rcpp-Paket findet).
  
  ## -> Gehe zu NAMESPACE
  ## Im NAMESPACE wird der useDynLib-Befehl und ein importFrom(Rcpp, evalCpp) 
  ## gesetzt, damit Rcpp im Paket richtig instanziert wird.

  ## -> Gehe zu /src
  ## Das src-Verzeichnis wird erzeugt (falls es nicht schon besteht).
  ## Wenn cpp_files angegeben werden, werden diese dorhin kopiert.

  ## [NEXT]!!! wenn wir in Paket01 schon Bearbeitungen vorgenommen haben, 
  ## würden wir diese einfach in das neue Paket kopieren; und 
  ## wir würden mysumC.Rd das internal-keyword geben (und eigentlich eine 
  ## datei linmod-internal.Rd anlegen; TAM nennt die TAM-utilities.Rd ohne 
  ## das keyword festzulegen)

# ________________________________ ----
# Paket02 ----

  ## Mit Rstudio und zweier Tools, nämlich devtools und roxygen2 wird vieles noch 
  ## sehr viel leichter. 
  ## Eine Einführung gibt's auch in Youtube: 
  ##  https://www.youtube.com/watch?v=9PyQlbAEujY
  ## Ganz narrensicher ist das aber nicht; weswegen mir wichtig war zu zeigen, 
  ## was da im Hintergrund läuft und welche Prozesse vereinfach werden.

  ## _> Rstudio-Projekte und devtools ---- 

  ## devtools unterstützt uns beim Kompilieren, Paket-Bau und einiges weiteres
  ## roxygen2 ist für die Paket-Dokumentatin hilfreich.

pacman::p_load(c("devtools", "roxygen2")) ## pacman ist ein Tool, das Pakete, 
                                          ## die noch nicht verfügbar sind, gleich 
                                          ## versucht zu installieren.
 
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

  ## Wir nutzen load_all aus dem Paket pkgload (dependend on devtools) um 
  ## alle Methoden und Abhängigkeiten in den Workspace zu laden. Das brauchen 
  ## wir, wenn wir unsere Methoden/Scripte überarbeiten und ausprobieren wollen.
  ## Ohne diese Funktionalität wäre das Arbeiten v.a. mit externen Bibliotheken 
  ## (also Cpp) deutlich schwieriger, weil man die erst (im Fall von Windows) in 
  ## eine dll binden muss und dann laden.

pkgload::load_all() ## kopiere das in Konsole von R-Paket-Projekt
devtools::find_rtools() ## aus pkgbuild
pkgbuild::rtools_path()

  ## jetzt bauen wir das einfach... 
  ## zuerst gehe zu Packages->linmod? nicht da
  ## jetzt einfach: Install and Restart
  ## gehe zu Packages -> update -> linmod? da is es
  ## -> anklicken
  ## "There are no help pages in this package"
  ## (man-Verzeichnis ist leer)

  ## _> Erste Hilfeseiten mit Roxygen ---- 

  ## wir wissen schon, wie man das man-Verzeichnis befüllt. Jetzt schauen wir 
  ## uns an, wie wir roxygen dazu nutzen können.

  ## Zuerst müssen wir den NAMESPACE entfernen, weil der nicht von roxygen 
  ## erzeugt wurde; Roxygen braucht dort ein spezielles Feld und Design vom 
  ## NAMESPACE, um ihn ersetzen zu können. (Siehe Build-Note bzgl. Namespace)
  ## Klick NAMESPACE-Datei -> Delete
  ## Build-tab: Install and Restart -> writing NAMESPACE -> NAMESPACE

  ## Ersetze Beschreibung von R -> 05c_funktionen.R linmodEst mit 
  ## 05c_funktionen-roxygen-01.R
  
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

  ## Aber eigentlich wollten wir gar nicht linmodEst verfügbar machen, 
  ## sondern die anderen.
  ## Und wir wollen -- was häufig der Fall ist -- mehrere Methoden in einer 
  ## Hilfeseite beschreiben 

  ## _> Weitere nützliche Roxygen-Tags ---- 

  ## Ersetze Beschreibung von R -> 05c_funktionen.R linmodEst mit 
  ## 05c_funktionen-roxygen-02.R

  ## linmodEst
  ## -> export entfernt
 
  ## linmod
  ## -> @rdname xxx for including in other help file
  ## -> @param \dots müssen hinzugefügt werden; werden durch linmodEst noch nicht 
  ##    beschrieben
  ## -> die wird jetzt exportiert

  ## linmod.default
  ## -> @rdname linmod-internal => nur für die Interne Nutzung
  ##    wird von linmod über UseMethod angesteuert.
  ## @method-Tag können wir hier weglassen (siehe @method print linmod), weil 
  ## Roxygen das selbst erkennt durch die Schlüssel-Wörter UseMehod und .default
  ## selbst erkennt.

  ## print.linmod 
  ## -> @rdname hier wird erst linmod-internal erzeugt (Roxygen ist 
  ##    nicht auf Reihenfolge angewiesen).
  ## -> @param da werden auch x und dots erzeugt, die in linmod.default auch 
  ##    verwendet werden
  ## -> das hier müssen wir exportieren (sonst ist die print-Methode im Suchpfad
  ##    nicht sichtbar und die print.default-Methode wird verwendet.
  ## -> hier zeigen wir an, dass es sich um die print-Methode für linmod handelt, 
  ##    sonst exportieren wir nur print.methods, haben aber keinen Bezug zur S3-
  ##    Vererbungshierarchie 

  ## Build-tab: Install and Restart -> writing linmodEst.Rd/linmod-internal.Rd/
  ## NAMESPACE
  ## -> Help -> Reload Help page -> linmodEST
  ## -> Index -> print
  ## -> gehe zu NAMESPACE (=> S3method)

  ## _> CHECK zur Veröffentlichung ----

  ## So schnell kann man sich ein Paket bauen. Wenn man es veröffentlichen 
  ## oder weitergeben will, muss man noch etwas mehr tun. Build/Install führen  
  ## nur sehr grobe Plausibilitätsprüfungen durch.

  ## Aber CHECK führt eine ganze Menge weiterer Tests durch; u.a. ob alle 
  ## Parameter dokumentiert sind.
  ## Build-Tag: Check
  ## -> zum Glück habe ich eine Tex-Distribution, weswegen R das Manual auch 
  ##    erstellen kann; sonst erhält man "pdflatex is not available" und könnte 
  ##    die Option --no-manual unter "Configure Build Tools" setzen.
  ## -> dort auch --timings und --as-cran
  ## -> Keine Lizenz: -> Description
  ##    siehe https://cran.r-project.org/web/licenses/

  ## Jetzt: Build Source
  ## Gehe zu Files -> Paket02

# ________________________________ ----
# Vignette ----

  ## Hat schon wer mit Latex gearbeitet? Mit Markdown? HTML, o.ä.? Alles, außer
  ## Word?

  ## In Vignetten wird beschrieben, wie ein Paket zu Nutzen intendiert ist. RD-
  ## Hilfeseiten und das Manual sind nützlich, wenn man weiß, wie die Funktionen
  ## heißen, die gesucht werden. In Vignetten dagegen werden häufig Anwendungs-
  ## fälle gezeigt, die den Aufbau des Pakets von der Datenexploration über die
  ## Hauptfunktionalität bis zum Berichten zeigen. Einige dieser Vignetten werden 
  ## als begleitende Papers in Journalen veröffentlicht.
  ## Auch die Vignette kann man per hand bauen. Das muss nichtmal mit R-Markdown 
  ## sein; Die Standard-Maschivignetten ist Sweave; diese galt auch als Standard-
  ## Werkzeug für das Journal of Statistical Software. 
  ## Der Clou dieser Vignetten-Maschinen ist, dass sie neben Text/Grafiken/
  ## Tabellen auch R-Befehle enthalten können, die zum Zeitpunkt an dem das 
  ## (z.B. PDF/HTML-)Dokument erstellt wird, ausgewertet werden.
  ## Teilweise erzeugen diese R-Befehle ihrerseits wieder Text, Grafiken oder 
  ## Tabellen.
  ## Der Hauptnutzen ist also, dass die Ausgaben, die in der Vignette abgedruckt 
  ## sind, mit dem Paket, mit dem die Vignette erzeugt wurde, reproduziert 
  ## werden können.

  ## Wir bedienen uns hierbei der Markdown-Maschine. Außerhalb von RStudio 
  ## müsste man hierfür Pandoc installieren, der Markdown-Dokumente konvertiert
  ## (http://pandoc.org/installing.html); Mir RStudio und rmarkdown kommt das 
  ## automatisch mit.

pacman::p_load("rmarkdown")
usethis::use_vignette() # früher Teil von devtools [man sieht: devtools -> gesplittet in mehrere Pakete]
                        # verwende usethis::use_vignette("linmod") in linmod-Projekt
 
  ## Zeige Message von usethis::use_vignette()
  ## gehe zu linmod/vignettes

  ## gehe zu linmod/vignettes/linmod.Rmd
  ## -> Es gibt 3 wesentliche Komponenten einer Markdown-Datei
  ## --> Der YAML [yet another markdown language] Header mit Datei-Meta-Tags
  ##     So etwas gibt's in Latex auch -- Meta-Befehle, die nicht Text sind, 
  ##     aber das Ausgabedokument struktieren.
  ## --> Text, der ggf. durch Markdown-Syntax formatiert wird
  ##     Der Text kann -- je nach Ausgabeformat (z.B. Latex-PDF/HTML) -- auch 
  ##     mit latex bzw. HTML Befehlen formatiert werden. Das schöne an reiner 
  ##     Markdownsyntax ist, dass man den Output-Datei-Typ mit einem einzigen 
  ##     YAML-Tag ändern kann.
  ## --> knitr-Felder, die in den Fließtext/oder davon abgehoben eingebaut 
  ##     werden können, die R-Code und deren Ergebnisse erzeugen.
  ##     dabei sehen wir, dass die Knitr-Felder mit Optionen daher kommen. 
  ##     Include = FALSE z.B. bewirkt, dass dieses Feld zur Laufzeit ausgewertet 
  ##     wird, aber weder deren Ergebnisse noch der Code selbst in das Dokument
  ##     einfließen.

  ## Schaun wir einfach mal, was der Beispiel-Input rauswirf
  ## Klick -> knit oder [STRG+Shift+K] (hier: HTML-Vignetten-Output; für PDF 
  ## wird Latex-Distribution benötigt)

  ## Neben dem Knit-Button -> Drop-Down Settings -> Preview in Viewer Panet
  ## [STRG+Shift+K]

  ## Gehe zu Files (/vignettes)
  ## Man sollte in /vignettes oder /inst/doc die Output-Dateien nicht behalten, 
  ## Weil R sonst beim Paket-Bau den Schritt der Erzeugung einer Vignette 
  ## überspringt und dann ist die Vignette eben gerade nicht mehr aktuell, veraltet 
  ## und potenziell fehlerhaft bzw. nichtmehr reproduzierbar.
  ## daher werden durch den RStudio/den Knit-Aufruf absichtlich keine .R oder 
  ## .HTML-Dateien abgelegt
  ## und daher gibt es auch die Einträge in .gitignore (bei der KOmmunikation 
  ## mit Git/Github, werden bei der Versionierung alle Dateien ignoriert, die 
  ## sich unter .gitignore befinden).
  ## man könnte das erzwingen mit rmarkdown::render("vignettes/linmod.Rmd") 
  ## (R-Project als working directory; get.wd(); sonst voller Pfad angeben)

  ## Man kann die Vignette auch mit devtools::build_vignettes() in /doc (nicht 
  ## /inst/doc) forcieren; aber auch das ist meist nicht nötig. Am besten 
  ## erzeugt man die Vignette beim Aufruf von devtools::build() bzw. durch 
  ## Build-Tab->More->Build Source, wodurch ein Paket erzeugt wird.
  ## -> gehe zu ../; unzip, unzip-To-
  ## -> gehe zu source../inst/doc
  ## Install and Restart und install_github erzeugen auch keine Vignette, weil 
  ## das meist nicht benötigt wird und Zeit kostet und u.U. weitere Pakete 
  ## benötigt. Kann man forcieren mit 
  ## devtools::install_github(build_vignettes = TRUE) [was dann auch alle 
  ## benötigten Pakete mitinstalliert]. 

  ## Wenn die Vignette vorliegt (Paketbau, und von Source intallieren), kann man 
  ## mit utils::browseVignettes() darauf zugreifen.
browseVignettes("devtools")

  ## Öffne 05d_vignette_linmod.Rmd

  ## Knit -> drop-down -> knit pdf vs. knit html_vignette.
  ## Viele Optionen, die es für html-Dokumente gibt, (Section 3.1, 
  ## https://bookdown.org/yihui/rmarkdown/html-document.html) gehen auch 
  ## für html_vignette; andere gibt es für pdf_document
  
  ## date: `r expr`; so wird R-code in-line oder im Header eingefügt.

  ## opts_chunk, die für knitr verfügbar sind, kriegt man unter 
  ## str(knitr::opts_chunk$get())
  ## vgl. unter https://yihui.name/knitr/options/#chunk_options
  ## include hatten wird, eval, echo, und fig-Options verwende ich häufiger.

  ## wie gesagt; html_vignette ist nicht zwingend notwendig; kann auch pdf_document
  ## sein; html_vignette hat aber vorteile (s. - in RMD)
  ## findet man auch unter rmarkdown.rstudio.com/package_vignette_format.html 

  ## -> kopiere nach Vignette, 
  ## -> kopiere/schneide Namen ab von 05d_vignette_linmod__apa6 und references
  ## -> knit
  ## - in RMD machen Listen

  ## weitere Format-Elemente findet man auch im Markdown-Cheat-Sheet unter 
  ## https://www.rstudio.com/resources/cheatsheets/ bzw. 
  ## https://raw.githubusercontent.com/rstudio/cheatsheets/main/rmarkdown.pdf

  ## Wir gehen hier ein paar durch

  ## ganz nett finde ich APA-Zitationsweise; muss im YAML-Header angegeben werden.
  ## Achtung, hier ist die Location aus der der knitr-Befehl ausgeführt wird,
  ## wichtig. 
  ## Wenn das in einem Subverzeichnis liegt, kann man bspw. mit 
  ## csl: "./Ressources/apa6.csl" darauf zugreifen.

  ## In Bezug zum Pfad kann man neuerdings auch ein Subverzeichnis setzen, in 
  ## welches der _Output_ geschrieben wird. 
  ## Bei Vignetten nicht relevant; aber wenn man Markdown zum Bericht Schreiben
  ## verwenden will, ist das hilfreich.
  ## indem man den render-Befehl direkt in den YAML-Header schreibt
  ## knit: (function(input, ...) {
  ##           rmarkdown::render(
  ##              input,
  ##              output_dir = "Bericht",
  ##              output_file = paste0("Bericht", gsub("-", "", Sys.Date()))
  ##           )
  ##        })

  ## Weitere Möglichkeiten sind Kreuzreferenzierungen mit Tabellen, Grafiken, 
  ## Abschnitt-Überschriften
  ## (vgl. https://bookdown.org/yihui/bookdown/cross-references.html).

  ## Es gibt fast nichts, was nicht geht -- es werden jede Menge ganzer Bücher 
  ## in Markdown bzw. Bookdown geschrieben; auch Dissertationen mit thesisdown
  ## (vgl. https://bookdown.org/thea_knowles/dissertating_rmd_presentation/compiling-the-dissertation.html)
  ## see https://livefreeordichotomize.com/2018/09/14/one-year-to-dissertate/

  ## Wenn man mal an die Grenzen zu stoßen meint, führt einen Stackoverflow 
  ## meist wieder auf den richtigen Weg -- wie bei den meisten Problemen, die 
  ## R betreffen.
  ## Yihui Xi (Autor von knitr und bookdown) ist dort übrigens sehr aktiv.
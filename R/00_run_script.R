##R --vanilla
library(nlme)
##library(foreign)
##library(lme4)
##library(lattice)
library(ape)

if (Sys.getenv("WORKDIR") != "") {
  ## en Linux/Mac podemos correr esto en el terminal:
  ## source env/$(hostname).sh
  work.dir <- Sys.getenv("WORKDIR")
  script.dir <- Sys.getenv("SCRIPTDIR")
} else {
  ## si no hay que especificar las direcciones manualmente para
  ## a) directorio de trabajo (puede ser un directorio temporal)
  work.dir <- readline(prompt="Enter working directory: ")
  ## b) dirección local del repositorio:
  script.dir <- readline(prompt="Enter path to script directory: ")
}

system(sprintf("mkdir -p %s",work.dir))
setwd(work.dir)

## This will run the markdown document with R script to produce the output pdf

mi.arch <- 'Analisis_conjunto.Rmd'
rmarkdown::render(sprintf("%s/R/%s",script.dir,mi.arch),"word_document")

## preview document in linux
##system(sprintf("atril %s/R/%s &",script.dir,gsub(".Rmd",".pdf",mi.arch)))

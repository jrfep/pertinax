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

## datos de Aratinga pertinax, versión de 2019
dts <- read.csv(sprintf("%s/data/mdf_JR_15viii19.csv",script.dir))

str(dts)

## reclassificamos la variable Region para facilitar interpretación:

dts$Region <- factor(dts$Region,levels=c("main","isl"))

## Comparamos ocho modelos:
##    MODELO             FIJO              ALEATOREO              HETERO
##     f000               isla              constante               sin
##     f010               isla              isla                    sin
##     f001               isla              constante               isla
##     f011               isla              isla                    isla

##     f100               isla+long         constante               sin
##     f110               isla+long         isla                    sin
##     f101               isla+long         constante               isla
##     f111               isla+long         isla                    isla

## Variable S1_dur
## modelo nulo con efecto fijo de la isla
f000 <- lme(S1_dur~Region,dts,random=~1|LocCode/IndivGroup, method="ML")
## nulo + efecto aleatorio de isla/continente
f010 <- lme(S1_dur~Region,dts,
  random=list(LocCode=pdDiag(~Region),IndivGroup=pdDiag(~Region)), method="ML")
## nulo + heterocedasticidad
f001 <- lme(S1_dur~Region,dts,random=~1|LocCode/IndivGroup,weights=varIdent(form=~1|Region), method="ML")
## nulo + efecto aleatorio de isla/continente + heterocedasticidad
f011 <-
  lme(S1_dur~Region,dts,
    random=list(LocCode=pdDiag(~Region),IndivGroup=pdDiag(~Region)),
    weights=varIdent(form=~1|Region), method="ML")

## modelo alternativo con efecto fijo de la isla y longitud
f100 <- lme(S1_dur~Region+Long,dts,random=~1|LocCode/IndivGroup, method="ML")
## alternativo + efecto aleatorio de isla/continente
f110 <- lme(S1_dur~Region+Long,dts,
      random=list(LocCode=pdDiag(~Region),IndivGroup=pdDiag(~Region)), method="ML")
## alternativo + heterocedasticidad
f101 <- lme(S1_dur~Region+Long,dts,random=~1|LocCode/IndivGroup,weights=varIdent(form=~1|Region), method="ML")

## alternativo + efecto aleatorio de isla/continente + heterocedasticidad
f111 <-
      lme(S1_dur~Region+Long,dts,
        random=list(LocCode=pdDiag(~Region),IndivGroup=pdDiag(~Region)),
        weights=varIdent(form=~1|Region), method="ML")

anova(f000,f010,f001,f011,
  f100,f110,f101,f111)

mis.aics <- AIC(f000,f010,f001,f011,
  f100,f110,f101,f111)
cbind(mis.aics,delta.AIC=mis.aics[,2]-min(mis.aics[,2]))

f010

summary(f010)

intervals(f010)

VarCorr(f010)

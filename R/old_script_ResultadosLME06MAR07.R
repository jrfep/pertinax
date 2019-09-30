##R --vanilla
library(nlme)
library(foreign)
##library(lme4)
library(lattice)
library(ape)


## datos de Aratinga pertinax
LLC1 <- unique(read.dbf("Apertinax_S1_JRF.dbf"))
LLCT <- read.dbf("Apertinax_Tcall_JRF.dbf")
options(width=140)

s1 <- LLC1$S1__MS_
loc <- factor(LLC1$LOCALIDAD)
grp <- factor(LLC1$ID_GRUPO)
isla <- factor(LLC1$TIPO)
costa <- factor(LLC1$COSTA_VS_T)
long <- factor(LLC1$UBICACION)
lvn.s1 <- residuals(lm(log(s1)~grp))
isln <- factor(paste(isla,long))

s1.ap <- data.frame(s1,lvn.s1,loc,grp,isla,costa,long,isln)

rm(loc,grp,isla,costa,long,lvn.s1,isln,s1)

rownames(s1.ap) <- LLC1$ID_LLAMADA

## OJO: ahora son seis modelos
##    MODELO             FIJO              ALEATOREO              HETERO
##     f00               isla+long          constante               sin
##     f10               isla+long          isla                    sin
##     f20               isla+long          constante+isla          sin

##     f01               isla+long          constante               isla
##     f11               isla+long          isla                    isla
##     f21               isla+long          constante+isla          isla

f00 <- lme(s1~isla+long,s1.ap,random=~1|loc/grp)
f10 <- lme(s1~isla+long,s1.ap,
           random=list(loc=pdDiag(~isla-1),grp=pdDiag(~isla-1)))
f20 <- lme(s1~isla+long,s1.ap,
           random=list(loc=pdDiag(~isla),grp=pdDiag(~isla)))

f01 <- lme(s1~isla+long,s1.ap,random=~1|loc/grp,weights=varIdent(form=~1|isla))
f11 <-
  lme(s1~isla+long,s1.ap,
      random=list(loc=pdDiag(~isla-1),grp=pdDiag(~isla-1)),
      weights=varIdent(form=~1|isla))
f21 <-
  lme(s1~isla+long,s1.ap,
      random=list(loc=pdDiag(~isla),grp=pdDiag(~isla)),
      weights=varIdent(form=~1|isla))

anova(f00,f10,f20,f01,f11,f21)
##    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
##f00     1  7 7992.408 8025.747 -3989.204
##f10     2  9 7978.563 8021.427 -3980.281 1 vs 2 17.84551  0.0001
##f20     3  9 7996.408 8039.273 -3989.204
##f01     4  8 7968.606 8006.707 -3976.303 3 vs 4 25.80271  <.0001
##f11     5 10 7960.815 8008.442 -3970.408 4 vs 5 11.79042  0.0028
##f21     6 10 7972.606 8020.233 -3976.303

mis.aics <- AIC(f00,f10,f20,f01,f11,f21)
cbind(mis.aics,delta.AIC=mis.aics[,2]-min(mis.aics[,2]))
##    df      AIC delta.AIC
##f00  7 7992.408 31.593137
##f10  9 7978.563 17.747628
##f20  9 7996.408 35.593138
##f01  8 7968.606  7.790423
##f11 10 7960.815  0.000000
##f21 10 7972.606 11.790424

f11
##Linear mixed-effects model fit by REML
##  Data: s1.ap 
##  Log-restricted-likelihood: -3970.408
##  Fixed: s1 ~ isla + long 
##(Intercept)       islaT       longE       longO 
## 154.019803  -35.493424   -3.566749  -28.186260 

##Random effects:
## Formula: ~isla - 1 | loc
## Structure: Diagonal
##           islaI    islaT
##StdDev: 40.53988 12.98659

## Formula: ~isla - 1 | grp %in% loc
## Structure: Diagonal
##           islaI    islaT Residual
##StdDev: 22.39946 14.48688 23.38989

##Variance function:
## Structure: Different standard deviations per stratum
## Formula: ~1 | isla 
## Parameter estimates:
##        I         T 
##1.0000000 0.7823097 
##Number of Observations: 869
##Number of Groups: 
##         loc grp %in% loc 
##          15          203 

summary(f11)
##Linear mixed-effects model fit by REML
## Data: s1.ap
##       AIC      BIC    logLik
##  7960.815 8008.442 -3970.408

##Random effects:
## Formula: ~isla - 1 | loc
## Structure: Diagonal
##           islaI    islaT
##StdDev: 40.53988 12.98659

## Formula: ~isla - 1 | grp %in% loc
## Structure: Diagonal
##           islaI    islaT Residual
##StdDev: 22.39946 14.48688 23.38989

##Variance function:
## Structure: Different standard deviations per stratum
## Formula: ~1 | isla
## Parameter estimates:
##        I         T
##1.0000000 0.7823097
##Fixed effects: s1 ~ isla + long
##                Value Std.Error  DF   t-value p-value
##(Intercept) 154.01980  20.94333 666  7.354123  0.0000
##islaT       -35.49342  20.04817  11 -1.770408  0.1043
##longE        -3.56675  14.10992  11 -0.252783  0.8051
##longO       -28.18626  11.09952  11 -2.539412  0.0275
## Correlation:
##      (Intr) islaT  longE
##islaT -0.957
##longE -0.364  0.251
##longO -0.398  0.251  0.297

##Standardized Within-Group Residuals:
##        Min          Q1         Med          Q3         Max
##-3.12349139 -0.51179000 -0.09907885  0.33854180  4.79373783

##Number of Observations: 869
##Number of Groups:
##         loc grp %in% loc
##          15          203

intervals(f11)
##Approximate 95% confidence intervals

## Fixed effects:
##                lower       est.      upper
##(Intercept) 112.89690 154.019803 195.142703
##islaT       -79.61914 -35.493424   8.632290
##longE       -34.62247  -3.566749  27.488974
##longO       -52.61614 -28.186260  -3.756375

## Random Effects:
##  Level: loc 
##              lower     est.    upper
##sd(islaI) 19.522321 40.53988 84.18477
##sd(islaT)  6.018961 12.98659 28.02002
##  Level: grp 
##             lower     est.    upper
##sd(islaI) 17.39979 22.39946 28.83573
##sd(islaT) 11.48763 14.48688 18.26920

## Variance function:
##      lower      est.    upper
##T 0.7013009 0.7823097 0.872676

## Within-group standard error:
##   lower     est.    upper 
##21.54626 23.38989 25.39129 

VarCorr(f11)
##         Variance         StdDev  
##loc =    pdDiag(isla - 1)         
##islaI    1643.4821        40.53988
##islaT     168.6514        12.98659
##grp =    pdDiag(isla - 1)         
##islaI     501.7357        22.39946
##islaT     209.8698        14.48688
##Residual  547.0871        23.38989


###########################################
## ahora probamos con un subconjunto de los datos
#############
gr.slc <- names(table(s1.ap$grp))[table(s1.ap$grp)>2]

sf01 <- lme(s1~isla+long,s1.ap,
            random=~1|loc/grp,
            weights=varIdent(form=~1|isla),
            subset=grp %in% gr.slc)
sf11 <- lme(s1~isla+long,s1.ap,
            random=list(loc=pdDiag(~isla-1),grp=pdDiag(~isla-1)),
            weights=varIdent(form=~1|isla),
            subset=grp %in% gr.slc)

anova(sf01,sf11)
##    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
##sf01     1  8 7049.779 7086.971 -3516.890                        
##sf11     2 10 7039.552 7086.042 -3509.776 1 vs 2 14.22724   8e-04

sf11

##Linear mixed-effects model fit by REML
##  Data: s1.ap 
##  Subset: grp %in% gr.slc 
##  Log-restricted-likelihood: -3509.776
##  Fixed: s1 ~ isla + long 
##(Intercept)       islaT       longE       longO 
## 154.777262  -38.747113   -6.412634  -26.702212 

##Random effects:
## Formula: ~isla - 1 | loc
## Structure: Diagonal
##           islaI    islaT
##StdDev: 46.72079 12.75793

## Formula: ~isla - 1 | grp %in% loc
## Structure: Diagonal
##           islaI    islaT Residual
##StdDev: 20.17996 12.22285 23.18651

##Variance function:
## Structure: Different standard deviations per stratum
## Formula: ~1 | isla 
## Parameter estimates:
##        I         T 
##1.0000000 0.7719403 
##Number of Observations: 776
##Number of Groups: 
##         loc grp %in% loc 
##          15          110 

intervals(sf11)
##Approximate 95% confidence intervals

## Fixed effects:
##                lower       est.      upper
##(Intercept) 108.62391 154.777262 200.930616
##islaT       -88.73998 -38.747113  11.245753
##longE       -37.46733  -6.412634  24.642065
##longO       -51.03628 -26.702212  -2.368144

## Random Effects:
##  Level: loc 
##              lower     est.    upper
##sd(islaI) 22.399181 46.72079 97.45145
##sd(islaT)  5.682279 12.75793 28.64428
##  Level: grp 
##              lower     est.    upper
##sd(islaI) 15.327976 20.17996 26.56782
##sd(islaT)  9.610133 12.22285 15.54588

## Variance function:
##    lower      est.    upper
##T 0.69292 0.7719403 0.859972

## Within-group standard error:
##   lower     est.    upper 
##21.36865 23.18651 25.15902 




############################################################################
################## AHORA TCALL


##LLCT
tcall <- LLCT$TCALL
loc <- factor(LLCT$LOC)
grp <- factor(LLCT$GRP)
isla <- factor(LLCT$TIPO)
costa <- factor(LLCT$COSTA)
long <- factor(LLCT$UBICACION)
lvn.tcall <- residuals(lm(log(tcall)~grp))

tcall.ap <- data.frame(tcall,lvn.tcall,loc,grp,isla,costa,long)
rownames(tcall.ap) <- LLCT$LLMD

rm(loc,grp,isla,costa,long,lvn.tcall,tcall)

f00 <- lme(tcall~isla+long,tcall.ap,
           random=~1|grp)
f10 <- lme(tcall~isla+long,tcall.ap,
            random=list(grp=pdDiag(~isla-1)))
f20 <- lme(tcall~isla+long,tcall.ap,
            random=list(grp=pdDiag(~isla)))
f01 <- lme(tcall~isla+long,tcall.ap,
           random=~1|grp,
           weights=varIdent(form=~1|isla))
f11 <- lme(tcall~isla+long,tcall.ap,
            random=list(grp=pdDiag(~isla-1)),
           weights=varIdent(form=~1|isla))
f21 <- lme(tcall~isla+long,tcall.ap,
            random=list(grp=pdDiag(~isla)),
           weights=varIdent(form=~1|isla))

anova(f00,f10,f20,f01,f11,f21)
##   Model df      AIC      BIC    logLik   Test    L.Ratio p-value
##f00     1  6 10772.68 10801.26 -5380.340
##f10     2  7 10774.61 10807.95 -5380.304 1 vs 2 0.07275846  0.7874
##f20     3  7 10774.61 10807.95 -5380.304
##f01     4  7 10774.29 10807.62 -5380.143
##f11     5  8 10776.27 10814.37 -5380.136 4 vs 5 0.01483706  0.9031
##f21     6  8 10776.27 10814.37 -5380.136

mis.aics <- AIC(f00,f10,f20,f01,f11,f21)
cbind(mis.aics,delta.AIC=mis.aics[,2]-min(mis.aics[,2]))
##    df      AIC delta.AIC
##f00  6 10772.68  0.000000
##f10  7 10774.61  1.927242
##f20  7 10774.61  1.927240
##f01  7 10774.29  1.605514
##f11  8 10776.27  3.590677
##f21  8 10776.27  3.590677



f00
##Linear mixed-effects model fit by REML
##  Data: tcall.ap 
##  Log-restricted-likelihood: -5380.34
##  Fixed: tcall ~ isla + long 
##(Intercept)       islaT       longE       longO 
##  306.08868   -44.23445     7.67765   -11.90434 

##Random effects:
## Formula: ~1 | grp
##        (Intercept) Residual
##StdDev:    68.44307 109.0788

##Number of Observations: 869
##Number of Groups: 203 

varcomp(f00)/sum(varcomp(f00))
##   grp       Within
## .2824       .71750

intervals(f00)
##Approximate 95% confidence intervals

## Fixed effects:
##                lower      est.     upper
##(Intercept) 266.01191 306.08868 346.16546
##islaT       -75.07301 -44.23445 -13.39589
##longE       -32.27602   7.67765  47.63132
##longO       -47.57036 -11.90434  23.76169
##attr(,"label")
##[1] "Fixed effects:"

## Random Effects:
##  Level: grp 
##                   lower     est.    upper
##sd((Intercept)) 56.31323 68.44307 83.18566

## Within-group standard error:
##   lower     est.    upper 
##103.4044 109.0788 115.0645 

f01
##Linear mixed-effects model fit by REML
##  Data: tcall.ap 
##  Log-restricted-likelihood: -5380.143
##  Fixed: tcall ~ isla + long 
##(Intercept)       islaT       longE       longO 
## 306.119572  -44.211597    7.515407  -11.918170 

##Random effects:
## Formula: ~1 | grp
##        (Intercept) Residual
##StdDev:    68.36268 106.9565

##Variance function:
## Structure: Different standard deviations per stratum
## Formula: ~1 | isla 
## Parameter estimates:
##       I        T 
##1.000000 1.034243 
##Number of Observations: 869
##Number of Groups: 203 

VarCorr(f01)
##grp = pdLogChol(1) 
##            Variance  StdDev   
##(Intercept)  4673.457  68.36268
##Residual    11439.685 106.95646

intervals(f01)

##Approximate 95% confidence intervals

## Fixed effects:
##                lower       est.     upper
##(Intercept) 265.99655 306.119572 346.24260
##islaT       -75.02809 -44.211597 -13.39511
##longE       -32.55621   7.515407  47.58702
##longO       -47.73360 -11.918170  23.89726
##attr(,"label")
##[1] "Fixed effects:"

## Random Effects:
##  Level: grp 
##                   lower     est.    upper
##sd((Intercept)) 56.20942 68.36268 83.14365

## Variance function:
##      lower     est.    upper
##T 0.9345367 1.034243 1.144588
##attr(,"label")
##[1] "Variance function:"

## Within-group standard error:
##    lower      est.     upper 
## 98.81424 106.95646 115.76960 



###########################################
## ahora probamos con un subconjunto de los datos
#############

gr.slc <- names(table(tcall.ap$grp))[table(tcall.ap$grp)>2]


sf00 <- lme(tcall~isla+long,tcall.ap,
           random=~1|grp,
           subset=grp %in% gr.slc)


sf01 <- lme(tcall~isla+long,tcall.ap,
           random=~1|grp,
           weights=varIdent(form=~1|isla),
           subset=grp %in% gr.slc)

anova(sf00,sf01)
##     Model df      AIC      BIC    logLik   Test   L.Ratio p-value
##sf00     1  6 9570.321 9598.214 -4779.160                         
##sf01     2  7 9571.409 9603.951 -4778.704 1 vs 2 0.9119915  0.3396


sf00
##Linear mixed-effects model fit by REML
##  Data: tcall.ap 
##  Subset: grp %in% gr.slc 
##  Log-restricted-likelihood: -4779.16
##  Fixed: tcall ~ isla + long 
##(Intercept)       islaT       longE       longO 
##  309.70716   -43.33023   -12.99123    -6.74032 

##Random effects:
## Formula: ~1 | grp
##        (Intercept) Residual
##StdDev:    62.64469 107.4750

##Number of Observations: 776
##Number of Groups: 110 

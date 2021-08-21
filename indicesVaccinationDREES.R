# Initializations

source("usefulFunctions.R")

library(RColorBrewer)
cols <- c("#B10029", "#06417E", "#3682D1", "#2AD29B")
names(cols) <- c("Non-vaccinés", "Primo dose récente", "Primo dose efficace", "Vaccination complète")

# Load datasets
dat3 <- read.csv("data/covid-19-resultats-issus-des-appariements-entre-si-vic-si-dep-et-vac-si_2021-08-20.csv", sep = ";")
dat2 <- read.csv("data/covid-19-resultats-issus-des-appariements-entre-si-vic-si-dep-et-vac-si_2021-08-13.csv", sep = ";")
dat1 <- read.csv("data/covid-19-resultats-issus-des-appariements-entre-si-vic-si-dep-et-vac-si_2021-08-08.csv", sep = ";")

head(dat3)
head(dat2)

# Function to reformat the data
reformatData <- function(dat){
  # Rename columns ("+" is lost)
  nnames <- c("date", "vac_statut", "nb_PCR", "nb_PCR_sympt", "nb_PCRpos", "nb_PCRpos_sympt", "HC", "HC_PCRpos", "SC", "SC_PCRpos", "DC", "DC_PCRpos", "effectif.J.7")
  # Check names
  print(cbind(names(dat), nnames))
  # Assign names
  names(dat) <- nnames

  # Sort by date
  dat <- dat[order(dat$date), ]

  # Quality check
  print(table(dat$date))
  
  # Vaccine statuses
  vacstat <- unique(dat$vac_statut)

  # New datasets for each category
  dat_novac <- dat[dat$vac_statut == "Non-vaccinés", ]
  dat_primorec <- dat[dat$vac_statut == "Primo dose récente", ]
  dat_primoeff <- dat[dat$vac_statut == "Primo dose efficace", ]
  dat_vaccompl <- dat[dat$vac_statut == "Vaccination complète", ]

  # "Ensemble" does not always exist in the dataset
  if(is.element("Ensemble", vacstat)){
    dat_tot <- dat[dat$vac_statut == "Ensemble", ]
  }else{
    dat_tot <- dat_novac
    dat_tot$vac_statut <- "Ensemble"
    dat_tot[, 3:13] <- dat_novac[, 3:13] + dat_primorec[, 3:13] + dat_primoeff[, 3:13] + dat_vaccompl[, 3:13]
  }
  
  # Compute sum of 1+14j and 2
  dat_vac <- dat_novac
  dat_vac$vac_statut <- "1eff et 2"
  dat_vac[, 3:13] <- dat_primoeff[, 3:13] + dat_vaccompl[, 3:13]

  # Export as list
  list(dat_novac = dat_novac, 
       dat_primoeff = dat_primoeff, 
       dat_primorec = dat_primorec, 
       dat_vaccompl = dat_vaccompl, 
       dat_vac = dat_vac, 
       dat_tot = dat_tot)
}

dats1 <- reformatData(dat1)
dats2 <- reformatData(dat2)
dats3 <- reformatData(dat3)

names(dats1)

dats1$dat_tot

col1 <- "#5A0DAC"
col2 <- "#B30097"
col3 <- "#FEC400"
# C0F400
# 2318B1

pch3 <- 1
pch2 <- 2
pch1 <- 3

## PCR

tits <- c("dat_tot", "dat_vac", "dat_novac", "dat_primoeff", "dat_primored", "dat_vaccompl")
names(tits) <- c("Ensemble", "Primo dose efficace et Vaccination complète", "Non-vaccinés", "Primo dose récente", "Primo dose efficace", "Vaccination complète")

par(las = 1)
compareDataPlot <- function(tit, column, addLines = FALSE){
  dataset <- tits[tit]
  par(mar = c(3, 3, 3, 2))
  plot(as.Date(dats3[[dataset]]$date), dats3[[dataset]][, column], type = "o", xlab = "date", ylab = column, col = col3, main = paste0(tit, ", ", column), pch = pch3)
  
  if(addLines){
    for(i in 0:100){
      abline(h = i, col = gray(0.95))
    }
  }
  lines(as.Date(dats2[[dataset]]$date), dats2[[dataset]][, column], type = "o", lty = 2, col = col2, pch = pch2)
  lines(as.Date(dats1[[dataset]]$date), dats1[[dataset]][, column], type = "o", lty = 2, col = col1, pch = pch1)
  
  legend("topleft", col = c(col3, col2, col1), legend = c("2021-08-20", "2021-08-13", "2021-08-08"), lty = c(1, 2, 2), title = "Date dataset", bty = "n")
  axis(4)
}

compareDataPlot("Ensemble", "nb_PCR")
compareDataPlot("Ensemble", "nb_PCRpos")
compareDataPlot("Ensemble", "SC_PCRpos")
compareDataPlot("Ensemble", "effectif.J.7")

names(dats1[["dat_novac"]])
compareDataPlot("Vaccination complète", "nb_PCRpos")
compareDataPlot("Vaccination complète", "SC_PCRpos", addLines = TRUE)
compareDataPlot("Vaccination complète", "HC_PCRpos")
compareDataPlot("Vaccination complète", "effectif.J.7")

compareDataPlot("Non-vaccinés", "SC_PCRpos")
compareDataPlot("Non-vaccinés", "HC_PCRpos")
compareDataPlot("Non-vaccinés", "effectif.J.7")

xx <- as.Date(dat_novac$date)

colAll_compl <- "#00CE8B"
colPos_compl <- "#58ED00"

colAll_novacc <- "#FF5600"
colPos_novacc <- "#EA0037"

propHosp <- function(col, dataset, scale = 10^6, pos = 7){
  # col is column for which we want to compute the proportions of hospitalizations
  # dataset is the ... dataset :-)
  # scale is the population scale (by how many)
  # pos is the position of the sliding window
  w1 <- dataset[, col] / dataset$effectif.J.7 * scale
  w2 <- sliding.window(dataset[, col], pos = pos, FUN = sum) / sliding.window(dataset$effectif.J.7, pos = pos, FUN = mean) * scale
  w3 <- sliding.window(w1, pos = pos, FUN = sum)
  w4 <- sliding.window(dataset[, col], pos = pos, FUN = sum) / dataset$effectif.J.7 * scale
  list(w1, w2, w3, w4)
}

computeRisk <- function(col, dataset, scale = 10^6){
  sliding.window(dataset[, col], pos = 7, FUN = sum) / sliding.window(dataset$effectif.J.7, pos = 7, FUN = mean) * scale
}

#-----------------------

# Function to compute Risk Ratio
computeRR <- function(col, dataset1, dataset2){
  r1 <- sliding.window(dataset1[, col], pos = 7, FUN = sum) / sliding.window(dataset1$effectif.J.7, pos = 7, FUN = mean)
  
  r2 <- sliding.window(dataset2[, col], pos = 7, FUN = sum) / sliding.window(dataset2$effectif.J.7, pos = 7, FUN = mean)
  r1/r2
}

#-----------------------

# Plot the RR for a specific column, comparing the datasets
plotCompareRR <- function(col = "SC_PCRpos", dat2 = "dat_vaccompl", addDate = TRUE, tt = "2021-07-25", ...){
  # col     Column to be plotted
  # dat2    Comparison dataset (either full vaccinated, or like vaximpact also those with 1 old injection)
  # addDate Whether to add a vertical line to identify a date
  # tt      Date to be identified (could be multiple ones in the future?)
  
  x1 <- as.Date(dats1[["dat_novac"]]$date)
  x2 <- as.Date(dats2[["dat_novac"]]$date)
  x3 <- as.Date(dats3[["dat_novac"]]$date)
  rr1 <- computeRR(col, dats1[["dat_novac"]], dats1[[dat2]])
  rr2 <- computeRR(col, dats2[["dat_novac"]], dats2[[dat2]])
  rr3 <- computeRR(col, dats3[["dat_novac"]], dats3[[dat2]])
  
  # Initialize plot
  ymax <- max(c(rr1, rr2, rr3), na.rm = TRUE)
  par(mar = c(3, 3, 0.5, 2), mgp = c(1.2, 0.25, 0), tck = -0.01)
  plot(range(c(x1, x2, x3)) + c(6, 0), c(0, ymax), 
       xlab = "Date", ylab = paste("RR,", col), type = "n")
  for(i in 1:ymax) abline(h = i, col = gray(0.95))
  points(x3, rr3, col = col3, type = "o", pch = pch3)
  points(x2, rr2, col = col2, type = "o", pch = pch2, lty = 2)
  points(x1, rr1, col = col1, type = "o", pch = pch1, lty = 2)
  
  legend("topleft", col = c(col3, col2, col1), legend = c("2021-08-20", "2021-08-13", "2021-08-08"), lty = c(1, 2, 2), title = "Date dataset", bty = "n", pch = c(pch3, pch2, pch1))
  axis(4)
  if(addDate){
    axis(1, at = as.Date(tt), labels = format.Date(tt, "%b %d"), lwd = 0, lwd.ticks = 1)
    abline(v = as.Date(tt))
  }
}

#-----------------------

plotCompareRR()  
plotCompareRR("DC_PCRpos")  
plotCompareRR("nb_PCR_sympt")  
plotCompareRR("nb_PCRpos", addDate = FALSE)  
plotCompareRR("SC_PCRpos", addDate = FALSE)  



### OLD

computeRR("SC_PCRpos", dats2[["dat_novac"]], dats2[["dat_vaccompl"]])
computeRR("DC_PCRpos", dats2[["dat_novac"]], dats2[["dat_vaccompl"]])

n <- length(xx)

# Vaccination complete
ph_compl_HC <- propHosp("HC", dat_vaccompl)

ph_compl_HCpos <- propHosp("HC_PCRpos", dat_vaccompl)
ph_compl_SCpos <- propHosp("SC_PCRpos", dat_vaccompl)
ph_compl_symp <- propHosp("nb_PCRpos_sympt", dat_vaccompl)

# Vaccination 1 et 2
ph_12_HC <- propHosp("HC", dat_vac)
ph_12_HCpos <- propHosp("HC_PCRpos", dat_vac)
ph_12_SCpos <- propHosp("SC_PCRpos", dat_vac)
ph_12_symp <- propHosp("nb_PCRpos_sympt", dat_vac)


round(c(ph_compl_HCpos[[1]][n], ph_compl_HCpos[[2]][n], ph_compl_HCpos[[3]][n], ph_compl_HCpos[[4]][n]), 1)

ph_novac_HC <- propHosp("HC", dat_novac)
ph_novac_HCpos <- propHosp("HC_PCRpos", dat_novac)
ph_novac_SCpos <- propHosp("SC_PCRpos", dat_novac)
ph_novac_symp <- propHosp("nb_PCRpos_sympt", dat_novac)

round(c(ph_novac_HCpos[[1]][n], ph_novac_HCpos[[2]][n], ph_novac_HCpos[[3]][n], ph_novac_HCpos[[4]][n]), 1)


max(unlist(ph_novac_HC), na.rm = TRUE)
plot(xx, ph_compl_HC[[1]], col = colAll_compl, xlab = "date", ylab = "admissions", pch = 2)
lines(xx, ph_compl_HC[[2]], col = colAll_compl)
lines(xx, ph_compl_HC[[3]], col = colAll_compl, lty = 2, lwd = lwdd)

points(xx, v1, col = colPos_compl)
lines(xx, v2, col = colPos_compl, lwd = 1)
lines(xx, v3, col = colPos_compl, lty = 2, lwd = lwdd)

colHC <- "#EA0037"
colsymp <- "#00AA72"

plot(xx, ph_novac_HCpos[[3]] / ph_compl_HCpos[[3]], ylim = c(0, 15), 
     ylab = "Rapport entrées non vaccinés / totalement vaccinés", pch = 16, col = colHC, 
     xlab = "date", 
     yaxs = "i", xaxs = "i", frame.plot = FALSE, xlim = as.Date(c("2021-06-01", "2021-08-01")), main= "Formule b)")
for(i in 1:15){
  abline(h = i, col = gray(0.9))
}
#points(xx, ph_novac_HCpos[[2]] / ph_compl_HCpos[[2]], ylim = c(0, 15), 
#    ylab = "Rapport entrées non vaccinés / totalement vaccinés", col = "gray")
colSC <- "#FF9900"
points(xx, ph_novac_SCpos[[3]] / ph_compl_SCpos[[3]], col = colSC, pch = 17)
#
legend("topleft", col = c(colSC, colHC), legend = c("Soins critiques", "Hospitalisation conventionnelle"), pch = c(17, 16), bty = "n")
axis(4)

## Moyenne glissante avant ratio
plot(xx, ph_novac_HCpos[[2]] / ph_compl_HCpos[[2]], ylim = c(0, 15), 
     ylab = "Rapport entrées non vaccinés / vaccinés", pch = 16, col = colHC, 
     xlab = "date", 
     yaxs = "i", xaxs = "i", frame.plot = FALSE, xlim = as.Date(c("2021-06-01", "2021-08-03")), main = "")
for(i in 1:15){
  abline(h = i, col = gray(0.9))
}
points(xx, ph_novac_symp[[2]] / ph_compl_symp[[2]], col = colsymp, pch = 15)
points(xx, ph_novac_symp[[2]] / ph_12_symp[[2]], col = colsymp, pch = 0)
points(xx, ph_novac_HCpos[[2]] / ph_12_HCpos[[2]], col = colHC, pch = 1)
points(xx, ph_novac_SCpos[[2]] / ph_compl_SCpos[[2]], col = colSC, pch = 17)
points(xx, ph_novac_SCpos[[2]] / ph_12_SCpos[[2]], col = colSC, pch = 2)
#
legend("topleft", col = c(colSC, colHC, colsymp, colSC, colHC, colsymp), legend = c("Soins critiques, vs totalement vaccinés", "Hospitalisation conventionnelle, vs totalement vaccinés", "Cas symptomatiques, vs totalement vaccinés", "Soins critiques, vs vaccinés 1 efficace et 2", "Hospitalisation conventionnelle, vs vaccinés 1 efficace et 2", "Cas symptomatiques, vs vaccinés 1 efficace et 2"), pch = c(17, 16, 15, 2, 1, 0), bty = "n")
axis(4)

(round(ph_novac_HCpos[[2]]) / round(ph_compl_HCpos[[2]]))
(ph_novac_HCpos[[3]] / ph_compl_HCpos[[3]])

(ph_novac_HCpos[[3]] / ph_12_HCpos[[3]])

## CI with epitools
library(epitools)

RRtable <- matrix(c(1017,2260,165,992),nrow = 2, ncol = 2)
RRtable
riskratio.wald(RRtable)

# This is where the orientation of the contingency table is critical, i.e., with the unexposed (reference) group in the first row and the subjects without the outcome in the first column.

scale <- 10*10^6
col <- "SC_PCRpos"
t <- nrow(dat_vac)
datv <- dat_vaccompl
scale <- 10^6
pos <- 7

slideSum <- function(v, t){
  sum(v[(t - 7 + 1):t])
}

# Contigency table
m <- matrix(c(slideSum(datv[, "effectif.J.7"] - datv[, col], t), 
              slideSum(datv[, col], t), 
              slideSum(dat_novac[, "effectif.J.7"] - dat_novac[t, col], t), 
              slideSum(dat_novac[, col], t)), byrow = TRUE, ncol = 2)
rr <- epitools::riskratio(m, method = "small")
rr$measure[2, ]







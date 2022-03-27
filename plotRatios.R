
# Initializations ####

dlData <- TRUE # Whether to download the data

source("usefulFunctions.R") # Load sliding average function

# Load data ####

# Source: https://www.data.gouv.fr/fr/datasets/synthese-des-indicateurs-de-suivi-de-lepidemie-covid-19/
URL2 <- "https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617"

dataFile2 <- "data/FranceCase_dateResultat.csv"
if(dlData){
  download.file(URL2, dataFile2)
}

dat <- read.csv(dataFile2, stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)

tail(dat, 10) # Check the dates

# Check that all dates are consecutive (necessary to compute ratios)
stopifnot(all(diff(dat$date) == 1))

# Load population size used by SPF to convert case numbers to incidence per 100'000
pops <- read.csv("../data/SPFpopFra2020_90.csv")
popFR <- pops[pops$clage_vacsi == 0, "pop"]

# Load holidays (jours feries), to identify days on which data can be unusual
feries <- read.csv("data/jours_feries_metropole.csv")
feries$date <- as.Date(feries$date)
dat$ferie <- is.element(dat$date, feries$date)

#........................................................................................
# Compute ratios etc. ####
n1 <- nrow(dat)
n2 <- n1 - 3 # Max number of values of cases by test date

# Compute positive cases on date of result (need a 1-day offset)
dat$P_dateResult <- c(tail(dat$conf_j1, -1), NA)
# Rename positive results on date of test
dat$P_dateTest <- dat$pos

# Set lines for jours feries to NA
dat[which(dat$ferie), c("P_dateResult", "P_dateTest")] <- NA

# Compute 7-d incidence; center at the middle of the interval
# (7 * mean instead of sum to account for NAs)
# (restricted to 1:n2, where we have data)
dat$incid7J <- c(7 * sliding.window(dat$P_dateTest[1:n2], FUN = mean) / popFR * 10^5, rep(NA, 3))

# Compute ratios for the three types of case numbers
dat$ratio7_dateRes <- NA
dat$ratio7_dateRes[8:n1] <- dat$P_dateResult[8:n1] / dat$P_dateResult[8:n1 - 7]

dat$ratio7_dateTest <- NA
dat$ratio7_dateTest[8:n2] <- dat$P_dateTest[8:n2] / dat$P_dateTest[8:n2 - 7]

dat$ratio7_incid <- NA
dat$ratio7_incid[8:n1] <- dat$incid7J[8:n1] / dat$incid7J[8:n1 - 7]

# Remove the last line because we do not have case data on the last day
dat <- head(dat, -1)

# Plot settings
# Colors chosen using Paletton 
# https://paletton.com/#uid=30a0u0kprrvfFAukHtUsrmuw4hF
colCroiss <- "#DB512D"
colDecroiss <- "#22698B"

par(las = 1)

# Define data frame to prepare the horizontal lines of doubling times
doublings <- data.frame(times = c(1, 2, 4, 10/7, 5/7),
                        namesFR = c("1 semaine", "2 semaines", "4 semaines", "10 jours", "5 jours"))

minDate <- as.Date("2021-12-15") # minimum date in the plot
maxDate <- max(dat$date) # max date in the plot
ylm <- c(0.5, 3) # y range
lang <- "FR" # Language (EN is work in progress)

fname <- "pics/ratiosFrance.png" # output name
png(filename = fname, width = 10.66, height = 6, units = "in", res = 300)

par(mgp = c(2.5, 0.5, 0.5), las = 1, 
    mar = c(3, 8, 3, 8), 
    cex = 1)
# Initialize plot figure
plot(dat$date, dat$ratio7_dateTest, pch = 16, xlim = c(minDate, maxDate), ylim = ylm, log = "y", type = "n", axes = FALSE, xlab = "", ylab = "")

# Axes labels
txt <- ifelse(lang == "FR", 
              "cas(j)/cas(j-7), échelle log", 
              "cases(j)/cases(j-7), log scale")
mtext(txt, side = 2, line = 2, las = 3)
mtext("date", side = 1, line = 0.5)


limPlot <- par("usr") # limits of the plot in user coordinates
limPlot[2] <- maxDate + 1
op <- 0.25 # Opacity parameter

# Add Shading
# Decroissance
polygon(x = as.Date(c(limPlot[1], limPlot[2], limPlot[2], limPlot[1], limPlot[1]), origin = "1970-01-01"), 
        y = c(1, 1, ylm[1], ylm[1], 1), 
        border = NA, col = adjustcolor(colDecroiss, op))
# Croissance
polygon(x = as.Date(c(limPlot[1], limPlot[2], limPlot[2], limPlot[1], limPlot[1]), origin = "1970-01-01"), 
        y = c(1, 1, ylm[2], ylm[2], 1), 
        border = NA, col = adjustcolor(colCroiss, op))

# Add horizontal lines for doubling times
par(xpd = FALSE)
lines(as.Date(limPlot[1:2], origin = "1970-01-01"), rep(1, 2), lwd = 2)
#abline(h = 1, lwd = 2)
colline <- "white" # Line color
lwdd <- 1.5 # Line width
dxx <- 1 # offset for labels
cexx <- 1/1.2
for(tt in doublings$times){
  # Horizontal lines for x and / doubling/halving times
  abline(h = exp(log(2) / tt), col = colline, lwd = lwdd)
  abline(h = exp(-log(2) / tt), col = colline, lwd = lwdd)
  
  # Add text legend
  par(xpd = TRUE)
  text(limPlot[2] + dxx, y = exp(log(2) / tt), labels = doublings[doublings$times == tt, "namesFR"], col = colCroiss, adj = 0, cex = cexx)
  if(tt >= 1){
    text(limPlot[2] + dxx, y = exp(- log(2) / tt), labels = doublings[doublings$times == tt, "namesFR"], col = colDecroiss, adj = 0, cex = cexx)
  }
  par(xpd = FALSE)
}

# Add titles for the two halves
par(xpd = TRUE)
text(limPlot[2] + dxx, y = exp(log(2) / 8), labels = "Cas x2 en...", adj = 0, cex = 1.2 * cexx, col = colCroiss)
text(limPlot[2] + dxx, y = exp(- log(2) / 8), labels = "Cas ÷2 en...", adj = 0, cex = 1.2 * cexx, col = colDecroiss)
par(xpd = FALSE)

# Add mention direction in shaded areas
cexlabshade <- 0.9
dxl <- 0.5
txt <- ifelse(lang == "FR", 
              "   Le nombre de cas décroît",
              "   Cases decrease")
text(as.Date((limPlot[1]+limPlot[2])/2, origin = "1970-01-01"), ylm[1], adj = c(0.5, 0 - dxl), labels = txt, col = colDecroiss, font = 1, cex = cexlabshade)
txt <- ifelse(lang == "FR", 
              "   Le nombre de cas croît",
              "   Cases increase")
text(as.Date((limPlot[1]+limPlot[2])/2, origin = "1970-01-01"), ylm[2], adj = c(0.5, 1 + dxl), labels = txt, col = colCroiss, font = 1, cex = cexlabshade)


# Add axes
tckk <- -0.02 # Ticks
cexx <- 0.8 # Font size
days <- seq(as.Date("2020-01-01"), as.Date(maxDate)+30, by = "day")
dateFormat <- "%d/%m"
adj <- 0.5

# axis(1, at = as.Date(range(days)), labels = rep("", 2), tck = 0, pos = ylm[1])

mnths <- seq(as.Date("2020-01-01"), as.Date(maxDate), by = "month")
axis(1, at = mnths, labels = format(mnths, format = dateFormat), las = 1, cex.axis = cexx, tck = tckk, lwd = 0, lwd.ticks = 1, pos = ylm[1], adj = adj)
axis(1, at = mnths + 14, labels = format(mnths + 14, format = dateFormat), las = 1, cex.axis = cexx, tck = tckk, lwd = 0, lwd.ticks = 1, pos = ylm[1], adj = adj)

axis(2, at = c(seq(0.5, 1, by = 0.1), seq(1.2, 3, by = 0.2)), tck = -.01, pos = as.Date(limPlot[1], origin = "1970-01-01"), cex.axis = cexx)

# Plot parameters for the points and lines (Test and Res)
pchTest <- 16
pchRes <- 1

colTest <- gray(0)
colRes <- gray(0.5)

# Curve for 7-d average of the ratios
opC <- 0.7
lines(dat$date[1:n2], sliding.window(dat$ratio7_dateTest[1:n2]), col = adjustcolor(colTest, opC))
lines(dat$date, sliding.window(dat$ratio7_dateRes), col = adjustcolor(colRes, opC))

# Points!
points(dat$date, dat$ratio7_dateTest, pch = pchTest, col = colTest)
points(dat$date, dat$ratio7_dateRes, pch = pchRes, col = colRes, cex = 1.1)

# Legend of the points
legend(as.Date((limPlot[1]+limPlot[2])/2, origin = "1970-01-01"), ylm[2], horiz = TRUE, 
       pch = c(pchTest, pchRes), 
       col = c(colTest, colRes), 
       legend = c("en date de prélévement", "en date de résultat"), yjust = 0.1, xjust = 0.5, box.col = gray(0, 0), box.lwd = 0, pt.cex = 1, cex = cexx, bty = "n")

# Title of the plot
txt <- ifelse(lang == "FR", 
              "Ratio du nombre de cas Covid-19 détectés en France 
d'une semaine à l'autre, jusqu'au ", 
              "Week-to-week ratio of the number of detected Covid-19 cases in France
(by sampling date), until ")
mtext(paste0(txt, format(as.Date(maxDate), format = "%d/%m/%Y")), font = 1, cex = 1.1, line = 0.8, adj = 0.5)

# Credits
txt <- ifelse(lang == "FR", 
              "Inspiré des figures de @BristOliver 
Données: https://www.data.gouv.fr/fr/datasets/synthese-des-indicateurs-de-suivi-de-lepidemie-covid-19/
Code: https://github.com/flodebarre/covid_indicateurs/blob/main/plotRatios.R", 
              "@flodebarre, Inspired by @BristOliver
Data: https://www.data.gouv.fr/fr/datasets/synthese-des-indicateurs-de-suivi-de-lepidemie-covid-19/
Code: https://github.com/flodebarre/covid_indicateurs/blob/main/plotRatios.R")
mtext(txt, side = 1, line = 2., adj = 0, cex = 0.5, family = "mono")

dev.off()

system(paste("open", fname))

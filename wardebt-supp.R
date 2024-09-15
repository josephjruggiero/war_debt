# Create dataframe wardebt
wardebt <- read.csv("wardebt.csv")

# TABLE A1: Replication of Lemke with Directed Dyads ---------

lemke1 <- glm(war ~ relpow + pow_prev + allies*pow_prev + 
              contig.x*pow_prev + democ_a*pow_prev + rivals*pow_prev, 
              data = wardebt, family = "binomial")
lemke2 <- glm(war ~ relpow + mil_prev + allies*mil_prev + 
              contig.x*mil_prev + democ_a*mil_prev + rivals*mil_prev, 
              data = wardebt, family = "binomial")
lemke3 <- glm(war ~ relpow + pow_prev + allies*pow_prev + 
              contig.x*pow_prev + democ_a*pow_prev + rivals*pow_prev, 
              data = subset(wardebt, original==1), family = "binomial")
lemke4 <- glm(war ~ relpow + mil_prev + allies*mil_prev + 
              contig.x*mil_prev + democ_a*mil_prev + rivals*mil_prev, 
              data = subset(wardebt, original==1), family = "binomial")


# TABLE A2: Replication of Lemke with Nondirected Dyads ---------

lemke5 <- glm(war ~ relpow + pow_abs + allies*pow_abs + contig.x*pow_abs + 
              rivals*pow_abs, data = wardebt, family = "binomial")
lemke6 <- glm(war ~ relpow + mil_abs + allies*mil_abs + contig.x*mil_abs + 
              rivals*mil_abs, data = wardebt, family = "binomial")
lemke7 <- glm(war ~ relpow + pow_abs + allies*pow_abs + contig.x*pow_abs + 
              rivals*pow_abs, data = subset(wardebt ,original==1), 
              family = "binomial")
lemke8 <- glm(war ~ relpow + mil_abs + allies*mil_abs + contig.x*mil_abs + 
              rivals*mil_abs, data = subset(wardebt, original==1), 
              family = "binomial")


# TABLE A3: Logistic Regressions on War with Consol Yields ---------

# Logistic regressions
m1 <- glm(war ~ pow_prev*rate, 
          data = wardebt, family = "binomial")
m2 <- glm(war ~ mil_prev*rate, 
          data = wardebt, family = "binomial")
m3 <- glm(war ~ pow_abs*rate, 
          data = wardebt, family = "binomial")
m4 <- glm(war ~ mil_abs*rate, 
          data = wardebt, family = "binomial")
m5 <- glm(war ~ allies + contig.x + democ_a + rivals + relpow + pow_prev*rate, 
          data = wardebt, family = "binomial")
m6 <- glm(war ~ allies + contig.x + democ_a + rivals + relpow + mil_prev*rate, 
          data = wardebt, family = "binomial")

# Constant adjustment
# Run the Lemke replications in wardebt-supp.R
# Subset data to operative observations
wardebt1 <- wardebt[!is.na(wardebt$pow_prev),]
wardebt2 <- wardebt[!is.na(wardebt$mil_prev),]
# Inferring fraction of 1s in total pop
y1 <- mean(wardebt1$war)/(1-mean(wardebt1$war))
y2 <- mean(wardebt2$war)/(1-mean(wardebt2$war))
c1 <- lemke1$coefficients[1]
c2 <- lemke2$coefficients[1]
c3 <- lemke5$coefficients[1]
c4 <- lemke6$coefficients[1]
lem12 <- -6.38  # from Lemke (2003) Table 2 models 1a and 1b
lem3 <- -6.5    # from Lemke (2003) Table 3 model 1a 
lem4 <- -6.56   # from Lemke (2003) Table 3 model 1b
x1 <- exp(lem12)*y1/(exp(c1)+y1*exp(lem12))
x2 <- exp(lem12)*y2/(exp(c2)+y2*exp(lem12))
x3 <- exp(lem3)*y1/(exp(c3)+y1*exp(lem3))
x4 <- exp(lem4)*y2/(exp(c4)+y2*exp(lem4))
# Each x is the inferred fraction of 1s in the total pop based on respective subset
# Note x3 and x4 take y1 and y2 since they run on the same subset wardebt1 and wardebt2
beta01 <- m1$coefficients[1] - log((1-x1)/x1*y1)
beta02 <- m2$coefficients[1] - log((1-x2)/x2*y2)
beta03 <- m3$coefficients[1] - log((1-x3)/x3*y1)
beta04 <- m4$coefficients[1] - log((1-x4)/x4*y2)
beta05 <- m5$coefficients[1] - log((1-x1)/x1*y1)
beta06 <- m6$coefficients[1] - log((1-x2)/x2*y2)

# Calculating adjusted standard errors for constant
# Run secalc.R
source('secalc.R')
secalc1 <- secalc(m1)
secalc2 <- secalc(m2)
secalc3 <- secalc(m3)
secalc4 <- secalc(m4)
secalc5 <- secalc(m5)
secalc6 <- secalc(m6)

# Marginal effects
# Run logitmfxRE.R and logitmfxestRE.R, and load mfx package
source('logitmfxRE.R')
source('logitmfxestRE.R')
m1_mfx <- logitmfxRE(formula = war ~ pow_prev*rate, 
                     data = wardebt, atmean = F, robust = T, recorrect = beta01)
m2_mfx <- logitmfxRE(formula = war ~ mil_prev*rate, 
                     data = wardebt, atmean = F, robust = T, recorrect = beta02)
m3_mfx <- logitmfxRE(formula = war ~ pow_abs*rate, 
                     data = wardebt, atmean = F, robust = T, recorrect = beta03)
m4_mfx <- logitmfxRE(formula = war ~ mil_abs*rate, 
                     data = wardebt, atmean = F, robust = T, recorrect = beta04)
m5_mfx <- logitmfxRE(formula = war ~ allies + contig.x + democ_a + rivals + relpow + pow_prev*rate, 
                     data = wardebt, atmean = F, robust = T, recorrect = beta05)
m6_mfx <- logitmfxRE(formula = war ~ allies + contig.x + democ_a + rivals + relpow + mil_prev*rate, 
                     data = wardebt, atmean = F, robust = T, recorrect = beta06)


# TABLE A4: Logistic regressions with Lemke Interactions ---------

a3_1 <- glm(war ~ allies*pow_prev + contig.x*pow_prev + 
            democ_a*pow_prev + rivals*pow_prev + relpow + pow_prev*rate, 
            data = wardebt, family = "binomial")
a3_2 <- glm(war ~ allies*mil_prev + contig.x*mil_prev + 
            democ_a*mil_prev + rivals*mil_prev + relpow + mil_prev*rate, 
            data = wardebt, family = "binomial")


# TABLE A5: Linear Probability Models with Consol Yields ---------

linear1 <- lm(war ~ pow_prev*rate, data = wardebt)
linear2 <- lm(war ~ mil_prev*rate, data = wardebt)
linear3 <- lm(war ~ pow_abs*rate, data = wardebt)
linear4 <- lm(war ~ mil_abs*rate, data = wardebt)
linear5 <- lm(war ~ allies + contig.x + democ_a + rivals + relpow + 
              pow_prev*rate, data = wardebt)
linear6 <- lm(war ~ allies + contig.x + democ_a + rivals + relpow + 
              mil_prev*rate, data = wardebt)


# TABLE A6: Linear Probability Models with Real Rate ---------

linear1r <- lm(war ~ pow_prev*real, data = wardebt)
linear2r <- lm(war ~ mil_prev*real, data = wardebt)
linear3r <- lm(war ~ pow_abs*real, data = wardebt)
linear4r <- lm(war ~ mil_abs*real, data = wardebt)
linear5r <- lm(war ~ allies + contig.x + democ_a + rivals + relpow + 
               pow_prev*real, data = wardebt)
linear6r <- lm(war ~ allies + contig.x + democ_a + rivals + relpow + 
               mil_prev*real, data = wardebt)


# TABLE A7: Replication of B & J's Rare Events Logit ---------

# Read BellJohnsonISQrepDDyadic.dta with foreign package and call it bell
bell <- read.dta("BellJohnsonISQrepDDyadic.dta")

# Rare events logits using Zelig package
bell1 <- zelig(war ~ powershift, data = bell, model = "relogit", cite = FALSE)
bell2 <- zelig(war ~ powershift + lndistance + contig + dem + s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3, 
               data = bell, model = "relogit", cite = FALSE)
bell3 <- zelig(war2 ~ powershift, data = bell, model = "relogit", cite = FALSE)
bell4 <- zelig(war2 ~ powershift + lndistance + contig + dem + s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3, 
               data = bell, model = "relogit", cite = FALSE)


# TABLE A8: Rare events logistic regressions on war with Consol yields ---------

# Load interest rate data LTCYUK.csv, call it intrate, and some cleaning
intrate <- read.csv("LTCYUK.csv")
intrate$year <- as.numeric(substr(intrate$DATE, 1, 4))
rf <- vector()
for(i in 1753:2016){
  rf[2017-i] <- mean(intrate[intrate$year==i,"LTCYUK"])
}
intrate$rate <- rf[2017-intrate$year]
intrate <- unique(intrate[c("year","rate")])

# Merge B & J data with interest rate data and call it bellrate
bellrate <- merge(intrate, bell, by = "year")

# Rare events logits
bellrate1 <- zelig(war ~ powershift*rate, data = bellrate, model = "relogit", cite=FALSE)
bellrate2 <- zelig(war ~ powershift*rate + lndistance + contig + dem + s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3, 
                   data = bellrate, model = "relogit", cite=FALSE)
bellrate3 <- zelig(war2 ~ powershift*rate, data = bellrate, model = "relogit", cite=FALSE)
bellrate4 <- zelig(war2 ~ powershift*rate + lndistance + contig + dem + s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3, 
                   data = bellrate, model = "relogit", cite=FALSE)


# TABLE A9: Rare events logistic regressions on war with real long-rate ---------

# Load interest rate data LTCYUK.csv, call it intrate, and some cleaning
realrate <- read.csv("a-millennium-of-macroeconomic-data-for-the-uk.csv")
realrate$year <- as.numeric(realrate$X)
realrate$realrate <- realrate$Real.long.rates
realrate <- unique(realrate[c("year","realrate")])

# Merge B & J data with interest rate data and call it bellrate
bellrealrate <- merge(realrate, bell, by = "year")

# Rare events logits
bellrealrate1 <- zelig(war ~ powershift*realrate, data = bellrealrate, model = "relogit", cite=FALSE)
bellrealrate2 <- zelig(war ~ powershift*realrate + lndistance + contig + dem + s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3, 
                       data = bellrealrate, model = "relogit", cite=FALSE)
bellrealrate3 <- zelig(war2 ~ powershift*realrate, data = bellrealrate, model = "relogit", cite=FALSE)
bellrealrate4 <- zelig(war2 ~ powershift*realrate + lndistance + contig + dem + s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3, 
                       data = bellrealrate, model = "relogit", cite=FALSE)

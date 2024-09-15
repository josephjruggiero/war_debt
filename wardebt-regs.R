# Create dataframe wardebt
wardebt <- read.csv("wardebt.csv")

# TABLE 2: Logistic Regressions on War ---------

# Logistic regressions
m1r <- glm(war ~ pow_prev*real, 
           data = wardebt, family = "binomial")
m2r <- glm(war ~ mil_prev*real, 
           data = wardebt, family = "binomial")
m3r <- glm(war ~ pow_abs*real, 
           data = wardebt, family = "binomial")
m4r <- glm(war ~ mil_abs*real, 
           data = wardebt, family = "binomial")
m5r <- glm(war ~ allies + contig.x + democ_a + rivals + relpow + pow_prev*real, 
           data = wardebt, family = "binomial")
m6r <- glm(war ~ allies + contig.x + democ_a + rivals + relpow + mil_prev*real, 
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
beta01r <- m1r$coefficients[1] - log((1-x1)/x1*y1)
beta02r <- m2r$coefficients[1] - log((1-x2)/x2*y2)
beta03r <- m3r$coefficients[1] - log((1-x3)/x3*y1)
beta04r <- m4r$coefficients[1] - log((1-x4)/x4*y2)
beta05r <- m5r$coefficients[1] - log((1-x1)/x1*y1)
beta06r <- m6r$coefficients[1] - log((1-x2)/x2*y2)

# Calculating adjusted standard errors for constant
# Run secalc.R
source('secalc.R')
secalc1r <- secalc(m1r)
secalc2r <- secalc(m2r)
secalc3r <- secalc(m3r)
secalc4r <- secalc(m4r)
secalc5r <- secalc(m5r)
secalc6r <- secalc(m6r)

# Marginal effects
# Run logitmfxRE.R and logitmfxestRE.R, and load mfx package
source('logitmfxRE.R')
source('logitmfxestRE.R')
m1r_mfx <- logitmfxRE(formula = war ~ pow_prev*real, 
                      data = wardebt, atmean = F, robust = T, recorrect = beta01r)
m2r_mfx <- logitmfxRE(formula = war ~ mil_prev*real, 
                      data = wardebt, atmean = F, robust = T, recorrect = beta02r)
m3r_mfx <- logitmfxRE(formula = war ~ pow_abs*real, 
                      data = wardebt, atmean = F, robust = T, recorrect = beta03r)
m4r_mfx <- logitmfxRE(formula = war ~ mil_abs*real, 
                      data = wardebt, atmean = F, robust = T, recorrect = beta04r)
m5r_mfx <- logitmfxRE(formula = war ~ allies + contig.x + democ_a + rivals + relpow + pow_prev*real, 
                      data = wardebt, atmean = F, robust = T, recorrect = beta05r)
m6r_mfx <- logitmfxRE(formula = war ~ allies + contig.x + democ_a + rivals + relpow + mil_prev*real, 
                      data = wardebt, atmean = F, robust = T, recorrect = beta06r)


# FIGURE 6: Interaction Effects Over Time ---------

# Interaction effects
# Fix model so it has the correct (adjusted) constant
m1ra <- m1r
m1ra$coefficients[1] <- beta01r

# Get observation-level interaction effects for m1
# Load DAMisc package
m1r_intE <- intEff(m1ra, c("pow_prev", "real"), wardebt)

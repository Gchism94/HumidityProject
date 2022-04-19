#########################################################################################################################################
# RELATIVE HUMIDITY AND BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: POWER ANALYSES OF LINEAR REGRESSIONS USED BELOW
# The script below is to determine the statistical power of our linear regressions testing the effect of humidity on nest wall properties
#########################################################################################################################################

# Power analysis for humidity & nest properties linear mixed effects models
# We used the simr package to test the power of each of our linear mixed effects models 
# 1. Assign each model to an object
# 2. Calculate the beta coefficient needed for a Cohen's d effect size of 0.1 (a small effect - Cohen 2013)
# 3. Simulate the power of our models through 200 Monte Carlo simulations

### NOTE, COLONY SIZE POWER ANALYSES NOT UPDATED YET! ###

# Global sample size for all models
n = Complete_Data_Final %>% nrow()

# Global Cohen's d effect size
d = 0.2

# Wall weight
# Linear mixed effects model 
fit <- lmer(CollWallWt ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Standard error of the Humidity factor
SE.Wt <- as.matrix(coef(summary(fit))[, "Std. Error"])

# Select the correct SE value
SE = SE.Wt[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.Wt = -d * SD

# Set beta effect size new value in model fit
fixef(fit)["Humidity"] <- Beta.Wt

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
powerSim(fit, nsim = 200, progress = FALSE)

# Wall length
# Linear mixed effects model 
fit1 <- lmer(Length ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Standard error of the Humidity factor
SE.Length <- as.matrix(coef(summary(fit1))[, "Std. Error"])

# Select the correct SE value
SE = SE.Length[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.Length = -d * SD

# Actual Cohen's d
# d.Length = as.matrix((fixef(fit1)["Humidity"])) [1] / SD

# Set beta effect size new value in model fit
fixef(fit1)["Humidity"] <- Beta.Length

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
powerSim(fit1, nsim = 200, progress = FALSE)

# Wall area
# Linear mixed effects model 
fit2 <- lmer(Area ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Standard error of the Humidity factor
SE.Area <- as.matrix(coef(summary(fit2))[, "Std. Error"])

# Select the correct SE value
SE = SE.Area[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.Area = -d * SD

# Set beta effect size new value in model fit
fixef(fit2)["Humidity"] <- Beta.Area

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
powerSim(fit2, nsim = 200, progress = FALSE)

# Wall density
# Linear mixed effects model 
fit3 <- lmer(Density ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Standard error of the Humidity factor
SE.Dens <- as.matrix(coef(summary(fit3))[, "Std. Error"])

# Select the correct SE value
SE = SE.Dens[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.Dens = d * SD

# Set beta effect size new value in model fit
fixef(fit3)["Humidity"] <- Beta.Dens

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
powerSim(fit3, nsim = 200, progress = FALSE)

# Wall composition
# Linear mixed effects model 
fit4 <- lmer(PropIIWall ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Standard error of the Humidity factor
SE.Compn <- as.matrix(coef(summary(fit4))[, "Std. Error"])

# Select the correct SE value
SE = SE.Compn[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.Compn = d * SD

# Set beta effect size new value in model fit
fixef(fit4)["Humidity"] <- Beta.Compn

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
powerSim(fit4, nsim = 200, progress = FALSE)

# Internal nest area
# Linear mixed effects model 
fit5 <- lmer(Nest.Area ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Standard error of the Humidity factor
SE.IntArea <- as.matrix(coef(summary(fit5))[, "Std. Error"])

# Select the correct SE value
SE = SE.IntArea[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.IntArea = d * SD

# Set beta effect size new value in model fit
fixef(fit5)["Humidity"] <- Beta.IntArea

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
powerSim(fit5, nsim = 200, progress = FALSE)

### NOTE, NOT UPDATED YET!
#########################################################################################################################################
# COLONY SIZE BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: POWER ANALYSES OF LINEAR MIXED EFFECTS MODELS
# The script below is to determine the statistical power of our spearman's rho correlations testing the effect of colony size on nest wall properties
#########################################################################################################################################

# Power analysis for colony size & nest properties linear mixed effects models
# We used the simr package to test the power of each of our linear mixed effects models 
# 1. Assign each model to an object
# 2. Simulate the power of our models through 200 Monte Carlo simulations and use a likelihood ratio test for effect size

# Wall weight
# Linear mixed effects models
# Brood
fitBrood <- lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount)

# Standard error of the Humidity factor
SE.BroodWt <- as.matrix(coef(summary(fitBrood))[, "Std. Error"])

# Select the correct SE value
SE = SE.BroodWt[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.BroodWt = d * SD

# Set beta effect size new value in model fit
fixef(fitBrood)["Number.Colony"] <- Beta.BroodWt

# Workers
fitWorker <- lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Standard error of the Humidity factor
SE.WorkerWt <- as.matrix(coef(summary(fitWorker))[, "Std. Error"])

# Select the correct SE value
SE = SE.WorkerWt[2]  

# Standard deviation calculation
SD = sqrt(n) * SE

# Beta coefficient
Beta.WorkerWt = d * SD

# Set beta effect size new value in model fit
fixef(fitWorker)["Number.Colony"] <- Beta.WorkerWt1

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
powerSim(fitBrood, nsim = 200, progress = FALSE)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Workers
powerSim(fitWorker, nsim = 200, progress = FALSE)

WallWtBrood

WallWtWorker

# Wall length
# Linear mixed effects model 
# Brood
fitBrood1 <- lmer(Length ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount)

# Workers
fitWorker1 <- lmer(Length ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
LengthBrood <- powerSim(fitBrood1, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

# Workers
LengthWorker <- powerSim(fitWorker1, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

LengthBrood

LengthWorker

# Wall area
# Linear mixed effects models
# Brood
fitBrood2 <- lmer(Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount)

# Workers
fitWorker2 <- lmer(Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
AreaBrood <- powerSim(fitBrood2, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

# Workers
AreaWorker <- powerSim(fitWorker2, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

AreaBrood

AreaWorker

# Wall density
# Linear mixed effects models
# Brood
fitBrood3 <- lmer(Density ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount)

# Workers
fitWorker3 <- lmer(Density ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
DensityBrood <- powerSim(fitBrood3, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

# Workers
DensityWorker <- powerSim(fitWorker3, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

DensityBrood

DensityWorker

# Wall composition
# Linear mixed effects models
# Brood
fitBrood4 <- lmer(PropIIWall ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount)

# Workers
fitWorker4 <- lmer(PropIIWall ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
CompnBrood <- powerSim(fitBrood4, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

# Workers
CompnWorker <- powerSim(fitWorker4, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

CompnBrood

CompnWorker

# Internal nest area
# Linear mixed effects models
# Brood
fitBrood5 <- lmer(Nest.Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount)

# Workers
fitWorker5 <- lmer(Nest.Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
Int.AreaBrood <- powerSim(fitBrood5, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

# Workers
Int.AreaWorker <- powerSim(fitWorker5, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

Int.AreaBrood

Int.AreaWorker
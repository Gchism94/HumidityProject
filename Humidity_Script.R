#########################################################################################################################################
# Author: GREG CHISM
# Date: Nov 2021
# email: gchism@email.arizona.edu
# Project: "Temnothorax rugatulus" ants do not change their nest walls in response to environmental humidity
# Title: Humidity, colony percentage death, porosity data grooming, visualization, and analyses
#########################################################################################################################################

# The below script was utilized to analyze and visualize data from the associated manuscript.
# The associated databases are "HumidityExperimentDatabase.csv", "SupplementalHygrometerDatabase.csv", "and"HumidMortality.csv", and "PorosityComparison.csv"

# MUST RUN THE NEXT TWO LINES (PARTICULARLY PACMAN) BEFORE ANY OTHER!
# Installing and loading packages used for graphs and analyses
install.packages("pacman") # Download package with function to load multiple packaged at once

# Loading required packages for code below. p_load() will download packages that aren't in system library
pacman::p_load(FSA,
               ggpubr,
               lme4,
               lmerTest,
               MuMIn,
               papeR,
               pwr,
               scales,
               simr,
               tidyverse)

#########################################################################################################################################
# IMPORTING THE REQUIRED DATASETS TO RUN THE BELOW SCRIPTS
#########################################################################################################################################
# Main database with nest wall properties
HumidityExperimentalDatabase <- read.csv("HumidityExperimentalDatabase.csv")

# Supplemental database with relative humidity (%) and temperature (celcius) values from experimental container
SupplementalHygrometerDatabase <- read.csv("SupplementalHygrometerDatabase.csv")

# Database showing the number of workers and brood that died after each colony was exposed to their first relative humidity (%) trial
HumidMortalityRaw <- read.csv("HumidMortalityRaw.csv")

# Porosity values for all substrates
PorosityComparison <- read.csv("PorosityComparison.csv")

#########################################################################################################################################
# MEAN HUMIDITY VALUES
# The script below is used to find mean humidity values from hygrometer data (SupplementalHygrometerDatabase)
#########################################################################################################################################

# Constructing the final experimental database using the HumidityExperimentDatabase and SupplementalHygrometerDatabase databases
# Reducing the full database to mean and sd.dev for relative humidity (%) and temperature (celcius)
SupplementalHygrometerDatabaseReduced <- SupplementalHygrometerDatabase %>%
  select(c("Colony", "TrialNumber", "Temp", "RH", "Salt", "Trial")) %>%
  group_by(Colony, TrialNumber) %>%
  mutate(Humidity = mean(RH), Temperature = mean(Temp), Std.DevHum = sd(RH), Std.DevTemp = sd(Temp)) %>%
  group_by(Salt, Trial) %>%
  mutate(FullHumidity = mean(RH), FullTemperature = mean(Temp), FullStd.DevHum = sd(RH), FullStd.DevTemp = sd(Temp)) %>%
  select(-c("RH", "Temp")) %>%
  distinct()

# Calculating salt's average and standard deviation in humidity produced in each trial
SupplementalHygrometerDatabaseTrials <- SupplementalHygrometerDatabaseReduced %>%
  select(Trial, Salt, FullHumidity, FullStd.DevHum, FullTemperature, FullStd.DevTemp) %>%
  distinct()

# Calculating the average and standard deviation of the temperature in the experimental assays
SupplementalHygrometerDatabaseReduced %>%
  ungroup() %>%
  mutate(MeanTemp = mean(Temperature), SdTemp = sd(Temperature)) %>%
  select(MeanTemp, SdTemp) %>%
  distinct()

#########################################################################################################################################
# FINAL DATASET OF HUMIDITY AND BUILD NEST WALL PROPERTIES
# The script below is used TO join the mean humidity values from above to the built wall properties dataset (HumidityExperimentalDatabase)
#########################################################################################################################################

# Separating the wall building experimental data into Trial 1 and Trial 2 and joining average humidity data
Complete_Data_final_Trial1 <- HumidityExperimentalDatabase %>%
  filter(Trial == 1) %>%
  # Calculate nest wall volume as Area * 1.5, which is the height of the nest
  mutate(Volume = Area * 1.5,
  # Calculate nest wall density: If wall volume isn't 0, then calculate by dividing wall weight and wall volume, else 0
         Density = ifelse(Volume != 0, CollWallWt / Volume, 0),
         PropIIWall = ((StartWtII - UsedWtII) / (StartWtII - UsedWtII + StartWtI - UsedWtI)),
         PropIWall = 1 - PropIIWall) %>%
  left_join(SupplementalHygrometerDatabaseReduced)

# Same procedure as above but for trial 2
Complete_Data_final_Trial2 <- HumidityExperimentalDatabase %>%
  filter(Trial == 2) %>%
# Calculate nest wall volume as Area * 1.5, which is the height of the nest
mutate(Volume = Area * 1.5,
# Calculate nest wall density: If wall volume isn't 0, then calculate by dividing wall weight and wall volume, else 0
       Density = ifelse(Volume != 0, CollWallWt / Volume, 0),
       PropIIWall = ((StartWtII - UsedWtII) / (StartWtII - UsedWtII + StartWtI - UsedWtI)),
       PropIWall = 1 - PropIIWall) %>%
left_join(SupplementalHygrometerDatabaseReduced)

# Full join data for plots & analyses 
Complete_Data_Final <- full_join(Complete_Data_final_Trial1, Complete_Data_final_Trial2)

#########################################################################################################################################
# PREFERED NEST WALL SUBSTRATE AND SIDE BIAS
# The script below used binomial tests to see if a building substrate or substrate placement (left / right) was favored
# The script also finds the median ratio of substrate II used in wall building for both trials
#########################################################################################################################################

# Mann-Whitney U tests to determine building substrate preference
# Trial 1
wilcox.test(Complete_Data_final_Trial1$PropIIWall, Complete_Data_final_Trial1$PropIWall, paired = FALSE)

# Trial 2
wilcox.test(Complete_Data_final_Trial2$PropIIWall, Complete_Data_final_Trial2$PropIWall, paired = FALSE)

# Median amount of substrate I and substrate II used to build walls, and the median ratio
# Trial 1
Complete_Data_final_Trial1 %>%
  mutate(MedSubI = median(StartWtI - UsedWtI), MedSubII = median(StartWtII - UsedWtII), MedRatio = median(PropIIWall)) %>%
  select(c(MedSubI, MedSubII, MedRatio)) %>%
  distinct()

# Trial 2
Complete_Data_final_Trial2 %>%
  mutate(MedSubI = median(StartWtI - UsedWtI), MedSubII = median(StartWtII - UsedWtII), MedRatio = median(PropIIWall)) %>%
  select(c(MedSubI, MedSubII, MedRatio)) %>%
  distinct()

#########################################################################################################################################
# RELATIVE HUMIDITY AND BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: PLOTS AND ANALYSES
# The script below is to plot and analyze linear relationships between humidity on nest wall properties
#########################################################################################################################################

# WALL WEIGHT
# Scatter plots with Humidity (%) on the x-axis and the weight of the built nest walls (g)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
WeightPlot <- ggplot(Complete_Data_Final, aes(x = Humidity, y = CollWallWt, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall weight") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall weight (g)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26, color = "black"),
        axis.title.x = element_text(size = 26, color = "white"),
        legend.text = element_text(size = 26, color = "black"),
        legend.title = element_text(size = 26,color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) 

# Linear mixed effects models that assess the influence of relative humidity on nest wall weight
summary(lmer(CollWallWt ~ Humidity + (1|Trial), data = Complete_Data_Final))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(CollWallWt ~ Humidity + (1|Trial), data = Complete_Data_Final))

# WALL LENGTH
# Scatter plots with Humidity (%) on the x-axis and the length of the built nest walls (mm)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
LengthPlot <- ggplot(Complete_Data_Final, aes(x = Humidity, y = Length, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall length") +
  xlab("Relative humidity (%)") +
  ylab("Wall length (mm)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26, color = "black"),
        axis.title.x = element_text(size = 26, color = "white"),
        legend.text = element_text(size = 26, color = "black"),
        legend.title = element_text(size = 26,color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  ylim(0, 400)

# Linear mixed effects model that assess the influence of relative humidity on nest wall length
summary(lmer(Length ~ Humidity + (1|Colony), data = Complete_Data_Final))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Length ~ Humidity + (1|Trial), data = Complete_Data_Final))

# WALL AREA
# Scatter plots with Humidity (%) on the x-axis and the area of the built nest walls (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
AreaPlot <- ggplot(Complete_Data_Final, aes(x = Humidity, y = Area, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26, color = "black"),
        axis.title.x = element_text(size = 26, color = "white"),
        legend.text = element_text(size = 26, color = "black"),
        legend.title = element_text(size = 26,color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  ylim(0, 8000)

# Linear mixed effects model that assess the influence of relative humidity on nest wall area
summary(lmer(Area ~ Humidity + (1|Trial), data = Complete_Data_Final))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Area ~ Humidity + (1|Trial), data = Complete_Data_Final))

# WALL DENSITY
# Scatter plots with Humidity (%) on the x-axis and the density of the built nest walls (g / mm^3)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Changes the significant figures for the x-axis to 4
scaleFUN <- function(x) sprintf("%.4f", x)

DensityPlot <- ggplot(Complete_Data_Final, aes(x = Humidity, y = Density, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall density") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, color = "black", hjust = 0.785, vjust = 0),
        axis.text = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26, color = "black"),
        axis.title.x = element_text(size = 26, color = "white"),
        legend.text = element_text(size = 26, color = "black"),
        legend.title = element_text(size = 26,color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  scale_y_continuous(labels = scaleFUN)


# Linear mixed effects model that assess the influence of relative humidity on nest wall density
summary(lmer(Density ~ Humidity + (1|Trial), data = Complete_Data_Final))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Density ~ Humidity + (1|Trial), data = Complete_Data_Final))

# WALL COMPOSITION
# Scatter plots with Humidity (%) on the x-axis and the substrate composition of the built nest walls (proportion of the small building substrate used)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

CompnPlot <- ggplot(Complete_Data_Final, aes(x = Humidity, y = PropIIWall, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall composition") +
  xlab("Relative humidity (%)") +
  ylab("Substrate II propn") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, color = "black", hjust = 0.795, vjust = 0),
        axis.text = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26, color = "black"),
        axis.title.x = element_text(size = 26, color = "white"),
        legend.text = element_text(size = 26, color = "black"),
        legend.title = element_text(size = 26,color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  ylim(0.2, 1)

# Linear mixed effects model that assess the influence of relative humidity on nest wall composition
summary(lmer(PropIIWall ~ Humidity + (1|Trial), data = Complete_Data_Final))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(PropIIWall ~ Humidity + (1|Trial), data = Complete_Data_Final))

# INTERNAL NEST AREA
# Scatter plots with Humidity (%) on the x-axis and the internal nest area (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
IntAreaPlot <- ggplot(Complete_Data_Final, aes(x = Humidity, y = Nest.Area, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Internal nest area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, color = "black", hjust = 0.720, vjust = 0),
        axis.text = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26, color = "black"),
        axis.title.x = element_text(size = 26, color = "white"),
        legend.text = element_text(size = 26, color = "black"),
        legend.title = element_text(size = 26,color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  ylim(0, 8000)

# Linear mixed effects model that assess the influence of relative humidity on nest internal area
summary(lmer(Nest.Area ~ Humidity + (1|Trial), data = Complete_Data_Final))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Nest.Area ~ Humidity + (1|Trial), data = Complete_Data_Final))

# Arrange all plots relating to the effect of humidity on nest wall traits
HumidPlots <- ggarrange(WeightPlot, LengthPlot,
                       AreaPlot, DensityPlot,
                       CompnPlot, IntAreaPlot,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26,  face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3,
                       common.legend = TRUE,
                       legend = "top")

# Annotate the arranged plot
annotate_figure(HumidPlots,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 32, x = 0.5, y = 1),
                left = NULL,
                right = NULL
)

#########################################################################################################################################
# RELATIVE HUMIDITY AND BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: POWER ANALYSES OF LINEAR REGRESSIONS USED BELOW
# The script below is to determine the statistical power of our linear regressions testing the effect of humidity on nest wall properties
#########################################################################################################################################

# Power analysis for humidity & nest properties linear mixed effects models
# We used the simr package to test the power of each of our linear mixed effects models 
# 1. Assign each model to an object
# 2. Simulate the power of our models through 200 Monte Carlo simulations and use a likelihood ratio test for effect size

# Wall weight
# Linear mixed effects model 
fit <- lmer(CollWallWt ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
WallWt <- powerSim(fit, nsim = 200, fixed("Humidity", "lr"), progress = FALSE)

# Saved object to show output
WallWt

# Wall length
# Linear mixed effects model 
fit1 <- lmer(Length ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
WallLength <- powerSim(fit1, nsim = 200, fixed("Humidity", "lr"), progress = FALSE)

WallLength

# Wall area
# Linear mixed effects model 
fit2 <- lmer(Area ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
WallArea <- powerSim(fit2, nsim = 200, fixed("Humidity", "lr"), progress = FALSE)

WallArea

# Wall density
# Linear mixed effects model 
fit3 <- lmer(Density ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
WallDens <- powerSim(fit3, nsim = 200, fixed("Humidity", "lr"), progress = FALSE)

WallDens

# Wall composition
# Linear mixed effects model 
fit4 <- lmer(PropIIWall ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
WallCompn <- powerSim(fit4, nsim = 200, fixed("Humidity", "lr"), progress = FALSE)

WallCompn

# Internal nest area
# Linear mixed effects model 
fit5 <- lmer(Nest.Area ~ Humidity + (1|Trial), data = Complete_Data_Final)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
Int.Area <- powerSim(fit5, nsim = 200, fixed("Humidity", "lr"), progress = FALSE)

Int.Area

#########################################################################################################################################
# COLONY SIZE BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: COMPARATIVE STATISTICS & DATA PROCESSING
# The script below is to find the median & range of worker and brood number and to creating working datasets for plots and analyses
#########################################################################################################################################

# Median & range of worker and brood number
# Trial 1
# Workers
median(Complete_Data_final_Trial1$Number.Ant)
range(Complete_Data_final_Trial1$Number.Ant)

# Brood
median(Complete_Data_final_Trial1$Number.Brood)
range(Complete_Data_final_Trial1$Number.Brood)

# Queens (same across trials)
median(Complete_Data_final_Trial1$Number.Queens)
range(Complete_Data_final_Trial1$Number.Queens)

# Trial 2
# Workers
median(Complete_Data_final_Trial2$Number.Ant)
range(Complete_Data_final_Trial2$Number.Ant)

# Brood
median(Complete_Data_final_Trial2$Number.Brood)
range(Complete_Data_final_Trial2$Number.Brood)

# Creating the working data sets for plotting and analysis below
# Trial 1
# Worker count
Complete_Data_FinalWorkerCount <- Complete_Data_Final %>%
  select(-c(Number.Brood)) %>%
  rename(Number.Colony = Number.Ant) %>%
  mutate(ColonyMember = "Workers")

# Brood count
Complete_Data_FinalBroodCount <- Complete_Data_Final %>%
  select(-c(Number.Ant))%>%
  rename(Number.Colony = Number.Brood) %>%
  mutate(ColonyMember = "Brood")

# Combined data set for plots and analysis
Complete_Data_Final_ColonyCount <- full_join(Complete_Data_FinalWorkerCount, Complete_Data_FinalBroodCount)

#########################################################################################################################################
# COLONY SIZE BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: PLOTS AND ANALYSES
# The script below is to plot and analyze relationships between worker and brood number and built nest wall traits and internal nest area
#########################################################################################################################################

# Wall Weight
# Scatter plots either worker or brood number on the x-axis and the weight of the built nest walls (g)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
WeightPlotColony <- ggplot(Complete_Data_Final_ColonyCount, aes(x = Number.Colony, y = CollWallWt, color = ColonyMember, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall weight") +
  xlab(NULL) +
  ylab(expression(paste('Wall weight (g)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26,  color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26,  color = "black"),
        axis.title.y = element_text(size = 26,  color = "black"),
        axis.title.x = element_text(size = 26,  color = "white"),
        legend.text = element_text(size = 26,  color = "black"),
        legend.title = element_text(size = 26,  color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial"),
         color = guide_legend(title = "Colony member")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                    values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Linear mixed effects models that examine the relationship between the number of ants and brood in a colony and wall weight

# Number of brood
summary(lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))$coefficients

# Marginal and conditional r-squared
r.squaredGLMM(lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Number of workers
summary(lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))$coefficients

# Marginal and conditional r-squared
r.squaredGLMM(lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Wall Length
# Scatter plots either worker or brood number on the x-axis and the length of the built nest walls (mm)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
LengthPlotColony <- ggplot(Complete_Data_Final_ColonyCount, aes(x = Number.Colony, y = Length, color = ColonyMember, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall length") +
  xlab(NULL) +
  ylab(expression(paste('Wall length (mm)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26,  color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26,  color = "black"),
        axis.title.y = element_text(size = 26,  color = "black"),
        axis.title.x = element_text(size = 26,  color = "white"),
        legend.text = element_text(size = 26,  color = "black"),
        legend.title = element_text(size = 26,  color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial"),
         color = guide_legend(title = "Colony member")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                     values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Linear mixed effects models that examine the relationship between the number of ants and brood in a colony and wall length
# Number of brood
summary(lmer(Length ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Length ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Number of workers
summary(lmer(Length ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Length ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Wall Area
# Scatter plots either worker or brood number on the x-axis and the area of the built nest walls (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
AreaPlotColony <- ggplot(Complete_Data_Final_ColonyCount, aes(x = Number.Colony, y = Area, color = ColonyMember, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall area") +
  xlab(NULL) +
  ylab(expression(paste('Wall area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26,  color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26,  color = "black"),
        axis.title.y = element_text(size = 26,  color = "black"),
        axis.title.x = element_text(size = 26,  color = "white"),
        legend.text = element_text(size = 26,  color = "black"),
        legend.title = element_text(size = 26,  color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial"),
         color = guide_legend(title = "Colony member")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                     values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Linear mixed effects models that examine the relationship between the number of ants and brood in a colony and wall area
# Number of brood
summary(lmer(Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))$coefficients

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Number of workers
summary(lmer(Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))$coefficients

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Wall Density
# Scatter plots either worker or brood number on the x-axis and the density of the built nest walls (g / mm^3)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

scaleFUN <- function(x) sprintf("%.4f", x)
DensityPlotColony <- ggplot(Complete_Data_Final_ColonyCount, aes(x = Number.Colony, y = Density, color = ColonyMember, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall density") +
  xlab(NULL) +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26,  color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26,  color = "black"),
        axis.title.y = element_text(size = 26,  color = "black"),
        axis.title.x = element_text(size = 26,  color = "white"),
        legend.text = element_text(size = 26,  color = "black"),
        legend.title = element_text(size = 26,  color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial"),
         color = guide_legend(title = "Colony member")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                     values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Linear mixed effects models that examine the relationship between the number of ants and brood in a colony and wall density
# Number of brood
summary(lmer(Density ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Density ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Number of workers
summary(lmer(Density ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))$coefficients

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Density ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Wall Substrate Composition
# Scatter plots with the number of workers or brood on the x-axis and the substrate composition of the built nest walls (proportion of the small building substrate used)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
CompnPlotColony <- ggplot(Complete_Data_Final_ColonyCount, aes(x = Number.Colony, y = PropIIWall, color = ColonyMember, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Wall composition") +
  xlab(NULL) +
  ylab("Substrate II propn") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26,  color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26,  color = "black"),
        axis.title.y = element_text(size = 26,  color = "black"),
        axis.title.x = element_text(size = 26,  color = "white"),
        legend.text = element_text(size = 26,  color = "black"),
        legend.title = element_text(size = 26,  color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                     values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Linear mixed effects models that examine the relationship between the number of ants and brood in a colony and wall area

# Number of brood
summary(lmer(PropIIWall ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(PropIIWall ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Number of workers
summary(lmer(PropIIWall ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))$coefficients

# Marginal and conditional r-squared
r.squaredGLMM(lmer(PropIIWall ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Internal Nest Area
# Scatter plots with the number of workers or brood on the x-axis and internal area of the nest (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
IntAreaColony <- ggplot(Complete_Data_Final_ColonyCount, aes(x = Number.Colony, y = Nest.Area, color = ColonyMember, shape = as.factor(Trial))) +
  geom_point(size = 6, alpha = 0.66) +
  ggtitle("Internal nest area") +
  xlab(NULL) +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26,  color = "black", hjust = 0.775, vjust = 0),
        axis.text = element_text(size = 26,  color = "black"),
        axis.title.y = element_text(size = 26,  color = "black"),
        axis.title.x = element_text(size = 26,  color = "white"),
        legend.text = element_text(size = 26,  color = "black"),
        legend.title = element_text(size = 26,  color = "black"),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(shape = guide_legend(title = "Trial")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                     values = c("red", "blue")) +
  ylim(0, 8000) +
  scale_x_continuous(breaks = seq(75, 225, by = 75)) 

# Linear mixed effects models that examine the relationship between the number of ants and brood in a colony and wall area
# Number of brood
summary(lmer(Nest.Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Nest.Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalBroodCount))

# Number of workers
summary(lmer(Nest.Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Marginal and conditional r-squared
r.squaredGLMM(lmer(Nest.Area ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount))

# Arrange all plots relating to the effect of colony size on nest wall traits
HumidPlotsColony <- ggarrange(WeightPlotColony, LengthPlotColony,
                       AreaPlotColony, DensityPlotColony,
                       CompnPlotColony, IntAreaColony,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26,  face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3,
                       common.legend = TRUE,
                       legend = "top")

# Annotate the combined plot
annotate_figure(HumidPlotsColony,
                top = NULL,
                bottom = text_grob("Number of workers / brood", color = "black",
                                   size = 32, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)

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

# Workers
fitWorker <- lmer(CollWallWt ~ Number.Colony + (1|Trial), data = Complete_Data_FinalWorkerCount)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Brood
WallWtBrood <- powerSim(fitBrood, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

# Power simulation, 200 simulations, uses the likelihood ratio test for simulated p-values, does not show progress of simulations
# Workers
WallWtWorker <- powerSim(fitWorker, nsim = 200, fixed("Number.Colony", "lr"), progress = FALSE)

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

#########################################################################################################################################
# WORKER AND BROOD DEATH AND RELATIVE HUMIDITY: FINAL DATASET
# The script below is create the final dataset for testing the relationship between humidity exposure and colony worker and brood death
#########################################################################################################################################

# Joining the mean humidity values (SupplementalHygrometerDatabaseReduced) to the worker and brood % loss dataset (HumidMortalityRaw)
HumidMortality <- HumidMortalityRaw %>%
  left_join(SupplementalHygrometerDatabaseReduced) %>%
  select(-c(TrialNumber))

WorkerMortality <- HumidMortality %>%
  select(-BroodDeath) %>%
  rename(ColonyDeath = WorkerDeath) %>%
  mutate(ColonyMember = "Workers")

BroodMortality <- HumidMortality %>%
  select(-WorkerDeath) %>%
  rename(ColonyDeath = BroodDeath) %>%
  mutate(ColonyMember = "Brood")

# Join the data sets for plotting below
MortalityPlots <- full_join(WorkerMortality, BroodMortality)

#########################################################################################################################################
# WORKER AND BROOD DEATH AND RELATIVE HUMIDITY: PLOTS AND ANALYSES
# The script below is to plot and analyze relationships between worker and brood death the relative humidity colonies were first exposed to
##########################################################################################################################################

# Scatter plots with humidity (%) on the x-axis and either worker and brood death on the y-axis (% less after trial 1)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

ggplot(MortalityPlots, aes(x = Humidity, y = ColonyDeath, color = ColonyMember)) +
  geom_point(size = 6, alpha = 0.66) +
  xlab("Relative Humidity (%)") +
  ylab("Colony member death (%)") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 22,  color = "black"),
        axis.title.y = element_text(size = 22,  color = "black"),
        axis.title.x = element_text(size = 22,  color = "black"),
        legend.text = element_text(size = 22,  color = "black"),
        legend.title = element_text(size = 22,  color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key = element_blank()) +
  guides(color = guide_legend(title = "Colony Member")) +
  scale_color_manual(breaks = c("Brood", "Workers"), 
                     values = c("red", "blue"))

# Generalized linear models that examine the relationship between % of ant and brood loss after trial 1 and exposed humidity (%)
# Workers
summary(glm(WorkerDeath ~ Humidity, family = "binomial", data = HumidMortality))

# Brood
summary(glm(BroodDeath ~ Humidity, family = "binomial", data = HumidMortality))

#########################################################################################################################################
# POROSITY COMPARISONS: PLOTS AND ANALYSES
# The script below is to plot and analyze the porosities of experimental substrates, built walls, and natural walls
##########################################################################################################################################
# Uses the PorosityComparison dataset

# Creating data subsets for plotting and Mann-Whitney U tests comparing all porosity combinations
# Substrate I
PorosityComparisonI <- PorosityComparison %>%
  filter(SubCategory == "Sub I")

# Substrate II
PorosityComparisonII <- PorosityComparison %>%
  filter(SubCategory == "Sub II")

# Natural
PorosityComparisonNat <- PorosityComparison %>%
  filter(SubCategory == "Natural")

# Built wall
PorosityComparisonBuilt <- PorosityComparison %>%
  filter(SubCategory == "Built")


# Box plot comparing all porosity values
# Brackets show significant "***" and non-significant "NS" Mann-Whitney U tests (found below)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)
ggplot(PorosityComparison, aes(x = reorder(SubCategory, Porosity, FUN = median), y = Porosity)) +
  geom_boxplot(coef = 200, lwd = 0.55) +
  geom_bracket(
    xmin = c("Sub I", "Sub I", "Sub I", "Sub II", "Built", "Built"), xmax = c("Sub II", "Natural", "Built", "Natural", "Sub II", "Natural"),
    y.position = c(90, 87, 84, 73, 77, 81), label = c("***", "***", "***", "NS", "NS", "NS"),
    tip.length = 0.01, size = 0.65, label.size = 6, family = "Arial") +
  ggtitle("Porosity comparison") +
  xlab("Substrate category") +
  ylab("Porosity (%)") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22,  color = "black", hjust = -0.1, vjust = 0),
        axis.text = element_text(size = 22,  color = "black"),
        axis.title.y = element_text(size = 22,  color = "black"),
        axis.title.x = element_text(size = 22,  color = "black"))

# Kruskal-Wallis test to compare artificial and natural nest substrate porosities
kruskal.test(PorosityComparison$SubCategory, PorosityComparison$Porosity)

# Post-hoc Dunn's test for multiple comparisons, using the Benjamini-Hockberg adjustment to control for family-wide error
dunnTest(PorosityComparison$Porosity ~ PorosityComparison$SubCategory, method = "holm")

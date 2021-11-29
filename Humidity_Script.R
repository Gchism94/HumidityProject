#########################################################################################################################################
# Autor: Greg CHISM
# Date: Nov 2021
# email: gchism@email.arizona.edu
# Project: "Temnothorax rugatulus" ants do not change their nest walls in response to environmental humidity 
# Title: Humidity, colony percentage death, porosity data grooming, visualization, and analyses
#########################################################################################################################################

# The below script was utilized to analyze and visualize data from the associated manuscript.
# The associated databases are "HumidityExperimentDatabase.csv", "SupplementalHygrometerDatabase.csv", "and"HumidMortality.csv", and "PorosityComparison.csv" 

# Installing and loading packages used for graphs and analyses
install.packages("pacman") #Download package with function to load multiple packaged at once

#Loading required packages for code below. p_load() will download packages that aren't in system library
pacman::p_load(ggpubr, #Loading required packages for code below. p_load() will download packages that aren't in system library
               pwr,
               scales,
               tidyverse)

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
# FINAL DATASET OFMEAN HUMIDITY AND BUILD NEST WALL PROPERTIES 
# The script below is used TO join the mean humidity values from above to the built wall properties dataset (HumidityExperimentalDatabase)
#########################################################################################################################################

# Separating the wall building experimental data into Trial 1 and Trial 2 and joining average humidity data
Complete_Data_final_Trial1 <- HumidityExperimentalDatabase %>%
  filter(Trial == 1) %>% 
  #Calculate nest wall density: If wall volume isn't 0, then calculate by dividing wall weight and wall volume, else 0
  mutate(Density = ifelse(Volume != 0, CollWallWt / Volume, 0)) %>%
  left_join(SupplementalHygrometerDatabaseReduced)

# Same procedure as above but for trial 2
Complete_Data_final_Trial2 <- HumidityExperimentalDatabase %>%
  filter(Trial == 2) %>% 
  #Calculate nest wall density: If wall volume isn't 0, then calculate by dividing wall weight and wall volume, else 0
  mutate(Density = ifelse(Volume != 0, CollWallWt / Volume, 0)) %>%
  left_join(SupplementalHygrometerDatabaseReduced)

#########################################################################################################################################
# PREFERED NEST WALL SUBSTRATE AND SIDE BIAS 
# The script below used binomial tests to see if a building substrate or substrate placement (left / right) was favored 
# The script also finds the median ratio of substrate II used in wall building for both trials
#########################################################################################################################################

# Binomial test to determine if colonies preferred substrate II
# Here we first distinguish a success as having a wall composed with greater than 50% of substrate II 
Complete_Data_final_Trial1 %>% 
  group_by(Colony) %>%
  mutate(SubPrefer = ifelse(PropIIWall > 0.5, 1, 0)) %>%
  select(c(SubPrefer)) %>%
  ungroup() %>%
  filter(SubPrefer == "1") %>%
  summarise(n = n())
# Produces 18 successes and 1 failure, which will be successes (x) for the binomial test 

# Binomial test with 18 successes across 19 observations
binom.test(18, 19, p = 0.5)

# Same procedure as above but for trial 2
Complete_Data_final_Trial2%>% 
  group_by(Colony) %>%
  mutate(SubPrefer = ifelse(PropIIWall > 0.5, 1, 0)) %>%
  select(c(SubPrefer)) %>%
  ungroup() %>%
  filter(SubPrefer == "1") %>%
  summarise(n = n())
# Produces 14 successes and 2 failures, which will be successes (x) for the binomial test

# Binomial test with 28 successes across 30 observations
binom.test(14, 16, p = 0.5)

# Binomial test to determine if a side bias was observed
# Here we first distinguish a success as choosing primarily substrate II with substrate I on the left side 
Complete_Data_final_Trial1 %>%
  mutate(SideChoice = ifelse(SubstrateISide == "L" & PropIIWall > 0.5, 1, 0)) %>%
  select(c(SideChoice)) %>%
  ungroup() %>%
  filter(SideChoice == "1") %>%
  summarise(n = n())
# Produces 9 successes and 10 failures, which will be successes (x) for the binomial test

# Binomial test with 9 successes across 19 observations
binom.test(9, 19, p = 0.5)

# Same procedure as above but for trial 2
Complete_Data_final_Trial2 %>%
  mutate(SideChoice = ifelse(SubstrateISide == "L" & PropIIWall > 0.5, 1, 0)) %>%
  select(c(SideChoice)) %>%
  ungroup() %>%
  filter(SideChoice == "1") %>%
  summarise(n = n())
# Produces 7 successes and 9 failures, which will be successes (x) for the binomial test

# Binomial test with 7 successes across 16 observations
binom.test(7, 16, p = 0.5)

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
# RELATIVE HUMIDITY AND BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: POWER ANALYSES OF LINEAR REGRESSIONS USED BELOW
# The script below is to determine the statistical power of our linear regressions testing the effect of humidity on nest wall properties 
#########################################################################################################################################

# Power analysis for a linear regression 
# We used the true sample size of our trials and determined the power of our linear regressions, where 0.1, 0.3, and 0.5 represent a "small", "medium", and "large" effect size 
# pwr.f2.test considers u (numerator degrees of freedom), v (denominator degrees of freedom), f2 (effect size), sig.level (significance level - type I error probability)
# Trial 1
pwr.f2.test(u = 2, v = 16, f2 = 0.02, sig.level = 0.05)
pwr.f2.test(u = 2, v = 16, f2 = 0.15, sig.level = 0.05)
pwr.f2.test(u = 2, v = 16, f2 = 0.35, sig.level = 0.05)

#Trial 2
pwr.f2.test(u = 2, v = 14, f2 = 0.02, sig.level = 0.05)
pwr.f2.test(u = 2, v = 14, f2 = 0.15, sig.level = 0.05)
pwr.f2.test(u = 2, v = 14, f2 = 0.35, sig.level = 0.05)

# Calculating required sample size for 80% conventional statistical power (Cohen 2013)
# Remove v and include power (Power of test - 1 minus Type II error probability)
pwr.f2.test(u = 2, f2 = 0.02, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 2, f2 = 0.15, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 2, f2 = 0.35, sig.level = 0.05, power = 0.8)

#########################################################################################################################################
# RELATIVE HUMIDITY AND BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: PLOTS AND ANALYSES
# The script below is to plot and analyze linear relationships between humidity on nest wall properties 
#########################################################################################################################################
# Trial 1 uses the Complete_Data_final_Trial1 dataset created above
# Trial 2 uses the Complete_Data_final_Trial2 dataset created above

#Color palette used for trial 1 to give each colony a unique color
ColonyPalette1 <- c("1" = "coral3", "2" = "chartreuse2", "3" = "cadetblue", "4" = "burlywood4", "5" = "grey40", 
                    "7" = "blue4", "8" = "lightslateblue", "9" = "purple2", "10" = "blue", "11" = "darkseagreen4",
                    "12" = "turquoise2", "13" = "darkgreen", "14" = "firebrick4", "15" = "magenta3", "17" = "paleturquoise3",
                    "18" = "seagreen3", "20" = "grey12", "21" = "rosybrown3", "22" = "yellow3")

#Color palette used for trial 2 to give each colony a unique color
ColonyPalette2 <- c("1" = "coral3", "2" = "chartreuse2", "3" = "cadetblue", "4" = "burlywood4", "5" = "brown4", 
                    "7" = "blue4", "11" = "darkseagreen4", "12" = "turquoise2", "13" = "darkgreen", 
                    "14" = "firebrick4", "15" = "magenta3", "17" = "paleturquoise3",
                    "18" = "seagreen3", "20" = "grey12", "21" = "rosybrown3", "22" = "yellow3")

# Wall Weight
# Scatterplots with Humidity (%) on the x-axis and the weight of the built nest walls (g)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
WeightPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x = Humidity, y = CollWallWt)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall weight") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall weight (g)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) 

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
WeightPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = CollWallWt)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall weight") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall weight (g)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

# Linear and quadratic regressions that assess the influence of relative humidity on nest wall weight
# Linear fits are used to compare adjusted R-squared between the linear and quadratic regressions

# Trial 1
# Linear fit
summary(lm(CollWallWt ~ Humidity, data = Complete_Data_final_Trial1))

# Quadratic fit
summary(lm(CollWallWt ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

# Trial 2
# Linear fit
summary(lm(CollWallWt ~ Humidity, data = Complete_Data_final_Trial2))

# Quadratic fit
summary(lm(CollWallWt ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

# Wall Length
# Scatterplots with Humidity (%) on the x-axis and the length of the built nest walls (mm)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
LengthPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x = Humidity, y = Length)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall length") +
  xlab("Relative humidity (%)") +
  ylab("Wall length (mm)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.justification = c(1, 1),
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 400)

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
LengthPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = Length)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall length") +
  xlab("Relative humidity (%)") +
  ylab("Wall length (mm)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.justification = c(1, 1),
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 400)

# Linear and quadratic regressions that assess the influence of relative humidity on nest wall length
# Linear fits are used to compare adjusted R-squared between the linear and quadratic regressions
# Trial 1
# Linear fit
summary(lm(Length ~ Humidity, data = Complete_Data_final_Trial1))

# Quadratic fit
summary(lm(Length ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

# Trial 2
# Linear fit
summary(lm(Length ~ Humidity, data = Complete_Data_final_Trial2))

# Quadratic fit
summary(lm(Length ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

# Wall Area
# Scatterplots with Humidity (%) on the x-axis and the area of the built nest walls (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
AreaPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x = Humidity, y = Area)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 8000)

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
AreaPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = Area)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 8000)

# Linear and quadratic regressions that assess the influence of relative humidity on nest wall area
# Linear fits are used to compare adjusted R-squared between the linear and quadratic regressions
# Linear regression 
# Trial 1
# Linear fit
summary(lm(Area ~ Humidity, data = Complete_Data_final_Trial1))

# Quadratic fit
summary(lm(Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

# Trial 2
# Linear fit
summary(lm(Area ~ Humidity, data = Complete_Data_final_Trial2))

# Quadratic fit
summary(lm(Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

# Wall Density
# Scatterplots with Humidity (%) on the x-axis and the density of the built nest walls (g / mm^3)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Changes the significant figures for the x-axis to 4
scaleFUN <- function(x) sprintf("%.4f", x)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
DensityPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x = Humidity, y = Density)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall density") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.785, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  scale_y_continuous(labels = scaleFUN)

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
DensityPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = Density)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall density") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.785, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  scale_y_continuous(labels=scaleFUN)

# Linear and quadratic regressions that assess the influence of relative humidity on nest wall density
# Linear fits are used to compare adjusted R-squared between the linear and quadratic regressions
# Trial 1
# Linear fit
summary(lm(Density ~ Humidity, data = Complete_Data_final_Trial1))

# Quadratic fit
summary(lm(Density ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

# Trial 2
# Linear fit
summary(lm(Density ~ Humidity, data = Complete_Data_final_Trial2))

# Quadratic fit
summary(lm(Density ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

# Wall Substrate Composition
# Scatterplots with Humidity (%) on the x-axis and the substrate composition of the built nest walls (proportion of the small building substrate used)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
CompnPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x = Humidity, y = PropIIWall)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall composition") +
  xlab("Relative humidity (%)") +
  ylab("Substrate II propn")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.795, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  ylim(0.2, 1)

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
CompnPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = PropIIWall)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall composition") +
  xlab("Relative humidity (%)") +
  ylab("Substrate II propn") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.795, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) +
  ylim(0.2, 1)

# Linear and quadratic regressions that assess the influence of relative humidity on nest wall composition
# Linear fits are used to compare adjusted R-squared between the linear and quadratic regressions
# Trial 1
# Linear fit
summary(lm(PropIIWall ~ Humidity, data = Complete_Data_final_Trial1))

# Quadratic fit
summary(lm(PropIIWall ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

# Trial 2
# Linear fit
summary(lm(PropIIWall ~ Humidity, data = Complete_Data_final_Trial2))

# Quadratic fit
summary(lm(PropIIWall ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

# Internal Nest Area
# Scatterplots with Humidity (%) on the x-axis and the internal nest area (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
IntAreaPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x = Humidity, y = Nest.Area))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Internal nest area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.720, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 8000)

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
IntAreaPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = Nest.Area)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Internal nest area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.720, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 8000)

# Linear and quadratic regressions that assess the influence of relative humidity on nest wall weight
# Linear fits are used to compare adjusted R-squared between the linear and quadratic regressions
# Trial 1
# Linear fit
summary(lm(Nest.Area ~ Humidity, data = Complete_Data_final_Trial1))

# Quadratic fit
summary(lm(Nest.Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

# Trial 2
# Linear fit
summary(lm(Nest.Area ~ Humidity, data = Complete_Data_final_Trial2))

# Quadratic fit
summary(lm(Nest.Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

# Trial 1 arranged & annotated combine plot
# Arrange all plots relating to the effect of humidity on nest wall traits 
HumidPlots1<-ggarrange(WeightPlot1, LengthPlot1, 
                       AreaPlot1, DensityPlot1, 
                       CompnPlot1, IntAreaPlot1,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3, 
                       common.legend = TRUE,
                       legend = "top") 

# Annotate the arranged plot
annotate_figure(HumidPlots1,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 32, x = 0.5, y = 1, family = "Arial"),
                left = NULL,
                right = NULL
)

# Trial 2 arranged & annotated combine plot
# Arrange all plots relating to the effect of humidity on nest wall traits 
HumidPlots2<-ggarrange(WeightPlot2, LengthPlot2, 
                       AreaPlot2, DensityPlot2, 
                       CompnPlot2, IntAreaPlot2,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3, 
                       common.legend = TRUE,
                       legend = "top") 

# Annotate the arranged plot
annotate_figure(HumidPlots2,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 32, x = 0.5, y = 1, family = "Arial"),
                left = NULL,
                right = NULL
)

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

# Creating the working datasets for plotting and analysis below
# Trial 1
# Worker count
Complete_Data_final_Trial1WorkerCount <- Complete_Data_final_Trial1 %>%
  select(-c(Number.Brood)) %>%
  rename(Number.Colony = Number.Ant) %>%
  mutate(ColonyMember = "Workers")

# Brood count
Complete_Data_final_Trial1BroodCount <- Complete_Data_final_Trial1 %>%
  select(-c(Number.Ant))%>%
  rename(Number.Colony = Number.Brood) %>%
  mutate(ColonyMember = "Brood")

# Combined dataset for plots and analysis
Complete_Data_final_Trial1ColonyCount <- full_join(Complete_Data_final_Trial1WorkerCount, Complete_Data_final_Trial1BroodCount)

# Trial 2
# Worker count
Complete_Data_final_Trial2WorkerCount <- Complete_Data_final_Trial2 %>%
  select(-c(Number.Brood)) %>%
  rename(Number.Colony = Number.Ant) %>%
  mutate(ColonyMember = "Workers")

# Brood count
Complete_Data_final_Trial2BroodCount <- Complete_Data_final_Trial2 %>%
  select(-c(Number.Ant))%>%
  rename(Number.Colony = Number.Brood) %>%
  mutate(ColonyMember = "Brood")

# Combined dataset for plots and analysis
Complete_Data_final_Trial2ColonyCount <- full_join(Complete_Data_final_Trial2WorkerCount, Complete_Data_final_Trial2BroodCount)

#########################################################################################################################################
# COLONY SIZE BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: POWER ANALYSES OF CORRELATIONS TESTS USED BELOW
# The script below is to determine the statistical power of our spearman's rho correlations testing the effect of colony size on nest wall properties
#########################################################################################################################################

# Power analysis for a correlation test
# We used the true sample size of our trials and determined the power of our correlation tests, where 0.1, 0.3, and 0.5 
# represent a "small", "medium", and "large" effect size 

# Trial 1
pwr.r.test(n = 19, r = 0.1, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 19, r = 0.3, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 19, r = 0.5, sig.level = 0.05, alternative = "two.sided")
 
#Trial 2
pwr.r.test(n = 17, r = 0.1, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 17, r = 0.3, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 17, r = 0.5, sig.level = 0.05, alternative = "two.sided")

# Needed sample size
pwr.r.test(r = 0.1, sig.level = 0.05, power = 0.8, alternative = "two.sided")
pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.8, alternative = "two.sided")
pwr.r.test(r = 0.5, sig.level = 0.05, power = 0.8, alternative = "two.sided")

# Needed effect size
pwr.r.test(r = 0.6, sig.level = 0.05, power = 0.8, alternative = "two.sided")

#########################################################################################################################################
# COLONY SIZE BOTH NEST WALL FEATURES AND INTERNAL NEST AREA: PLOTS AND ANALYSES
# The script below is to plot and analyze relationships between worker and brood number and built nest wall traits and internal nest area
#########################################################################################################################################

# Wall Weight
# Scatterplots either worker or brood number on the x-axis and the weight of the built nest walls (g)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
WeightPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x = Number.Colony, y = CollWallWt, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall weight") +
  xlab(NULL) +
  ylab(expression(paste('Wall weight (g)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) +
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
WeightPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x = Number.Colony, y = CollWallWt, group = ColonyMember, linetype = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  geom_smooth(color = "black", se = FALSE, method = "lm", size = 2.5) +
  ggtitle("Wall weight") +
  xlab(NULL) +
  ylab(expression(paste('Wall weight (g)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.key.width = unit(2.5, "line")) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial"),
         linetype = guide_legend(title = "Colony member", family = "Arial")) + 
  facet_wrap( ~ ColonyMember) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

# Spearman's rho correlations tests that examine the relationship between the number of ants and brood in a colony and wall weight
# Trial 1
# Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$CollWallWt, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$CollWallWt, method = 'spearman', alternative = "two.sided")

# Trial 2
# Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$CollWallWt, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$CollWallWt, method = 'spearman', alternative = "two.sided")

# Wall Length
# Scatterplots either worker or brood number on the x-axis and the length of the built nest walls (mm)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
LengthPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x = Number.Colony, y = Length, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  geom_smooth(data = Complete_Data_final_Trial1ColonyCount %>% filter(ColonyMember == "Brood"), 
              aes(x = Number.Colony, y = Length), 
              color = "black", se = FALSE, method = "lm", size = 2.5) +
  ggtitle("Wall length") +
  xlab(NULL) +
  ylab(expression(paste('Wall length (mm)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  facet_wrap(~ColonyMember) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
LengthPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x = Number.Colony, y = Length, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall length") +
  xlab(NULL) +
  ylab(expression(paste('Wall length (mm)'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

# Spearman's rho correlations tests that examine the relationship between the number of ants and brood in a colony and wall length (mm)
# Trial 1
# Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Length, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Length, method = 'spearman', alternative = "two.sided")

# Trial 2
# Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Length, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Length, method = 'spearman', alternative = "two.sided")

# Wall Area
# Scatterplots either worker or brood number on the x-axis and the area of the built nest walls (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
AreaPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x = Number.Colony, y = Area, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  geom_smooth(data = Complete_Data_final_Trial1ColonyCount %>% filter(ColonyMember == "Brood"), 
              aes(x = Number.Colony, y=Area), 
              color = "black", se = FALSE, method = "lm", size = 2.5) +
  ggtitle("Wall area") +
  xlab(NULL) +
  ylab(expression(paste('Wall area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  facet_wrap(~ColonyMember) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
AreaPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x = Number.Colony, y = Area, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall area") +
  xlab(NULL) +
  ylab(expression(paste('Wall area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

# Spearman's rho correlations tests that examine the relationship between the number of ants and brood in a colony and wall area (mm^2)
# Trial 1
# Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Area, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Area, method = 'spearman', alternative = "two.sided")

# Trial 2
# Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Area, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Area, method = 'spearman', alternative = "two.sided")

# Wall Density
# Scatterplots either worker or brood number on the x-axis and the density of the built nest walls (g / mm^3)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
scaleFUN <- function(x) sprintf("%.4f", x)
DensityPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x = Number.Colony, y = Density, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall density") +
  xlab(NULL) +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
DensityPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x = Number.Colony, y = Density, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall density") +
  xlab(NULL) +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

# Spearman's rho correlations tests that examine the relationship between the number of ants and brood in a colony and wall density (g / mm^3)
# Trial 1
# Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Density, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Density, method = 'spearman', alternative = "two.sided")

# Trial 2
# Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Density, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Density, method = 'spearman', alternative = "two.sided")

# Wall Substrate Composition
# Scatterplots with the number of workers or brood on the x-axis and the substrate composition of the built nest walls (proportion of the small building substrate used)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
CompnPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x = Number.Colony, y = PropIIWall, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall composition") +
  xlab(NULL) +
  ylab("Substrate II propn") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
CompnPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x = Number.Colony, y = PropIIWall, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall composition") +
  xlab(NULL) +
  ylab("Substrate II propn") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

# Spearman's rho correlations tests that examine the relationship between the number of ants and brood in a colony and wall composition (proportion of small building substrate)
# Trial 1
# Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$PropIIWall, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$PropIIWall, method = 'spearman', alternative = "two.sided")

# Trial 2
# Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$PropIIWall, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$PropIIWall, method = 'spearman', alternative = "two.sided")

# Internal Nest Area
# Scatterplots with the number of workers or brood on the x-axis and internal area of the nest (mm^2)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Trial 1
# Color scale was manually produced above - see "ColonyPallette1"
IntAreaColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x = Number.Colony, y = Nest.Area, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Internal nest area") +
  xlab(NULL) +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.775, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 8000) +
  scale_x_continuous(breaks = seq(50, 225, by = 75))

# Trial 2
# Color scale was manually produced above - see "ColonyPallette2"
IntAreaColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x = Number.Colony, y = Nest.Area, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Internal nest area") +
  xlab(NULL) +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.775, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 8000)

# Spearman's rho correlations tests that examine the relationship between the number of ants and brood in a colony and internal nest area (mm^2)
# Trial 1
# Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Nest.Area, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Nest.Area, method = 'spearman', alternative = "two.sided")

# Trial 2
# Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Nest.Area, method = 'spearman', alternative = "two.sided")

# Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Nest.Area, method = 'spearman', alternative = "two.sided")

# Trial 1 arranged & annotated combine plot
# Arrange all plots relating to the effect of colony size on nest wall traits 
HumidPlotsColony1<-ggarrange(WeightPlotColony1, LengthPlotColony1, 
                       AreaPlotColony1, DensityPlotColony1, 
                       CompnPlotColony1, IntAreaColony1,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3, 
                       common.legend = TRUE,
                       legend = "top") 

# Annotate the combined plot
annotate_figure(HumidPlotsColony1,
                top = NULL,
                bottom = text_grob("Number of workers / brood", color = "black",
                                   size = 32, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)

# Trial 2 arranged & annotated combine plot
# Arrange all plots relating to the effect of colony size on nest wall traits
HumidPlotsColony2<-ggarrange(WeightPlotColony2, LengthPlotColony2, 
                             AreaPlotColony2, DensityPlotColony2, 
                             CompnPlotColony2, IntAreaColony2,
                             labels = c("(a)", "(b)",
                                        "(c)", "(d)",
                                        "(e)", "(f)"),
                             font.label = list(size = 26, family = "Arial", face = "plain"),
                             label.x = 0.9,
                             ncol = 2, nrow = 3, 
                             common.legend = TRUE,
                             legend = "top") 

# Annotate the combined plot
annotate_figure(HumidPlotsColony2,
                top = NULL,
                bottom = text_grob("Number of workers / brood", color = "black",
                                   size = 32, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)

#########################################################################################################################################
# WORKER AND BROOD DEATH AND RELATIVE HUMIDITY: FINAL DATASET
# The script below is create the final dataset for testing the relationship between humidity exposure and colony worker and brood death  
#########################################################################################################################################


# Joining the mean humidity values (SupplementalHygrometerDatabaseReduced) to the worker and brood % loss dataset (HumidMortalityRaw)
HumidMortality <- HumidMortalityRaw %>%
  left_join(SupplementalHygrometerDatabaseReduced) %>%
  select(-c(TrialNumber))

#########################################################################################################################################
# WORKER AND BROOD DEATH AND RELATIVE HUMIDITY: PLOTS AND ANALYSES
# The script below is to plot and analyze relationships between worker and brood death the relative humidity colonies were first exposed to
########################################################################################################################################## 

# Scatterplots with humidity (%) on the x-axis and either worker and brood death on the y-axis (% less after trial 1)
# Plot, axes, and legend titles are specified
# Theme changes are used for editing the axes (text, ticks, and labels), plot title, and legend (text, title, position, and key)

# Workers
# Color scale was manually produced above - see "ColonyPallette1"
WorkerDeathPlot <- ggplot(HumidMortality, aes(x = Humidity, y = WorkerDeath)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Worker death") +
  xlab(NULL) +
  ylab("Worker death (%)") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 22, family = "Arial", color = "black"),
        legend.text = element_text(size = 22, family = "Arial", color = "black"),
        legend.title = element_text(size = 22, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) 

# Brood
# Color scale was manually produced above - see "ColonyPallette1"
BroodDeathPlot <- ggplot(HumidMortality, aes(x = Humidity, y = BroodDeath)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Brood death") +
  xlab(NULL) +
  ylab("Brood death (%)") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 22, family = "Arial", color = "black"),
        legend.text = element_text(size = 22, family = "Arial", color = "black"),
        legend.title = element_text(size = 22, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) 

# Arranging the worker and brood death plots
ColonyDeathHumid <- ggarrange(WorkerDeathPlot, BroodDeathPlot,
                       labels = c("(a)", "(b)"),
                       font.label = list(size = 22, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       label.y = 1.005,
                       ncol = 2, nrow = 1, 
                       common.legend = TRUE,
                       legend = "top") 

# Annotating the combined plot
annotate_figure(ColonyDeathHumid,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 26, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)

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
    y.position = c(58, 62, 54, 43, 46.5, 51), label = c("***", "***", "***", "NS", "NS", "NS"),
    tip.length = 0.01, size = 0.65, label.size = 6, family = "Arial") +
  ggtitle("Porosity comparison") +
  xlab("Substrate category") +
  ylab("Porosity (%)") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22, family = "Arial", color = "black", hjust = -0.1, vjust = 0),
        axis.text = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 22, family = "Arial", color = "black")) 

# Wilcoxon signed rank tests that compare artificial and natural nest substrate porosities
# Substrate I v. Substrate II
wilcox.test(PorosityComparisonI$PorosityII, PorosityComparisonII$PorosityII, paired = FALSE, alternative = "two.sided")

# Substrate I v. Natural
wilcox.test(PorosityComparisonI$PorosityII, PorosityComparisonNat$PorosityII, paired = FALSE, alternative = "two.sided")

# Substrate II v. Natural
wilcox.test(PorosityComparisonII$PorosityII, PorosityComparisonNat$PorosityII, paired = FALSE, alternative = "two.sided")

# Substrate I v. Built
wilcox.test(PorosityComparisonI$PorosityII, PorosityComparisonBuilt$PorosityII, paired = FALSE, alternative = "two.sided")

#Substrate II v. Built 
wilcox.test(PorosityComparisonII$PorosityII, PorosityComparisonBuilt$PorosityII, paired = FALSE, alternative = "two.sided")

#Natural v. Built
wilcox.test(PorosityComparisonNat$PorosityII,PorosityComparisonBuilt$PorosityII, paired = FALSE, alternative = "two.sided")

library(WDI)

countries <- "all"
start_year <- 2015
end_year <- 2018
# all indicators
all_indicators <- c(
  "SP.DYN.IMRT.IN",    # Mortality rate, infant
  "SH.IMM.IDPT",       # Immunization coverage
  "SH.STA.MMRT",       # Maternal mortality ratio
  "SN.ITK.DEFC.ZS",    #Prevalence of undernourishment (% of population)
  "NY.GDP.PCAP.CD",    # GDP Per Capita / Current US$
  "FP.CPI.TOTL.ZG",    # inflation Rate
  "SH.XPD.CHEX.PC.CD"  # Health expenditure per capita (current US$)
) 


# Fetch health and economic data
data <- WDI(country = countries, indicator = all_indicators, start = start_year, end = end_year
            , extra = TRUE)
 

#Data Analysis:

#Numerical Summaries:
summary(data)

#Graphical Summaries:
library(ggplot2)

plot1_data <- data.frame(GDP = data_3$NY.GDP.PCAP.CD, 
                        Maternal_Mortality_Rate = data_3$SH.STA.MMRT,
                        Region = data_3$region)

# Create boxplot by region
ggplot(plot1_data, aes(x = GDP, y = Maternal_Mortality_Rate, color = Region)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  labs(x = "GDP per Capita (USD)", 
       y = "Maternal Mortality Ratio",
       title = "Scatterplot of GDP per Capita (USD) vs. Maternal Mortality Ratio by Region") +
  theme(plot.title = element_text(hjust = 0.5))


# Scatterplot with Regression Line: Immunization Coverage vs. Healthcare Expenditure
plot(data$SH.IMM.IDPT, data$SH.XPD.CHEX.PC.CD,
     xlab = "Immunization Coverage (%)",
     ylab = "Healthcare Expenditure per Capita (USD)",
     main = "Immunization Coverage vs. Healthcare Expenditure",
     log ="y")

# Scatterplot of Healthcare Expenditure vs. Infant Mortality Rate by Region

plot2_data <- data.frame(Health_Expenditure = data_3$SH.XPD.CHEX.PC.CD, 
                         Infant_Mortality_Rate = data_3$SP.DYN.IMRT.IN,
                         Region = data_3$region)

ggplot(plot2_data, aes(x = Health_Expenditure, y = Infant_Mortality_Rate, color = Region)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  labs(x = "Healthcare Expenditure", 
       y = "Infant Mortality Rate",
       title = "Scatterplot of Healthcare Expenditure vs. Infant Mortality Rate by Region") +
  theme(plot.title = element_text(hjust = 0.5))

# Scatterplot Inflation vs Undernourishment
plot3_data <- data.frame(Inflation = data_3$FP.CPI.TOTL.ZG, 
                         Undernourishment = data_3$SN.ITK.DEFC.ZS,
                         Region = data_3$region)

ggplot(plot3_data, aes(x = Inflation, y = Undernourishment, color = Region)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  labs(x = "Inflation", 
       y = "Prevalence of Undernourishment",
       title = "Scatterplot of Inflation vs. Undernourishment by Region") +
  theme(plot.title = element_text(hjust = 0.5))


# Heatmap of Correlation Matrix
correlation_matrix <- cor(data2[, 2:ncol(data2)])

library(corrplot)
corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.cex = 0.7)

#Data Cleaning and preparing:

library(missForest)
data$country <- as.factor(data$country)
data2 <- subset(data, select = -c(iso2c, iso3c, status, lastupdated, capital, longitude, latitude, income, lending, region))
data_copy <- data2
data_3 <- data
data_copy$country <- NULL
imputed_data <- missForest(data_copy)
data_3[is.na(data_3)] <- imputed_data$ximp[is.na(data_3)]
data_3$SH.XPD.CHEX.PC.CD[is.na(data_3$SH.XPD.CHEX.PC.CD)] <- imputed_data$ximp$SH.XPD.CHEX.PC.CD[is.na(data_3$SH.XPD.CHEX.PC.CD)]

# Replace missing values for FP.CPI.TOTL.ZG
data_3$FP.CPI.TOTL.ZG[is.na(data_3$FP.CPI.TOTL.ZG)] <- imputed_data$ximp$FP.CPI.TOTL.ZG[is.na(data_3$FP.CPI.TOTL.ZG)]

# Replace missing values for NY.GDP.PCAP.CD
data_3$NY.GDP.PCAP.CD[is.na(data_3$NY.GDP.PCAP.CD)] <- imputed_data$ximp$NY.GDP.PCAP.CD[is.na(data_3$NY.GDP.PCAP.CD)]

# Replace missing values for SN.ITK.DEFC.ZS
data_3$SN.ITK.DEFC.ZS[is.na(data_3$SN.ITK.DEFC.ZS)] <- imputed_data$ximp$SN.ITK.DEFC.ZS[is.na(data_3$SN.ITK.DEFC.ZS)]

# Replace missing values for SH.STA.MMRT
data_3$SH.STA.MMRT[is.na(data_3$SH.STA.MMRT)] <- imputed_data$ximp$SH.STA.MMRT[is.na(data_3$SH.STA.MMRT)]

#For heatmap:
data2[is.na(data2)] <- imputed_data$ximp[is.na(data2)]
data2$SH.XPD.CHEX.PC.CD[is.na(data2$SH.XPD.CHEX.PC.CD)] <- imputed_data$ximp$SH.XPD.CHEX.PC.CD[is.na(data2$SH.XPD.CHEX.PC.CD)]



#Modelling


#Linear Regression for capturing linear relationships:

#Infant mortality rate
lm_model_infant <- lm(SP.DYN.IMRT.IN ~ NY.GDP.PCAP.CD + FP.CPI.TOTL.ZG + SH.XPD.CHEX.PC.CD + income , data = data_3)

#Immunization converage
lm_model_immune <- lm(SH.IMM.IDPT ~ NY.GDP.PCAP.CD + FP.CPI.TOTL.ZG + SH.XPD.CHEX.PC.CD + income , data = data_3)

#Maternal Mortality ratio
lm_model_maternal <- lm(SH.STA.MMRT ~ NY.GDP.PCAP.CD + FP.CPI.TOTL.ZG + SH.XPD.CHEX.PC.CD + income , data = data_3)

#Prevalence of undernourishment (% of population)
lm_model_undernourishment <- lm(SN.ITK.DEFC.ZS ~ NY.GDP.PCAP.CD + FP.CPI.TOTL.ZG + SH.XPD.CHEX.PC.CD + income , data = data_3)

summary(lm_model_infant)
summary(lm_model_immune)
summary(lm_model_maternal)
summary(lm_model_undernourishment)

#Generalized Additive Models (GAM) for capturing non linear relationships:

library(mgcv)

all_indicators <- c(
  "SP.DYN.IMRT.IN",    # Mortality rate, infant
  "SH.IMM.IDPT",       # Immunization coverage
  "SH.STA.MMRT",       # Maternal mortality ratio
  "SN.ITK.DEFC.ZS",    #Prevalence of undernourishment (% of population)
  "NY.GDP.PCAP.CD",    # GDP Per Capita / Current US$
  "FP.CPI.TOTL.ZG",    # inflation Rate
  "SH.XPD.CHEX.PC.CD"  # Health expenditure per capita (current US$)
) 

# Define the formula for each health indicator
formula_IMRT <- as.formula("SP.DYN.IMRT.IN ~ s(NY.GDP.PCAP.CD, k = 5) + s(FP.CPI.TOTL.ZG, k = 5) + s(SH.XPD.CHEX.PC.CD, k = 5)")
formula_IDPT <- as.formula("SH.IMM.IDPT ~ s(NY.GDP.PCAP.CD, k = 5) + s(FP.CPI.TOTL.ZG, k = 5) + s(SH.XPD.CHEX.PC.CD, k = 5)")
formula_MMRT <- as.formula("SH.STA.MMRT ~ s(NY.GDP.PCAP.CD, k = 5) + s(FP.CPI.TOTL.ZG, k = 5) + s(SH.XPD.CHEX.PC.CD, k = 5)")
formula_DEFC <- as.formula("SN.ITK.DEFC.ZS ~ s(NY.GDP.PCAP.CD, k = 5) + s(FP.CPI.TOTL.ZG, k = 5) + s(SH.XPD.CHEX.PC.CD, k = 5)")

# Fit GAM models for each health indicator
gam_model_infant <- gam(formula_IMRT, data = data_3)
gam_model_immune <- gam(formula_IDPT, data = data_3)
gam_model_maternal <- gam(formula_MMRT, data = data_3)
gam_model_undernourishment <- gam(formula_DEFC, data = data_3)

# Summarize each GAM model
summary(gam_model_infant)
summary(gam_model_immune)
summary(gam_model_maternal)
summary(gam_model_undernourishment)


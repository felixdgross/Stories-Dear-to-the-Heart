## 1 Setup
##

## install and load packages 
install_and_load_packages <- function() 
{
  # List of packages
  packages <- c("readxl", "writexl", "tidyverse", "dplyr", "purrr", "fuzzyjoin", "rlang",
                "compare", "lme4", "lmerTest", "haven", "psych","lmtest", "car", "ggplot2","moments")
  
  # Check if each package is installed, install if not
  for (pkg in packages) 
  {
    if (!requireNamespace(pkg, quietly = TRUE)) 
    {
      install.packages(pkg)
    }
  }
  
  # Load all the packages
  for (pkg in packages) 
  {
    if (requireNamespace(pkg, quietly = TRUE)) 
    {
      library(pkg, character.only = TRUE)
    }
  }
}

# error mitigation (remove all packages)
#for (pkg in packages) {
 # remove.packages(pkg)
#}

# Call the function to install and load packages
install_and_load_packages()

# Loaded packages
loaded_packages <- search()
loaded_package_names <- gsub("^package:", "", loaded_packages)
loaded_package_names

#################################################################################################################################################################################
#################################################################################################################################################################################
##2 ANALYSIS

dat_comprehensive <- read.csv("C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_comprehensive.csv")


# get a sliced df
dat_comprehensive_sliced <- dat_comprehensive %>%
  group_by(subject) %>%
  slice(1)
View(dat_comprehensive_sliced)

#omit all not exluded SCL Data
dat_SCL <- dat_comprehensive[dat_comprehensive$EDA.inout == 1, ]

# Convert "cond" to a categorical (factor) variable
dat_comprehensive$cond <- factor(dat_comprehensive$cond)

####################################################################################################
## H1.1 Higher cognitive transportation/attentional focus leads to lower levels of heart rate (mean)

# Get the mean HeartRate for each VP.
dat_hr <- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(MeanHeartRate = mean(HeartRate, na.rm = TRUE))
dat_hr

# Join the two data frames together
dat_1_1 <- inner_join(dat_comprehensive_sliced, dat_hr, by = "subject")

# Performing the linear regression
model1_1 <- lm(cognitive_ts ~ HeartRate, data = dat_1_1)
summary(model1_1)

#########################################################################################
## H1.2 Higher cognitive transportation/attentional focus leads to increasing skin conductance levels over time (trend).
#Colheads (see Code A for genesis)
#subject_mean_SCL = mean(SCL.sqrt.M)
#centered_SCL = SCL.sqrt.M - subject_mean_SCL
#sd_SCL = sd(SCL.sqrt.M)
#standardized_SCL = (SCL.sqrt.M - subject_mean_SCL) / sd_SCL) 


dat_SCL_lmError <- dat_SCL[,c("subject_mean_SCL",
                       "centered_SCL",
                       "SCL_sqrt_M",
                       "sd_SCL",
                       "standardized_SCL",
                       "cognitive_ts", 
                       "emotional_ts",
                       "imaginative_ts",
                       "general_ts",
                       "Epoch", 
                       "subject")]

View(dat_SCL_lmError)
write.csv(dat_SCL_lmError, file = "C:/Users/Thinkpad/Documents/_MasterThesis/Daten/dat_SCL_lmError.csv", row.names = FALSE)


dat_SCL$centered_SCL
range(dat_SCL$centered_SCL)
dat_SCL$cognitive_ts[is.na(dat_SCL$cognitive_ts)] <- mean(dat_SCL$cognitive_ts, na.rm = TRUE)


dat_SCL$cognitive_ts <- na.omit(dat_SCL$cognitive_ts)

#Without slope
model1_2 <- lmer(cognitive_ts ~ Epoch * centered_SCL + (1|subject), data = dat_SCL)
summary(model1_2)

#BB using by MLS suggested approach (following Sukalla) described here# //MLS: https://stats.stackexchange.com/questions/122009/extracting-slopes-for-cases-from-a-mixed-effects-model-lme4
#first extracting slopes to then predict remaining variance 

#facilitating convergence 
control = lmerControl(optimizer="bfgs", optCtrl=list(maxfun=100000))

#preparation to extract slopes (does not converge)
model1_2 <- lmer(centered_SCL ~ Epoch + (Epoch|subject), data = dat_SCL)

coef(summary(model1_2))[ , "Estimate"]
coef(summary(model1_2))
beta <- ranef(model1_2)$subject
colnames(beta) <- c("Intercept", "EpochSlope")


#How many epochs does every subject have?
table(table(dat_1_2$subject))

# Convert row names to a new column
beta$subject <- rownames(beta)

# Reset row names
rownames(beta) <- NULL

#merge
data1_2 <- merge(beta, dat_SCL, by = "subject", all = FALSE)


#num VP left
num_distinct_VP <- length(unique(dat_SCL$subject))
num_distinct_VP

model1_2_final <- lmer(SCL.sqrt.M ~ EpochSlope * cognitive_ts + (1|subject), data = data1_2)
summary(model1_2_final)

#########################################################################################
## H2.1 Higher emotional transportation leads to increases of skin conductance levels over time (trend)


model2_1 <- lmer(emotional_ts ~ Epoch * centered_SCL + (1|subject), data = dat_SCL)
summary(model2_1)

#########################################################################################
## H2.2 Higher emotional transportation leads to lower HRV. 


# get mean RMSSD for each VP (via all RMSSD values)
dat_RMSSD<- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(RMSSD = mean(RMSSD, na.rm = TRUE))
dat_RMSSD

# get mean RMSSD for each VP (via all three story part's RMSSD values)
# Sum the three columns for each subject
dat_comprehensive_sliced$RMSSD <- (dat_comprehensive_sliced$RMSSD_mean_Pt_0 + dat_comprehensive_sliced$RMSSD_mean_Pt_1 +dat_comprehensive_sliced$RMSSD_mean_Pt_2)/3 
dat_RMSSD <- dat_comprehensive_sliced[, c("subject", "RMSSD")]


# Join the two data frames together (change here wheter to use dat_RMSSD or dat_comprehensive_sliced)
dat_2_2 <- inner_join(dat_RMSSD, dat_emotional_ts, by = "subject")

# Cleaning and converting the data (removing comata and change data type to numeric)
dat_2_2$RMSSD <- as.numeric(gsub(",", ".", dat_2_2$RMSSD))
dat_2_2$ts.sf4 <- as.numeric(gsub(",", ".", dat_2_2$ts.sf4))

# control
dat_2_2$RMSSD

# Performing the linear regression
model2_2 <- lm(emotional_ts ~ RMSSD , data = dat_comprehensive)
summary(model2_2)

#########################################################################################
## H3.1 More (compared to less) emotional shifts lead to higher transportation

# Create histograms for each group
ggplot(dat_comprehensive_sliced, aes(x = general_ts)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ cond, scales = "free") +
  ggtitle("Histograms of general_ts by cond")

#Remember that normality is an assumption, and in many cases, moderate deviations from normality 
#may not invalidate the results of a t-test, especially with reasonably large sample sizes. 
#The decision to apply transformations or choose alternative tests should be made based on 
#the specific characteristics of your data and the goals of your analysis. Always interpret 
#the results of any normality test in conjunction with other evidence and the context of your study.

#homogeniety of variances (approx. equal)
leveneTest(general_ts ~ cond, data = dat_comprehensive_sliced)

result1 <- lm(general_ts ~ cond, data = dat_comprehensive, var.equal = TRUE) 
summary(result1)

#########################################################################################
## H3.2 More (compared to less) emotional shifts lead to higher emotional transportation


# Create histograms for each group
ggplot(dat_comprehensive_sliced, aes(x = emotional_ts)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ cond, scales = "free") +
  ggtitle("Histograms of emotional_ts by cond")


#homogeniety of variances (approx. equal)
leveneTest(emotional_ts ~ cond, data = dat_comprehensive_sliced)

result2 <- lm(emotional_ts ~ cond, data = dat_comprehensive_sliced, var.equal = TRUE) 
summary(result2)

#########################################################################################
## H3.3 More (compared to less) emotional shifts lead to higher increases of skin conductance levels over time (trend)

model3_3 <- lmer(centered_SCL ~ cond + Epoch + cond:Epoch + (1|subject), data = dat_SCL)
summary(model3_3)

#########################################################################################
## H3.4 More (compared to less) emotional shifts lead to lower HRV

# Join the two relevant data frames together
dat_3_4 <- inner_join(dat_comprehensive, dat_comprehensive_sliced, by = "subject")


result <- lm(RMSSD.x ~ cond.x, data = dat_3_4, var.equal = TRUE)
summary(result)

#########################################################################################
#Research Question 1 (RQ 1) How does imaginative transportation affect heart rate and skin conductance?

# Get the mean HeartRate for each VP.
dat_hr <- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(MeanHeartRate = mean(HeartRate, na.rm = TRUE))

# Join the two data frames together
dat_RQ_1_HR <- inner_join(dat_comprehensive_sliced, dat_hr, by = "subject")
View(dat_RQ_1_HR)

# Cleaning and converting the data (removing comata and change data type to numeric)
dat_RQ_1_HR$imaginative_ts <- as.numeric(gsub(",", ".", dat_RQ_1_HR$imaginative_ts))
dat_RQ_1_HR$HeartRate <- as.numeric(gsub(",", ".", dat_RQ_1_HR$HeartRate))


# Performing the linear regression
modelRQ_1_HR <- lm(HeartRate ~ imaginative_ts, data = dat_RQ_1_HR)
summary(modelRQ_1_HR)

# Summary of the model to get coefficients
summary(modelRQ_1_HR)

# Plot diagnostics
dev.new(width=5, height=5)
par(mfrow = c(2, 2))
plot(modelRQ_1_HR)

# Check for normality of residuals
shapiro.test(resid(modelRQ_1_HR))

# Test for heteroskedasticity (constant variance of residuals)
bptest(modelRQ_1_HR, studentize=FALSE)

#########################################################################################
## SCL RQ 1
dat_hr <- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(MeanHeartRate = mean(HeartRate, na.rm = TRUE))


# Join the two data frames together
dat_RQ_2_HR <- inner_join(dat_comprehensive_sliced, dat_hr, by = "subject")
View(dat_RQ_2_HR)

# Cleaning and converting the data (removing comata and change data type to numeric)
dat_RQ_2_HR$mean_general_ts <- as.numeric(gsub(",", ".", dat_RQ_2_HR$mean_general_ts))
dat_RQ_2_HR$HeartRate <- as.numeric(gsub(",", ".", dat_RQ_2_HR$HeartRate))


# Performing the linear regression
modelRQ_1_SCL <- lmer(imaginative_ts ~ Epoch * centered_SCL + (1|subject), data = dat_SCL)
summary(modelRQ_1_SCL)


#########################################################################################  
#Research Question 2 (RQ 2) How does general transportation affect heart rate and skin conductance?

# Get the mean HeartRate for each VP.
dat_hr <- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(MeanHeartRate = mean(HeartRate, na.rm = TRUE))

# Join the two data frames together
dat_RQ_2_HR <- inner_join(dat_comprehensive_sliced, dat_hr, by = "subject")

# Cleaning and converting the data (removing comata and change data type to numeric)
dat_RQ_2_HR$mean_general_ts <- as.numeric(gsub(",", ".", dat_RQ_2_HR$general_ts))
dat_RQ_2_HR$HeartRate <- as.numeric(gsub(",", ".", dat_RQ_2_HR$HeartRate))

# Performing the linear regression
modelRQ_2_HR <- lm(general_ts ~ HeartRate, data = dat_RQ_2_HR)

# Summary of the model to get coefficients
summary(modelRQ_2_HR)

# Plot diagnostics
dev.new(width=5, height=5)
par(mfrow = c(2, 2))
plot(modelRQ_2_HR)

# Check for normality of residuals
shapiro.test(resid(modelRQ_2_HR))

# Test for heteroskedasticity (constant variance of residuals)
bptest(modelRQ_2_HR, studentize=FALSE)

#########################################################################################
## SCL RQ 2

delRQ_2_SCL <- lmer(general_ts ~ Epoch * centered_SCL + (1|subject), data = dat_SCL)
summary(modelRQ_2_SCL)


#########################################################################################
#Research Question 3 (RQ 3) How do more emotional shifts influence heart rate?
# Join the two relevant data frames together

# Get the mean HeartRate for each VP.
dat_hr <- dat_comprehensive %>%
  group_by(subject) %>%
  summarise(MeanHeartRate = mean(HeartRate, na.rm = TRUE))

# Join the two data frames together
dat_RQ_3 <- inner_join(dat_comprehensive_sliced, dat_hr, by = "subject")

result3 <- lm(HeartRate ~ cond, data = dat_RQ_3, var.equal = TRUE)
result3

#########################################################################################  
#Research Question 4 (RQ 4) /and Hypohthesis 4 How is HRV connected with Heart Rate and SCL and transportation subscales

# Fit the multiple linear regression model
modelRQ_4 <- lm(RMSSD ~ HeartRate + centered_SCL * Epoch , data = dat_comprehensive)

# View the summary of the model
summary(modelRQ_4)



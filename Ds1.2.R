library(dplyr)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)



diabetes <- read.csv("diabetes_prediction_dataset.csv",header = T)
View(diabetes)

########  1)	Introduction  ################

########### 2)	Complete Data description (source of the data, number of observations
# , target population, clear explanation for the variables measured, etc.)   ##########

######gender
# Gender refers to the biological sex of the individual, which can have an impact
# on their susceptibility to diabetes. There are three categories in it male ,female and other.

#######Age
# Age is an important factor as diabetes is more commonly diagnosed in older adults.
# Age ranges from 0-80 in our dataset.

##########hypertension
# Hypertension is a medical condition in which the blood pressure in the arteries is 
# persistently elevated. It has values a 0 or 1 where 0 indicates they don’t have hypertension 
# and for 1 it means they have hypertension.

#########heart_disease
# Heart disease is another medical condition that is associated with an increased risk of 
# developing diabetes. It has values a 0 or 1 where 0 indicates they don’t have heart disease
# and for 1 it means they have heart disease.


#######smoking_history
# Smoking history is also considered a risk factor for diabetes and can exacerbate the 
# complications associated with diabetes.In our dataset we have 5 categories i.e not current,
# former,No Info,current,never and ever.

#########bmi
# BMI (Body Mass Index) is a measure of body fat based on weight and height. Higher BMI values 
# are linked to a higher risk of diabetes. The range of BMI in the dataset is from 10.16 to 71.55.
# less than 18.5 is underweight, 18.5-24.9 is normal, 25-29.9 is overweight, and 30 or more is 
# obese.

########HbA1c_level
# HbA1c (Hemoglobin A1c) level is a measure of a person's average blood sugar level over the past 2-3 months. 
# Higher levels indicate a greater risk of developing diabetes. Mostly more than 6.5% of HbA1c Level indicates diabetes.

########blood_glucose_level
# Blood glucose level refers to the amount of glucose in the bloodstream at a given time. 
# High blood glucose levels are a key indicator of diabetes.

######diabetes
# Diabetes is the target variable being predicted, with values of 1 indicating the presence 
# of diabetes and 0 indicating the absence of diabetes.



# 3)	Complete Descriptive analysis (numerical measures, tables, plots)######

#Finding out duplicates 
duplicated(diabetes)
sum(duplicated(diabetes))

#Removing Dupliactes 
diabetes=unique(diabetes)

#cleaning the data
library(dplyr)

str(diabetes)
summary(diabetes)


table(diabetes$gender)

diabetes$gender <- na_if(diabetes$gender, "Other") ## removing other from categories of gender

table(diabetes$smoking_history)
#Recoding the categories
diabetes$smoking_history [diabetes$smoking_history  =="never"]="Non Smoker"
diabetes$smoking_history [diabetes$smoking_history  =="No Info"]="Non Smoker"
diabetes$smoking_history [diabetes$smoking_history  =="ever"]="Past Smoker"
diabetes$smoking_history [diabetes$smoking_history  =="former"]="Past Smoker"
diabetes$smoking_history [diabetes$smoking_history  =="not current"]="Past Smoker"
# Check the new value counts
table(diabetes$smoking_history )
clean_data <-  na.omit(diabetes)
#Changing Variables to factors 
clean_data$gender <- as.factor(clean_data$gender)
clean_data$smoking_history <- as.factor(clean_data$smoking_history)
clean_data$diabetes=as.factor(clean_data$diabetes)
clean_data$heart_disease=as.factor(clean_data$heart_disease)
clean_data$hypertension=as.factor(clean_data$hypertension)
##############################################################################
library(psych)
library(DescTools)


Quant_var <- clean_data[,c(2,6,7,8)]

Qual_var <- clean_data[,-c(2,6,7,8)]

Mode(Qual_var$gender)
Mode(Qual_var$hypertension)
Mode(Qual_var$heart_disease)
Mode(Qual_var$smoking_history)
Mode(Qual_var$diabetes)

describe(Quant_var$age)
describe(Quant_var$bmi)
describe(Quant_var$HbA1c_level)
describe(Quant_var$blood_glucose_level)

##Categorical tables
table(Qual_var$gender)
table(Qual_var$hypertension)
table(Qual_var$heart_disease)
table(Qual_var$smoking_history)
table(Qual_var$diabetes)

table(Qual_var$gender,Qual_var$diabetes)
table(Qual_var$hypertension,Qual_var$diabetes)
table(Qual_var$heart_disease,Qual_var$diabetes)
table(Qual_var$smoking_history,Qual_var$diabetes)
####################################################################################

#Formattable Tables & Plots 
library(ggplot2)

#Table 1: Gender Tables 
# Create a summary 
table_1<- table(Qual_var$gender)
table_1

# Convert the table to a dataframe
table_1_df <- as.data.frame(table_1)

colnames(table_1_df) <- c("Gender", "Frequency")

#Pie Charts 

# Calculate percentage
table_1_df$Percentage <- (table_1_df$Frequency / sum(table_1_df$Frequency)) * 100

# Plotting a pie chart with blue shades
blue_palette <- c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef","#c6dbe9")  # Blue shades

p1 <- ggplot(table_1_df, aes(x = "", y = Frequency, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start = 0) +
  ggtitle("Gender Distribution") +
  theme_void() +  # Remove background and gridlines
  theme(legend.position = "right") +
  scale_fill_manual(values = blue_palette)  # Blue color palette

p1


#Frequency table 
library(formattable)
formattable(table_1_df)

#Table 2: Hypertesion
# Create a summary 
table_1<- table(Qual_var$hypertension)
table_1

# Convert the table to a dataframe
table_1_df <- as.data.frame(table_1)

colnames(table_1_df) <- c("Hypertesion", "Frequency")

#Pie Charts 

# Calculate percentage
table_1_df$Percentage <- (table_1_df$Frequency / sum(table_1_df$Frequency)) * 100

# Plotting a pie chart with blue shades
blue_palette <- c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef","#c6dbe9")  # Blue shades

p1 <- ggplot(table_1_df, aes(x = "", y = Frequency, fill = Hypertesion)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start = 0) +
  ggtitle("Hypertension Distribution") +
  theme_void() +  # Remove background and gridlines
  theme(legend.position = "right") +
  scale_fill_manual(values = blue_palette)  # Blue color palette

p1

#Frequency table 
library(formattable)
formattable(table_1_df)


#Table 3: Heart Disease
# Create a summary 
table_1<- table(Qual_var$heart_disease)
table_1

# Convert the table to a dataframe
table_1_df <- as.data.frame(table_1)
colnames(table_1_df) <- c("Heart Disease", "Frequency")

#Frequency table 
library(formattable)
formattable(table_1_df)

#Barchart for heart disease variable
p3 <- ggplot(Qual_var, aes(x = heart_disease, fill = "Specific Color")) + 
  geom_bar(position = "dodge") +
  ggtitle("Heart Disease Bar Chart") +
  xlab("Heart Disease")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7"), guide = FALSE)  # Adjust fill colors to blue tones

p3





#Table 4: smoking_history
# Create a summary
table_1<- table(Qual_var$smoking_history)
table_1

# Convert the table to a dataframe
table_1_df <- as.data.frame(table_1)
colnames(table_1_df) <- c("Smoking History", "Frequency")

#Frequency table 
library(formattable)
formattable(table_1_df)

#Barchart for heart disease variable
p3 <- ggplot(Qual_var, aes(x = smoking_history, fill = "Specific Color")) + 
  geom_bar(position = "dodge") +
  ggtitle("Smoking History Bar Chart") +
  xlab("Smoking History")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7"), guide = FALSE)  # Adjust fill colors to blue tones

p3


#Table 5: Diabetes
# Create a summary 
table_1<- table(Qual_var$diabetes)
table_1

# Convert the table to a dataframe
table_1_df <- as.data.frame(table_1)
colnames(table_1_df) <- c("Diabetes", "Frequency")

#Frequency table 
library(formattable)
formattable(table_1_df)

#Barchart for heart disease variable
p3 <- ggplot(Qual_var, aes(x = diabetes, fill = "Specific Color")) + 
  geom_bar(position = "dodge") +
  ggtitle("Diabetes Bar Chart") +
  xlab("Diabetes")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7"), guide = FALSE)  # Adjust fill colors to blue tones

p3

###############################################################
###Quantitative Variables
#Variable 1: Age 

p1=ggplot(data = Quant_var, aes(x = age)) +
  geom_histogram(binwidth = 20, fill = "#98bece" , color = "#336699") +
  ggtitle("Age Histogram") +
  xlab("Age Price") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p1

# Boxplot
p2= ggplot(data = Quant_var, aes(x = "", y = age)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of Age") +
  ylab("Unit Price") +
  theme_bw()+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p2

# Density Plot
p3=ggplot(data = Quant_var, aes(x = age)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of Age") +
  xlab("Age") +
  ylab("Density") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )


# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(Quant_var$age)

#######################################################################################
# Function to create the plots
create_plots <- function(var_name) {
  p_hist <- ggplot(data = Quant_var, aes(x ==var_name)) +
    geom_histogram(binwidth = 20, fill = "#98bece" , color = "#336699") +
    ggtitle(paste(var_name, "Histogram")) +
    xlab(paste(var_name, "Price")) +
    ylab("Frequency") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  p_boxplot <- ggplot(data = Quant_var, aes(x = "", y = !!sym(var_name))) +
    geom_boxplot(fill = "#98bece", color = "#336699") +
    ggtitle(paste("Boxplot of", var_name)) +
    ylab("Unit Price") +
    theme_bw() + theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  p_density <- ggplot(data = Quant_var, aes(x = !!sym(var_name))) +
    geom_density(fill = "#98bece", color = "#336699") +
    ggtitle(paste("Density Plot of", var_name)) +
    xlab(var_name) +
    ylab("Density") +
    theme_bw() + theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  combined_plots <- plot_grid(p_hist, p_density, p_boxplot, nrow = 2)
  return(combined_plots)
}

# Apply the function to each variable in the Quant_var data frame

create_plots("bmi")
create_plots("HbA1c_level")
create_plots("blood_glucose_level")
##################################################################################
#Variable 2: BMI

###Quantitative Variables
#Variable 2: bmi

p1=ggplot(data = Quant_var, aes(x = bmi)) +
  geom_histogram(binwidth = 20, fill = "#98bece" , color = "#336699") +
  ggtitle("BMI Histogram") +
  xlab("BMI ") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p1

# Boxplot
p2= ggplot(data = Quant_var, aes(x = "", y = bmi)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of BMI") +
  ylab("Unit Price") +
  theme_bw()+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+ylim(0,65)
p2

# Density Plot
p3=ggplot(data = Quant_var, aes(x = bmi)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of BMI") +
  xlab("BMI") +
  ylab("Density") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p3

# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(Quant_var$bmi)

#############################################
#Variable 3: HbA1c_level

###Quantitative Variables
#Variable 3: HbA1c_level

p1=ggplot(data = Quant_var, aes(x = HbA1c_level)) +
  geom_histogram(binwidth = 0.25, fill = "#98bece" , color = "#336699") +
  ggtitle("HbA1c level Histogram") +
  xlab("HbA1c level") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p1

# Boxplot
p2= ggplot(data = Quant_var, aes(x = "", y =HbA1c_level)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of HbA1c level") +
  ylab("HbA1c level") +
  theme_bw()+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p2

# Density Plot
p3=ggplot(data = Quant_var, aes(x = HbA1c_level)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of HbA1c level") +
  xlab("HbA1c level") +
  ylab("Density") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p3

# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(Quant_var$HbA1c_level)
########################################################33
##blood_glucose_level

#Variable 3: blood_glucose_level

###Quantitative Variables
#Variable 3: blood_glucose_level

p1=ggplot(data = Quant_var, aes(x =blood_glucose_level)) +
  geom_histogram( fill = "#98bece" , color = "#336699") +
  ggtitle("Blood glucose level Histogram") +
  xlab("HBlood glucose level") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p1

# Boxplot
p2= ggplot(data = Quant_var, aes(x = "", y =blood_glucose_level)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of Blood glucose level level") +
  ylab("Blood glucose level level") +
  theme_bw()+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p2

# Density Plot
p3=ggplot(data = Quant_var, aes(x = blood_glucose_level)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of Blood glucose level") +
  xlab("Blood glucose  level") +
  ylab("Density") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p3

# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(Quant_var$blood_glucose_level)

############################################################################################
#Na2es part analysing variables together
#Correlation Plot
#Heat Map
#Barcharts 


############################################################################################
#Logistic Model

# Split the dataset into a training set and a testing test with 70% of the data used for training and 30%
#for testing.

indexes  <- sample(1: nrow(clean_data), size = 0.7*nrow(clean_data))

train1 = clean_data[indexes,]
test_set = clean_data[-indexes,]
Outcome_test = clean_data$diabetes[-indexes]

nrow(train1)

nrow(test_set)

# We have class imbalance so my model may not be the best model out there

table(clean_data$diabetes)

# Libraries that I use.
#install.packages("themis") # for additional algorithms (sampling)
#install.packages("DMwR") # for smote

library(themis)
library(ROSE)

train1$diabetes=as.factor(train1$diabetes)

train_set2=upSample(x=train1[,-9],y=train1$diabetes)

table(train_set2$Class)

diab_train_fit <- glm(Class ~ .  , data= train_set2, family = binomial)
summary(diab_train_fit)

train.prob = predict(diab_train_fit, test_set, type = "response")



diabetes_test_LR <-test_set

diabetes_test_LR$pred_model<- predict(object = diab_train_fit ,newdata = diabetes_test_LR,type = "response")
diabetes_test_LR$label_model <- as.factor(ifelse(diabetes_test_LR$pred_model > .5, "Diabetes", "Not Diabetes"))

ggplot(diabetes_test_LR, aes(x=pred_model)) +
  geom_density(lwd=0.5) +
  labs(title = "Distribution of Probability Prediction Data",x="Diabetes Probability",y="Density") +
  theme_minimal()


confusionMatrix_LR <- confusionMatrix(data = diabetes_test_LR$label_model,reference = diabetes_test_LR$Outcome,positive = "Diabetes")
confusionMatrix_LR

diab_test.pred <- rep(0,  61284)
diab_test.pred[train.prob> 0.5] = 1
table(diab_test.pred, Outcome_test)


mean(diab_test.pred == Outcome_test)

mean(diab_test.pred != Outcome_test)

################################################################
## Random Forest // our machine learning model

#We will take a sample as the computer proccesor can not allocate a vector of
#size 111.9 GB

set.seed(42)
data2=sample_n(train_set2,10000 , replace = FALSE)


model=  randomForest(Class ~ ., data=data2, proximity=TRUE)

model
## Now check to see if the random forest is actually big enough...
## Up to a point, the more trees in the forest, the better. You can tell when
## you've made enough when the OOB no longer improves.
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "0", "1"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"0"], 
          model$err.rate[,"1"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#Lets try increasing the number of my trees to 1000 isntead of 500
model2=  randomForest(Class ~ ., data=data2,ntree=1000 ,proximity=TRUE)

model2

#No change in the oob error rate, no better preformnace 

#Are we considering the optimum of variables?

## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Class ~ ., data=data2, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
model3 <- randomForest(Class ~ ., 
                       data=data2,
                       ntree=500, 
                       proximity=TRUE, 
                       mtry=which(oob.values == min(oob.values)))


model3
#############################################################
#Last trial for logistic model
#Load packages
library(caret)
library(pROC)
library(plotROC)
library(ROCR)
library(pscl)
library(lmtest)
library(ResourceSelection)
library(rms)

################################################################################

train1$diabetes=as.factor(train1$diabetes)

train_set2=upSample(x=train1[,-9],y=train1$diabetes)

table(train_set2$Class)

diab_train_fit <- glm(Class ~ .  , data= train_set2, family = binomial)
summary(diab_train_fit)


#Creating the Logistic Regression Model
#It models the probability of the binary outcome variable 
logmodel <- glm(Class ~ .  , data= train_set2, family= binomial(link='logit'))
#Summary of the model including coefficients & statistical significance
summary(logmodel)

#Odds ratios
odds_ratios <- exp(coef(logmodel))
print(odds_ratios)
t(as.matrix(odds_ratios))
df <- as.data.frame.matrix(t(as.matrix(odds_ratios)))
library(formattable)
formattable(df)

#Analysis of deviance table
anova(logmodel)
anova(logmodel, test = "Chisq")
anova(logmodel, test ="LRT")

#Predicted logit values
predictedlogit <- predict(logmodel,test_set) 
summary(predictedlogit)
#Generationg the predicted probabilities
predictedprob <- predict(logmodel, test_set, type="response")
summary(predictedprob)
# ROC plot
roc_score <- roc(test_set$diabetes, predictedprob)
roc_curve <- plot(roc_score, main="ROC curve – Binary Logistic Regression", lwd = 2,  
                  xlab = "1 - Specificity")
axis(1, at = seq(0, 1, by = 0.1), labels = seq(1, 0, by = -0.1))
auc_value <- auc(roc_curve)

#Finding the optimal cutoff based on sensitivity and specificity
optimal_cutoff <- coords(roc_curve, "best", best.method = "closest.topleft")$threshold

optimal_cutoff
#Classification table
predictclass <- ifelse(predictedprob > optimal_cutoff, 1, 0)
CM <- table(test_set$diabetes, predictclass)
Classification_table <- as.matrix(CM)
#Error metrics and confusion matrix with Caret
#Providing metrics like like accuracy, sensitivity, specificity, etc.
confusionMatrix(factor(test_set$diabetes), factor(predictclass))

# McFadden's R²
mcfadden_r2 <- pscl::pR2(logmodel)["McFadden"]

# VIF values for each predictor variable
vif_values <- car::vif(logmodel)

mcfadden_r2
vif_values






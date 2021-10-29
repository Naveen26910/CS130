Collaborators "Eisha"

#Q1
#step 1
#defined DGP
dgp = rnorm(500)

#irreducible error(stochastic component)
irr_error <- rnorm(500, mean = 1, sd = 0.2)

#independent variables
sleep <- sample(1:8, 1000, replace=T)
prep_level <- sample(18:80, 1000, replace=T)

#dependent variable
test_scores <- sleep + 10 * prep_level + irr_error + 8 * dgp

data_1 <- data.frame(test_scores, dgp, sleep, prep_level, irr_error)
head(data_1)

#step 2
#reg_1= fitting regression model
reg_1 <- lm(test_scores ~ dgp + sleep + prep_level, data = data_1)
summary(reg_1)

reg_1$coeff

#step 3
#using confint function to find confidence interval by interpolation in profile traces
lm_test_scores <- lm( test_scores ~ sleep + prep_level)
confint(lm_test_scores, 'sleep', level=0.95)
confint(lm_test_scores, 'prep_level', level=0.95)

#step4 
quantile(sim(reg_1, n.sims = 100000)@coef[,2], probability = c(0.025, 0.975))
quantile(sim(reg_1, n.sims = 100000)@coef[,3], probability = c(0.025, 0.975))

#step5
predicted_outcome <- coefficient_test_scores[,1] + coefficient_sleep * 1 + coefficient_prep_level * -2
quantile(predicted_outcome,probability=c(0.025,0.975))

#step 6
coefficient_function <- function(data_1,i){
 model_1 <- lm(data[,1][i]~data[,2][i] + data[,3][i])
 model_1$coefficient
}

i_1 <-sample(500,500, replace = T)
coefficient_function (data_1, i_1)

#finding bootstrapping for CI
boot_strapping <- boot(data_1,coefficient_function)
boot.ci(boot.out= boot_strapping, type= ("norm"))

#Q2
housing =
read.csv("https://github.com/ageron/handson-ml/raw/master/datasets/housing/housing.csv")

#step 1
#using validation set approach
#installing library Metrics to use MSE function 


#randomising division of data into two sets
set.seed(1503)

#splitting data into training and testing sets

#source: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
housing <- drop_na(housing)
index <- sample((nrow(housing)), size = nrow(housing) * 0.8)
housing.training_set <- housing[index,]
housing.testing_set <- housing[-index,]

#first linear model as given in instructions
model_1 <- lm(median_house_value ~ housing_median_age + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing, housing.training_set )

#second linear model as given in instructions
model_2 <- lm(median_house_value ~ housing_median_age + I(housing_median_age^2) + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing, housing.training_set)

#using predict function to find output of model_1 with testing data
predict_model_1 <- predict(model_1,housing.testing_set)

#using predict function to find output of model_2 with testing data
predict_model_2 <- predict(model_2,housing.testing_set)

#source: https://www.statology.org/how-to-calculate-mse-in-r/
#calculating MSE for model_1 and model_2

mse_model_1 <- (mean((housing.testing_set$median_house_value-predict_model_1)^2))
mse_model_2 <- (mean((housing.testing_set$median_house_value-predict_model_2)^2))

mse_model_1
mse_model_2

install.packages(boot)
library(boot)

#step 2
#writing them as glm models
#glm model_1 and glm model_2
glm_model_1 <- glm(median_house_value ~ housing_median_age + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing)
gml_model_2 <- glm(median_house_value ~ housing_median_age + I(housing_median_age^2) + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing)

#running LOOCV, using k=1
cv.err1 <- cv.glm (housing , glm_model_1)
cv.err2 <- cv.glm (housing , glm_model_2)
cv.err1$delta
cv.err2$delta

#step 3

#writing them as glm models
#glm model_1 and glm model_2
glm_model_1 <- glm(median_house_value ~ housing_median_age + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing)
gml_model_2 <- glm(median_house_value ~ housing_median_age + I(housing_median_age^2) + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing)

#running k fold cross validation, using k=10
cv.err1 <- cv.glm (housing , glm_model_1, 10)
cv.err2 <- cv.glm (housing , glm_model_2, 10)
cv.err1$delta
cv.err2$delta

#Q3 
#step 1
library(boot)

data("ToothGrowth")
#dividing data into different treatments of vitamin C
#source:https://towardsdatascience.com/a-practical-guide-to-bootstrap-with-r-examples-bd975ec6dcea
#defining function for avg treatment effect (ATE)

function_1 <- function(data, i){
#selecting observations for data for Avg_treat_effects
    
a <- c(data[,1][i])
b <- c(data[,1][-i])

#final defined function
Avg_treat_effects <- 1/length(i) * (a-b)
    
Avg_treat_effects

}

#filtering out dataset to leave out OJ 
ToothGrowth_1 <- ToothGrowth[ToothGrowth$supp=='OJ',]]
dose_1 <- ToothGrowth_1[ToothGrowth_1$dose==0.5,]
dose_2 <- ToothGrowth_1[ToothGrowth_1$dose==2,]

#Q4

admission <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#step 1
admit_model1 <- glm(admit ~ ., data = admission, family = "binomial")
summary(admit_model1)

#step 2
#we will write new function to predict change to logit if GPA changes
predict_logit <- function()


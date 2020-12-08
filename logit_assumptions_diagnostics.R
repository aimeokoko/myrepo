#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# The logistic regression method assumes that:
#   
#   The outcome is a binary or dichotomous variable like yes vs no, positive vs negative, 1 vs 0.
# There is a linear relationship between the logit of the outcome and each predictor variables. Recall that the logit function is logit(p) = log(p/(1-p)), where p is the probabilities of the outcome (see Chapter @ref(logistic-regression)).
# There is no influential values (extreme values or outliers) in the continuous predictors
# There is no high intercorrelations (i.e. multicollinearity) among the predictors.

library(tidyverse)
library(broom)
theme_set(theme_classic())

#Building a logistic regression model Fri Oct 09 05:37:47 2020----------

##predicting the probability of diabetes test positivity
# Load the data
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Fit the logistic regression model
model <- glm(diabetes ~., data = PimaIndiansDiabetes2, 
             family = binomial)
# Predict the probability (p) of diabete positivity
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)


##Linearity assumption----

#check the linear relationship between 
#continuous predictor variables and the logit of the outcome
## scatter plot between each predictor and the logit values
##Remove qualitative variables from the original data

# Select only numeric predictors
mydata <- PimaIndiansDiabetes2 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

##Create the scatter plots
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#Influential values Fri Oct 09 05:56:35 2020----------
##visualizing the Cook’s distance values
plot(model, which = 4, id.n = 3)

##standardized residual error can be inspected. 
##Data points with an absolute standardized residuals above 3 
##represent possible outliers and may deserve closer attention.

## standardized residuals (.std.resid) and the Cook’s distance (.cooksd)

# Extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 

## top 3 largest values, according to the Cook’s distance
model.data %>% top_n(3, .cooksd)

##Plot the standardized residuals:
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = diabetes), alpha = .5) +
  theme_bw()

##Filter potential influential data points
model.data %>% 
  filter(abs(.std.resid) > 3)

# 
# When you have outliers in a continuous predictor, potential solutions include:
#   
#   Removing the concerned records
# Transform the data into log scale
# Use non parametric methods

#Multicollinearity Fri Oct 09 05:59:47 2020----------

##data contain highly correlated predictor variables
# removing the concerned variables
car::vif(model)

## a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity

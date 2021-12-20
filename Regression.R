library(tidymodels)
library(corrplot)
library(dplyr)    
library(broom)

#Q1 

data(ames)

ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price)

ames_train <- training(ames_split)
ames_test <- testing(ames_split)


# Q2 and Q3

linear_model <- lm(Sale_Price ~ MS_SubClass + MS_Zoning + Lot_Frontage + Lot_Area + Street+ Alley + Lot_Shape + 
                     Land_Contour + Utilities + Lot_Config,data = ames_train)
summary(linear_model)


# Q4 

simple_ames <- 
  recipe(Sale_Price ~ MS_SubClass + MS_Zoning + Lot_Frontage + Lot_Area + Street+ Alley + Lot_Shape + Land_Contour + Utilities + Lot_Config,
         data = ames_train) %>%
  step_dummy(all_nominal()) %>%
  prep(training = ames_train)


train_new <- bake(simple_ames, new_data = NULL)
test_new <- bake(simple_ames, new_data = ames_test)


# Q5 Getting correlations between variables

correlations <- cor(train_new, method="pearson")
correlations
corrplot(correlations, method = "circle", tl.cex = 0.5)    

  
# as we can see from the plot some variables like MS_Zoning_Residential_Medium_Density and MS_Zoning_Residential_Low_Density has high corellations 
# and Alley_No_Alley_Access and Alley_No_Paved. Land_Contour_Lvl, Land_Contour_HLS

train_new <- select(train_new, -'Alley_No_Alley_Access', -'MS_Zoning_Residential_Medium_Density', -'Land_Contour_HLS')
test_new <- select(test_new, -'Alley_No_Alley_Access', -'MS_Zoning_Residential_Medium_Density', -'Land_Contour_HLS')


# Q6 P Values 

pvalues <- lm(Sale_Price ~ MS_SubClass + MS_Zoning + Lot_Frontage + Lot_Area + Street+ Alley + Lot_Shape + Land_Contour + Utilities + Lot_Config, data = ames)
summary(pvalues)

# observed the summary of the linear model and got the information that variables given below has high(p>0.05) p values so they are statistically 
# insignificant and we can exclude them from our model.

# MS_SubClass_One_Story_with_Finished_Attic_All_Ages, MS_SubClass_Two_Story_1945_and_Older, MS_SubClass_One_and_Half_Story_PUD_All_Ages,
# MS_SubClass_Two_Story_PUD_1946_and_Newer, MS_SubClass_PUD_Multilevel_Split_Level_Foyer,Alley_No_Alley_Access, Alley_Paved,Lot_Shape_Irregular,Land_Contour_Low,
# Utilities_NoSeWa,Utilities_NoSewr,Lot_Config_FR2, Lot_Config_FR3, Lot_Config_Inside                                                                                                                                                                                                                                                                                                             
                                                                                                                       
train_excluded <- select(train_new, -'MS_SubClass_One_Story_with_Finished_Attic_All_Ages', -'MS_SubClass_Two_Story_1945_and_Older', -'MS_SubClass_One_and_Half_Story_PUD_All_Ages', -'MS_SubClass_Two_Story_PUD_1946_and_Newer', -'MS_SubClass_PUD_Multilevel_Split_Level_Foyer',  -'Alley_Paved', -'Lot_Shape_Irregular', -'Land_Contour_Low', -'Utilities_NoSeWa', -'Utilities_NoSewr', -'Lot_Config_FR2', -'Lot_Config_FR3', -'Lot_Config_Inside' )
test_excluded <- select(test_new, -'MS_SubClass_One_Story_with_Finished_Attic_All_Ages', -'MS_SubClass_Two_Story_1945_and_Older', -'MS_SubClass_One_and_Half_Story_PUD_All_Ages', -'MS_SubClass_Two_Story_PUD_1946_and_Newer', -'MS_SubClass_PUD_Multilevel_Split_Level_Foyer',  -'Alley_Paved', -'Lot_Shape_Irregular', -'Land_Contour_Low', -'Utilities_NoSeWa', -'Utilities_NoSewr', -'Lot_Config_FR2', -'Lot_Config_FR3', -'Lot_Config_Inside' )



mdl <- lm(Sale_Price ~ . , data = train_excluded)

summary(mdl)
glance(mdl)
tidy(mdl)

predict(mdl, test_excluded)

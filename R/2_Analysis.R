# import data
source("R/1_Import_Prepare_Dataset.R")

library(nlme)
library(emmeans)


#############################################################
######## PRODUCTION DEFICIT (FRUITSET * WEIGHT) #############

#Analysis for Cultivar, Region and Year separately
Model_Production1 <- lme(DeficitFruitSet ~ Apple_variety, random = ~1|Tree/Location, data = PollinationDeficit2)
Model_Production2 <- lme(DeficitFruitSet ~ Region, random = ~1|Tree/Location, data = PollinationDeficit2)
Model_Production3 <- lme(DeficitFruitSet ~ Year, random = ~1|Tree/Location, data = PollinationDeficit2) 

#Model_Production6 <- lme(DeficitFruitSet ~ Apple_variety + Region + Year, random = ~1|Tree/Location, data = PollinationDeficit2) 

summary(Model_Production2)

#Get significance
#Cultivar
Var_Prod = emmeans(Model_Production1, spec = ~ Apple_variety)
Var_Prod_tukey = contrast(Var_Prod, method = 'pairwise')
Var_Proddf <- summary(Var_Prod_tukey)
Var_Proddf

#Region
Reg_Prod = emmeans(Model_Production2, spec = ~ Region)
Reg_Prod_tukey = contrast(Reg_Prod, method = 'pairwise')
Reg_Proddf <- summary(Reg_Prod_tukey)
Reg_Proddf

#Year
Year_Prod = emmeans(Model_Production3, spec = ~ Year)
Year_Prod_tukey = contrast(Year_Prod, method = 'pairwise')
Year_Proddf <- summary(Year_Prod_tukey)
Year_Proddf


####################################################
########### POLLINATION DEFICIT (SEED SET) #########

#Analysis for Cultivar, Region and Year separately
Model_Pollination1 <- lme(DeficitSeeds ~ Apple_variety, random = ~1|Tree/Location, data = SeedSetDeficit3)
Model_Pollination2 <- lme(DeficitSeeds ~ Region, random = ~1|Tree/Location, data = SeedSetDeficit3)
Model_Pollination3 <- lme(DeficitSeeds ~ Year, random = ~1|Tree/Location, data = SeedSetDeficit3) 

#Model_Pollination6 <- lme(DeficitSeeds ~ Apple_variety + Region + Year, random = ~1|Tree/Location, data = SeedSetDeficit3)

summary(Model_Pollination3)

#Get significance
#Cultivar
Var_Poll = emmeans(Model_Pollination1, spec = ~ Apple_variety)
Var_Poll_tukey = contrast(Var_Poll, method = 'pairwise')
Var_Polldf <- summary(Var_Poll_tukey)
Var_Polldf

#Region
Reg_Poll = emmeans(Model_Pollination2, spec = ~ Region)
Reg_Poll_tukey = contrast(Reg_Poll, method = 'pairwise')
Reg_Polldf <- summary(Reg_Poll_tukey)
Reg_Polldf

#Year
Year_Poll = emmeans(Model_Pollination3, spec = ~ Year)
Year_Poll_tukey = contrast(Year_Poll, method = 'pairwise')
Year_Polldf <- summary(Year_Poll_tukey)
Year_Polldf

#############################################################
############ POLLINATOR OBSERVATIONS (CAMERA 2023) ##########
#############################################################


#Connect prodiction deficit and pollination deficit to number of visits (on flower) and observations (in photo frame but not on a flower)
#Only eastern Norway

#Visits on flower per pollinator group
#Production deficit
Model_ProdDefVisit1 <- lme(ProdDef ~ Apple_variety + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_ProdDefVisit2 <- lme(ProdDef ~ Apple_variety + SolitaryBee_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_ProdDefVisit3 <- lme(ProdDef ~ Apple_variety + Bombus_AvrgPh, random = ~1|Location, data = Deficit_Visits)

Model_ProdDefVisit4 <- lme(ProdDef ~ Apple_variety + SolitaryBee_AvrgPh + Bombus_AvrgPh + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)

summary(Model_ProdDefVisit1)


# anova(Model_ProdDefVisit1) #model selection based on anova and drop not significant values
# anova(Model_ProdDefVisit2)
# anova(Model_ProdDefVisit3)



#Visits on flower per pollinator group
#Pollination deficit
Model_PollDefVisit1 <- lme(PollDef ~ Apple_variety + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit2 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit3 <- lme(PollDef ~ Apple_variety + Bombus_AvrgPh, random = ~1|Location, data = Deficit_Visits)

Model_PollDefVisit4 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh + Bombus_AvrgPh + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit5 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)

AIC(Model_PollDefVisit4, Model_PollDefVisit5, Model_PollDefVisit2, Model_PollDefVisit1, Model_PollDefVisit3)

summary(Model_PollDefVisit1)



##### OBSERVATIONS (not on flower)

#Observation per pollinator group
#Production deficit
Model_ProdDefObs1 <- lme(PollDef ~ Apple_variety + Apis_AvrgPh, random = ~1|Location, data = Deficit_Observations)
Model_ProdDefObs2 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh, random = ~1|Location, data = Deficit_Observations)
Model_ProdDefObs3 <- lme(PollDef ~ Apple_variety + Bombus_AvrgPh, random = ~1|Location, data = Deficit_Observations)

summary(Model_ProdDefObs3)


#Observation per pollinator group
#Pollination deficit
Model_PollDefObs1 <- lme(PollDef ~ Apple_variety + Apis_AvrgPh, random = ~1|Location, data = Deficit_Observations)
Model_PollDefObs2 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh, random = ~1|Location, data = Deficit_Observations)
Model_PollDefObs3 <- lme(PollDef ~ Apple_variety + Bombus_AvrgPh, random = ~1|Location, data = Deficit_Observations)


summary(Model_PollDefObs3)







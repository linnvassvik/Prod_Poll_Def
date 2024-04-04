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
 
#Including Cultivar, Location and Year, with analysis separate for the two regions
PollinationDeficit2_East <- PollinationDeficit2 %>% 
  filter(Region == 'Svelvik')
PollinationDeficit2_West <- PollinationDeficit2 %>% 
  filter(Region == 'Ullensvang')


Model_Production4 <- lme(DeficitFruitSet ~ Apple_variety * Location * Year, random = ~1|Tree/Location, data = PollinationDeficit2_East)
Model_Production5 <- lme(DeficitFruitSet ~ Apple_variety * Location * Year, random = ~1|Tree/Location, data = PollinationDeficit2_West)


#summary(Model_Production4)


#Get significance
#Cultivar
Var_Prod = emmeans(Model_Production1, spec = ~ Apple_variety)
Var_Prod_tukey = contrast(Var_Prod, method = 'pairwise')
Var_Proddf <- summary(Var_Prod_tukey)
#write_xlsx(Var_Proddf, "Excel\\emmeans_Var_Prod.xlsx")

#Region
Reg_Prod = emmeans(Model_Production2, spec = ~ Region)
Reg_Prod_tukey = contrast(Reg_Prod, method = 'pairwise')
Reg_Proddf <- summary(Reg_Prod_tukey)
#write_xlsx(Reg_Proddf, "Excel\\emmeans_Reg_Prod.xlsx")

#Year
Year_Prod = emmeans(Model_Production3, spec = ~ Year)
Year_Prod_tukey = contrast(Year_Prod, method = 'pairwise')
Year_Proddf <- summary(Year_Prod_tukey)
#write_xlsx(Year_Proddf, "Excel\\emmeans_Year_Prod.xlsx")




#Location * Apple variety * Year in East
LocVarYear_Prod = emmeans(Model_Production4, spec = ~ Location * Apple_variety * Year)
LocVarYear_Prod_tukey = contrast(LocVarYear_Prod, method = 'pairwise')
LocVarYear_Proddf <- summary(LocVarYear_Prod_tukey)
#write_xlsx(LocVarYear_Proddf, "Excel\\emmeans_LocVarYear_Prod_East.xlsx")

#Location * Apple variety * Year in West
LocVarYear_Prod = emmeans(Model_Production5, spec = ~ Location * Apple_variety * Year)
LocVarYear_Prod_tukey = contrast(LocVarYear_Prod, method = 'pairwise')
LocVarYear_Proddf <- summary(LocVarYear_Prod_tukey)
#write_xlsx(LocVarYear_Proddf, "Excel\\emmeans_LocVarYear_Prod_West.xlsx")



####################################################
########### POLLINATION DEFICIT (SEED SET) #########

#Analysis for Cultivar, Region and Year separately
Model_Pollination1 <- lme(DeficitSeeds ~ Apple_variety, random = ~1|Tree/Location, data = SeedSetDeficit3)
Model_Pollination2 <- lme(DeficitSeeds ~ Region, random = ~1|Tree/Location, data = SeedSetDeficit3)
Model_Pollination3 <- lme(DeficitSeeds ~ Year, random = ~1|Tree/Location, data = SeedSetDeficit3) 

#Including Cultivar, Location and Year, with analysis separate for the two regions
SeedSetDeficit3_East <- SeedSetDeficit3 %>% 
  filter(Region == 'Svelvik')
SeedSetDeficit3_West <- SeedSetDeficit3 %>% 
  filter(Region == 'Ullensvang')

Model_Pollination4 <- lme(DeficitSeeds ~ Apple_variety * Location * Year, random = ~1|Tree/Location, data = SeedSetDeficit3_East)
Model_Pollination5 <- lme(DeficitSeeds ~ Apple_variety * Location * Year, random = ~1|Tree/Location, data = SeedSetDeficit3_West)



#Get significance
#Cultivar
Var_Poll = emmeans(Model_Pollination1, spec = ~ Apple_variety)
Var_Poll_tukey = contrast(Var_Poll, method = 'pairwise')
Var_Polldf <- summary(Var_Poll_tukey)
#write_xlsx(Var_Polldf, "Excel\\emmeans_Var_Poll.xlsx")

#Region
Reg_Poll = emmeans(Model_Pollination2, spec = ~ Region)
Reg_Poll_tukey = contrast(Reg_Poll, method = 'pairwise')
Reg_Polldf <- summary(Reg_Poll_tukey)
#write_xlsx(Reg_Prolldf, "Excel\\emmeans_Reg_Poll.xlsx")

#Year
Year_Poll = emmeans(Model_Pollination3, spec = ~ Year)
Year_Poll_tukey = contrast(Year_Poll, method = 'pairwise')
Year_Polldf <- summary(Year_Poll_tukey)
#write_xlsx(Year_Polldf, "Excel\\emmeans_Year_Poll.xlsx")




#Location * Apple variety * Year in East
LocVarYear_Poll = emmeans(Model_Pollination4, spec = ~ Location * Apple_variety * Year)
LocVarYear_Poll_tukey = contrast(LocVarYear_Poll, method = 'pairwise')
LocVarYear_Polldf <- summary(LocVarYear_Poll_tukey)
#write_xlsx(LocVarYear_Polldf, "Excel\\emmeans_LocVarYear_Poll_East.xlsx")

#Location * Apple variety * Year in West
LocVarYear_Poll = emmeans(Model_Pollination5, spec = ~ Location * Apple_variety * Year)
LocVarYear_Poll_tukey = contrast(LocVarYear_Poll, method = 'pairwise')
LocVarYear_Polldf <- summary(LocVarYear_Poll_tukey)
#write_xlsx(LocVarYear_Polldf, "Excel\\emmeans_LocVarYear_Poll_West.xlsx")




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

summary(Model_ProdDefVisit3)


# anova(Model_ProdDefVisit1) #model selection based on anova and drop not significant values
# anova(Model_ProdDefVisit2)
# anova(Model_ProdDefVisit3)



#Visits on flower per pollinator group
#Pollination deficit
Model_PollDefVisit1 <- lme(PollDef ~ Apple_variety + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit2 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit3 <- lme(PollDef ~ Apple_variety + Bombus_AvrgPh, random = ~1|Location, data = Deficit_Visits)


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







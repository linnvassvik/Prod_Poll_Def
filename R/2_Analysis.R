# import data
source("R/1_Import_Prepare_Dataset.R")

library(nlme)
library(emmeans)
library(MASS)


#############################################################
######## PRODUCTION DEFICIT (FRUITSET * WEIGHT) #############

Model_Production <- lme(DeficitFruitSet ~ Apple_variety + Region + Year, random = ~1|Tree/Location, data = PollinationDeficit2) 

summary(Model_Production6)

#Get significance
#Cultivar
Var_Prod = emmeans(Model_Production, spec = ~ Apple_variety)
Var_Prod_tukey = contrast(Var_Prod, method = 'pairwise')
Var_Proddf <- summary(Var_Prod_tukey)
Var_Proddf

#Region
Reg_Prod = emmeans(Model_Production, spec = ~ Region)
Reg_Prod_tukey = contrast(Reg_Prod, method = 'pairwise')
Reg_Proddf <- summary(Reg_Prod_tukey)
Reg_Proddf

#Year
Year_Prod = emmeans(Model_Production, spec = ~ Year)
Year_Prod_tukey = contrast(Year_Prod, method = 'pairwise')
Year_Proddf <- summary(Year_Prod_tukey)
Year_Proddf


####################################################
########### POLLINATION DEFICIT (SEED SET) #########

#Analysis for Cultivar, Region and Year separately
Model_Pollination <- lme(DeficitSeeds ~ Apple_variety + Region + Year, random = ~1|Tree/Location, data = SeedSetDeficit3)

summary(Model_Pollination)

#Get significance
#Cultivar
Var_Poll = emmeans(Model_Pollination, spec = ~ Apple_variety)
Var_Poll_tukey = contrast(Var_Poll, method = 'pairwise')
Var_Polldf <- summary(Var_Poll_tukey)
Var_Polldf

#Region
Reg_Poll = emmeans(Model_Pollination, spec = ~ Region)
Reg_Poll_tukey = contrast(Reg_Poll, method = 'pairwise')
Reg_Polldf <- summary(Reg_Poll_tukey)
Reg_Polldf

#Year
Year_Poll = emmeans(Model_Pollination, spec = ~ Year)
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

summary(Model_ProdDefVisit)



#Visits on flower per pollinator group
#Pollination deficit
Model_PollDefVisit1 <- lme(PollDef ~ Apple_variety + Apis_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit2 <- lme(PollDef ~ Apple_variety + SolitaryBee_AvrgPh, random = ~1|Location, data = Deficit_Visits)
Model_PollDefVisit3 <- lme(PollDef ~ Apple_variety + Bombus_AvrgPh, random = ~1|Location, data = Deficit_Visits)


summary(Model_PollDefVisit5)



##### OBSERVATIONS (not on flower) **NOT INCLUDED IN ARTICLE**

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




######
#General tests
# Difference in seed production between the three cultivars: 

SeedSet_stages_N <- SeedSet_stages %>% 
  filter(Treatment == 'N') %>% 
  pivot_wider(names_from = seed_stage, values_from = Total_Seeds_Stage)

average_seeds <- SeedSet_stages_N %>%
  group_by(Apple_variety) %>%
  summarise(avg_Total_Seeds_Treatment = mean(Total_Seeds_Treatment, na.rm = TRUE))

anova_result <- aov(Total_Seeds_Treatment ~ Apple_variety, data = SeedSet_stages_N)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)




### Test difference in bee activity between locations
# Reshape data to long format

PolliObs_Photo_Visits_long <- PolliObs_Photo_Visits %>%
  select(Location, Apis, SolitaryBee, Bombus) %>%
  pivot_longer(cols = c(Apis, SolitaryBee, Bombus),
               names_to = "Pollinator",
               values_to = "Count")

#data is overdispersed, therefore used negative binomial:
model_nb <- glm.nb(Count ~ Pollinator * Location, 
                   data = PolliObs_Photo_Visits_long)

summary(model_nb)

pairwise <- emmeans(model_nb, pairwise ~ Pollinator * Location, adjust = "Tukey")
pairwise



#Calculate percentage of different bees in the different locations

PolliObs_Photo_Visits_long %>%
  group_by(Location, Pollinator) %>%
  summarise(Total_bees = sum(Count)) %>%
  ungroup() %>%
  mutate(Global_total_bees = sum(Total_bees)) %>%
  mutate(Percentage = Total_bees / Global_total_bees * 100)



##Fruit set per treatment and cultivar

FruitSetAvrg %>% 
  mutate(FruitSet2 = (apples / (apples + Flowers)*100)) %>%
  group_by(Treatment) %>%
  summarise(
    mean_FruitSet2 = mean(FruitSet2, na.rm = TRUE), 
    n = n(),                           
    se_FruitSet2 = sd(FruitSet2, na.rm = TRUE) / sqrt(n))

FruitSetAvrg %>% 
  mutate(FruitSet2 = (apples / (apples + Flowers)*100)) %>%
  group_by(Treatment, Apple_variety) %>%
  summarise(
    mean_FruitSet2 = mean(FruitSet2, na.rm = TRUE), 
    n = n(),                           
    se_FruitSet2 = sd(FruitSet2, na.rm = TRUE) / sqrt(n))

SeedSet_calculations %>%
  mutate(SeedSet2 = (Seeds_fully_developed/(Seeds_partially_developed+Seeds_not_developed+Seeds_fully_developed)*100)) %>% 
  group_by(Treatment) %>%
  summarise(
    mean_SeedSet2  = mean(SeedSet2, na.rm = TRUE), 
    n = n(),                           
    se_SeedSet2 = sd(SeedSet2, na.rm = TRUE) / sqrt(n))

SeedSet_calculations %>%
  mutate(SeedSet2 = (Seeds_fully_developed/(Seeds_partially_developed+Seeds_not_developed+Seeds_fully_developed)*100)) %>% 
  group_by(Treatment, Apple_variety) %>%
  summarise(
    mean_SeedSet2  = mean(SeedSet2, na.rm = TRUE), 
    n = n(),                           
    se_SeedSet2 = sd(SeedSet2, na.rm = TRUE) / sqrt(n))

###### DATA FOR PRODUCTION AND POLLINATION DEFICIT STUDY ######

library(ggpp)
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(readr)
library(writexl)


AppleQualityData <- read_excel("Data/AQ_22+23.xlsx")


## PREPARING SEEDSET DATA ##

#Get number of seeds in new row
SeedSet_1 <- AppleQualityData %>%
  pivot_longer(Seeds_fully_developed:Seeds_not_developed, names_to = "seed_stage")

#Remove columns not needed for seed set analysis
SeedSet <- SeedSet_1 %>% 
  dplyr::select(-c(Weight, Height, Diameter, Shape, Damage)) 


# organize database to obtain three seed_stage for each treatment, and not per apple
SeedSet_stages <- SeedSet %>% 
  group_by(Year, Apple_variety, Region, Location, ID, Treatment, Tree, Apple_number, seed_stage) %>%
  summarise(tot = sum(value)) %>%
  ungroup() %>%
  group_by(Year, Apple_variety, Region, Location, ID, Tree, Apple_number, Treatment) %>%
  mutate(tot2 = sum(tot))  %>% 
  dplyr::rename(Total_Seeds_Stage = tot) %>% 
  dplyr::rename(Total_Seeds_Treatment = tot2)


# Calculate percentage of each seed stage for each treatment  
SeedSet_Percentage <- SeedSet_stages %>% 
  mutate(Percentage_Seeds_Stage = (Total_Seeds_Stage/Total_Seeds_Treatment)*100)


# Calculate mean of seeds per tree and treatment
SeedSet_average <- AppleQualityData %>% 
  dplyr::select(-c(Weight, Height, Diameter, Shape, Damage, Seeds_partially_developed, Seeds_not_developed)) %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  mutate(Total_seeds = sum(Seeds_fully_developed)) %>% 
  ungroup() %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  top_n(1, Apple_number) %>% 
  rename(Total_number_apples = Apple_number) %>% 
  ungroup() %>% 
  mutate(Average_seeds = (Total_seeds/Total_number_apples)) %>% 
  dplyr::select(-c(Seeds_fully_developed))


SeedSet_developed <- AppleQualityData %>% 
  dplyr::select(-c(Weight, Height, Diameter, Shape, Damage, Seeds_partially_developed, Seeds_not_developed)) %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  ungroup()

SeedSet_Quality <- AppleQualityData %>% 
  dplyr::select(-c(Damage, Seeds_partially_developed, Seeds_not_developed)) %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  ungroup()


###########################################################


#DATASET FRUITSET

FruitSet <- read_excel("Data/ClusterFruit_22+23.xlsx")

FruitSet <- FruitSet %>% 
  dplyr::select(-c(Kommentar))

FruitSet <- FruitSet %>% 
  mutate(Tree = factor(substring(ID, 7,8))) %>% 
  relocate(Tree, .after = ID) %>% 
  mutate(Tree = as.character(Tree))


#DATASET Apples
#Due to some branches having to be remarked, these had high uncertancy if they got remarked correctly, these were remove. See tests done in different R sheet
FruitSet1 <- read_excel("Data/ClusterFruit_22+23_RemovedUnsure.xlsx")


FruitSet1 <- FruitSet1 %>% 
  dplyr::select(-Kommentar) 

FruitSet1 <- FruitSet1 %>% 
  mutate(Tree = factor(substring(ID, 7,8))) %>% 
  relocate(Tree, .after = ID) %>% 
  mutate(Tree = as.character(Tree)) %>% 
  rename(Year = Year_wrong) #not wrong year, been corrected

FruitSet1$Tree <- as.character(FruitSet1$Tree)

#Calculate FruitSet (using average number of flowers per cluster per variety (counted in field))
FruitSet1 <- FruitSet1 %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  mutate(flowers = case_when(
    Apple_variety == "Summerred" ~ clusters * 5.3,
    Apple_variety == "Discovery" ~ clusters * 5.8,
    Apple_variety == "Aroma" ~ clusters * 5.1))
  


FruitSet2 <- FruitSet1 %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  mutate(FruitSet = (apples/flowers)*100) %>% 
  ungroup() %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  mutate(FruitSetProp = apples/flowers) %>% 
  ungroup() %>% 
  dplyr::select(-ID) 


#New dataframe
FruitSeedSet <- FruitSet2 %>%  
  right_join(SeedSet_average, by = c("Year", "Region", "Apple_variety", "Location", "Tree", "Treatment")) %>% 
  dplyr::select(-ID) %>% 
  ungroup()



#Fruitset, seedset and quality

FruitSetAvrg <- FruitSet2 %>% 
  group_by(Year, Region, Apple_variety, Location, Treatment, Tree) %>% 
  summarise(FruitSet = sum(FruitSet) / n(), Flowers = sum(flowers) / n(), apples = sum(apples) / n()) %>%
  ungroup()


SeedSet_calculations <- AppleQualityData %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>%
  mutate(SeedSet = (Seeds_fully_developed/(Seeds_partially_developed+Seeds_not_developed+Seeds_fully_developed))*100) %>% 
  ungroup()
  
  
SeedSet_Quality2 <- SeedSet_calculations %>% 
  group_by(Year, Region, Apple_variety, Location, Treatment, Tree) %>% 
  summarise(Weight = sum(Weight) / n(), Height = sum(Height) / n(), Diameter = sum(Diameter) / n(), SeedSet = sum(SeedSet) / n(), Shape = sum(Shape) / n()) %>%
  ungroup()

Avrg_SeedFruitQuality <- SeedSet_Quality2 %>% 
  right_join(FruitSetAvrg, by = c("Year", "Region", "Apple_variety", "Location", "Treatment", "Tree"))

Avrg_SeedFruitQuality <- Avrg_SeedFruitQuality %>% 
  group_by(Year, Region, Apple_variety, Location, Treatment, Tree) %>% 
  mutate(Prop_SeedSet = SeedSet / 100, Prop_FruitSet = FruitSet / 100) %>% 
  ungroup() %>% 
  filter(!is.na(Prop_FruitSet))
  


#Dataframes for fruitset and seedset for linear mixed effect models

#Fruitset
FruitSet_Bionomial2 <- Avrg_SeedFruitQuality


FruitSet_Bionomial2 <- FruitSet_Bionomial2 %>% 
  select(-Weight, -Height, -Diameter, -SeedSet, -Shape, -FruitSet, -Prop_SeedSet, -Prop_FruitSet) %>% 
  mutate(Flowers_rounded = round(Flowers)) %>% 
  mutate(Flowers_not_apple = Flowers_rounded - apples)

FruitSet_Bionomial <- FruitSet_Bionomial2 %>% 
  select(-Flowers, -Flowers_rounded)

Seeds_Binomial <- SeedSet_calculations %>% 
  select(-Weight, -Height, -Diameter, -SeedSet, -Shape, -Damage) %>%
  mutate(Seeds_partially_none = Seeds_partially_developed + Seeds_not_developed)
  
Seeds_Binomial <- Seeds_Binomial %>% 
  select(-Seeds_partially_developed, -Seeds_not_developed)



### PREPARE DATA FOR SEEDSET CALCULATIONS

SeedSetDecifiency <- SeedSet_1 %>% 
  dplyr::select(-c(Weight, Height, Diameter, Shape, Damage)) %>% 
  group_by(Year, Region, Apple_variety, Location, Tree, Treatment, seed_stage) %>%
  summarise(tot = sum(value)) %>% 
  ungroup() %>%
  group_by(Year, Apple_variety, Region, Location, Tree, Treatment) %>%
  mutate(tot2 = sum(tot))

# Calculate percentage  
SeedSetDeficit <- SeedSetDecifiency %>% 
  mutate(percentage = tot/tot2) %>% 
  filter(Treatment != 'C') #dont need C as we want to compare N to HP only


SeedSetDeficit2 <- SeedSetDeficit %>% 
  pivot_wider(names_from = seed_stage, values_from = percentage) %>% 
  dplyr::select(-Seeds_not_developed, -Seeds_partially_developed) %>% 
  drop_na()

SeedSetDeficit2 <- SeedSetDeficit2 %>%
  dplyr::select(-tot, -tot2) %>% 
  pivot_wider(names_from = Treatment, values_from = Seeds_fully_developed) %>% 
  drop_na()

### Dataframe with seedset in HP higher than in N
data1 <- filter(SeedSetDeficit2, HP > N)

data1 <- data1 %>% 
  group_by(Year, Region, Apple_variety, Location, Tree) %>% 
  mutate(DeficitSeeds = sum((HP-N)/HP))

data1 <- data1 %>% 
  dplyr::select(-HP, -N)

### Dataframe with seedset in N higher than in HP
data2 <- filter(SeedSetDeficit2, HP < N)

data2 <- data2 %>% 
  group_by(Year, Region, Apple_variety, Location, Tree) %>% 
  mutate(DeficitSeeds = sum((HP-N)/N))

data2 <- data2 %>% 
  dplyr::select(-HP, -N)


#Merge dataframes

SeedSetDeficit3 <- rbind(data1, data2)


#only for mean deficit
SeedSetDeficit4 <- SeedSetDeficit3 %>%   
  group_by(Year, Apple_variety, Region, Location) %>%
  summarise(mean_deficit = mean(DeficitSeeds)) %>% 
  add_column(add_column = "Norway") %>% 
  mutate(Country = add_column) %>% 
  dplyr::select(-add_column)

#add errorbar
SeedSetDeficit5 <- plyr::ddply(SeedSetDeficit3, c("Year", "Region", "Apple_variety", "Location"), summarise,
                               N = length(DeficitSeeds),
                               mean = mean(DeficitSeeds),
                               sd = sd(DeficitSeeds),
                               se = sd / sqrt(N),
                               ConfInterval = 1.96 * (sd(DeficitSeeds) / sqrt(N)))  

SeedSetDeficit5 <-SeedSetDeficit5 %>% 
  add_column(Country = "Norway") 




##PREPARE DATA FOR FRUITSET AND PRODUCTION DEFICIT CALCULATIONS

#Include average weight per treatment per ID into fruitset table
WeightTreatment <- SeedSet_1 %>% 
  dplyr::select(-Height, -Diameter, -Shape, -Damage, -seed_stage, -value, -ID_Year) %>% 
  unique(by = c(Year, Apple_variety, Region, Location, ID, Tree, Treatment))


WeightTreatment2 <- WeightTreatment %>% 
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>% 
  mutate(TotWeight = sum(Weight)) %>% 
  ungroup()

WeightTreatment2 <- WeightTreatment2 %>%       
  group_by(Year, Apple_variety, Region, Location, Treatment, Tree) %>% 
  top_n(1, Apple_number) %>% 
  dplyr::rename(Total_number_apples = Apple_number) %>% 
  dplyr::select(-Weight)   

WeightTreatment3 <- WeightTreatment2 %>% 
  group_by(Year, Apple_variety, Region, Location, ID, Tree, Treatment) %>%
  mutate(AvrgWeight = sum(TotWeight/Total_number_apples))  %>%
  ungroup() %>% 
  dplyr::select(-ID, -Total_number_apples, -TotWeight)



PollinationDeficit0 <- FruitSeedSet %>%  
  right_join(WeightTreatment3, by = c("Year", "Region", "Apple_variety", "Location", "Tree", "Treatment")) %>% 
  dplyr::select(-ID_Year)


#Dataframe with weight*fruitset(%) per tree per treatment
PollinationDeficit <- PollinationDeficit0 %>% 
  group_by(Year, Apple_variety, Region, Location, Tree, Treatment) %>%
  mutate(Yield = sum(FruitSet*AvrgWeight))  %>%
  ungroup() %>% 
  filter(Treatment != 'C') %>% 
  dplyr::select(-FruitSet, -flowers, -Total_number_apples, -Total_seeds, -Average_seeds, -AvrgWeight, -FruitSetProp, -clusters, -apples)

#pivot dataframe for further wrangling
PollinationDeficit1 <- PollinationDeficit %>% 
  pivot_wider(names_from = Treatment, values_from = Yield)


### Dataframe with fruitset in HP higher than in N
data3 <- filter(PollinationDeficit1, N < HP)

data3 <- data3 %>% 
  group_by(Year, Region, Apple_variety, Location, Tree) %>% 
  mutate(DeficitFruitSet = sum((HP-N)/HP))

data3 <- data3 %>% 
  dplyr::select(-HP, -N)

### Dataframe with seedset in N higher than in HP
data4 <- filter(PollinationDeficit1, HP < N)

data4 <- data4 %>% 
  group_by(Year, Region, Apple_variety, Location, Tree) %>% 
  mutate(DeficitFruitSet = sum((HP-N)/N))

data4 <- data4 %>% 
  dplyr::select(-HP, -N)


#Merge dataframes

PollinationDeficit2 <- rbind(data3, data4) 



#make error bars for production deficit (fruitset*weight)
PollinationDeficit4 <- plyr::ddply(PollinationDeficit2, c("Year", "Region", "Apple_variety", "Location"), summarise,
                                   N = length(DeficitFruitSet),
                                   mean = mean(DeficitFruitSet),
                                   sd = sd(DeficitFruitSet),
                                   se = sd / sqrt(N),
                                   ConfInterval = 1.96 * (sd(DeficitFruitSet) / sqrt(N)))

PollinationDeficit4 <-PollinationDeficit4 %>% 
  add_column(Country = "Norway") 


#only for mean deficit
PollinationDeficit3 <- PollinationDeficit2 %>%   
  group_by(Year, Apple_variety, Region, Location) %>%
  summarise(mean_deficit = mean(DeficitFruitSet)) %>% 
  add_column(add_column = "Norway") %>% 
  mutate(Country = add_column) %>% 
  dplyr::select(-add_column)






#####Pollinator dependence (HP-C)
#calculation of controbution of pollinators on fruitset
PollinatorPCFruitSet <- PollinationDeficit0 %>% 
  dplyr::select(-clusters, - apples, -FruitSetProp, -flowers, -Total_number_apples, -Total_seeds, -Average_seeds, -AvrgWeight) %>% 
  pivot_wider(names_from = Treatment, values_from = FruitSet) %>% 
  dplyr::select(-N) %>% 
  mutate_at(c(6,7), ~ replace_na(.,0))

PC1 <- filter(PollinatorPCFruitSet, HP < C) %>% 
  group_by(Year, Apple_variety, Region, Location, Tree) %>%
  mutate(FruitSetPC = 1-(HP/C)) %>%
  ungroup()

PC2 <- filter(PollinatorPCFruitSet, HP > C) %>%
  group_by(Year, Apple_variety, Region, Location, Tree) %>%
  mutate(FruitSetPC = 1-(C/HP)) %>%
  ungroup()

PCFruitSet <- rbind(PC1, PC2) 

#make error bars
PCFruitSetPlot <- plyr::ddply(PCFruitSet, c("Year", "Region", "Apple_variety", "Location"), summarise,
                              N = length(FruitSetPC),
                              mean = mean(FruitSetPC),
                              sd = sd(FruitSetPC),
                              se = sd / sqrt(N))



#Pollinator contribution seed set
PollinatorPCSeedSet <- PollinationDeficit0 %>% 
  dplyr::select(-clusters, - apples, -FruitSetProp, -flowers, -Total_number_apples, -Total_seeds, -FruitSet, -AvrgWeight) %>% 
  pivot_wider(names_from = Treatment, values_from = Average_seeds) %>% 
  dplyr::select(-N) %>% 
  mutate_at(c(6,7), ~ replace_na(.,0))

PCSeed1 <- filter(PollinatorPCSeedSet, HP < C) %>% 
  group_by(Year, Apple_variety, Region, Location, Tree) %>%
  mutate(SeedSetPC = 1-(HP/C)) %>%
  ungroup()

PCSeed2 <- filter(PollinatorPCSeedSet, HP > C) %>%
  group_by(Year, Apple_variety, Region, Location, Tree) %>%
  mutate(SeedSetPC = 1-(C/HP)) %>%
  ungroup()

PCSeedSet <- rbind(PCSeed1, PCSeed2) 

#make error bars
PCSeedSetPlot <- plyr::ddply(PCSeedSet, c("Year", "Region", "Apple_variety", "Location"), summarise,
                             N = length(SeedSetPC),
                             mean = mean(SeedSetPC),
                             sd = sd(SeedSetPC),
                             se = sd / sqrt(N))



##################################
###### POLLINATOR DATA ###########
##################################


# #Pollinator visits
# #Manual observations eastern Norway 2022 (S-H-SR Tree 1, 3, 6 and 10 only 1 observation)
# Pollinator_observations_2022 <- read_excel("Data/Pollinator_observations_2022.xlsx")
# 
# Pollinator_observations_2022 <- Pollinator_observations_2022 %>% 
#   select(-Time_start,-Time_length, -Time_end, -Weather, -Name, -Species, -Comment, -Date, -ID,-Region) %>% 
#   rename(Solitarybee = Wildbee)
# 
# Pollinator_observations_2022_avrg <- Pollinator_observations_2022 %>% 
#   group_by(Location, Apple_variety) %>% 
#   summarise_all(list(sum = sum)) %>% 
#   select(-Tree_sum, -Observation_nbr_sum)
# 
# Pollinator_observations_2022_grouped <- Pollinator_observations_2022 %>% 
#   mutate(Wild_bees = Solitarybee + Bumblebee) %>% 
#   mutate(Diptera = Hoverfly + Fly) %>% 
#   mutate(Tree = as.character(Tree))


#Camera observations easter Norway 2023 (Missing for Berle)
PolliObs_2023 <- read_csv("Data/PolliObs_2023.csv")

PolliObs_2023 <- PolliObs_2023 %>% 
  mutate(date_time = dmy_hms(paste(Date, Time)))

PolliObs_2023 <- PolliObs_2023 %>%   
  mutate(Rounded_Time = round_date(date_time, "hour"))
  
PolliObs_2023 <- PolliObs_2023 %>%   
  mutate(Hour = hour(Rounded_Time))

PolliObs_2023 <- PolliObs_2023 %>%   
 mutate(doy = yday(date_time)) %>% 
  select(-Comments, -Comments_picture, -`Photo_s/s`, -"Feil tid", -date_time, -Rounded_Time)

#Dataframes to make descriptive plots
PolliObs_2023_doy <- PolliObs_2023 %>%
  group_by(doy, Location, Apple_variety, Groups) %>%
  summarise(Total = sum(n()))

PolliObs_2023_time <- PolliObs_2023 %>%
  group_by(Hour, Location, Apple_variety, Groups) %>%
  summarise(Total = sum(n()))

PolliObs_2023_temp <- PolliObs_2023 %>%
  group_by(Temperature, Location, Apple_variety, Groups) %>%
  summarise(Total = sum(n()))
  

PolliObs_2023_Bar <- PolliObs_2023 %>% 
  group_by(Location, Apple_variety, Groups) %>%
  summarise(Total = sum(n()))





#Add number of photos taken in total
PolliObs_2023_Loc <- PolliObs_2023 %>% 
  group_by(Camera_number, Location, Apple_variety, Groups, On_flower) %>%
  summarise(Total = sum(n())) %>% 
  pivot_wider(names_from = Groups, values_from = Total) %>% 
  mutate_all(.funs = function(x) ifelse(is.na(x), 0, x))

Photo_numbers <- read_csv("Data/Photo_numbers.csv")

Photo_numbers <- Photo_numbers %>% 
  select(-Comment)

PolliObs_Photo <- PolliObs_2023_Loc %>% 
  right_join(Photo_numbers, by = c("Camera_number", "Location", "Apple_variety"))

PolliObs_Photo <- PolliObs_Photo %>% 
  mutate(Total_visits = Apis + Diptera + SolitaryBee + Unknown_bee + Lepidoptera + Bombus) %>% 
  mutate(Avrg_Visit_Photo = Total_visits/Photos_number) %>% 
  mutate(Avrg_Visit_Photo_percent = (Total_visits/Photos_number)*100)

PolliObs_Photo_Visits <- PolliObs_Photo %>% 
  filter(On_flower == "Y")

PolliObs_Photo_Observations <- PolliObs_Photo %>% 
  filter(On_flower == "N")


PolliObs_Photo_Visits <- PolliObs_Photo_Visits %>% 
  mutate(Apis_AvrgPh = Apis/Photos_number) %>% 
  mutate(Diptera_AvrgPh = Diptera/Photos_number) %>% 
  mutate(SolitaryBee_AvrgPh = SolitaryBee/Photos_number) %>% 
  mutate(Bombus_AvrgPh = Bombus/Photos_number) %>% 
  mutate(UnknownBees_AvrgPh = Unknown_bee/Photos_number)
  
PolliObs_Photo_Visits2 <- PolliObs_Photo_Visits %>%
  select(-Apis, -Diptera, -SolitaryBee, -Bombus, -Other, -Lepidoptera, -Unknown_bee, -On_flower, -Avrg_Visit_Photo_percent)

#Calculate mean for each Variety and location 

#write_xlsx(PolliObs_Photo_Visits2, "Excel\\PolliObs_Photo_Visits2.xlsx") 

PolliObs_Photo_Observations <- PolliObs_Photo_Observations %>% 
  mutate(Apis_AvrgPh = Apis/Photos_number) %>% 
  mutate(Diptera_AvrgPh = Diptera/Photos_number) %>% 
  mutate(SolitaryBee_AvrgPh = SolitaryBee/Photos_number) %>% 
  mutate(Bombus_AvrgPh = Bombus/Photos_number) %>% 
  mutate(UnknownBees_AvrgPh = Unknown_bee/Photos_number)

PolliObs_Photo_Observations2 <- PolliObs_Photo_Observations %>%
  select(-Apis, -Diptera, -SolitaryBee, -Bombus, -Other, -Lepidoptera, -Unknown_bee, -On_flower, -Avrg_Visit_Photo_percent)

#write_xlsx(PolliObs_Photo_Observations2, "Excel\\PolliObs_Photo_Observations2.xlsx") 

PolliObs_Photo_Visits2_Avrg <- read_excel("Data/PolliObs_Photo_Visits2_Avrg.xlsx")
PolliObs_Photo_Observations2_Avrg <- read_excel("Data/PolliObs_Photo_Observations2_Avrg.xlsx")


#Combine with Production and pollination deficit
ProdDef_Visit <- PollinationDeficit4 %>% 
  filter(Region == "Svelvik") %>% 
  filter(Year == "2023") %>% 
  select(-Year, -Region) %>% 
  mutate(Location = replace(Location, Location == "Høyen", "Hoyen"))


ProdDef_Visit2 <- ProdDef_Visit %>% 
  rename(ProdDef = mean) %>%
  rename(ConfInverval_ProdDef = ConfInterval) %>% 
  rename(se_ProdDef = se) %>%
  select(-sd, -Country, -N)

PollDef_Visit <- SeedSetDeficit5 %>% 
  filter(Region == "Svelvik") %>% 
  filter(Year == "2023") %>% 
  select(-Year, -Region) %>% 
  mutate(Location = replace(Location, Location == "Høyen", "Hoyen"))

PollDef_Visit2 <- PollDef_Visit %>% 
  rename(PollDef = mean) %>% 
  rename(ConfInverval_PollDef = ConfInterval) %>% 
  rename(se_PollDef = se) %>%
  select(-sd, -Country, -N)

Deficit <- ProdDef_Visit2 %>% 
  right_join(PollDef_Visit2, by = c("Apple_variety", "Location"))

Deficit_Visits <- Deficit %>% 
  right_join(PolliObs_Photo_Visits2_Avrg, by = c("Location", "Apple_variety")) %>% 
  mutate(WildBees_AvrgPh = SolitaryBee_AvrgPh + Bombus_AvrgPh)

Deficit_Observations <- Deficit %>% 
  right_join(PolliObs_Photo_Observations2_Avrg, by = c("Location", "Apple_variety")) %>% 
  mutate(WildBees_AvrgPh = SolitaryBee_AvrgPh + Bombus_AvrgPh)



#correlation tests

# Deficit_Visits_corrtest <- Deficit_Visits %>% 
#   select(-ProdDef, -se_ProdDef, -ConfInverval_ProdDef, -PollDef, -se_PollDef, -ConfInverval_PollDef, -Photos_number, -Total_visits) %>% 
#   rename(Avrg_Visit = Avrg_Visit_Photo) %>% 
#   rename(Apis_Visit = Apis_AvrgPh) %>% 
#   rename(Diptera_Visit = Diptera_AvrgPh) %>% 
#   rename(Solitary_Visit = SolitaryBee_AvrgPh) %>% 
#   rename(Bombus_Visit = Bombus_AvrgPh) %>% 
#   rename(UnknownBees_Visit = UnknownBees_AvrgPh) %>% 
#   rename(WildBees_Visit = WildBees_AvrgPh)
# 
# Deficit_Observations_corrtest <- Deficit_Observations %>% 
#   select(-ProdDef, -se_ProdDef, -ConfInverval_ProdDef, -PollDef, -se_PollDef, -ConfInverval_PollDef, -Photos_number, -Total_visits) %>% 
#   rename(Avrg_Obs = Avrg_Visit_Photo) %>% 
#   rename(Apis_Obs = Apis_AvrgPh) %>% 
#   rename(Diptera_Obs = Diptera_AvrgPh) %>% 
#   rename(Solitary_Obs = SolitaryBee_AvrgPh) %>% 
#   rename(Bombus_Obs = Bombus_AvrgPh) %>% 
#   rename(UnknownBees_Obs = UnknownBees_AvrgPh) %>% 
#   rename(WildBees_Obs = WildBees_AvrgPh)
# 
# Deficit_ObsVis_corrtest <- Deficit_Visits_corrtest %>% 
#   right_join(Deficit_Observations_corrtest, by = c("Apple_variety", "Location"))
# 
# 
# cor(Deficit_ObsVis_corrtest$Apis_Obs, Deficit_ObsVis_corrtest$Apis_Visit)
# cor(Deficit_ObsVis_corrtest$Solitary_Obs, Deficit_ObsVis_corrtest$Solitary_Visit)
# cor(Deficit_ObsVis_corrtest$Bombus_Obs, Deficit_ObsVis_corrtest$Bombus_Visit)
# cor(Deficit_ObsVis_corrtest$Diptera_Obs, Deficit_ObsVis_corrtest$Diptera_Visit)














## OLD CODE

#write_xlsx(PollinationDeficit4, "Excel\\PollinationDeficit4.xlsx")

#PolliObs_2023_NoCamera <- PolliObs_2023 %>% 
  #group_by(Location, Apple_variety, Groups) %>%
  #summarise(Total = sum(n())) %>% 
  #pivot_wider(names_from = Groups, values_from = Total) %>% 
  #mutate_all(.funs = function(x) ifelse(is.na(x), 0, x))

#write_xlsx(PolliObs_2023_NoCamera, "Excel\\PolliObs_2023_NoCamera.xlsx")



#Production deficit per variety
#PollinationDeficit4_East <- PollinationDeficit4 %>% 
  #filter(Region == "Svelvik") %>% 
 # filter(Year == "2023") %>% 
  #select(-Year, -Region) %>% 
  #mutate(Location = replace(Location, Location == "Høyen", "Hoyen"))


#ProdDef_Polli <- PollinationDeficit4_East %>% 
  #right_join(PolliObs_Photo_NoCamera, by = c("Location", "Apple_variety")) %>% 
  #mutate(WildBees = SolitaryBee + Bombus)

#ProdDef_Polli2 <- ProdDef_Polli %>% 
  #pivot_longer(Apis:Bombus, names_to = "Group", values_to = "count")



#Pollination deficit per variety
#SeedSetDeficit5_East <- SeedSetDeficit5 %>% 
  #filter(Region == "Svelvik") %>% 
  #filter(Year == "2023") %>% 
  #select(-Year, -Region) %>% 
  #mutate(Location = replace(Location, Location == "Høyen", "Hoyen"))


#SeedSet_Polli <- SeedSetDeficit5_East %>% 
  #right_join(PolliObs_Photo_NoCamera, by = c("Location", "Apple_variety")) %>% 
  #mutate(WildBees = SolitaryBee + Bombus)

#SeedSet_Polli2 <- SeedSet_Polli %>% 
  #pivot_longer(Apis:Bombus, names_to = "Group", values_to = "count")





######

#Photo_numbers2 <- read_csv("Data/Photo_NoCamera.csv") %>% 
  #select(-4)

#PolliObs_Photo_NoCamera <- PolliObs_2023_NoCamera %>% 
  #right_join(Photo_numbers2, by = c("Location", "Apple_variety")) %>% 
  #mutate(Total_visits = Apis + Diptera + SolitaryBee + Unknown_bee + Lepidoptera + Bombus) %>% 
  #mutate(Avrg_Visit_Photo = Total_visits/Photos_number) %>% 
  #mutate(Avrg_Visit_Photo_percent = (Total_visits/Photos_number)*100)


  
  
  
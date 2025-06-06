## FIGURES FOR PAPER ##

library(ggplot2)
library(ggpubr)

# import data
source("R/2_Analysis.R")
source("R/Predictions.R")


#################################
### PRODUCTION DEFICIT (FRUITSET * WEIGHT)

#Total with CI
TotalYield <- PollinationDeficit4 %>% 
  ggplot(aes(x = Country, y = mean, color = Country)) +
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  scale_color_manual (values =  c("#996600")) +
  labs(x = "", y = "Production deficit", color = "Apple variety", title = "") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(TotalYield, filename = "Figures/TotalYield.jpeg", height = 6, width = 3)


#Apple variety with CI
VarietyYield <- PollinationDeficit4 %>% 
  ggplot(aes(x = Apple_variety, y = mean, color = Apple_variety)) + 
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  theme(strip.text.x = element_blank()) +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "", y = "", title = "", color = "Apple variety") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none") 
#ggsave(VarietyYield, filename = "Figures/VarietyYield.jpeg", height = 6, width = 3)


#Region with CI
RegionYield <- PollinationDeficit4 %>%
  ggplot(aes(x = Region, y = mean, color = Region)) + 
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  scale_color_manual(values = c("#CC99CC", "#990066"), labels = c('East', 'West')) +
  labs(x = "", y = "", color = "Region", title = "") +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0)) +
  scale_x_discrete(labels = c("Svelvik" = "East", "Ullensvang" = "West")) +
  theme(
    line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(RegionYield, filename = "Figures/RegionYield.jpeg", height = 6, width = 3)


#Year with CI
YearYield <- PollinationDeficit4 %>% 
  ggplot(aes(x = factor(Year), y = mean, color = factor(Year))) + 
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "", title = "", color = "Year") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(panel.background = element_blank()) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(YearYield, filename = "Figures/YearYield.jpeg", height = 6, width = 3)



FourGraphs <- ggarrange(TotalYield, VarietyYield, RegionYield, YearYield, nrow = 1, common.legend = TRUE, align = "h", labels = c("a", "b", "c", "d"))
ggsave(FourGraphs, filename = "Figures/FourGraphs.jpeg", height = 6, width = 11)




#################################
### POLLINATION DEFICIT (SEED SET)

#Total
TotalSeedSet <- SeedSetDeficit5 %>% 
  ggplot(aes(x = Country, y = mean, color = Country)) + 
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  scale_color_manual (values =  c("#996600")) +
  labs(x = "", y = "Pollination deficit", title = "") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(TotalSeedSet, filename = "Figures/TotalSeedSet.jpeg", height = 6, width = 3)


#Apple variety 
VarietySeedSet <- SeedSetDeficit5 %>% 
  ggplot(aes(x = Apple_variety, y = mean, color = Apple_variety)) + 
  geom_point(position = position_jitter(), size = 3) +
  #geom_errorbar(aes(ymin=mean-ConfInterval, ymax=mean+ConfInterval), width=.1) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  theme(strip.text.x = element_blank()) +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "", y = "", title = "") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(VarietySeedSet, filename = "Figures/VarietySeedSet.jpeg", height = 6, width = 3)

#Region
RegionSeedSet <- SeedSetDeficit5 %>% 
  ggplot(aes(x = Region, y = mean, color = Region)) + 
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  theme(strip.text.x = element_blank()) +
  scale_color_manual (values =  c("#CC99CC", "#990066"), labels = c('East', 'West')) +
  scale_x_discrete(labels = c("Svelvik" = "East", "Ullensvang" = "West")) +
  labs(x = "", y = "", title = "") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(RegionSeedSet, filename = "Figures/RegionSeedSet.jpeg", height = 6, width = 3)

#Year
YearSeedSet <- SeedSetDeficit5 %>% 
  ggplot(aes(x = factor(Year), y = mean, color = factor(Year))) + 
  geom_point(position = position_jitter(), size = 3) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "", title = "") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  guides(color = "none")
#ggsave(YearSeedSet, filename = "Figures/YearSeedSet.jpeg", height = 6, width = 3)

FourGraphsSeedSet <- ggarrange(TotalSeedSet, VarietySeedSet, RegionSeedSet, YearSeedSet, nrow = 1, common.legend = TRUE, align = "h", labels = c("a", "b", "c", "d"))
ggsave(FourGraphsSeedSet, filename = "Figures/FourGraphsSeedSet.jpeg", height = 6, width = 11)




#############################################
#PRODUCTION DEFICIT SPLIT BY YEAR AND VARIETY

ProdDef_ranked1 <- PollinationDeficit4 %>% 
  filter(Apple_variety == 'Aroma') %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = as.character(Year), shape = Region)) +
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_dodge(0.4)) +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "Production deficit", color = "Year", shape = "Region") +
  scale_shape_discrete(labels = c("East", "West")) +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

ProdDef_ranked2 <- PollinationDeficit4 %>% 
  filter(Apple_variety == 'Discovery') %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = as.character(Year), shape = Region)) +
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_dodge(0.4)) +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "", color = "Year", shape = "Region") +
  scale_shape_discrete(labels = c("East", "West")) +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

ProdDef_ranked3 <- PollinationDeficit4 %>% 
  filter(Apple_variety == 'Summerred') %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = as.character(Year), shape = Region)) +
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_dodge(0.4)) +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "", color = "Year", shape = "Region") +
  scale_shape_discrete(labels = c("East", "West")) +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

ProdDef_Loc <- ggarrange(ProdDef_ranked1, ProdDef_ranked2, ProdDef_ranked3, nrow = 1, common.legend = TRUE, align = "h", labels = c("Aroma", "Discovery", "Summerred"))
ggsave(ProdDef_Loc, filename = "Figures/ProdDef_Loc.jpeg", height = 6, width = 10)


#############################################
#POLLINATION DEFICIT SPLIT BY YEAR AND VARIETY

PollDef_ranked1 <- SeedSetDeficit5 %>% 
  filter(Apple_variety == 'Aroma') %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = as.character(Year), shape = Region)) +
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_dodge(0.4)) +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "Pollination deficit", color = "Year", shape = "Region") +
  scale_shape_discrete(labels = c("East", "West")) +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

PollDef_ranked2 <- SeedSetDeficit5 %>% 
  filter(Apple_variety == 'Discovery') %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = as.character(Year), shape = Region)) +
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_dodge(0.4)) +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "", color = "Year", shape = "Region") +
  scale_shape_discrete(labels = c("East", "West")) +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

PollDef_ranked3 <- SeedSetDeficit5 %>% 
  filter(Apple_variety == 'Summerred') %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = as.character(Year), shape = Region)) +
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_dodge(0.4)) +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "", color = "Year", shape = "Region") +
  scale_shape_discrete(labels = c("East", "West")) +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

PollDef_Loc <- ggarrange(PollDef_ranked1, PollDef_ranked2, PollDef_ranked3, nrow = 1, common.legend = TRUE, align = "h", labels = c("Aroma", "Discovery", "Summerred"))
ggsave(PollDef_Loc, filename = "Figures/PollDef_Loc.jpeg", height = 6, width = 10)


PollDef_ranked <- SeedSetDeficit5 %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = reorder(Location, mean), y = mean, color = Year)) + 
  geom_pointrange(aes(ymin = mean - ConfInterval, ymax = mean + ConfInterval), 
                  position = position_jitterdodge()) +
  #stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", color = "black") +
  scale_color_manual (values =  c("#99CCFF", "#333399")) +
  labs(x = "", y = "Pollination deficit", color = "Year") +
  ylim(-1, 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  facet_wrap(~Apple_variety)
ggsave(PollDef_ranked, filename = "Figures/PollDef_ranked.jpeg", height = 6, width = 10)




#######################################
##### POLLINATOR VISITS TAKEN BY CAMERA

#### PRODUCTION DEFICIT ##############

CameraVisSB <- expand.grid(SolitaryBee_AvrgPh = seq(0., 0.00175, length = 100), Apple_variety = c("Aroma", "Discovery", "Summerred"))

CamVisSB_22 <- make_prediction(CameraVisSB, Model_ProdDefVisit2)

SBCam <- make_prettyplot(dat = Deficit_Visits,
                         Newdata = CamVisSB_22, 
                         xaxis = SolitaryBee_AvrgPh, 
                         yaxis = ProdDef, 
                         prediction = pred, 
                         ColorVariable = Apple_variety, 
                         SE = SE,
                         line_type = "solid") +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  scale_fill_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "Solitary bee/recording", y = "Production deficit", color = "Apple cultivar", fill = "Apple cultivar") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(legend.position = "top") +
  ylim(-1, 1)


CameraVisHB <- expand.grid(Apis_AvrgPh = seq(0.0014, 0.0113, length = 100), Apple_variety = c("Aroma", "Discovery", "Summerred"))

CamVisHB_22 <- make_prediction(CameraVisHB, Model_ProdDefVisit1) #%>% 
  filter(Apple_variety == c('Aroma')) #filtered on the middle variety to only get one line in plot


HBCam <- make_prettyplot(dat = Deficit_Visits,
                         Newdata = CamVisHB_22, 
                         xaxis = Apis_AvrgPh, 
                         yaxis = ProdDef, 
                         prediction = pred, 
                         ColorVariable = Apple_variety, 
                         SE = SE,
                         line_type = "solid") +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  scale_fill_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "Honeybee/recording", y = "Production deficit", color = "Apple cultivar", fill = "Apple cultivar") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(legend.position = "none") +
  ylim(-1, 1)


CameraVisBB <- expand.grid(Bombus_AvrgPh = seq(0, 0.001027, length = 100), Apple_variety = c("Aroma", "Discovery", "Summerred"))

CamVisBB_22 <- make_prediction(CameraVisBB, Model_ProdDefVisit3) #%>% 
  filter(Apple_variety == 'Aroma') #filtered on the middle variety to only get one line in plot


BBCam <- make_prettyplot(dat = Deficit_Visits,
                         Newdata = CamVisBB_22, 
                         xaxis = Bombus_AvrgPh, 
                         yaxis = ProdDef, 
                         prediction = pred, 
                         ColorVariable = Apple_variety, 
                         SE = SE,
                         line_type = "solid") +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  scale_fill_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "Bumblebee/recording", y = "Production deficit", color = "Apple cultivar", fill = "Apple cultivar") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(legend.position = "none") +
  ylim(-1, 1)




#ggsave(CamProdDef, filename = "Figures/CamProdDef.jpeg", height = 8, width = 6)


#### POLLINATION DEFICIT ##############

PollVisSB <- expand.grid(SolitaryBee_AvrgPh = seq(0., 0.00175, length = 100), Apple_variety = c("Aroma", "Discovery", "Summerred"))

PollVisSB_22 <- make_prediction(PollVisSB, Model_PollDefVisit2)

SBPoll <- make_prettyplot(dat = Deficit_Visits,
                          Newdata = PollVisSB_22, 
                          xaxis = SolitaryBee_AvrgPh, 
                          yaxis = ProdDef, 
                          prediction = pred, 
                          ColorVariable = Apple_variety, 
                          SE = SE,
                          line_type = "solid") +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  scale_fill_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "Solitary bee/recording", y = "Pollination deficit", color = "Apple cultivar", fill = "Apple cultivar") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(legend.position = "top") +
  ylim(-1, 1)


PollVisHB <- expand.grid(Apis_AvrgPh = seq(0.0014, 0.0113, length = 100), Apple_variety = c("Aroma", "Discovery", "Summerred"))

PollVisHB_22 <- make_prediction(PollVisHB, Model_PollDefVisit1) 

HBPoll <- make_prettyplot(dat = Deficit_Visits,
                          Newdata = PollVisHB_22, 
                          xaxis = Apis_AvrgPh, 
                          yaxis = ProdDef, 
                          prediction = pred, 
                          ColorVariable = Apple_variety, 
                          SE = SE,
                          line_type = "solid") +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  scale_fill_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "Honeybee/recording", y = "Pollination deficit", color = "Apple cultivar", fill = "Apple cultivar") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(legend.position = "none") +
  ylim(-1, 1)


PollVisBB <- expand.grid(Bombus_AvrgPh = seq(0, 0.00102, length = 100), Apple_variety = c("Aroma", "Discovery", "Summerred"), weight = 0)

PollVisBB_22 <- make_prediction(PollVisBB, Model_PollDefVisit3) 

BBPoll <- make_prettyplot(dat = Deficit_Visits,
                          Newdata = PollVisBB_22, 
                          xaxis = Bombus_AvrgPh, 
                          yaxis = ProdDef, 
                          prediction = pred, 
                          ColorVariable = Apple_variety, 
                          SE = SE,
                          line_type = "solid") +
  scale_color_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  scale_fill_manual (values =  c("#CC6666", "#CCCC99", "#FF9966")) +
  labs(x = "Bumblebee/recording", y = "Pollination deficit", color = "Apple cultivar", fill = "Apple cultivar") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  theme(legend.position = "none") +
  ylim(-1, 1)

#CamPollDef <- ggarrange(SBPoll, BBPoll, HBPoll, nrow = 3)
CamSBDef <- ggarrange(SBPoll, SBCam, common.legend = TRUE, labels = c("a", "b"))
ggsave(CamSBDef, filename = "Figures/CamSBDef.jpeg", height = 6, width = 10)

CamDef <- ggarrange(BBPoll, BBCam, HBPoll, HBCam, common.legend = TRUE, labels = c("a", "b"))
ggsave(CamDef, filename = "Figures/CamDef.jpeg", height = 8, width = 10)

#ggsave(CamPollDef, filename = "Figures/CamPollDef.jpeg", height = 8, width = 6)

CamComb <- ggarrange(CamPollDef, CamProdDef, nrow = 1, labels = c("a", "b"))
ggsave(CamComb, filename = "Figures/CamComb.jpeg", height = 8, width = 10)




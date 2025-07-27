######################################################################################################
#########################       Persuading Groups -  Experiment 1         ############################
######################################################################################################
library(tidyverse)
library(ggbeeswarm)
library(ggdist)
library(ggh4x)
library(ggtext)
library(patchwork)
library(broom)
library(knitr)
library(kableExtra)
library(performance)
### import data
library(readxl)
PGS_Exp1 <- read_excel("data/24.09.23_PGS_Exp1.xlsx", sheet = "RData")
str(PGS_Exp1)


############
############ Create clean theme
theme_cleanPub<-theme_minimal()+theme(axis.text.x=element_text(size=14, face="bold",
                            family = "Palatino", angle=0, hjust=.5, vjust=.5, color="black"),
                        axis.title.x=element_text(size=14, face="bold", color="black"),
                        axis.title.y=element_text(size=16, face="bold", color="black"),
                        axis.text.y=element_text(size=16, face="bold", color="black"),
                        legend.text = element_text(size=13, face="bold", color="black"),
                        legend.position="bottom",
                        legend.title = element_blank(),
                        plot.title = element_text(hjust = 0.5, family="Palatino",
                                                  size=23, face="bold", colour="black"),
                        plot.subtitle=element_text(size=14, hjust=0.5, family="Palatino",
                                                   face="italic", color="black"),
                        strip.text = element_text(size=16, face="bold", color="black"),
                        strip.background = element_rect(fill="gray82", color="black"))

##### Remove explanations
PGS_Exp1<-PGS_Exp1%>%
  select(-Date, -Exp_Contrast, -Exp_Div, -Exp_Size, -Exp_Build)%>%
  # filter(Exclusion == 0)%>%
  separate(Age, into = c("AgeYears", "Months"), sep=";")%>%
  mutate(AgeGroup = factor(AgeGroup, levels = c("Younger", "Older", "Adults")),
         AgeYears = ifelse(AgeGroup == "Adults", NA, as.numeric(AgeYears)),
         AgeMonths = as.numeric(ifelse(is.na(Months), 12*as.numeric(AgeYears),
                                  12*as.numeric(AgeYears)+as.numeric(Months)))/12,
         AgeGroup2 = ifelse(AgeGroup == "Adults", "Adults", "Children"),
         AgeGroup2 = factor(AgeGroup2, levels = c("Children", "Adults")),
         Ct_AgeYears = ifelse(AgeGroup == "Adults", NA, scale(AgeYears)), .after = "Months")%>%
  relocate(AgeGroup2, .after=AgeGroup)%>%
  select(-Months)
str(PGS_Exp1)


##### Check exclusions
PGS_Exp1%>%
  group_by(AgeYears, Exclusion)%>%
  summarise(Total = n())

PGS_Exp1<-subset(PGS_Exp1, Exclusion == 0)

############# Demographics & Summary Stats
##### AgeYears (NA=Adults)
PGS_Exp1%>%
  group_by(AgeGroup)%>%
  summarise(AvgAge = mean(AgeMonths), SD_Age = sd(AgeMonths))

##### Gender (NA=Adults)
PGS_Exp1%>%
  group_by(Gender)%>%
  summarise(Totals = n())

##### Counterbalances (Target: 15 per cell)
PGS_Exp1%>%
  group_by(OrderCB, ColorCB)%>%
  summarise(Totals = n())

###### Pivot to longform data
PGS_Exp1_long<-PGS_Exp1%>%
  pivot_longer(cols = c("Contrast", "Div", "Size", "Build",), 
               names_to = "QType", values_to = "Values")%>%
  mutate(Researcher = factor(Researcher),
         OrderCB = factor(OrderCB, levels = c("DS_D_S", "DS_S_D", "D_S_DS", "S_D_DS")),
         ColorCB = factor(ColorCB),
         QType = factor(QType, levels = c("Contrast", "Div", "Size", "Build")),
         Ct_Values = Values - 4)
str(PGS_Exp1_long)


######################################################################################################
################################             Visualization                ############################
######################################################################################################

### Decision time judgments
PGS_Exp1_Time<-PGS_Exp1_long%>%
  filter(QType != "Build")%>%
  ggplot(aes(x=AgeGroup, y=Values, fill=AgeGroup))+
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept=4), color = "black", linetype="dashed", size=1.5)+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(group=AgeGroup), 
               position=position_dodge(width=1), size=2, width=.25, color="black")+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), group=AgeGroup), 
               position=position_dodge(width=1), color="black", size=6,
               fill="grey")+
  scale_y_continuous("Rate confidence: which team took longer?", 
                     limits = c(1,7), breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("Very\nSure \nBlue", "Pretty\nSure \nBlue", "A Little\nSure \nBlue", "Same\namount\nof time",
                                "A Little\nSure \nGreen", "Pretty\nSure \nGreen","Very\nSure \nGreen"))+
  scale_fill_manual(values = c("#EBC7C7","#CE7474", "#B22222"), 
                    guide = guide_legend(title = NULL),
                    labels = c("Ages 6-7\n(n=40)", "Ages 8-9\n(n=40)", "Adult-MTurk\n(n=41)"))+
  facet_grid(~QType, labeller=
               labeller(QType = as_labeller(c('Contrast' = "Contrast: Diversity vs. Size \n[insert test image: \nBlue large, Green diverse]",
                                              'Div' = "Diversity \n[insert test image: \n # people equal, Green diverse]", 
                                              'Size' = "Size \n[insert test image: \n Blue large, # factions equal",
                                              'Build' = "Build \n[insert test image: \n Blue large, no factions"))))+
  theme_cleanPub+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(), # element_text(size=14, face="bold", color="black", family="Palatino"),
        legend.text = element_text(size=13, face="bold", color="black", family="Palatino"),
        legend.box.spacing = unit(0, "pt"),
        axis.text.x = element_blank(),
        axis.title.y=element_text(size=13, face="bold", color="black", family="Palatino"),
        axis.text.y=element_text(size=9, face="bold", color="black", family="Palatino"),
        strip.text.x = element_text(size=11, margin = margin(0,0,3,0, "cm")),
        plot.title =  element_blank(),
        plot.subtitle=element_markdown(size=12, hjust=0.5, family="Palatino", face="italic", color="black"),
        plot.tag.position = c(.10, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"))+
  labs(title = "Experiment 1",
       tag = "(A)",
       subtitle = "'Which team will take longer to <strong>decide</strong> on a propeller?'<br>")

PGS_Exp1_Time

# Build time judgments
PGS_Exp1_Build<-PGS_Exp1_long%>%
  filter(QType == "Build")%>%
  ggplot(aes(x=AgeGroup, y=Values, fill=AgeGroup))+
  geom_boxplot(outlier.shape = NA)+
  geom_hline(aes(yintercept=4), color = "black", linetype="dashed", size=1.5)+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(group=AgeGroup), 
               position=position_dodge(width=1), size=2, width=.25, color="black")+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), group=AgeGroup), 
               position=position_dodge(width=1), color="black", size=6,
               fill="grey")+
  scale_y_continuous(limits = c(1,7), breaks = c(1, 2, 3, 4, 5, 6, 7))+
  scale_x_discrete(labels = c("Younger\n(6-7s)", "Older\n(8-9s)", "Adults\n(MTurk)"))+
  scale_fill_manual(values = c("#EBC7C7","#CE7474", "#B22222"), guide = "none",
                    labels = c("Younger\n(6-7s)", "Older\n(8-9s)", "Adults\n(MTurk)"))+
  facet_grid(~QType, labeller=
               labeller(QType = as_labeller(c('Contrast' = "Contrast \n[insert test image: \nBlue large, Green diverse]",
                                              'Div' = "Diversity \n[insert test image: \n # people equal, Green diverse]", 
                                              'Size' = "Size \n[insert test image: \n Blue large, # factions equal",
                                              'Build' = "Build \n[insert test image: \n Blue large, no factions"))))+
  theme_cleanPub+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size=11, margin = margin(0,0,3,0, "cm")),
        plot.title = element_blank(),
        plot.subtitle=element_markdown(size=12, hjust=0.5, family="Palatino",
                                       face="italic", color="black"),
        plot.tag.position = c(.01, 1),
        plot.tag = element_text(size = 12, face="bold", family="Arial",colour="black"))+
  labs(title = "Experiment 1",
       tag = "(B)",
       subtitle = "'<strong>After</strong> deciding on a<br> propeller, which team will take<br> longer to <strong>build</strong> the drone?'")

PGS_Exp1_Build

#### Combine plots using library(patchwork); library(ggtext)
PGS_Exp1_Time+PGS_Exp1_Build+
  plot_layout(ncol=2, widths = c(3, 1),
              guides = 'collect')+
  plot_annotation(
    title = 'Experiment 1',
    theme = theme(
      legend.position = "bottom",
      legend.box.spacing = unit(0, "pt"),
      plot.title = element_text(hjust = 0.5, family="Palatino", size=23, face="bold", colour="black"),
      plot.subtitle=element_text(hjust=0.5, family="Palatino", size=14, face="italic", color="black")))


##################################################
################################################## EXTRA FIGURES
################################################## 
# ###### Main Measures: Age in Years 
PGS_Exp1_long%>%
  mutate(AgeYears = ifelse(AgeGroup == "Adults", 18, AgeYears))%>%
  filter(QType != "Build")%>%
  ggplot(aes(x=as.numeric(AgeYears), y=Values, fill=QType))+
  geom_smooth(method = "lm", level=.95, aes(color=QType))+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(color=QType, group=QType),
               position=position_dodge(width=1), size=2, width=.25)+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), fill=QType, group=QType),
               position=position_dodge(width=1), color="black", size=6)+
  scale_y_continuous("Rating (1=Larger/Consensus, 7=More Diverse)",
                     limits = c(0,8), breaks = c(1, 2, 3, 4, 5, 6, 7))+
  scale_fill_manual(values = c("dodgerblue","goldenrod2", "seagreen", "firebrick"))+
  scale_color_manual(values = c("dodgerblue","goldenrod2", "seagreen", "firebrick"))+
  theme_cleanPub+
  facet_grid2(~QType+AgeGroup2, scales = "free_x", strip = strip_nested(size = "variable"))+
  ggtitle("Exp 1: 'Which team will take longer to decide on a propeller?'")


####### Categorical Judgments - which team will take longer?
View(PGS_Exp1_long)
PGS_Exp1_long%>%
  mutate(ValuesCAT = ifelse(Values == 4, "Same",
                            ifelse(Values < 4, "LargerHomogen", "SmallerDiv")),
         ValuesCAT = factor(ValuesCAT, levels = c("SmallerDiv", "Same", "LargerHomogen")))%>%
  ggplot(aes(x=AgeGroup, fill=ValuesCAT))+
  geom_bar(position=position_dodge2(preserve = "single"))+
  scale_x_discrete(labels = c("Younger\n(6-7s)", "Older\n(8-9s)", "Adults\n(MTurk)"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod2", "seagreen"))+
  theme_cleanPub+
  facet_grid(~QType)+
  ggtitle("Exp 1: Categorical Inferences",
          subtitle = "")


######################################################################################################
###############################           Statistical Analyses       #################################
######################################################################################################

########### PART 1a: first check whether counterbalancing question order had any effect (no).
#### OrderCB is not significant for any of the measures w/ adults or just w/ kids
PGS_Exp1_long%>%
  filter(AgeGroup != "Adults")%>%
  mutate(Ct_AgeYears = (AgeYears - mean(AgeYears)),
         Ct_Values = (Values - 4))%>%
  group_by(QType)%>%
  nest()%>%
  mutate(mod_OrderCB = map(data, ~tidy(anova(lm(Ct_Values ~ Ct_AgeYears*OrderCB, data=.)), 
                                       conf.int=T),.id="Measure"))%>%
  unnest(mod_OrderCB)%>%
  select(-data)%>%
  ### remove print_md() if not printing to console
  print_md()


########### PART 1b: OrderCB also not significant even if we don't include age in the model at all
#### Test OrderCB while dropping Ct_AgeYears, still no significant coefficients for orderCB levels
PGS_Exp1_long%>%
  mutate(Ct_AgeYears = (AgeYears - mean(AgeYears)),
         Ct_Values = (Values - 4))%>%
  group_by(QType)%>%
  nest()%>%
  mutate(mod_OrderCB = map(data, ~tidy(anova(lm(Ct_Values ~ OrderCB, data=.)), 
                                       conf.int=T),.id="Measure"))%>%
  unnest(mod_OrderCB)%>%
  select(-data)%>%
  ### remove print_md() if not printing to console
  print_md()

########### PART 2: proceed to main analysis 
#### intercept significant in all measures, but Ct_AgeYears is also significant in Build and Size
PGS_Exp1_long%>%
  filter(AgeGroup != "Adults")%>%
  mutate(Ct_AgeYears = (AgeYears - mean(AgeYears)),
         Ct_Values = (Values - 4))%>%
  group_by(QType)%>%
  nest()%>%
  mutate(mod_AgeYears = map(data, ~tidy(lm(Ct_Values ~ Ct_AgeYears, data=.), 
                                       conf.int=TRUE)))%>%
  unnest(mod_AgeYears)%>%
  mutate(across(estimate:conf.high, ~round(.x, digits=3)))%>%
  select(-data)%>%
  ### remove print_md() if not printing to console
  print_md()
  # add the remaining rows to see model output in viewer instead of console
  kbl(full_width = F, booktabs=T, align='lcc', times = 3)%>%
  kable_styling(full_width = F, "striped", html_font = "Palatino")%>%
  pack_rows(index = c("Contrast" = 2, "Div" = 2, "Size" = 2, "Build"=2))%>%
  add_header_above(c("Model Results for each QType: lm(Ct_Values ~ Ct_AgeYears)"= 8))

########### PART 3: compare each age group to chance for each measure as well
#### does the youngest age group also differ from chance for Build (yes) and Size (no)?
PGS_Exp1_long%>%
  mutate(Ct_AgeYears = (AgeYears - mean(AgeYears)),
         Ct_Values = (Values - 4))%>%
  group_by(AgeGroup, QType)%>%
  nest()%>%
  mutate(mod_AgeGroup = map(data, ~tidy(t.test(Ct_Values~1, mu=0, data=.), 
                                        conf.int=T),.id="Measure"))%>%
  unnest(mod_AgeGroup)%>%
  mutate(across(estimate:conf.high, ~round(.x, digits=3)))%>%
  select(-data)%>%
  arrange(by_group=AgeGroup)%>%
  relocate("QType", .before="AgeGroup")%>%
  ### remove print_md() if not printing to console
  print_md()
  # add the remaining rows to see model output in viewer instead of console
  # ungroup(AgeGroup)%>%
  # select(-AgeGroup)%>%
  # kbl(full_width = F, booktabs=T, align='lcc', times = 4)%>%
  # kable_styling(full_width = F, "striped", html_font = "Palatino")%>%
  # pack_rows(index = c("Younger"= 4, "Older" = 4, "Adult" = 4))%>%
  # add_header_above(c("t.tests by AgeGroup & QType"= 9))

  
  
  

  
  
  
  
  
  
  
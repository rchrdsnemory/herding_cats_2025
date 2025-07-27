######################################################################################################
#########################       Persuading Groups -  Experiment 2         ############################
######################################################################################################
library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom)
library(knitr)
library(kableExtra)
library(parameters)
library(performance)
library(rstatix)
library(RColorBrewer)
library(gt)
library(gtExtras)
# import data
library(readxl)
PGS_Exp2 <- read_excel("[insert pathname]~24.09.23_PGS_Exp2.xlsx", 
                       sheet = "RData")
str(PGS_Exp2)

############
############ Create clean theme
# theme_cleanPub<-theme_minimal()+theme(axis.text.x = element_text(size=12, lineheight=.9, face="bold",
#                                                                  color="black", family="Palatino",
#                                                                  angle=0, hjust=.5, vjust=0),
#                                       axis.title.x=element_text(size=14, face="bold", color="black", family="Palatino",),
#                                       axis.title.y=element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       axis.text.y=element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       legend.text = element_text(size=13, face="bold", color="black", family="Palatino",),
#                                       legend.position="bottom",
#                                       legend.title = element_blank(),
#                                       plot.title = element_text(hjust = 0.5, family="Palatino",
#                                                                 size=23, face="bold", colour="black"),
#                                       plot.subtitle=element_text(size=14, hjust=0.5, family="Palatino",
#                                                                  face="italic", color="black"),
#                                       strip.text = element_text(size=16, face="bold", color="black", family="Palatino",),
#                                       strip.background = element_rect(fill="gray82", color="black"))



##### Remove explanations and scale training Qs
PGS_Exp2<-PGS_Exp2%>%
  select(-Date, -Majority_ExplainTime, -Supermajority_ExplainTime, -Underdog_ExplainTime, -Diversity_ExplainTime)%>%
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
str(PGS_Exp2)


##### Check exclusions: excluding 6 adults and 2 kids
PGS_Exp2%>%
  group_by(AgeGroup, Exclusion)%>%
  summarise(Total = n())
### Remove exclusions
PGS_Exp2<-subset(PGS_Exp2, Exclusion == 0)

############# Demographics & Summary Stats
##### Age Group stats
PGS_Exp2%>%
  group_by(AgeGroup)%>%
  summarise(AvgAge = mean(AgeMonths), SD_Age = sd(AgeMonths), n())

##### Gender: 60 F / 39 M / 1 NA
PGS_Exp2%>%
  group_by(AgeGroup2, Gender)%>%
  summarise(Totals = n())

##### Counterbalances (Children Target: 25 per OrderCB)
PGS_Exp2%>%
  group_by(AgeGroup, OrderCB)%>%
  summarise(Totals = n())

## Total participants (planned sample: 150)
PGS_Exp2%>%
  group_by(Exclusion)%>%
  summarise(Final_N=n())

## Total by age (Target: 25 per year + 50 adults)
PGS_Exp2_longT%>%
  group_by(AgeYears)%>%
  summarise(Current_n=n_distinct(subID))


##############################################################################################
########################### Pivot data for time judgments
PGS_Exp2_longT<-PGS_Exp2%>%
  pivot_longer(cols = c("Majority_Time_B2v4G8v4", "Supermajority_Time_B6v4G16v4",  "Underdog_Time_B6vDivWG6vMajW", 
                        "Diversity_Time_B6vDivRG6vMajR"),
               names_pattern = "(.*)_(.*)_(.*)", names_to = c("Trial", "Measure", "GType"),
               values_to = "Values")%>%
  mutate(GType = factor(GType, levels = c("B2v4G8v4", "B6v4G16v4",
                                          "B6vDivWG6vMajW","B6vDivRG6vMajR")))%>%
  mutate(
    # recode values from adults - counterbalancing "double reversed" scales in qualtrics
    Values = ifelse(AgeGroup == "Adults" & ColorCB == "BlueHighDiv", -1*Values+8, Values),
    # Create an absolute difference score for time judgments
    AbsDiff_Values = abs(Values-4),
    AbsDiff_bySub = ifelse(Trial == "Supermajority", lead(AbsDiff_Values, 2)-AbsDiff_Values, 
                    ifelse(Trial == "Underdog", lead(AbsDiff_Values, 1)-AbsDiff_Values,NA)),
    StimOrder = case_when(
      OrderCB == "CB1" & Trial == "Majority" ~ "Stim1",
      OrderCB == "CB2" & Trial == "Majority" ~ "Stim2",
      OrderCB == "CB3" & Trial == "Majority" ~ "Stim3",
      OrderCB == "CB4" & Trial == "Majority" ~ "Stim4",
      OrderCB == "CB1" & Trial == "Supermajority" ~ "Stim2",
      OrderCB == "CB2" & Trial == "Supermajority" ~ "Stim1",
      OrderCB == "CB3" & Trial == "Supermajority" ~ "Stim2",
      OrderCB == "CB4" & Trial == "Supermajority" ~ "Stim3",
      OrderCB == "CB1" & Trial == "Underdog" ~ "Stim3",
      OrderCB == "CB2" & Trial == "Underdog" ~ "Stim4",
      OrderCB == "CB3" & Trial == "Underdog" ~ "Stim1",
      OrderCB == "CB4" & Trial == "Underdog" ~ "Stim2",
      OrderCB == "CB1" & Trial == "Diversity" ~ "Stim4",
      OrderCB == "CB2" & Trial == "Diversity" ~ "Stim3",
      OrderCB == "CB3" & Trial == "Diversity" ~ "Stim4",
      OrderCB == "CB4" & Trial == "Diversity" ~ "Stim1",
      TRUE~"Missing"),
    StimNum = as.numeric(as.factor(StimOrder)))%>%
  mutate(Trial = factor(Trial, levels = c("Majority", "Supermajority", "Underdog", "Diversity")))

##############################################################################################
########################### Pivot data for accuracy judgments
PGS_Exp2_longA<-PGS_Exp2%>%
  pivot_longer(cols = c("Majority_AccBlue", "Majority_AccGreen", 
                        "Supermajority_AccBlue", "Supermajority_AccGreen",
                        "Underdog_AccBlue", "Underdog_AccGreen",
                        "Diversity_AccBlue", "Diversity_AccGreen"),
               names_pattern = "(.*)_(.*)", names_to = c("Trial", "Measure"),
               values_to = "Accurate")%>%
  mutate(GType = case_when(
    ColorCB == "BlueHighDiv" & Trial == "Majority" & Measure == "AccBlue" ~ "2v4",
    ColorCB == "BlueHighDiv" & Trial == "Majority" & Measure == "AccGreen" ~ "8v4",
    ColorCB == "BlueHighDiv" & Trial == "Supermajority" & Measure == "AccBlue" ~ "6v4",
    ColorCB == "BlueHighDiv" & Trial == "Supermajority" & Measure == "AccGreen" ~ "16v4",
    ColorCB == "BlueHighDiv" & Trial == "Underdog" & Measure == "AccBlue" ~ "4vDivW",
    ColorCB == "BlueHighDiv" & Trial == "Underdog" & Measure == "AccGreen" ~ "4vMajW",
    ColorCB == "BlueHighDiv" & Trial == "Diversity" & Measure == "AccBlue" ~ "4vDivR",
    ColorCB == "BlueHighDiv" & Trial == "Diversity" & Measure == "AccGreen" ~ "4vMajR",
    ColorCB == "GreenHighDiv" & Trial == "Majority" & Measure == "AccBlue" ~ "8v4",
    ColorCB == "GreenHighDiv" & Trial == "Majority" & Measure == "AccGreen" ~ "2v4",
    ColorCB == "GreenHighDiv" & Trial == "Supermajority" & Measure == "AccBlue" ~ "16v4",
    ColorCB == "GreenHighDiv" & Trial == "Supermajority" & Measure == "AccGreen" ~ "6v4",
    ColorCB == "GreenHighDiv" & Trial == "Underdog" & Measure == "AccBlue" ~ "4vMajW",
    ColorCB == "GreenHighDiv" & Trial == "Underdog" & Measure == "AccGreen" ~ "4vDivW",
    ColorCB == "GreenHighDiv" & Trial == "Diversity" & Measure == "AccBlue" ~ "4vMajR",
    ColorCB == "GreenHighDiv" & Trial == "Diversity" & Measure == "AccGreen" ~ "4vDivR",
    TRUE ~ "NA"),
    AgeGroup = ifelse(is.na(AgeYears), "Adult", ifelse(AgeYears < 8, "Younger", "Older")),
    AgeGroup2 = ifelse(AgeGroup != "Adult", "Children", "Adult"),
    AgeGroup = factor(AgeGroup, levels = c("Younger", "Older", "Adult")),
    AgeGroup2 = factor(AgeGroup2, levels = c("Children", "Adult")),
    AgeYears = ifelse(is.na(AgeYears), 18, as.numeric(AgeYears)))%>%
  mutate(AccurateNUM = ifelse(Accurate == "Yes4", 1, 0),
         Trial = factor(Trial, levels = c("Majority", "Supermajority", "Underdog", "Diversity")),
         TimeRating = case_when(Trial == "Majority" ~ Majority_Time_B2v4G8v4,
                                Trial == "Supermajority" ~ Supermajority_Time_B6v4G16v4,
                                Trial == "Underdog" ~ Underdog_Time_B6vDivWG6vMajW,
                                Trial == "Diversity" ~ Diversity_Time_B6vDivRG6vMajR),
         TimeRating = ifelse(AgeGroup == "Adult" & ColorCB == "BlueHighDiv", -1*TimeRating+8, TimeRating),
         GType = factor(GType, levels = c("8v4", "2v4", "16v4", "6v4",
                                                 "4vMajR", "4vDivR","4vMajW", "4vDivW")))


######################################################################################################
################################             Visualization                ############################
######################################################################################################

PGS_Exp2_longT%>%
  ggplot(aes(x=AgeGroup, y=Values, fill=AgeGroup))+
  geom_boxplot(outlier.size=0)+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(group=AgeGroup), 
               position=position_dodge(width=1), size=2, width=.25, color="black")+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), group=AgeGroup), 
               position=position_dodge(width=1), color="black", size=6,
               fill="grey")+
  geom_hline(aes(yintercept=4), color = "black", linetype="dashed", size=1.5)+
  scale_y_continuous("Rate confidence: which team took longer?", 
                     limits = c(1,7), breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("Very\nSure \nBlue", "Pretty\nSure \nBlue", "A Little\nSure \nBlue", "Same\namount\nof time",
                                "A Little\nSure \nGreen", "Pretty\nSure \nGreen","Very\nSure \nGreen"))+
  scale_fill_manual(values = c("#EBC7C7","#CE7474", "#B22222"), 
                    guide = guide_legend(title.position = "top", title.hjust = .5, label.hjust=.5),
                    labels = c("Ages 6-7\n(n=50)", "Ages 8-9\n(n=50)", "Adult-MTurk\n(n=50)"))+
  facet_grid(~Trial, labeller=
               labeller(Trial = as_labeller(c('Majority' = "Maj_Min \n [insert test image: \n Blue 8v4, Green 2v3]",
                                              'Supermajority' = "SuperMaj_Maj \n[insert test image: \n Blue 16v4, Green 6v4]", 
                                              'Underdog' = "SuperMin_MinDiv \n[insert test image: \n Blue 4v16Maj, Green 4v16Div]",
                                              'Diversity' = "SuperMaj_PluralityDiv \n[insert test image: \n Blue 16v4, Green 6v14]"))))+
  theme_cleanPub+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(), # element_text(size=14, face="bold", color="black", family="Palatino"),
        legend.text = element_text(size=13, face="bold", color="black", family="Palatino"),
        legend.box.spacing = unit(0, "pt"),
        axis.text.x = element_blank(), # element_text(size=14, face="bold", color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        strip.text.x = element_text(size=11, margin = margin(0,0,3,0, "cm")),
        plot.title =  element_blank(),
        plot.subtitle=element_markdown(size=12, hjust=0.5, family="Palatino", face="italic", color="black"))+
  labs(title = "Experiment 2: Decision Speed",
       subtitle = "'Pretend we know that <strong>both</strong> teams chose the 4-blade propeller: 
                    which team took <strong>longer</strong> to decide?' <br>")


######## Accuracy Plots
# find colors
# palette_yellows <- colorRampPalette(colors = c("white", "#F9BB03"))(9)
# scales::show_col(palette_yellows) # "#F9BB03", "#FBD461", "#FDEEC0"

PGS_Exp2_longA%>%
  mutate(GType = case_when(
    Trial == "Majority" & GType == "8v4" ~ "8v4",
    Trial == "Majority" & GType == "2v4" ~ "2v4",
    Trial == "Supermajority" & GType == "16v4" ~ "16v4",
    Trial == "Supermajority" & GType == "6v4" ~ "6v4",
    Trial == "Underdog" & GType == "4vDivW" ~ "4v16Div",
    Trial == "Underdog" & GType == "4vMajW" ~ "4v16Maj",
    Trial == "Diversity" & GType == "4vDivR" ~ "6v14Div",
    Trial == "Diversity" & GType == "4vMajR" ~ "16v4"))%>%
  mutate(GType = factor(GType, levels = c("8v4", "2v4", "16v4", "6v4", "4v16Maj", "4v16Div", "16v4Maj", "6v14Div")))%>%
  ggplot(aes(x=AgeGroup, y=AccurateNUM, fill=AgeGroup))+
  geom_bar(stat = "summary", fun = "mean")+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(y=AccurateNUM, group=AgeGroup), 
               position=position_dodge(width=1), color="black", size=.85, width=.15)+
  stat_summary(fun ="mean", geom="label", aes(y=AccurateNUM,label=round(..y.., 2), group=GType), 
               position=position_dodge(width=1), color="black", size=4,
               fill="grey")+
  geom_hline(aes(yintercept=0.5), color = "black", linetype="dashed", size=1.5)+
  scale_fill_manual(values = c("#FDEEC0", "#FBD461", "#F9BB03"),
                    labels = c("Ages 6-7", "Ages 8-9", "Adults"))+
  scale_y_continuous("Percent Predicting Accurate Decision",labels=scales::percent,
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00), limits = c(0,1))+
  facet_grid2(~Trial+GType, scales="free_x", strip = strip_nested(size = "variable"),
              labeller = labeller(Trial = as_labeller(c('Majority' = "Maj_Min \n[insert test image]\n\n\n\n\n",
                                                        'Supermajority' = "SuperMaj_Maj \n[insert test image]\n\n\n\n\n", 
                                                        'Underdog' = "SuperMin_MinDiv \n[insert test image]\n\n\n\n\n",
                                                        'Diversity' = "SuperMaj_PluralityDiv \n[insert test image]\n\n\n\n\n"))))+
  theme_cleanPub+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_text(size=13, face="bold", color="black"),
        axis.text.y=element_text(size=13, face="bold", color="black"),
        strip.text.x = element_text(size=14, face="bold", color="black"), 
        plot.title =   element_markdown(hjust = 0.5, family="Palatino",size=23, face="bold", colour="black"),
        plot.subtitle= element_markdown(size=12, vjust=0.5, family="Palatino", face="italic", color="black"),
        legend.title = element_blank())+
  labs(title = "Experiment 2: Predict Decision",
       subtitle = "'Which propeller do you think each team will <strong>actually</strong> decide to use?'")


##################################################
################################################## EXTRA FIGURES
################################################## 

# ###### Main Measures: Age in Years 
PGS_Exp2_longT%>%
  mutate(AgeYears = ifelse(AgeGroup == "Adults", 18, AgeYears))%>%
  ggplot(aes(x=as.numeric(AgeYears), y=Values, fill=GType))+
  geom_smooth(method = "lm", level=.95, aes(color=GType))+
  stat_summary(fun.data ="mean_cl_normal", geom="errorbar", aes(color=GType, group=GType),
               position=position_dodge(width=1), size=2, width=.25)+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), fill=GType, group=GType),
               position=position_dodge(width=1), color="black", size=6)+
  scale_y_continuous("Rating (1=Larger/Consensus, 7=More Diverse)",
                     limits = c(0,8), breaks = c(1, 2, 3, 4, 5, 6, 7))+
  scale_fill_manual(values = c("dodgerblue","goldenrod2", "seagreen", "firebrick"))+
  scale_color_manual(values = c("dodgerblue","goldenrod2", "seagreen", "firebrick"))+
  theme_cleanPub+
  facet_grid2(~Trial+AgeGroup2, scales = "free_x", strip = strip_nested(size = "variable"))+
  ggtitle("Exp 2: 'Which team took longer to decide on a propeller?'")


####### Categorical Judgments - which team will take longer?
PGS_Exp2_longT%>%
  mutate(ValuesCAT = ifelse(Values == 4, "Same",
                            ifelse(Values < 4, "LargerHomogen", "SmallerDiv")),
         ValuesCAT = factor(ValuesCAT, levels = c("SmallerDiv", "Same", "LargerHomogen")))%>%
  ggplot(aes(x=AgeGroup, fill=ValuesCAT))+
  geom_bar(position=position_dodge2(preserve = "single"))+
  scale_x_discrete(labels = c("Younger\n(6-7s)", "Older\n(8-9s)", "Adults\n(MTurk)"))+
  scale_fill_manual(values = c("dodgerblue", "goldenrod2", "seagreen"))+
  theme_cleanPub+
  facet_grid(~Trial)+
  ggtitle("Exp 2: Categorical Inferences",
          subtitle = "")


######################################################################################################
###############################           Statistical Analyses       #################################
######################################################################################################

################################## 
################################## Time judgments
################################## 

########### PART 1: primary question of interest - which team does each age think took longer in each contrast?
### Run & save t.tests vs. chance for each QType and age
PGS_Exp2_longT%>%
  group_by(AgeGroup, Trial)%>%
  nest(data=c(-AgeGroup, -Trial))%>%
  mutate(mod_ttest= map(data, ~model_parameters(t.test(Values~1, mu=4, data=., alternative="two.sided"), 
                                                conf.int=T),.id="Measure"))%>%
  unnest(mod_ttest)%>%
  select(-data, -Parameter, -Method)%>%
  arrange(by_group=Trial)%>%
  relocate("Trial", .before="AgeGroup")%>%
  #### use print_md() to print a formatted table to the console, unselect if printing to kable
  print_md(digits="scientific2")
  #### add the code below to see pretty kable tables in viewer instead of console
  # kable(booktabs = T, digits=3) %>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  # group_rows("Maj_Min", 1,3)%>%
  # group_rows("SuperMaj_Maj", 4,6)%>%
  # group_rows("SuperMin_MinDiv", 7,9)%>%
  # group_rows("SuperMaj_PluralityDiv", 10,12)%>%
  # add_header_above(c("t.tests vs. chance for each AgeGroup & trial" = 12),
  #                  font_size = 18)%>%
  # kable_styling(full_width = F, html_font = "Palatino")

########### PART 2: do ages differ in time judgments?

### Run & save one-way ANOVA for each trial for age effects
modTime_aov<-PGS_Exp2_longT%>%
  group_by(Trial)%>%
  nest(data=c(-Trial))%>%
  mutate(mod_aov= map(data, ~model_parameters(aov(Values~AgeGroup, data=.), 
                                   effectsize_type="eta"),.id="Measure"),
         mod_pairs = map(data, ~emmeans_test(., Values ~ AgeGroup, 
                                             p.adjust.method = "bonferroni", detailed = TRUE)))

## Print ANOVA, PART 2a: do age groups differ in time judgments?
modTime_aov%>%
  unnest(mod_aov)%>%
  select(-data, -mod_pairs)%>%
  #### use print_md() to print a formatted table to the console
  print_md()
  #### add the code below to see pretty kable tables in viewer instead of console
  # kable(booktabs = T, digits=2)%>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  # group_rows("Majority", 1,2)%>%
  # group_rows("Supermajority", 3,4)%>%
  # group_rows("Underdog", 5,6)%>%
  # group_rows("Diversity", 7,8)%>%
  # add_header_above(c("Main effects for One-Way ANOVAs" = 8),
  #                  font_size = 18)%>%
  # kable_styling(full_width = F, html_font = "Palatino")

## Print ANOVA, PART 2b: which age groups differ in time judgments?
modTime_aov%>%
  unnest(mod_pairs)%>%
  select(-data, -mod_aov, -term, -.y., -null.value, -p.adj.signif)%>%
  #### use print_md() to print a formatted table to the console
  print_md()
  #### add the code below to see pretty kable tables in viewer instead of console
  # kable(booktabs = T, digits=2) %>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  # group_rows("Majority", 1,3)%>%
  # group_rows("Supermajority", 4,6)%>%
  # group_rows("Underdog", 7,9)%>%
  # group_rows("Diversity", 10,12)%>%
  # add_header_above(c("Pairwise Comparisons for One-Way ANOVAs" = 11),
  #                  font_size = 18)%>%
  # kable_styling(full_width = F, html_font = "Palatino")

################################## Accuracy judgments
### Run & print binomials for accuracy 
PGS_Exp2_longA%>%
  group_by(Trial, GType, AgeGroup, AccurateNUM)%>%
  summarise(Totals=n(), .groups="drop")%>%
  pivot_wider(id_cols = Trial:AgeGroup, values_from="Totals", values_fill = 0,
              names_from = "AccurateNUM", names_prefix = "Response")%>%
  mutate(Total_trials=Response0+Response1)%>%
  nest(data=c(-Trial, -GType, -AgeGroup))%>%
  mutate(accuracy_binoms = map(data, ~tidy(binom.test(.$Response1, .$Total_trials, 
                                                      p=.5, alternative = "two.sided", conf.level=0.95))))%>%
  unnest(accuracy_binoms)%>%
  select(-data, -method)%>%
  # print_md()
  ######## Add these rows for predictions & outcomes 
  mutate(Predictions = case_when(
    Trial == "Majority" & GType == "2v4" ~ "Choose Other",
    Trial == "Majority" & GType == "8v4" ~ "Choose Optimal",
    Trial == "Supermajority" & GType == "6v4" ~ "Choose Optimal",
    Trial == "Supermajority" & GType == "16v4" ~ "Choose Optimal",
    Trial == "Underdog" ~ "Choose Other",
    Trial == "Diversity" & GType == "4vDivR" ~ "Exploratory: Possible Plurality",
    Trial == "Diversity" & GType == "4vMajR" ~ "Choose Optimal"),
    Outcome = case_when(
      Trial == "Underdog" & estimate < .5 & p.value <.05 ~ "As Predicted",
      Trial == "Majority" & GType == "2v4" & estimate <.5 & p.value <.05 ~ "As Predicted",
      Trial == "Diversity" & GType == "4vDivR" ~ "No Prediction",
      estimate > .5 & p.value < .05 ~ "As Predicted",
      TRUE~"Null Finding"))%>%
  #### use print_md() to print a formatted table to the console
  print_md()
  #### add the code below & remove print_md() above to see pretty kable tables in viewer instead of console
  # kable(booktabs = T, digits=3) %>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  # group_rows("Majority", 1,6)%>%
  # group_rows("Supermajority", 7,12)%>%
  # group_rows("Underdog", 13,18)%>%
  # group_rows("Diversity", 19,24)%>%
  # add_header_above(c("Two-way binomials for Accuracy Judgments" = 12),
  #                  font_size = 18)%>%
  # kable_styling(full_width = F, html_font = "Palatino")



########################################################################
############### Table of binomial tests for final choice accuracy predictions
Table1<-PGS_Exp2_longA%>%
  mutate(Trial = case_when(Trial == "Majority" ~ "Maj.Min", Trial == "Supermajority" ~ "SuperMaj.Maj",
                           Trial == "Underdog" ~ "SuperMin.MinDiv", Trial == "Diversity" ~ "SuperMaj.PluralityDiv"),
         GType = case_when(GType == "4vDivR" ~ "6v14Div", GType == "4vMajR" ~ "16v4", 
                           GType == "4vMajW" ~ "4v16", GType == "4vDivW" ~ "4v16Div", TRUE ~ GType))%>%
  group_by(Trial, GType, AgeGroup, AccurateNUM)%>%
  summarise(Totals=n(), .groups="drop")%>%
  pivot_wider(id_cols = Trial:AgeGroup, values_from="Totals", values_fill = 0,
              names_from = "AccurateNUM", names_prefix = "Response")%>%
  mutate(Total_trials=Response0+Response1)%>%
  nest(data=c(-Trial, -GType, -AgeGroup))%>%
  mutate(accuracy_binoms = map(data, ~tidy(binom.test(.$Response1, .$Total_trials, 
                                                      p=.5, alternative = "two.sided", conf.level=0.95))))%>%
  unnest(accuracy_binoms)%>%
  rename(propSuccess = estimate)%>%
  mutate(binom.result = glue::glue('{statistic} of {parameter}'),
         p.value = format.pval(p.value, digits = 3, eps=0.001, nsmall=3),
         across(c("propSuccess", "conf.low", "conf.high"), ~ format(round(.x, 2), nsmall=2)),
         conf.interval = glue::glue("[{conf.low}-{conf.high}]"))%>%
  mutate(binom.test = glue::glue('{binom.result}, p(Success) = {propSuccess}'))%>%
  select(-data, -method, -alternative, -statistic, -parameter, -conf.high, -conf.low, -binom.result, -propSuccess)%>%
  relocate(c(binom.test, conf.interval, p.value), .after="AgeGroup")%>%
  # filter(str_detect(p.value, "<")==F)
  gt(groupname_col = "Trial")%>%
  opt_row_striping()%>%
  # gt_highlight_cols(columns = c(AgeGroup, GType), fill="lightgrey")%>%
  text_transform(
    locations = cells_body(columns=c(GType), rows = AgeGroup != "Younger"),
    fn = function(x) "")%>%
  cols_label(GType = "",AgeGroup = md("Age<br>Group"), binom.test = md("binomial<br>test"),
             conf.interval = md("confidence<br>interval"))%>%
  tab_style(style = cell_text(color="red"),
            locations = cells_body(
              columns = 4:6, rows = str_detect(p.value, "<")==F))%>%
  tab_style(style = cell_fill(color="lightgrey"), locations = list(cells_row_groups(), 
                                                                   cells_body(columns=c(GType, AgeGroup))))%>%
  tab_style(style = cell_text(v_align="middle"), locations = cells_column_labels(columns=p.value))%>%
  tab_style(style = cell_text(weight = "bold", font="Palatino"), locations = list(cells_row_groups(),
                                                                                  cells_body(columns=c(GType, AgeGroup)),
                                                                                  cells_column_spanners(),
                                                                                  cells_column_labels(),
                                                                                  cells_title()))%>%
  tab_style(style = cell_text(style = "italic", font="Palatino"),locations = cells_body(columns=contains(".")))%>%
  tab_header(title = md("SI Table 1: binomials for final team choice predictions"))
# print table
Table1<-Table1%>%
  gtsave("PGS_table1.png", expand = 10)





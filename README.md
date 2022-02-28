1. Overview

The code in this replication packages constructs our analysis using R Studio. There are four figures in this project. Figure 1 is original and figure 2-4 is replicated from the Kearney, Levine, and Pardue (2021). All the steps are done by R Studio. The following packages is needed before running the code.

library(readr)
library(tidyverse)
library(ggplot2)
library(haven)
library(gcookbook)#plot graph
library(ggrepel)
library(gtrendsR)
library(usmap)
library(dplyr)
library(car)
library(patchwork)

2. Data Availability and Provenance Statements

2.1 Statement about Rights
We certify that the authors of the manuscript have legitimate access to and permission to use the data used in this manuscript.

2.2 Summary of Availability
All data in the replication package is available here, though some births data relies on aggregations from restricted-use microdata

figs_1_2_3 <- read_csv("figs_1_2_3.csv")
figs_2a_2b <- read_csv("figs_2a_2b.csv")
age_race_comp_seer <- read_dta("age_race_comp_seer.dta")
nchs_births_pop_1990_2019 <- read_dta("nchs_births_pop_1990_2019.dta")    

3. Instruction and Code

Here are the instructions and codes to replicate the graph.

- Figure 1: 

This figure shows the female population trend in US from 1990 to 2020 by age group

### code for figure 1###
pop<-aggregate(age_race_comp_seer$pop1519,by=list(year = age_race_comp_seer$year), FUN=sum)%>%mutate(x = x/10000000)
pop15_19 <- ggplot(pop, aes(year,x)) +
  geom_line() +
  xlab("year") +
  theme_classic() +
  ylab("population of age 15-19 per 10000000") +
  labs(title="Female Population with Age 15-19")
pop<-aggregate(age_race_comp_seer$pop2034,by=list(year = age_race_comp_seer$year), FUN=sum)%>%mutate(x = x/10000000)
pop20_34 <- ggplot(pop, aes(year,x)) +
  geom_line() +
  theme_classic() +
  xlab("year") +
  ylab("population of age 20-34 per 10000000") +
  labs(title="Female Population with Age 20-34")
pop<-aggregate(age_race_comp_seer$pop3544,by=list(year = age_race_comp_seer$year), FUN=sum)%>%mutate(x = x/10000000)
pop35_44 <- ggplot(pop, aes(year,x)) +
  geom_line() +
  theme_classic() +
  xlab("year") +
  ylab("population of age 35-44 per 10000000") +
  labs(title="Female Population with Age 35-44")
(pop15_19 | pop20_34 | pop35_44)
### code for figure 1 ###


- Figure 2:

This figure shows the trends in birth rates by population subgroup. We used the dataset provided by the authors to reproduce figure1, which is named as “Births per 1,000 women age 15_44”. In the process, we simply take year as the x axis and brate_all as the y axis for the graph and plot it by ggplot line.

### code for figure 2 ###
ggplot(figs_1_2_3, aes(x=year, y = brate_all)) +
geom_line()+ 
theme_classic() +
labs(y = "Births per 1,000 women age 15-44", title = "Trend in US Birth Rates")
### code for figure 2 ###


- Figure 3, Panel A and B: 

We adopted the dataset "figs_2a_2b" provided by the authors to reproduce panels A and B in figure 3, which is named as "Five-year Age Group from 15 to 44" and "Race and Ethnicity from 15 to 44". To begin with, we renamed the variables into "year","15-19","20-24", "25-29", "30-34", "35-39", "40-44", "White_non_H", "Black_non_H", "Hispanic". Then, we collected a set of subgroups together using function gather to form a new dataframe and plotted the ggplot. In panel A, we gathered 6 age groups, including "15-19", "20-24", "25-29", "30-34", "35-39", and "40-44" from the dataset "figs_2a_2b". The rest variables in the dataset, including "White_non_H", "Black_non_H", and "Hispanic", are selected to a newly created dataframe for panel B. Finally, with dataframe ready for each panel, we draw their graph using ggplot.

### code for figure 3 panel A ###
colnames(figs_2a_2b)<- c("year","15-19","20-24", "25-29", "30-34","35-39","40-44","White_non_H", "Black_non_H","Hispanic")
dfa <- gather(figs_2a_2b, key = age_group, value = Births_per_1000_women, c("15-19","20-24", "25-29", "30-34","35-39","40-44"))
gfa <- ggplot(dfa, aes(x=year, y = Births_per_1000_women, group = age_group, colour = age_group)) +
  geom_line() + 
  geom_vline(xintercept = 2007,linetype="dashed") + 
  geom_text(aes(x=2009, label="2007", y=-5), colour="darkgreen", angle=0, vjust = 1.2, text=element_text(size=5)) + 
  theme_classic() +
  labs(y = "Births per 1,000 women in relevant population subgroup", title="Five-year age group")
### code for figure 3 panel A ###

### code for figure 3 panel B ###
dfb <-figs_2a_2b %>% 
select(year,White_non_H,Black_non_H, Hispanic)%>% 
filter(!is.na(White_non_H)) %>% 
gather(key = "race_group", value = "Births_per_1000_women", -year)
gfb<- ggplot(dfb, aes(x=year, y = Births_per_1000_women)) + 
  geom_line(aes(color = race_group)) + 
  scale_color_manual(values = c("darkred", "darkblue", "black")) + 
  geom_vline(xintercept = 2007,linetype="dashed") + 
  theme_classic() +
  geom_text(aes(x=2009, label="2007", y=4), colour="darkgreen", angle=0, vjust = 1.2, text=element_text(size=5)) +
  labs(y = "Births per 1,000 women in relevant population subgroup", title="Race and ethnicity (ages 15-44)")
### code for figure 3 panel B ###


- Figure 3, Panel C, D, and E:

We used the dataset "figs_1_2_3" provided by the authors to reproduce panel C, D, and E in figure 3, which is named as "Hispanic subpopulation from 15 to 44", "Mother’s level of education from 20 to 44", and "Marital status from 15 to 44". Firstly, we located the related four columns by index 20, 21, 22, 23 and renamed them as "Native_born_Mexican",  "Foreign_born_Mexican", "Native_born_non_Mexican", "Foreign_born_non_Mexican" for panel C; located columns by index 13, 14, 15, 16 and renamed them as “No_high_school_degree”, “high_school_degree”, “Some_college”, “College_graduate” for panel D; and located columns by index 17 and 18 and renamed them as “Unmarried” and “Married” for panel E. Then, we filtered out the missing value and selected columns by year. We gathered the value and rename the three columns as "year", "Hispanic", and "value". In the end, we created graphs for each panel by ggplot. 

### code for figure 3 panel C ###
names(figs_1_2_3)[20] <- "Native_born_Mexican"
names(figs_1_2_3)[21] <- "Foreign_born_Mexican"
names(figs_1_2_3)[22] <- "Native_born_non_Mexican"
names(figs_1_2_3)[23] <- "Foreign_born_non_Mexican"
dfc <-figs_1_2_3  %>% 
select(year, Native_born_Mexican, Foreign_born_Mexican, Native_born_non_Mexican, Foreign_born_non_Mexican)%>%
filter(!is.na(Native_born_Mexican)) %>%
  gather(key = " h", value = "Births_per_1000_women", -year)
colnames(dfc)<- c("year","Hispanic","Births_per_1000_women")
gfc <- dfc %>% 
ggplot( aes(x=year, y = Births_per_1000_women)) + 
  geom_line(aes(color =  Hispanic)) + 
  scale_color_manual(values = c("darkred", "darkblue", "black", "orange")) + 
  geom_vline(xintercept = 2007,linetype="dashed") + 
  geom_text(aes(x=2009, label="2007", y=5), colour="darkgreen", angle=0, vjust = 1.2, text=element_text(size=5)) +
  theme_classic() +
  labs(y = "Births per 1,000 women in relevant population subgroup", title="Hispanic subpopulation (ages 15-44)")
### code for figure 3 panel C ###

### code for figure 3 panel D ###
names(figs_1_2_3)[13] <- "No_high_school_degree"
names(figs_1_2_3)[14] <- "high_school_degree"
names(figs_1_2_3)[15] <- "Some_college"
names(figs_1_2_3)[16] <- "College_graduate"
dfd <-figs_1_2_3 %>% 
select(year, No_high_school_degree, high_school_degree, Some_college, College_graduate,) %>%
filter(!is.na(No_high_school_degree)) %>%
  gather(key = " e", value = "Births_per_1000_women", -year)
colnames(dfd)<- c("year","education","Births_per_1000_women")
gfd<- dfd %>% 
ggplot( aes(x=year, y = Births_per_1000_women)) + 
  geom_line(aes(color =  education)) + 
  scale_color_manual(values = c("darkred", "darkblue", "black", "orange")) + 
  geom_vline(xintercept = 2007,linetype="dashed") + 
  geom_text(aes(x=2009, label="2007", y=5), colour="darkgreen", angle=0, vjust = 1.2, text=element_text(size=5)) +
  theme_classic() +
  labs(y = "Births per 1,000 women in relevant population subgroup", title="Mother's level of education (ages 20-44)")
### code for figure 3 panel D ###

### code for figure 3 panel E ###
names(figs_1_2_3)[17] <- "Unmarried"
names(figs_1_2_3)[18] <- "Married"
dfe <-figs_1_2_3 %>% 
select(year, Unmarried,Married) %>%
  gather(key = " e", value = "Births_per_1000_women", -year)
colnames(dfe)<- c("year","marital_status","Births_per_1000_women")
gfe <- dfe %>% 
ggplot( aes(x=year, y = Births_per_1000_women)) + 
  geom_line(aes(color =  marital_status)) + 
  scale_color_manual(values = c("darkred", "darkblue")) + 
  geom_vline(xintercept = 2007,linetype="dashed") + 
  geom_text(aes(x=2009, label="2007", y=4), colour="darkgreen", angle=0, vjust = 1.2, text=element_text(size=5)) +
  theme_classic() +
  labs(y = "Births per 1,000 women in relevant population subgroup", title="Marital status (ages 15-44)")
### code for figure 3 panel E ###


- Figure 3, Panel F:

We used the datasets "age_race_comp_seer" and "nchs_births_pop_1990_2019" provided by the authors to reproduce panel F in figure 2, which is named as "Parity from ages 15 to 44". First, we merged these two datasets by "stname" (state name) and year. Then, we added up a new column called "pop2044", which is created by calculating the sum of “pop2024.x”, “pop2534.x” and “pop3544.x” using function mutate. Then, by function sum, we collapsed the “numbirth_first”, “numbirth_second”,  “numbirth_third”, “numbirth_fourth” and “pop1544”, “pop2044” by year. For each variable in “numbirth_first”, “numbirth_second”,  “numbirth_third”, “numbirth_fourth”, we generated a new value by calculating "numbirth" divided by "pop1544" multiplied 1000. Last but not least, we selected the new columns after the calculation and year. We renamed them into "year", "first_child", "second_child", "third_child", and "forth_child" by the same method. Insert this dataframe into ggplot and we got the panel F.

### code for figure 3 panel F ###
mergeddata <- merge(age_race_comp_seer,nchs_births_pop_1990_2019,by=c("stname", "year"))
mergeddata <- mergeddata[!(names(mergeddata) %in% "stname")]
collapseddata <- mergeddata  %>% mutate(pop2044 = pop2024.x + pop2534.x + pop3544.x)
collapseddata <- collapseddata %>% group_by(year) %>% summarise(numbirth_first=sum(numbirth_firstbirth),  numbirth_second = sum(numbirth_secondbirth), numbirth_thir= sum(numbirth_thirdbirth), numbirth_fourth= sum(numbirth_fourthbirth), pop1544= sum(pop1544), pop2044= sum(pop2044))
collapseddata  <- collapseddata %>% mutate(brate_first = numbirth_first/pop1544*1000,
                                           brate_second = numbirth_second/pop1544*1000,
                                           brate_third = numbirth_thir/pop1544*1000,
                                           brate_forth = numbirth_fourth/pop1544*1000,)
collapseddata <- collapseddata %>% select(year,brate_first,brate_second, brate_third, brate_forth)
colnames(collapseddata)<- c("year","first_child","second_child", "third_child", "forth_child")
dff <-collapseddata %>% select(year,first_child,second_child, third_child, forth_child)%>%
  gather(key = "number_of_children", value = "Births_per_1000_women", -year)
gff <-ggplot(dff, aes(x=year, y = Births_per_1000_women)) + 
  geom_line(aes(color = number_of_children)) + 
  scale_color_manual(values = c("darkred", "steelblue", "cornflowerblue", "black")) + 
  geom_vline(xintercept = 2007,linetype="dashed") + 
  geom_text(aes(x=2009, label="2007", y=1), colour="darkgreen", angle=0, vjust = 1.2, text=element_text(size=5)) +
  theme_classic() +
  labs(y = "Births per 1,000 women in relevant population subgroup", title="Parity (ages 15-44)")
### code for figure 3 panel F ###


- Figure 4:

Figure 4 is a state-level US map that indicates the changes in birth rates among women aged 15 to 44 between two five-year periods (2004-2008, 2015-2019) before and after the Great Recession. We plotted this figure using the datasets "nchs_births_pop_1990_2019" and "numbirth1544" provided by the authors. To begin with, we filtered out the missing value and observations outside the two five year period. We dropped the states with names of "AB","BC","MB","NB","NS","ON","QC","SK","XX" as well. Then, we selected useful variables from a large number of columns in each dataset to create a new one. For “state_births_1”, we collapsed the sum of “numbirth1544” and grouped it by the data under "stname" (state name) and "year". A similar procedure is adopted for “state_births_2”, except to summarize the “pop1544” instead. Then we merged them into one dataframe by "stname"and "year". We separated the data into two periods and called it "year2". The first period is from 2004 to 2008, which was named as “2004"; the second period is from 2015 to 2019, which was named as "2019". After this, we summarised the sum of “numbirth1544” and “pop1544”, grouped by “stname”,”year2”. The rate of birth among women aged 15 to 44 is calculated by dividing the number of the new birth ("numbirth1544") by the number of existing populations ("pop1544") multiplied by one thousand, and named it as “brate1544_thsnds”. we created two dataframes to separate observations of the year "2014" and year "2019" and kept only state name ("state"), year ("year"), and corresponding birth rate ("brate_bef" for 2004 and "brate_aft" for 2019) in the dataframes. Using function merge, we converted these two dataframes into one final dataframe. We added up a new column to record the changes in birth rates between two periods by function mutate and called it "diff". we made a plot of 50 states in the US with percentage change of birth rate and named as "Change in Birth Rates by State between 2004-2008 and 2015-2019". Specifically, the higher a state's fertility rate, the redder it is plotted. The color approaches white when the birth rate reaches negative twenty percentage points and approaches bright red when the birth rate is turned to be positive. The labels are zoomed out into size 3 to avoid the messy looking on the east coast.

### code for figure 4 ###
state_births_1 <- nchs_births_pop_1990_2019 %>% filter((year>=2004 & year<=2008) |(year>=2015 & year<=2019)& stname!= "AB" & stname!= "BC" & stname!= "MB" & stname != "NB" & stname!= "NS" & stname != "ON" & stname != "QC" & stname!= "SK" & stname!= "XX") %>% group_by(stname,year) %>% summarise(numbirth1544=sum(numbirth1544))
state_births_2 <- age_race_comp_seer %>% filter((year>=2004 & year<=2008) |(year>=2015 & year<=2019)) %>% mutate(pop1544 = pop1519 + pop2034 + pop3544)%>% group_by(stname,year) %>% summarise(pop1544=sum(pop1544))
state_births_fin <- merge(state_births_1,state_births_2,by=c("stname", "year"))
state_births_fin <- state_births_fin %>% mutate(year2 = case_when( year>=2004 & year<=2008 ~ "2004", 
                                              year>=2015 & year<=2019 ~ "2019"))%>% group_by(stname,year2) %>% summarise(numbirth1544=sum(numbirth1544), pop1544=sum(pop1544))
state_births_fin<- state_births_fin %>% mutate(brate1544_thsnds = numbirth1544/pop1544*1000)
state_births_fin1<- state_births_fin %>% filter(year2 == 2004) %>% select(stname, year2, brate1544_thsnds)
colnames(state_births_fin1)<- c("state","year", "brate_bef")
state_births_fin2<- state_births_fin %>% filter(year2 == 2019)%>% select(stname, year2, brate1544_thsnds)
colnames(state_births_fin2)<- c("state","year", "brate_aft")
state_births_finn <- merge(state_births_fin1,state_births_fin2,by=c("state"))
state_births_finn <- state_births_finn %>%mutate(diff = brate_aft -brate_bef)
p <- plot_usmap(data = state_births_finn, values = "diff", color = "red", labels=TRUE) + 
  scale_fill_continuous(low = "white", high = "red", 
                         name = "percent change of birth rate", label = scales::comma) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Change in Birth Rates by State between 2004-2008 and 2015-2019")
p$layers[[2]]$aes_params$size <- 3
print(p)
### code for figure 4 ###













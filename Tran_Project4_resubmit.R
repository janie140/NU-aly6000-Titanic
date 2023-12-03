### clear Stuff
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)
library(dplyr)
p_load(ggplot2)

df <- read_csv("Titanic-Dataset.csv")

df <- df |> select(-PassengerId, -Name, -Ticket, -Cabin) |> na.omit()

df <- mutate(df, Age = round(Age, digits = 0),
             Fare = round(Fare, digits = 1),
             Survived = as.character(Survived),
             Age = as.numeric(Age),
             Pclass = as.character(Pclass),
             SibSp = as.character(SibSp),
             Parch = as.character(Parch))

#the distribution of age in the Titanic passengers vs. the mean of age
ggplot(df, aes(x = Age, fill = Survived)) + 
  geom_histogram(stat = "count") + ylim(c(0,35)) +
  xlab("Age") +  ggtitle("Age count") +
  geom_vline(aes(xintercept = mean(df$Age)),
             color="blue", linetype="dashed", size=1) +
  theme_classic() 

#break age groups
df <- df |> mutate(gr_by_age = (cut(df$Age,
                                breaks = c(-Inf, 15, 30, 50, 70, Inf),
                        labels = c("<15","15-30", "30-50", "50-70", ">70"))))

age_gr <- df |> group_by(gr_by_age) |> summarise(count = n())

age_gr <- age_gr |> mutate(percent = round(count/sum(count)*100, digits = 1))

#sv filter
df_sv <- df |> filter(Survived == 1)

sv_age_gr <- df_sv |> group_by(gr_by_age) |> summarise(count_sv = n())

sv_age_gr <- sv_age_gr |> mutate(sv_pct = round(count_sv/sum(count_sv)*100, digits = 1))

sv_age_gr <- full_join(age_gr, sv_age_gr)

write.csv(sv_age_gr, "~/Documents/sv_age_gr.csv")

#percent of total passengers vs percent of survivors by age groups
ggplot(sv_age_gr, aes(x = gr_by_age))+
  geom_col(aes(y = percent, fill = gr_by_age)) + labs(x = "Age Group",
  title = "Percent of total passengers by age group vs. Percent of survivors by age group") +
  geom_point(aes(y = sv_pct), size = 1.5, color = "red") +
  geom_line(aes(y = sv_pct), group = 1, color = "red") +
  scale_fill_discrete(name = "Age group")
  theme_classic()
  
#men filter
sv_men <- df_sv |> filter(Sex == "male")

cbbPalette <- c("#56B4E9", "#F0E442", "#CC79A7")
ggplot(sv_men, aes(x = Pclass, fill = Pclass)) +
  geom_histogram(stat = "count") + ylim(c(0,50)) +
  ggtitle("Male survivors count by Ticket Class") +
  scale_fill_manual(values=cbbPalette) + theme_classic() 

#gender count
ggplot(df, aes(x = Sex, fill = Survived)) +
 geom_histogram(stat = "count") + ylim(c(0,500)) +
  ggtitle("Gender count") + theme_classic() 

#Pclass count
ggplot(df, aes(x = Pclass, fill = Survived)) +
  geom_histogram(stat = "count") + ylim(c(0,400)) +
  ggtitle("Pclass count") + theme_classic() 

cbbPalette <- c("#CC79A7", "#56B4E9")
ggplot(df, aes(x = Pclass, fill = Sex)) +
  geom_histogram(stat = "count") + ylim(c(0,400)) +
  ggtitle("Pclass count") + theme_classic() +
  scale_fill_manual(values=cbbPalette)

#port, fare, sv
ggplot(df, aes(x = Embarked, y = Fare, color = Survived)) +
  geom_point() +  scale_color_discrete(name = "Survived") +
  ggtitle("Emparked Port & Fare & Survivors")

#Pclass, fare, sv
ggplot(df, aes(x = Pclass, y = Fare, color = Survived)) +
  geom_point() +  scale_color_discrete(name = "Survived") + 
  ggtitle("Pclass & Fare & Survivors")

#port, fare, Pclass
ggplot(df, aes(x = Embarked, y = Fare, color = Pclass)) +
  geom_point() +  scale_color_discrete(name = "Pclass") +
  ggtitle("Emparked Port & Fare & Pclass")

#SibSp
ggplot(df, aes(x = SibSp, fill = Survived)) +
  geom_histogram(stat = "count") + ylim(c(0,500)) +
  xlab("Number of siblings/spouses aboard")+
  ggtitle("Distribution of passengers by the number of siblings/spouses aboard the Titanic")+
  theme_classic() 

#Parch
ggplot(df, aes(x = Parch, fill = Survived)) +
  geom_histogram(stat = "count") + ylim(c(0,600)) +
  xlab("Number of parents/children aboard")+
  ggtitle("Distribution of passengers by the number of parents/children aboard the Titanic") +
  theme_classic() 






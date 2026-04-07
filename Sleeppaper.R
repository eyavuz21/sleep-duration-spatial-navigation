#install relevant packages

#install.packages("openxlsx"); 
#install.packages("readxl"); 
#install.packages("data.table"); 
#install.packages("tidyr"); 
#install.packages("tidyverse"); 
#install.packages("zoo"); 
#install.packages("purrr"); 
#install.packages("car"); 
#install.packages("readr");
#install.packages("lm.beta");
#install.packages("ggplot2")

library(car);library(openxlsx);library(ggplot2);library(readr);library(lm.beta);library(readxl); library(data.table); library(tidyr); library(tidyverse); library(zoo); library(dplyr);library(purrr);library(car);library(ppcor)

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv")
correcteddf$gender[correcteddf$gender==1]='Male'
correcteddf$gender[correcteddf$gender==2]='Female'

#summary tables of demographic variables

correcteddfsummaryalpar <- data.frame(correcteddf$distance,correcteddf$age,correcteddf$gender,correcteddf$hours_of_phone_use_per_week,correcteddf$video_game_all_devices_hours_per_week,correcteddf$BMI,correcteddf$drink_alcohol_yes_noscale,correcteddf$drink_caffeine_yes_noscale,correcteddf$sunlight,correcteddf$physical_activity,correcteddf$smoking,correcteddf$education)
colnames(correcteddfsummaryalpar) <- c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week","BMI","drink_alcohol_yes_noscale","drink_caffeine_yes_noscale","sunlight","physical_activity","smoking","education")

correcteddfsummaryfemalealpar <- correcteddfsummaryalpar[correcteddfsummaryalpar$gender=='female',]
tab2female <- CreateTableOne(vars = c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week","BMI","drink_alcohol_yes_noscale","drink_caffeine_yes_noscale","sunlight","physical_activity","smoking","education"), data = correcteddfsummaryfemalealpar, factorVars = c("gender","smoking","physical_activity","education"))
tab2female <- print(tab2female,printToggle=FALSE)
write.csv(tab2female,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryfemaleALPAR.csv")

correcteddfsummarymalealpar <- correcteddfsummaryalpar[correcteddfsummaryalpar$gender=='male',]
tab2male <- CreateTableOne(vars = c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week","BMI","drink_alcohol_yes_noscale","drink_caffeine_yes_noscale","sunlight","physical_activity","smoking","education"), data = correcteddfsummarymalealpar, factorVars = c("gender","smoking","physical_activity","education"))
tab2male <- print(tab2male,printToggle=FALSE)
write.csv(tab2male,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summarymaleALPAR.csv")

#summary of sleep patterns for each gender

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddfsleep <- correcteddf[c('gender','difficultywakeup','hours_of_sleep','time_spent_awake_during_night_mins','sleepiness_resolution_index','sleepiness_waking','sleep_quality','nap_duration_mins','number_of_times_wakeup_during_night','time_in_bed_hours','sleepiness_bed')]
correcteddfsleepfemale <- correcteddfsleep[correcteddfsleep$gender=='female',]
correcteddfsleepmale <- correcteddfsleep[correcteddfsleep$gender=='male',]
correcteddfsleepfemale <- correcteddfsleepfemale[, -c(1)]
correcteddfsleepmale <- correcteddfsleepmale[, -c(1)]

correcteddfsleepmale$difficultywakeup <- as.numeric(correcteddfsleepmale$difficultywakeup)
correcteddfsleepmale$hours_of_sleep <- as.numeric(correcteddfsleepmale$hours_of_sleep)
correcteddfsleepmale$time_spent_awake_during_night_mins <- as.numeric(correcteddfsleepmale$time_spent_awake_during_night_mins)
correcteddfsleepmale$sleepiness_resolution_index <- as.numeric(correcteddfsleepmale$sleepiness_resolution_index)
correcteddfsleepmale$sleepiness_waking <- as.numeric(correcteddfsleepmale$sleepiness_waking)
correcteddfsleepmale$sleep_quality <- as.numeric(correcteddfsleepmale$sleep_quality)
correcteddfsleepmale$nap_duration_mins <- as.numeric(correcteddfsleepmale$nap_duration_mins)
correcteddfsleepmale$number_of_times_wakeup_during_night <- as.numeric(correcteddfsleepmale$number_of_times_wakeup_during_night)
correcteddfsleepmale$time_in_bed_hours <- as.numeric(correcteddfsleepmale$time_in_bed_hours)
correcteddfsleepmale$sleepiness_bed <- as.numeric(correcteddfsleepmale$sleepiness_bed)

correcteddfsleepfemale$difficultywakeup <- as.numeric(correcteddfsleepfemale$difficultywakeup)
correcteddfsleepfemale$hours_of_sleep <- as.numeric(correcteddfsleepfemale$hours_of_sleep)
correcteddfsleepfemale$time_spent_awake_during_night_mins <- as.numeric(correcteddfsleepfemale$time_spent_awake_during_night_mins)
correcteddfsleepfemale$sleepiness_resolution_index <- as.numeric(correcteddfsleepfemale$sleepiness_resolution_index)
correcteddfsleepfemale$sleepiness_waking <- as.numeric(correcteddfsleepfemale$sleepiness_waking)
correcteddfsleepfemale$sleep_quality <- as.numeric(correcteddfsleepfemale$sleep_quality)
correcteddfsleepfemale$nap_duration_mins <- as.numeric(correcteddfsleepfemale$nap_duration_mins)
correcteddfsleepfemale$number_of_times_wakeup_during_night <- as.numeric(correcteddfsleepfemale$number_of_times_wakeup_during_night)
correcteddfsleepfemale$time_in_bed_hours <- as.numeric(correcteddfsleepfemale$time_in_bed_hours)
correcteddfsleepfemale$sleepiness_bed <- as.numeric(correcteddfsleepfemale$sleepiness_bed)

meanmale <- aggregate(. ~ 1, data = correcteddfsleepmale, FUN = mean)
stdmale <- aggregate(. ~ 1, data = correcteddfsleepmale, FUN = sd)
meanfemale <- aggregate(. ~ 1, data = correcteddfsleepfemale, FUN = mean)
stdfemale <- aggregate(. ~ 1, data = correcteddfsleepfemale, FUN = sd)
var_names <- names(stdfemale)[-1] 
df_male <- data.frame(Variable = character(), Male = character(), stringsAsFactors = FALSE)
df_female <- data.frame(Variable = character(), Female = character(), stringsAsFactors = FALSE)
t_values <- c()
p_values <- c()
Variable <- c()
all <- c()

for (var in var_names) {
  meanvaluefemale <- round(meanfemale[[var]][1], 2)
  stdvaluefemale <- round(stdfemale[[var]][1], 2)
  meanvaluemale <- round(meanmale[[var]][1], 2)
  stdvaluemale <- round(stdmale[[var]][1], 2)
  row_female <- data.frame(Variable = var, Female = paste(meanvaluefemale, "±", stdvaluefemale))
  df_female <- rbind(df_female, row_female)
  row_male <- data.frame(Variable = var, Male = paste(meanvaluemale, "±", stdvaluemale))
  df_male <- rbind(df_male, row_male)
  
  # Perform t-test
  t_result <- t.test(correcteddfsleepfemale[[var]], correcteddfsleepmale[[var]])
  t_value <- round(t_result$statistic, 2)
  p_value <- round(t_result$p.value, 3)
  Variableinterest <- var 
  Variable <- c(Variable,Variableinterest)
  t_values <- c(t_values, t_value)
  p_values <- c(p_values, p_value)
}

merged_df <- merge(df_female, df_male, by = "Variable", all = TRUE)
combined_frame <- data.frame(Variable = Variable, t_values = t_values, p_values = p_values)
write.csv(combined_frame, "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleeppattALPAR.csv", row.names = FALSE)
write.csv(merged_df, "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleeppattALPARVALUES.csv", row.names = FALSE)

#chi-squared analysis for frequency of alarm use in men and women 

table <- table(correcteddf$gender, correcteddf$frequency_of_alarm_usage)
result <- chisq.test(table)
chi_squared <- result$statistic
df <- result$parameter
p_value <- result$p.value
library(vcd)
cramers_v <- assocstats(table)$cramer
test_stats_df <- data.frame(Statistic = chi_squared,
                            DF = df,
                            P_Value = p_value,
                            Cramers_V = cramers_v)
write.csv(test_stats_df, "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleeppattALPARfrequencyalarmCHIsq.csv", row.names = FALSE)

female_proportions <- correcteddf[correcteddf$gender == "female", ]$frequency_of_alarm_usage
female_proportions <- prop.table(table(female_proportions))*100
male_proportions <- correcteddf[correcteddf$gender == "male", ]$frequency_of_alarm_usage
male_proportions <- prop.table(table(male_proportions))*100
proportions_df <- data.frame(Frequency = c("Never", "Rarely", "Often", "Sometimes", "Always"),
                             Female = female_proportions,
                             Male = male_proportions)[, c(1, 3, 5)]

proportions_df$Female.Freq <- round(proportions_df$Female.Freq,1)
proportions_df$Male.Freq <- round(proportions_df$Male.Freq,1)
write.csv(proportions_df, "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleeppattALPARfrequencyalarm.csv", row.names = FALSE)

#correlations for each variable with BMI 

variable_names <- c("age", "hours_of_sleep", "hours_of_phone_use_per_week", "video_game_all_devices_hours_per_week")

listcorr <- numeric(length(variable_names))
listp <- numeric(length(variable_names))
CI_lower <- numeric(length(variable_names))
CI_upper <- numeric(length(variable_names))

for (i in seq_along(variable_names)) {
  output <- cor.test(correcteddf$BMI, correcteddf[[variable_names[i]]], 
                     method = "spearman")
  
  correlation <- output$estimate
  listcorr[i] <- correlation
  pvalue <- output$p.value
  listp[i] <- pvalue
}

alpha <- 0.05/length(variable_names)
zvalue <- qnorm(1 - alpha/2)
listcorr <- as.numeric(listcorr)
listcorrSD <- sd(listcorr)
CI_lower <- numeric(length(listcorr))
CI_upper <- numeric(length(listcorr)) 
for (i in seq_along(listcorr)) {
  CI_lower[[i]] <- listcorr[i] - zvalue*sqrt((1 - listcorr[i]**2) / (nrow(correcteddf) - 2))  
  CI_upper[[i]] <- listcorr[i] + zvalue*sqrt((1 - listcorr[i]**2) / (nrow(correcteddf) - 2))  
}
CI_lower <- as.numeric(CI_lower)
CI_upper <- as.numeric(CI_upper)
CI <- paste0("[", round(CI_lower, 2), ", ", round(CI_upper, 2), "]")
listcorr <- matrix(listcorr, nrow = 1)
listp <- matrix(listp, nrow = 1)
new <- data.frame(variable_names, r = as.numeric(listcorr), p = as.numeric(listp),CI_upper,CI_lower,CI)
new$absolute <- abs(as.numeric(new$r))
new <- new[order(-new$absolute), ]

write.csv(results_df, '/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/spearman_corr.csv', row.names = FALSE)

#partial correlations - preparing variables 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf$difficultywakeup <- as.numeric(correcteddf$difficultywakeup)
correcteddf$frequency_of_alarm_usage <- as.numeric(correcteddf$frequency_of_alarm_usage)
correcteddf$frequency_of_naps <- as.numeric(correcteddf$frequency_of_naps)
correcteddf$hours_of_sleep <- as.numeric(correcteddf$hours_of_sleep)
correcteddf$number_of_times_wakeup_during_night <- as.numeric(correcteddf$number_of_times_wakeup_during_night)
correcteddf$time_spent_awake_during_night_mins <- as.numeric(correcteddf$time_spent_awake_during_night_mins)
correcteddf$gender[correcteddf$gender=='male']=1
correcteddf$gender[correcteddf$gender=='female']=2
correcteddf$gender <- as.numeric(correcteddf$gender)
correcteddf$nap_duration_mins <- as.numeric(correcteddf$nap_duration_mins)
correcteddf$sleep_quality <- as.numeric(correcteddf$sleep_quality)
correcteddf$sleepiness_waking <- as.numeric(correcteddf$sleepiness_waking)
correcteddf$sleepiness_bed <- as.numeric(correcteddf$sleepiness_bed)
correcteddf$time_in_bed_hours <- as.numeric(correcteddf$time_in_bed_hours)
correcteddf$sleepiness_resolution_index <- as.numeric(correcteddf$sleepiness_resolution_index)
correcteddf$sunlight <- as.numeric(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.numeric(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- as.numeric(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- as.numeric(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$use_alarms_yes_no <- as.factor(correcteddf$use_alarms_yes_no)
correcteddf$takes_naps_yes_no <- as.factor(correcteddf$takes_naps_yes_no)

#partial correlation for both genders sleep 

names = c('frequency_of_alarm_usage','frequency_of_naps','difficultywakeup','hours_of_sleep','number_of_times_wakeup_during_night','time_spent_awake_during_night_mins','nap_duration_mins','sleep_quality','sleepiness_waking','sleepiness_bed','time_in_bed_hours','sleepiness_resolution_index')
listcorr <- list()
listp <- list()
j = 1 

correcteddf$BMI <- as.numeric(correcteddf$BMI)
correcteddf$sunlight <- as.numeric(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.numeric(correcteddf$`physical_activity-quantised`)
correcteddf$`smoking-quantised`<- as.numeric(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- as.numeric(correcteddf$drink_alcohol_yes_noscale)
correcteddf$drink_caffeine_yes_noscale <- as.numeric(correcteddf$drink_caffeine_yes_noscale)
correcteddf$education_coded <- as.numeric(correcteddf$education_coded)
  
for (i in names){
  output <- pcor.test(correcteddf[,i],correcteddf$distance,list(correcteddf$age,correcteddf$gender,correcteddf$BMI,correcteddf$sunlight,correcteddf$drink_alcohol_yes_noscale,correcteddf$drink_caffeine_yes_noscale,correcteddf$`smoking-quantised`,correcteddf$`physical_activity-quantised`,correcteddf$education_coded),method = c("spearman"))
  correlation = output[1,1]
  listcorr[j] = correlation
  pvalue = output[1,2]
  listp[j] = pvalue 
  j = j + 1
}

alpha <- 0.05/length(names)
zvalue <- qnorm(1 - alpha/2)
listcorr <- as.numeric(listcorr)
listcorrSD <- sd(listcorr)
CI_lower <- numeric(length(listcorr))
CI_upper <- numeric(length(listcorr)) 
for (i in seq_along(listcorr)) {
  CI_lower[[i]] <- listcorr[i] - zvalue*sqrt((1 - listcorr[i]**2) / (nrow(correcteddf) - 2))  
  CI_upper[[i]] <- listcorr[i] + zvalue*sqrt((1 - listcorr[i]**2) / (nrow(correcteddf) - 2))  
}
CI_lower <- as.numeric(CI_lower)
CI_upper <- as.numeric(CI_upper)
CI <- paste0("[", round(CI_lower, 2), ", ", round(CI_upper, 2), "]")
listcorr <- matrix(listcorr, nrow = 1)
listp <- matrix(listp, nrow = 1)
new <- data.frame(names, r = as.numeric(listcorr), p = as.numeric(listp),CI,CI_upper,CI_lower)
new$absolute <- abs(as.numeric(new$r))
new <- new[order(-new$absolute), ]
write.csv(new,'/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/partialcorr.csv')

#do for each gender separately 

correcteddf_male <- subset(correcteddf, gender == 1)  
correcteddf_female <- subset(correcteddf, gender == 2)  
names_to_analyze <- c('frequency_of_alarm_usage', 'frequency_of_naps', 'difficultywakeup', 'hours_of_sleep', 'number_of_times_wakeup_during_night', 'time_spent_awake_during_night_mins', 'nap_duration_mins', 'sleep_quality', 'sleepiness_waking', 'sleepiness_bed', 'time_in_bed_hours', 'sleepiness_resolution_index')

listcorr_male <- list()
listp_male <- list()
listcorr_female <- list()
listp_female <- list()

for (var in names_to_analyze){
  output_male <- pcor.test(correcteddf_male[[var]], correcteddf_male$distance, 
                           list(correcteddf_male$age, correcteddf_male$BMI), method = "spearman")
  correlation_male <- output_male$estimate
  pvalue_male <- output_male$p.value
  listcorr_male[var] <- correlation_male
  listp_male[var] <- pvalue_male
  
  output_female <- pcor.test(correcteddf_female[[var]], correcteddf_female$distance, 
                             list(correcteddf_female$age, correcteddf_female$BMI), method = "spearman")
  correlation_female <- output_female$estimate
  pvalue_female <- output_female$p.value
  listcorr_female[var] <- correlation_female
  listp_female[var] <- pvalue_female
}

results_male <- data.frame(names_to_analyze, r = unlist(listcorr_male), p = unlist(listp_male))
results_female <- data.frame(names_to_analyze, r = unlist(listcorr_female), p = unlist(listp_female))
results_male$absolute <- abs(as.numeric(results_male$r))
results_male <- results_male[order(-results_male$absolute), ]
results_female$absolute <- abs(as.numeric(results_female$r))
results_female <- results_female[order(-results_female$absolute), ]
results_male <- results_male[, !(names(results_male) %in% c("names_to_analyze", "absolute"))]
results_female <- results_female[, !(names(results_female) %in% c("names_to_analyze", "absolute"))]
results_male$r <- format(round(results_male$r, 2), nsmall = 2)
results_male$p <- format(round(results_male$p, 3), nsmall = 3)
results_female$r <- format(round(results_female$r, 2), nsmall = 2)
results_female$p <- format(round(results_female$p, 3), nsmall = 3)
write.csv(results_male, '/Users/macbook/Desktop/partialcorr_maleALPAR.csv')
write.csv(results_female, '/Users/macbook/Desktop/partialcorr_femaleALPAR.csv')

#forest plot for both genders sleep data  

correcteddfforest <- read.csv('/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/partialcorr.csv')

correcteddfforest$names[correcteddfforest$names == 'difficultywakeup'] <- 'Difficulty waking up' 
correcteddfforest$names[correcteddfforest$names == 'sleepiness_waking'] <- 'Sleep inertia'
correcteddfforest$names[correcteddfforest$names == 'sleepiness_resolution_index'] <- 'Sleepiness resolution index'
correcteddfforest$names[correcteddfforest$names == 'time_spent_awake_during_night_mins'] <- 'Time spent awake during the night (mins)'
correcteddfforest$names[correcteddfforest$names == 'number_of_times_wakeup_during_night'] <- 'Number of times one wakes up during the night'
correcteddfforest$names[correcteddfforest$names == 'sleep_quality'] <- 'Sleep quality'
correcteddfforest$names[correcteddfforest$names == 'frequency_of_naps'] <- 'Frequency of naps'
correcteddfforest$names[correcteddfforest$names == 'hours_of_sleep'] <- 'Sleep duration (hours)'
correcteddfforest$names[correcteddfforest$names == 'frequency_of_alarm_usage'] <- 'Frequency of alarm use'
correcteddfforest$names[correcteddfforest$names == 'time_in_bed_hours'] <- 'Time in bed (hours)'
correcteddfforest$names[correcteddfforest$names == 'nap_duration_mins'] <- 'Nap duration (mins)'
correcteddfforest$names[correcteddfforest$names == 'sleepiness_bed'] <- 'Sleepiness on going to bed'

desired_order <- c(
  'Sleep inertia',
  'Sleepiness resolution index',
  'Time spent awake during the night (mins)',
  'Number of times one wakes up during the night',
  'Sleep quality',
  'Frequency of naps',
  'Sleep duration (hours)',
  'Frequency of alarm use',
  'Time in bed (hours)',
  'Nap duration (mins)',
  'Sleepiness on going to bed',
  'Difficulty waking up'
)
correcteddfforest$names <- factor(correcteddfforest$names, levels = desired_order)

ggplot(data=correcteddfforest, aes(y=names, x=r, xmin=CI_lower, xmax=CI_upper), las=2) +
  geom_point() + geom_errorbarh(height=.1) + xlim(-0.4,0.4) +
  labs(x="Spearman's correlation coefficient (r)", y = "Sleep variable") +
  geom_vline(xintercept=0, color='black', linetype='dashed', size=0.5) +
  theme(axis.text.x=element_text(size=30, margin=margin(b=100)),
        axis.text.y=element_text(size=30),
        axis.title.x=element_text(size=30, face='bold', vjust=0, margin=margin(t=200)),
        axis.title.y=element_text(size=30, face='bold', vjust=0)) +
  theme_classic()

ggsave(file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleepassall.png", width=8, height=6)

#Main linear model 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf$gender_female <- as.factor(correcteddf$gender_female)
correcteddf$gender_male <- as.factor(correcteddf$gender_male)
correcteddf$gender <- as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)
correcteddf$frequency_of_alarm_usage <- as.factor(correcteddf$frequency_of_alarm_usage) 

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducation.csv")
sleepoutputall <- lm(zscore ~ age + gender*difficultywakeup + gender*hours_of_sleep + gender*sleepiness_waking + gender*sleep_quality + gender*time_spent_awake_during_night_mins + gender*sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATION.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCI.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducation.csv", row.names=FALSE)

#Main linear model with quadratic term 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf$gender_female <- as.factor(correcteddf$gender_female)
correcteddf$gender_male <- as.factor(correcteddf$gender_male)
correcteddf$gender <- as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationquadratic.csv")
sleepoutputall <- lm(zscore ~ age + gender*difficultywakeup + gender*I(hours_of_sleep^2) + gender*sleepiness_waking + gender*sleep_quality + gender*time_spent_awake_during_night_mins + gender*sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATIONquadratic.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCIquadratic.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducationquadratic.csv", row.names=FALSE)

#Sleep duration restricted hours 6-9

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENTRESTRICTED.csv')
correcteddf$gender_female <- as.factor(correcteddf$gender_female)
correcteddf$gender_male <- as.factor(correcteddf$gender_male)
correcteddf$gender <- as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationRESTRICTED.csv")
sleepoutputall <- lm(zscore ~ age + gender*difficultywakeup + gender*hours_of_sleep + gender*sleepiness_waking + gender*sleep_quality + gender*time_spent_awake_during_night_mins + gender*sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATIONRESTRICTED.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCIRESTRICTED.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducationRESTRICTED.csv", row.names=FALSE)

#Sleep duration men only 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf <- correcteddf[correcteddf$gender=='male',]
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationMEN.csv")
sleepoutputall <- lm(zscore ~ age + difficultywakeup + hours_of_sleep + sleepiness_waking + sleep_quality + time_spent_awake_during_night_mins + sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATIONMEN.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCIMEN.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducationMEN.csv", row.names=FALSE)

#Sleep duration women only 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf <- correcteddf[correcteddf$gender=='female',]
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationWOMEN.csv")
sleepoutputall <- lm(zscore ~ age + difficultywakeup + hours_of_sleep + sleepiness_waking + sleep_quality + time_spent_awake_during_night_mins + sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATIONWOMEN.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCIWOMEN.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducationWOMEN.csv", row.names=FALSE)

#Main linear model with phone use interaction 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf$gender_female <- as.factor(correcteddf$gender_female)
correcteddf$gender_male <- as.factor(correcteddf$gender_male)
correcteddf$gender <- as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)
correcteddf$frequency_of_alarm_usage <- as.factor(correcteddf$frequency_of_alarm_usage) 

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationPHONE.csv")
sleepoutputall <- lm(zscore ~ age + gender*difficultywakeup + gender*hours_of_sleep + gender*sleepiness_waking + gender*sleep_quality + gender*time_spent_awake_during_night_mins + gender*sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_sleep*hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATIONPHONE.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCIPHONE.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducationPHONE.csv", row.names=FALSE)

#Main linear model with randomly removed 61 rows

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
set.seed(123)
total_rows <- nrow(correcteddf)
rows_to_remove <- sample(total_rows, 61)
correcteddf <- correcteddf[-c(rows_to_remove), ]
correcteddf$gender_female <- as.factor(correcteddf$gender_female)
correcteddf$gender_male <- as.factor(correcteddf$gender_male)
correcteddf$gender <- as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week <- scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$difficultywakeup <- scale(correcteddf$difficultywakeup)
correcteddf$hours_of_sleep <- scale(correcteddf$hours_of_sleep)
correcteddf$time_spent_awake_during_night_mins <- scale(correcteddf$time_spent_awake_during_night_mins)
correcteddf$sleepiness_resolution_index <- scale(correcteddf$sleepiness_resolution_index)
correcteddf$sleepiness_waking <- scale(correcteddf$sleepiness_waking)
correcteddf$sleep_quality <- scale(correcteddf$sleep_quality)
correcteddf$sunlight <- scale(correcteddf$sunlight)
correcteddf$`physical_activity-quantised` <- as.factor(correcteddf$`physical_activity-quantised`)
correcteddf$drink_caffeine_yes_noscale <- scale(correcteddf$drink_caffeine_yes_noscale)
correcteddf$`smoking-quantised` <- as.factor(correcteddf$`smoking-quantised`)
correcteddf$drink_alcohol_yes_noscale <- scale(correcteddf$drink_alcohol_yes_noscale)
correcteddf$BMI <- scale(correcteddf$BMI)
correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$smokingquantisedcoded <- ifelse(correcteddf$smoking %in% c("never smoked"), 0, 1)
correcteddf$physicalactivitycoded <- ifelse(correcteddf$physical_activity %in% c("never","rarely"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducation.csv")
sleepoutputall <- lm(zscore ~ age + gender*difficultywakeup + gender*hours_of_sleep + gender*sleepiness_waking + gender*sleep_quality + gender*time_spent_awake_during_night_mins + gender*sleepiness_resolution_index + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + sunlight + education_coded + BMI + drink_alcohol_yes_noscale + drink_caffeine_yes_noscale + smokingquantisedcoded + physicalactivitycoded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/ALPARCF2EDUCATION.csv", row.names=FALSE)

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelALPAReducationCI.csv",row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=3)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFALPAReducation.csv", row.names=FALSE)

#strength of association between correlations: sleep duration - navigation (original sample)

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
if (!require(cocor)) {
  install.packages("cocor")
  library(cocor)
}
correcteddfmale <- subset(correcteddf, gender == 'male')
correcteddffemale <- subset(correcteddf, gender == 'female')
cor_male <- cor(correcteddfmale$zscore, correcteddfmale$hours_of_sleep)
cor_female <- cor(correcteddffemale$zscore, correcteddffemale$hours_of_sleep)
result <- cocor.indep.groups(cor_male, cor_female, length(correcteddfmale$zscore), length(correcteddffemale$zscore))

#strength of association between correlations: sleep duration - navigation (6-9 sample)

correcteddfrestrict69 <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENTRESTRICTED.csv')
correcteddfmale <- subset(correcteddfrestrict69, gender == 'male')
correcteddffemale <- subset(correcteddfrestrict69, gender == 'female')
cor_male <- cor(correcteddfmale$zscore, correcteddfmale$hours_of_sleep)
cor_female <- cor(correcteddffemale$zscore, correcteddffemale$hours_of_sleep)
result <- cocor.indep.groups(cor_male, cor_female, length(correcteddfmale$zscore), length(correcteddffemale$zscore))

#strength of association between correlations: sleep duration - navigation (7-9 sample)

correcteddfrestrict79 <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENTRESTRICTED79.csv')
correcteddfmale <- subset(correcteddfrestrict79, gender == 'male')
correcteddffemale <- subset(correcteddfrestrict79, gender == 'female')
cor_male <- cor(correcteddfmale$zscore, correcteddfmale$hours_of_sleep)
cor_female <- cor(correcteddffemale$zscore, correcteddffemale$hours_of_sleep)
result <- cocor.indep.groups(cor_male, cor_female, length(correcteddfmale$zscore), length(correcteddffemale$zscore))

#visualisations 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/ALPARCORRECTEDDFMOSTRECENT.csv')
correcteddf$gender[correcteddf$gender=='female']='Female'
correcteddf$gender[correcteddf$gender=='male']='Male'

#sleep duration

correcteddf$hours_of_sleep <- as.numeric(correcteddf$hours_of_sleep)
correcteddf$hours_of_sleep[correcteddf$hours_of_sleep>=9] = '>=9'
correcteddf$hours_of_sleep[correcteddf$hours_of_sleep==6] = '<=6'
correcteddf$hours_of_sleep[correcteddf$hours_of_sleep==5] = '<=6'

durationSE <- correcteddf %>%
  group_by(hours_of_sleep,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    max=max(distance),
    min=min(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(durationSE)[1] <- "Answer"
durationSE <- as.data.frame(durationSE)
durationSE$Answer <- factor(durationSE$Answer, levels = c('<=6', '7', '8','>=9'))
p = ggplot(durationSE, aes(x=Answer, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) +  ylim(8,15) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + labs(x = "Hours sleep duration") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)))
unique_durations <- unique(durationSE$Answer)
index_greater_than_9 <- which(unique_durations == ">=9")
x_position <- index_greater_than_9 + 0.5 # Replace 9 with the position of ">=9" on the x-axis
png("sleepdurationALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "sleepdurationALPAR.png")
dev.off()

resultsleepdur <- correcteddf %>%
  group_by(hours_of_sleep, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(resultsleepdur,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleepdurationcount.csv")

#sleep duration plot raw with mean + SE 

correcteddf$hours_of_sleep <- as.numeric(correcteddf$hours_of_sleep)
correcteddf$hours_of_sleep[correcteddf$hours_of_sleep>=9] = '>=9'
correcteddf$hours_of_sleep[correcteddf$hours_of_sleep==6] = '<=6'
correcteddf$hours_of_sleep[correcteddf$hours_of_sleep==5] = '<=6'

durationSE <- correcteddf %>%
  group_by(hours_of_sleep,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    max=max(distance),
    min=min(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(durationSE)[1] <- "Answer"
durationSE <- as.data.frame(durationSE)
durationSE$Answer <- factor(durationSE$Answer, levels = c('<=6', '7', '8','>=9'))
correcteddf$x1 <- factor(correcteddf$hours_of_sleep, levels = c('<=6', '7', '8','>=9'))
dodge <- position_dodge(width = 0.6)
p = ggplot(durationSE, aes(x=Answer, y=mean, col=gender, shape=gender)) + guides(color = guide_legend("Gender"),shape = guide_legend("Gender")) + scale_color_manual(values = c('darkgrey', 'black')) + scale_shape_manual(values = c("Male" = 24, "Female" = 21)) + scale_fill_manual(values = c("Male" = "black", "Female" = "grey")) + ylim(8,15) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se,colour=gender),width=0,size=0.5,show.legend = FALSE,position=dodge) + geom_point(data = correcteddf, aes(x = x1, y = distance, shape=gender, colour=gender), alpha = 0.4, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),show.legend = FALSE) + geom_point(data = durationSE, aes(x = Answer, y = mean, shape = gender), fill = "black", color="black", size = 2, show.legend = TRUE,position=dodge) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA,colour = NA)) + labs(x = "Hours sleep duration") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)))
unique_durations <- unique(durationSE$Answer)
index_greater_than_9 <- which(unique_durations == ">=9")
x_position <- index_greater_than_9 + 0.5 # Replace 9 with the position of ">=9" on the x-axis
png("sleepdurationALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "sleepdurationALPAR.png")
dev.off()

resultsleepdur <- correcteddf %>%
  group_by(hours_of_sleep, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(resultsleepdur,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleepdurationcount.csv")

#sleep duration plot raw 

correcteddf$Answer <- factor(correcteddf$hours_of_sleep, levels = c('<=6', '7', '8','>=9'))

p <- ggplot(correcteddf, aes(x = Answer, y = distance, col= gender)) +
  scale_color_manual(values = c('grey', 'black')) +
  guides(color=guide_legend("Gender",override.aes = list(fill = NA))) + 
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA, colour = NA)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = gender)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(
    x = "Hours sleep duration",
    y = "Weighted wayfinding distance (VR-m)"
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold")
  )

ggsave(
  filename = "sleep_linearinteraction.png",
  path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/",
  plot = p
)

#sleepiness resolution index

correcteddf$sleepiness_resolutionbin <- 0 
correcteddf$sleepiness_resolutionbin[correcteddf$sleepiness_resolution_index<0.5] = '<0.5'
correcteddf$sleepiness_resolutionbin[correcteddf$sleepiness_resolution_index>=0.5 & correcteddf$sleepiness_resolution_index<1.0] = '0.5-1'
correcteddf$sleepiness_resolutionbin[correcteddf$sleepiness_resolution_index>=1.0 & correcteddf$sleepiness_resolution_index<1.5] = '1-1.5'
correcteddf$sleepiness_resolutionbin[correcteddf$sleepiness_resolution_index>=1.5] = '>=1.5'

SRISE <- correcteddf %>%
  group_by(sleepiness_resolutionbin,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    max=max(distance),
    min=min(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(SRISE)[1] <- "Answer"
SRISE <- as.data.frame(SRISE)

SRISE$Answer <- factor(SRISE$Answer, levels = c('<0.5', '0.5-1', '1-1.5', '>=1.5'))
p = ggplot(SRISE, aes(x=Answer, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) +  ylim(8,15) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + labs(x = "Sleepiness resolution index") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 

png("SRIALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "SRIALPAR.png")
dev.off()

SRIcount <- correcteddf %>%
  group_by(sleepiness_resolutionbin, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(SRIcount,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleepresolutioncount.csv")

#difficulty waking up

correcteddf$difficultywakeup[correcteddf$difficultywakeup==9] = 10 
correcteddf$difficultywakeup[correcteddf$difficultywakeup==8] = 9 
correcteddf$difficultywakeup[correcteddf$difficultywakeup==7] = 8
correcteddf$difficultywakeup[correcteddf$difficultywakeup==6] = 7
correcteddf$difficultywakeup[correcteddf$difficultywakeup==5] = 6
correcteddf$difficultywakeup[correcteddf$difficultywakeup==4] = 5
correcteddf$difficultywakeup[correcteddf$difficultywakeup==3] = 4
correcteddf$difficultywakeup[correcteddf$difficultywakeup==2] = 3
correcteddf$difficultywakeup[correcteddf$difficultywakeup==1] = 2
correcteddf$difficultywakeup[correcteddf$difficultywakeup==0] = 1

correcteddf$difficultbin <- 0 
correcteddf$difficultbin[correcteddf$difficultywakeup<2.0] = '<2'
correcteddf$difficultbin[correcteddf$difficultywakeup>=2.0 & correcteddf$difficultywakeup<4.0] = '2-4'
correcteddf$difficultbin[correcteddf$difficultywakeup>=4.0 & correcteddf$difficultywakeup<6.0] = '4-6'
correcteddf$difficultbin[correcteddf$difficultywakeup>=6.0] = '>=6'

difficultywakeupSE <- correcteddf %>%
  group_by(difficultbin,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(difficultywakeupSE)[1] <- "Answer"
difficultywakeupSE <- as.data.frame(difficultywakeupSE)

difficultywakeupSE$Answer <- factor(difficultywakeupSE$Answer, levels = c('<2', '2-4', '4-6', '>=6'))
p = ggplot(difficultywakeupSE, aes(x=Answer, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) +  ylim(8,14.5) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + labs(x = "Difficulty waking up (1 = min, 10 = max)") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 

png("difficultywakeupALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "difficultywakeupALPAR.png")
dev.off()

difffallsleepcount <- correcteddf %>%
  group_by(difficultbin, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(difffallsleepcount,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/difffallwakeupcount.csv")

#quantile(correcteddf$difficultywakeup, probs = c(.25, .5, .75))

#sleepiness on waking

correcteddf$sleepwakebin <- 0 
correcteddf$sleepwakebin[correcteddf$sleepiness_waking<4.0] = '<4'
correcteddf$sleepwakebin[correcteddf$sleepiness_waking>=4.0 & correcteddf$sleepiness_waking<6.0] = '4-6'
correcteddf$sleepwakebin[correcteddf$sleepiness_waking>=6.0 & correcteddf$sleepiness_waking<8.0] = '6-8'
correcteddf$sleepwakebin[correcteddf$sleepiness_waking>=8.0] = '>=8'

sleepinesswakeSE <- correcteddf %>%
  group_by(sleepwakebin,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(sleepinesswakeSE)[1] <- "Answer"
sleepinesswakeSEmale <- as.data.frame(sleepinesswakeSE)

sleepinesswakeSE$Answer <- factor(sleepinesswakeSE$Answer, levels = c('<4', '4-6', '6-8', '>=8'))
p = ggplot(sleepinesswakeSE, aes(x=Answer, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) +  ylim(8,15) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + labs(x = "Sleep inertia (1 = min, 10 = max)") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 

png("SleepinesswakingALPARALPARmale.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "SleepinesswakingALPARALPARmale.png")
dev.off()

difficultcount <- correcteddf %>%
  group_by(sleepwakebin, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(difficultcount,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleepwakecount.csv")

#sleep quality 

correcteddf$sleepqualitybin <- 0 
correcteddf$sleepqualitybin[correcteddf$sleep_quality<=3.0] = '<=3'
correcteddf$sleepqualitybin[correcteddf$sleep_quality>3.0 & correcteddf$sleep_quality<5.0] = '3-5'
correcteddf$sleepqualitybin[correcteddf$sleep_quality>=5.0 & correcteddf$sleep_quality<7.0] = '5-7'
correcteddf$sleepqualitybin[correcteddf$sleep_quality>=7.0] = '>=7'

sleepqualitySE <- correcteddf %>%
  group_by(sleepqualitybin,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(sleepqualitySE)[1] <- "Answer"
sleepqualitySE <- as.data.frame(sleepqualitySE)

sleepqualitySE$Answer <- factor(sleepqualitySE$Answer, levels = c('<=3', '3-5', '5-7', '>=7'))
p = ggplot(sleepqualitySE, aes(x=Answer, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) +  ylim(8,15) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + labs(x = "Sleep quality (1 = min, 10 = max)") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 

png("SleepqualityALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "SleepqualityALPAR.png")
dev.off()

sleepqualitycount <- correcteddf %>%
  group_by(sleepqualitybin, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(sleepqualitycount,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/sleepqualitycount.csv")

#time spent awake during the night

correcteddf$timeawakebin <- 0 
correcteddf$timeawakebin[correcteddf$time_spent_awake_during_night_mins<10.0] = '<10'
correcteddf$timeawakebin[correcteddf$time_spent_awake_during_night_mins>=10.0 & correcteddf$time_spent_awake_during_night_mins<=25.0] = '10-25'
correcteddf$timeawakebin[correcteddf$time_spent_awake_during_night_mins>25.0] = '>25'

timeawakeSE <- correcteddf %>%
  group_by(timeawakebin,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

colnames(timeawakeSE)[1] <- "Answer"
timeawakeSE <- as.data.frame(timeawakeSE)

timeawakeSE$Answer <- factor(timeawakeSE$Answer, levels = c('<10', '10-25', '>25'))
p = ggplot(timeawakeSE, aes(x=Answer, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) +  ylim(8,15) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + theme(strip.text.x = element_text(size = 20,face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + labs(x = "Time spent awake during the night (mins)") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 

png("timeawakeALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "timeawakeALPAR.png")
dev.off()

timeawakecount <- correcteddf %>%
  group_by(timeawakebin, gender) %>%
  count() %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)
write.csv(timeawakecount,file="/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/timeawakecount.csv")

#covariates and supplementary materials 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/POSTHOCALPAR.csv')
correcteddfmale <- correcteddf[correcteddf$gender==1,]
correcteddfemale <- correcteddf[correcteddf$gender==2,]

#line plot age

p = ggplot(data=correcteddf, aes(x=age, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)))  
png("ageALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ageALPAR.png")
dev.off()

#plot for age binned

ageSE <- correcteddf %>%
  group_by(binage) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(ageSE,aes(x=binage, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("ageboxplotalpar.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ageboxplotalpar.png")
dev.off()

#weekly hours of video gaming plot

correcteddf$binvideo[correcteddf$binvideo=='>=1 and <5 hpw gaming on all devices']='>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo=='>=5 and <10 hpw gaming on all devices']='>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo=='>=10 hpw gaming on all devices']= '>=10'

binvideoSE <- correcteddf %>%
  group_by(binvideo) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(binvideoSE$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p = ggplot(binvideoSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours per week video gaming on all devices") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",hjust=1,margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("videoboxplotalpar.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "videoboxplotalpar.png")
dev.off()

#weekly hours of video gaming line plot 

p = ggplot(data=correcteddf, aes(x=video_game_all_devices_hours_per_week, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + scale_x_continuous(breaks=c(1, 5,10,15, 20),labels=c("1","5","10","15","20+"))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours per week video gaming on all devices") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=1,margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("VideoALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "VideoALPAR.png")
dev.off()

#gender plot

correcteddf <- correcteddf[(correcteddf$gender!="3"),]
correcteddf$gender[correcteddf$gender==1.0]='Male'
correcteddf$gender[correcteddf$gender==2.0]='Female'

genderSE <- correcteddf %>%
  group_by(gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(genderSE,aes(x=gender, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Gender") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=0,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("genderboxplotalpar.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "genderboxplotalpar.png")
dev.off()

#weekly hours of phone use binned 

phoneSE <- correcteddf %>%
  group_by(binphone) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

phoneSE$binphone[phoneSE$binphone=='>=1 and <15 hpw phone use']='>=1 and <15'
phoneSE$binphone[phoneSE$binphone=='>=15 and <28 hpw phone use']='>=15 and <28'
phoneSE$binphone[phoneSE$binphone=='>=28 and <41 hpw phone use']= '>=28 and <41'
phoneSE$binphone[phoneSE$binphone=='>=41 hpw phone use']= '>=41'

x1  = factor(phoneSE$binphone, levels=c(">=1 and <15",">=15 and <28",">=28 and <41",">=41"))
p = ggplot(data=phoneSE, aes(x=x1, y=mean, group=1)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of phone use") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("hoursphoneuseALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "hoursphoneuseALPAR.png")
dev.off()

#weekly hours of phone use line plot

p = ggplot(data=correcteddf, aes(x=hours_of_phone_use_per_week, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of phone use") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("hoursphoneuselinearALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "hoursphoneuselinearALPAR.png")
dev.off()

#sunlight

correcteddf$sunlight <- as.integer(correcteddf$sunlight)
p = ggplot(data=correcteddf, aes(x=sunlight, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Daily hours of sunlight") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("sunhourslinearALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "sunhourslinearALPAR.png")
dev.off()

#BMI

p = ggplot(data=correcteddf, aes(x=BMI, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "BMI") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("BMIlinearALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "BMIlinearALPAR.png")
dev.off()

#caffeine

correcteddf$drink_caffeine_yes_noscale <- as.integer(correcteddf$drink_caffeine_yes_noscale)
p = ggplot(data=correcteddf, aes(x=drink_caffeine_yes_noscale, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Daily cups of caffeine") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("coffeelinearALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "coffeelinearALPAR.png")
dev.off()

#alcohol

correcteddf$drink_alcohol_yes_noscale <- as.integer(correcteddf$drink_alcohol_yes_noscale)
p = ggplot(data=correcteddf, aes(x=drink_alcohol_yes_noscale, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly units of alcohol") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("alcohollinearALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "alcohollinearALPAR.png")
dev.off()

#smoking frequency

smokingSE <- correcteddf %>%
  group_by(smoking) %>%
  summarise( 
    n = n(),
    mean = mean(distance),
    sd = sd(distance)
  ) %>%
  mutate( se = sd / sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))

smokingSE$smoking[smokingSE$smoking=='former smoker']='Former smoker'
smokingSE$smoking[smokingSE$smoking=='current smoker']='Current smoker'
smokingSE$smoking[smokingSE$smoking=='never smoked']= 'Never smoked'

x1  = factor(smokingSE$smoking, levels=c("Never smoked","Former smoker","Current smoker"))
p <- ggplot(data = smokingSE, aes(x = x1, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, size = 0.5) +
  geom_point(size = 2) +
  ylim(8, 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 25, hjust = 0.5,vjust=0.5)) +
  labs(x = "Smoking frequency",
       y = "Weighted Wayfinding Distance (VR-m)") +
  theme(axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
png("smokingALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "smokingALPAR.png")
dev.off()

#physical activity frequency

physicSE <- correcteddf %>%
  group_by(physical_activity) %>%
  summarise( 
    n = n(),
    mean = mean(distance),
    sd = sd(distance)
  ) %>%
  mutate( se = sd / sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))

physicSE$physical_activity[physicSE$physical_activity=='always']='Always'
physicSE$physical_activity[physicSE$physical_activity=='never']='Never'
physicSE$physical_activity[physicSE$physical_activity=='often']= 'Often'
physicSE$physical_activity[physicSE$physical_activity=='rarely']= 'Rarely'
physicSE$physical_activity[physicSE$physical_activity=='sometimes']= 'Sometimes'

x1  = factor(physicSE$physical_activity, levels=c("Never","Rarely","Sometimes","Often","Always"))
p <- ggplot(data = physicSE, aes(x = x1, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, size = 0.5) +
  geom_point(size = 2) +
  ylim(8, 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 25, hjust = 0.5,vjust=0.5)) +
  labs(x = "Frequency of daily strenuous physical activity",
       y = "Weighted Wayfinding Distance (VR-m)") +
  theme(axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0),hjust=1.0),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0),vjust=0.95))
png("strenuousactALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "strenuousactALPAR.png")
dev.off()

#highest education level achieved plot

correcteddf$education <- as.factor(correcteddf$education)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

educationSE <- correcteddf %>%
  group_by(education_coded) %>%
  summarise( 
    n = n(),
    mean = mean(distance),
    sd = sd(distance)
  ) %>%
  mutate( se = sd / sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))

educationSE$education_coded[educationSE$education_coded == 0] = "High School or below"
educationSE$education_coded[educationSE$education_coded == 1] = "2-Year College/Uni or above"
x1  = factor(educationSE$education_coded, levels=c("High School or below","2-Year College/Uni or above"))

p <- ggplot(data = educationSE, aes(x = x1, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, size = 0.5) +
  geom_point(size = 2) +
  ylim(8, 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 25, hjust = 0.5,vjust=0.5)) +
  labs(x = "Highest education level achieved",
       y = "Weighted Wayfinding Distance (VR-m)") +
  theme(axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0),hjust=0.75),plot.margin = margin(30, 20, 20, 20, "pt"))

png("educationALPAR.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "educationALPAR.png")
dev.off()


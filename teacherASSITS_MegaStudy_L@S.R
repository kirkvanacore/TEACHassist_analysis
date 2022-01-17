
#### teacherASSIST Mega Study for L@S ####

# Notes: 
  # this uses the data files Ethan pulled in 12/2021
  # I believe these came from Assignments v2 <- MUST CONFIRM
  # Some of the language has changed from previous research on teacherASSIST. 
  # I am sticking with the original TEACHERassist language for consistency (teacherASSIST, tutoring staratigies, etc)




####Installing & Loading Packages####
library(lme4)
library(psych)
library(lme4)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggExtra)
library(hexbin)
library(sjPlot)
library(broom)
library(dplyr)
library(tidyr) #nest
library(broom) #tidy
library(purrr) #map
library(anytime)
library(mice)


### Read Data ####

##### Log File Data ####

getwd()
logs <- read.csv("Student Support Dataset/student_support_logs.csv")
colnames(logs)
head(logs)

# remove logs form prelim analysis
old_logs <-
  read.csv(
    "/Users/kirkvanacore/Documents/WPI Analyses/TEACHassist_analysis/TEACHassist_data2/teacherassist_logs.csv"
  )
colnames(old_logs)
glimpse(logs$timestamp)
glimpse(old_logs$timestamp)
describe(logs$timestamp)
describe(old_logs$timestamp)

logs_c <- logs %>%
  anti_join(old_logs %>%
              select(
                "user_id", # need to make sure that these IDs will work
                "problem_id",
                "timestamp"
              ),
              by = c("user_id", # need to make sure that these IDs will work
                     "problem_id",
                     "timestamp"))
logs <- logs_c
rm(logs_c)


###### Basic Descriptive ####
# number of logs
length(logs$problem_id)
#number of problems
length(unique(logs$problem_id))

# number of students
length(unique(logs$user_id))



###### Log Data Cleaning ####

# convert timestamp to datetime
logs$datetime <- anytime(logs$timestamp) # the converts the time since epoch to datatime
max(logs$datetime)
min(logs$datetime)
hist(logs$datetime, breaks = 100)

table(logs$ambiguous_problem_log) # students hit problem multiple times
table(logs$no_problem_log) # students log was deleted fro unclear reasons
table(logs$no_next_problem) # students no longer have a problem log

# Analysis Data: separate out instances where students are randomized between 2 tutoring startigies 
ad <- logs %>%
  filter(
    ambiguous_problem_log == 0 &
    no_problem_log == 0 &
    no_next_problem == 0 & 
      randomized_between_student_supports == 1 & # only students who have 
    is.na(alternative_student_support_id_2) # exclude instances were students where randomized between > 2 strategies
  ) %>%
  select(
    -alternative_student_support_id_2,
    -alternative_student_support_id_3,
    -alternative_student_support_id_4
  )
length(logs$user_id) - length(ad$user_id)

# missing data
#mice::md.pattern(ad)

table(is.na(ad$selected_student_support_id)) # √

table(is.na(ad$alternative_student_support_id_1)) # √
table(is.na(ad$alternative_student_support_id_2)) # √
table(is.na(ad$alternative_student_support_id_3)) # √

# there is a group of students who do not have teacher_id, class_id, or sequence_id
table(is.na(ad$teacher_id)) 
table(is.na(ad$class_id)) 
table(is.na(ad$sequence_id)) 


table(is.na(ad$next_problem_id)) # students who did not attempt the next problem

table(is.na(ad$next_problem_id), 
      is.na(ad$next_problem_correctness)) 
# next_problem_correctness is NA only when the student did not attempt the next problem

# next_problem_correctness_adjusted: mark students who did not attempt next problem as incorrect
ad$next_problem_correctness_adjusted <- ifelse(is.na(ad$next_problem_correctness),
                                                          0,
                                                          ad$next_problem_correctness)
table(is.na(ad$next_problem_correctness_adjusted), 
      is.na(ad$next_problem_correctness)) 

table(is.na(ad$next_problem_correctness), 
      ad$answer_given) 
  # what does it mean if the there isn't any next problem correctness, but an answer is given? (8149)


table(is.na(ad$answer_given)) 

##### Problem Features ####
pf <- read.csv("Student Support Dataset/problem_features.csv")
colnames(pf)
head(pf)

length(unique(pf$problem_id))
length(unique(ad$problem_id))

pf$mathflag =
  pf$problem_subject_apr  +  pf$problem_subject_bf  +   pf$problem_subject_c +
  pf$problem_subject_cc   +  pf$problem_subject_ced  +  pf$problem_subject_cn +    pf$problem_subject_co +
  pf$problem_subject_cp   +  pf$problem_subject_ee  +   pf$problem_subject_f   +   pf$problem_subject_g +
  pf$problem_subject_gmd  +  pf$problem_subject_gpe  +  pf$problem_subject_ic +  pf$problem_subject_id +
  pf$problem_subject_if   +  pf$problem_subject_le  +   pf$problem_subject_md  +   pf$problem_subject_mg +
  pf$problem_subject_nbt  +  pf$problem_subject_nf   +  pf$problem_subject_ns   +  pf$problem_subject_oa +
  pf$problem_subject_q    + pf$problem_subject_rei +  pf$problem_subject_rn   +  pf$problem_subject_rp +
  pf$problem_subject_sp   +  pf$problem_subject_srt +   pf$problem_subject_sse +   pf$problem_subject_tf + pf$problem_subject_vm

table(pf$mathflag)

mice::md.pattern(pf) # no NAs

###### Remove None Math Problems from Logs #####
 
logs <- logs %>%
  anti_join(pf %>%
              filter(mathflag == 0) %>%
              select(problem_id)
            ) 

##### Tutoring Strategy Features ####
ts <- read.csv("Student Support Dataset/student_support_features.csv")
colnames(ts)
head(ts)

length(unique(ts$tutor_strategy_id))
length(unique(ad$selected_student_support_id))
length(unique(ad$alternative_student_support_id_1))

table(ts$student_support_is_explanation, ts$student_support_is_hint)

mice::md.pattern(ts) # no NAs

#### Create Random Treatment Variable ####
Experiments <- ad %>%
  select(problem_id,
         selected_student_support_id,
         alternative_student_support_id_1) %>%
  group_by(problem_id,
           selected_student_support_id,
           alternative_student_support_id_1) %>%
  summarise(n_attempts = (n())) # NUMBER OF attempt PER CONDITION

Experiments <- Experiments %>%
  group_by(problem_id) %>%
  mutate(n_attempts = sum(n_attempts)) %>% # N attempts per problem
  mutate(n_conditions_per_problem = (n())) # N conditions

table(Experiments$n_conditions_per_problem) # there are 2,671 problems with conditions with 2 conditions problem

# add in a random dummy variable for "treatment_random"
set.seed(257)
Experiments <- Experiments %>% 
  ungroup() %>%
  filter(n_conditions_per_problem == 2) %>% # only keep problems/experiments with two conditions
  group_by(problem_id) %>%
  arrange(selected_student_support_id) %>%
  mutate(treatment= sample(seq(n())),) %>% 
  distinct( .keep_all=TRUE)


Experiments$treatment_random <- ifelse(Experiments$treatment == 1, 1, 0)

table(Experiments$treatment_random)
colnames(Experiments)

# add treatment_random to ad now, will add other variables later
class(ad$selected_student_support_id)
class(Experiments$selected_student_support_id)
class(ad$alternative_student_support_id_1)
class(Experiments$alternative_student_support_id_1)

ad <- Experiments %>%
  select(
    problem_id,
    selected_student_support_id,
    alternative_student_support_id_1,
    treatment_random) %>% 
  left_join(ad,
            by = c("problem_id",
                   "selected_student_support_id",
                   "alternative_student_support_id_1"))



### Calculate Feature Difference Variables ####
      

## calculate difference between the assigned and alt strategies
      # message_count
      # text_length
      # images
      # videos
      # hint
      # explanation


treat_probs <-Experiments %>%
  ungroup() %>%
  filter(treatment_random == 1) 
table(duplicated(treat_probs$problem_id))

class(pf$t)

class(ts$selected_student_support_id)
colnames(treat_probs)


# add in tutor strategies meta data
treat_probs <- treat_probs %>%
  left_join(ts,
            by = c("selected_student_support_id" = "student_support_id"),
            ) %>%
left_join(ts,
          by = c("alternative_student_support_id_1" = "student_support_id"),
)
colnames(treat_probs)



  #.x --> treatment
  #.y --> control

table(treat_probs$student_support_content_creator_id.x == treat_probs$student_support_content_creator_id.y)
mice::md.pattern(treat_probs)

# num messages
table(treat_probs$student_support_message_count.x)
table(treat_probs$student_support_message_count.x, treat_probs$student_support_message_count.y)
treat_probs$message_count_diff <- treat_probs$student_support_message_count.x - 
  treat_probs$student_support_message_count.y
table(treat_probs$message_count_diff)

# text_length 
describe(treat_probs$student_support_text_length.x)
plot(treat_probs$student_support_text_length.x, treat_probs$student_support_text_length.y)
treat_probs$text_length_dif <-treat_probs$student_support_text_length.x - 
  treat_probs$student_support_text_length.y
hist(treat_probs$text_length_dif)
describe(treat_probs$text_length_dif)

# Images
table(treat_probs$student_support_contains_image.x, treat_probs$student_support_contains_image.y)
treat_probs$images_diff <- treat_probs$student_support_contains_image.x -
   treat_probs$student_support_contains_image.y
table(treat_probs$images_diff)


# Videos
table(treat_probs$student_support_contains_video.x, treat_probs$student_support_contains_video.y)

treat_probs$videos_diff <- treat_probs$student_support_contains_video.x - 
  treat_probs$student_support_contains_video.y
table(treat_probs$videos_diff)

# -1 -> experimental conditions where students did not have a video, but would have in the alt condition
# 0 -> experiments where students either don't have accesses to a video in either condition or inb both conditions
# 1 -> experimental conditions where students did have a video, but wouldn't have in the alt condition

#hints
treat_probs$hint_diff <- treat_probs$student_support_is_hint.x - treat_probs$student_support_is_hint.y
table(treat_probs$hint_diff)
table(treat_probs$hint_diff, treat_probs$student_support_is_hint.x)
table(treat_probs$hint_diff, treat_probs$student_support_is_hint.y)

# explanations
treat_probs$explanation_diff <- treat_probs$student_support_is_explanation.x - 
  treat_probs$student_support_is_explanation.y
table(treat_probs$explanation_diff)
table(treat_probs$explanation_diff, treat_probs$explanation.x, treat_probs$explanation.y)
table(treat_probs$explanation_diff, treat_probs$hint_diff)
colnames(treat_probs)


# add experiment/tutor strategy meta data into analysis data

ad <- ad %>%
  left_join(treat_probs %>%
              select(-treatment_random, 
                     -selected_student_support_id, 
                     -alternative_student_support_id_1
                     ),
            by = "problem_id"
  )

colnames(ad)
# spot check
# check <- ad %>%
#   filter(problem_id == 1479204) %>%
#   select(user_id, 
#          treatment_random,
#          n_attempts.x,
#          ,
#          
#          )
# 
# colnames(ad)

### Student MetaData ####



##### Add in Student Prior Accuracy ######



### MODELS ####

##### Test Models: Subset of Data ######
table(ad$n_attempts > 2000)
table(ad$n_attempts > 500) 
table(ad$n_attempts > 750) / length(ad$n_attempts)

table(ad$n_attempts > 200)/ length(ad$n_attempts)

# create subset
ad_small <- ad %>%
  filter(n_attempts >= 750)
length(unique(ad$problem_id))
length(unique(ad_small$problem_id))

# Null Model 
mNull <- glmer(next_problem_correctness_adjusted ~
                 + (1|problem_id) 
               ,
               data =ad_small,
               family = binomial
)
summary(mNull)

# add random intercept for 
m1.1 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random
              + (1 |problem_id) ,
              data =ad_small,
              family = binomial
)
summary(m1.1)


# add random intercept for 
m1.2 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random
              + (1 + treatment_random|problem_id) ,
              data =ad_small,
              family = binomial
)
summary(m1.2)

anova(m1.1, m1.2) # p value for whether the variance for treatment_random is == 0

# add explanation
m1.3 <- glmer(next_problem_correctness_adjusted ~
            + treatment_random*
              (explanation_diff)
            + (1 + treatment_random|problem_id) 
            ,
            data =ad_small,
            family = binomial
)
summary(m1.3)

anova(m1.2, m1.3) 


# add videos
m1.4 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (
                  explanation_diff + 
                  videos_diff
                  )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad_small,
              family = binomial
)
summary(m1.4)

anova(m1.3, m1.4) 

# add text length
m1.5 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (
                   explanation_diff + 
                   videos_diff +
                   text_length_dif
                   )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad_small,
              family = binomial
)
summary(m1.5)

anova(m1.4, m1.5) 

# add message count
m1.6 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (
                  explanation_diff + 
                  videos_diff +
                  text_length_dif +
                  message_count_diff 
                )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad_small,
              family = binomial
)
summary(m1.6)

anova(m1.5, m1.6) 

# add images 
m1.7 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (videos_diff +
                 explanation_diff +
                 text_length_dif +
                 message_count_diff +
                 images_diff
                )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad_small,
              family = binomial
)
summary(m1.7)

anova(m1.6, m1.7) 

avg <- ad_small %>%
  ungroup() %>%
  group_by(problem_id,
           explanation_diff) %>%
  summarise(Pre_Cor = mean(next_problem_correctness_adjusted)) %>%
  group_by(explanation_diff) %>%
  summarise(Pre_Cor = mean(Pre_Cor),
            n = n())

avg <- ad %>%
  ungroup() %>%
  group_by(problem_id,
           explanation_diff) %>%
  summarise(Pre_Cor = mean(next_problem_correctness_adjusted)) %>%
  group_by(explanation_diff) %>%
  summarise(Pre_Cor = mean(Pre_Cor),
            n = n())
avg

##### Full Data Models ######

# Null Model 
mNull <- glmer(next_problem_correctness_adjusted ~
                 + (1|problem_id) 
               ,
               data =ad,
               family = binomial
)
summary(mNull) 
  
# add random intercept for 
m1.1 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random
              + (1 |problem_id) ,
              data =ad,
              family = binomial
)
summary(m1.1)


# add random intercept for 
m1.2 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random
              + (1 + treatment_random|problem_id) ,
              data =ad,
              family = binomial
)
summary(m1.2)

anova(m1.1, m1.2) # p value for whether the variance for treatment_random is == 0

# add explanation
m1.3 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (explanation_diff)
              + (1 + treatment_random|problem_id) 
              ,
              data =ad,
              family = binomial
)
summary(m1.3)

anova(m1.2, m1.3) 


# add videos
m1.4 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (
                  explanation_diff + 
                    videos_diff
                )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad,
              family = binomial
)
summary(m1.4)

anova(m1.3, m1.4) 

# add text length
m1.5 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (
                  explanation_diff + 
                    videos_diff +
                    text_length_dif
                )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad,
              family = binomial
)
summary(m1.5)

anova(m1.4, m1.5) 

# add message count
m1.6 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (
                  explanation_diff + 
                    videos_diff +
                    text_length_dif +
                    message_count_diff 
                )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad,
              family = binomial
)
summary(m1.6)

anova(m1.5, m1.6) 

# add images 
m1.7 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random*
                (videos_diff +
                   explanation_diff +
                   text_length_dif +
                   message_count_diff +
                   images_diff
                )
              + (1 + treatment_random|problem_id) 
              ,
              data =ad,
              family = binomial 
)
summary(m1.7)

anova(m1.6, m1.7) 

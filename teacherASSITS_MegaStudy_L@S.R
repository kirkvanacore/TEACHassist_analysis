
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
logs<-read.csv("Student Support Dataset/student_support_logs.csv")
colnames(logs)
head(logs)


###### Basic Descriptive ####
# number of logs
length(logs$problem_id)
#number of problems
length(unique(logs$problem_id))

# number of students
length(unique(logs$user_id))


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

mice::md.pattern(pf) # no NAs


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
Experiments <- Experiments %>% 
  filter(n_conditions_per_problem == 2) %>% # only keep problems/experiments with two conditions
  group_by(problem_id) %>%
  arrange(selected_student_support_id) %>%
  mutate(treatment=seq(n())) %>% 
  distinct( .keep_all=TRUE)


Experiments$treatment_random <- ifelse(Experiments$treatment ==1, 1, 0)

table(Experiments$treatment_random)
colnames(Experiments)

# add treatment_random to ad now, will add other variables later
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

# add in tutor strategies meta data
treat_probs <- treat_probs %>%
  left_join(ts,
            by = c("selected_student_support_id" = "tutor_strategy_id"),
            ) %>%
left_join(ts,
          by = c("alternative_student_support_id_1" = "tutor_strategy_id"),
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

### MODELS ####

##### Test Models: Subset of Data ######
table(ad$n_attempts > 2000)
# create subset
ad_small <- ad %>%
  filter(n_attempts >= 2000)
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

# ad_smalld random intercept for 
m1.1 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random
              + (1 |problem_id) ,
              data =ad_small,
              family = binomial
)
summary(m1.1)


# ad_smalld random intercept for 
m1.2 <- glmer(next_problem_correctness_adjusted ~
                + treatment_random
              + (1 + treatment_random|problem_id) ,
              data =ad_small,
              family = binomial
)
summary(m1.2)

anova(m1.1, m1.2) # p value for whether the variance for treatment_random is == 0

# ad_smalld explanation
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


# ad_smalld videos
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

# ad_smalld text length
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

# ad_smalld message count
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

# ad_smalld images 
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

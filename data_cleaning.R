

# Data Cleaning ####


## Load Packages ####
library(dplyr)
library(psych)

## Read Student Data ####

#### Assignment logs ####
al <- read.csv("TEACHassist_data/assignment_logs.csv")
colnames(al)
head(al)

# logs of assignments started 
    # STRUCTURE:
      # One row per student pre assignment
      # Does not include performance data
    # QUESTIONS
        # when assignment_end_time is NA: did not complete? 
        # Is this only teach assist students? (students in the experiment)
        # are these just the TEACHassist assignments?

# Data Exploration


# assignment Completion
table(is.na(al$assignment_end_time))/length(al$assignment_end_time)
# 15% of the time the students did not finish the assignment

# how many assignments did students start
length(unique(al$user_id))
describe(data.frame(table(al$user_id))$Freq)
hist(data.frame(table(al$user_id))$Freq, breaks = 1000, xlim = c(0,500))
# median 8; highly positively skewed


#### Problems logs ####
prl <- read.csv("TEACHassist_data/problem_logs.csv")
colnames(prl)
head(prl)
# logs of items started 
    # STRUCTURE: 
        # one row per student/problem, includes data on multiple attempts (wide structure)
    # QUESTIONS:
        # Is is only students who completed a problem? (what does that mean to complete a problem?)
        # why is the avg problems lower than the avg assignments? aren't assignments nested within problems
        # why are there way more students in this data file than the assignments set
        # is this only student performance prior to experiment (?) -> NO 
        # why are there some instances where the student go the problem correct, but their number of attempt is zero?
        # why are some problem corrects NA? is this because they never attempted? if so see question above

# how many problems did students start?
length(unique(prl$user_id))
describe(data.frame(table(prl$user_id))$Freq)
hist(data.frame(table(al$user_id))$Freq, breaks = 1000, xlim = c(0,500))
# median 6; highly positively skewed


#### TEACHassits logs ####
tal <- read.csv("TEACHassist_data/teacherassist_logs.csv")
colnames(tal)
head(tal)
# logs of TEACHassist problems with performance on next problem
    # STRUCTURE: 
        # one row per student/problem in the TEACHassist experiment,
    # QUESTIONS:
        # Is is only students who completed a problem? (what does that mean to complete a problem?)
        # why is the avg problems lower than the avg assignments? aren't assignments nested within problems
        # why are there way more students in this data file than the assignments set

# READOCDE HINTS OBSERVED
# some students are in the control and observed a hint 
  # checked with Ethan 
  #-> this just means they got the answer after all attempts
  # RECODING as 0 
# !!!
# this raises the question whether some students have a hint flag because they
# got the answer just like those in the control
# !!!
tal$tutoring_observed_adjusted <- ifelse(tal$randomized_with_control == 1, 0, tal$tutoring_observed)

# is randomization at the student or encounter (student/problem) level
table(tal$randomized_between_tutor_strategies)

table(tal$user_id, tal$randomized_between_tutor_strategies)

# next problem correness
table(is.na(tal$next_problem_correctness))

# number of uses and freq of use
length(unique(tal$user_id))
describe(data.frame(table(tal$user_id))$Freq)
# median 6; highly positively skewed

# variable for sequential exposures
tal<-tal %>%
  group_by(user_id) %>%
  arrange(timestamp, .by_group =T) %>%
  mutate(num_exposures = seq(n()))

check<- data.frame(table(tal[tal$num_exposures == 1, ]$user_id))

## Read Meta Data ####

##### Problem Features #####
pf <- read.csv("TEACHassist_data/problem_features.csv")
colnames(pf)
# QUESTIONS:
    # what does these variables represent?
        # problem type (1-17)
        # Level (k-8, hsa, hsf, hsg, hsm, hsn, hss)
        # subject
    # Why are their duplicated tutor_strategy_ids (5658 dups)
table(duplicated(pf$problem_id)) # No Dups yeah!!!



##### TEACHassits (Tutor) Features #####
taf <- read.csv("TEACHassist_data/tutor_strategy_features.csv")
colnames(taf)
# QUESTIONS:
    # Explanation -> is the a worked problem with the answer?
    # what does the hint flag mean? What are the Tutors that are not hints? --> Hints and expolaintions are mutually exclusiove

length(unique(taf$tutor_strategy_id)) # some duplicated ids!!
table(duplicated(taf$tutor_strategy_id))
table(duplicated(taf$tutor_strategy_id))/length(taf$tutor_strategy_id)

taf$dup <- duplicated(taf$tutor_strategy_id)





### !!! temporarily remove duplicates #######
taf <- taf %>% 
  filter(dup == F) %>%
  select(-dup)

length(unique(taf$content_creator_id)) # 19 content creators
table(taf$content_creator_id)

# content 
table(taf$content_creator_id, taf$contains_video) # teachers tend to either be video creators or not
table(taf$content_creator_id, taf$contains_image) # use of images are more consistently distributed across teachers
table(taf$content_creator_id, taf$contains_link) # no links

# text length
describe(taf$text_length)
hist(taf$text_length)
hist(taf$text_length, breaks = 150, xlim = c(0,1000))
# odd distribution
table(taf$text_length == 0)
table(taf$text_length <= 3)

# text length by contains video 
describeBy(taf$text_length, taf$contains_video)

table(taf$text_length <= 7, taf$contains_video)

# just the non video text
hist(taf[taf$contains_video == 0, ]$text_length, breaks = 150, xlim = c(0,1000))
describe(taf[taf$contains_video == 0, ]$text_length)
# bit more normal than the total pop

# color use
table(taf$color_use)
table(taf$content_creator_id, taf$color_use) # distributed across teachers


# font/text size --> just indicators (deviations for presets?)
table(taf$font_use)
table(taf$text_size_use)


# Aggregate student performance data ####
# student performance prior to any TEACHassist problems

### !! I AM ASSUMING THAT timestamp AND problem_start_time are in the same formate and are sequenental variables

# need min time stamp for students 
min_time <- tal %>%
  select(user_id, timestamp) %>%
  group_by(user_id) %>%
  mutate(timestamp = min(timestamp)) %>%
  distinct()

prl2 <- prl %>%
  right_join(min_time,
             by =c("user_id"))
length(unique(prl2$user_id))

# flag whether the performance was prior to the TEACHassist round
prl2$FLAG <- ifelse(prl2$problem_start_time < prl2$timestamp, 1, 0) # the problem
table(prl2$FLAG)

prl2 <- prl2 %>%
  filter(FLAG == 1,
         is.na(problem_first_attempt_correct) == F)

prior_performance <- aggregate(prl2$problem_first_attempt_correct, by = list(prl2$user_id), FUN = mean)

colnames(prior_performance) <- c("user_id", "prior_accuracy")
colnames(prior_performance)
table(is.na(prior_performance$prior_accuracy))

prior_num_problem <- data.frame(table(prl2$user_id))
colnames(prior_num_problem) <- c("user_id", "prior_num_problems")
colnames(prior_num_problem)
describe(prior_num_problem$prior_num_problems)

table(prior_num_problem$prior_num_problems > 1) 
table(prior_num_problem$prior_num_problems > 5) 

hist(prior_num_problem$prior_num_problems, breaks = 100)
prior_num_problem$user_id <- as.integer(as.character(prior_num_problem$user_id))

prior_performance <- prior_performance %>%
  left_join(prior_num_problem,
            by = c("user_id"))
    # NOTE THAT NOT ALL STUDENTS HAVE PRIOR DATA
        # CHECK WITH ETHAN ->this can because the students either didn't have any prior data 
        # or b/c their problem data had NA for accuracy (see questions in TEACHassist Logs section above)

#!! ALL STUDENTS HAD SOME FORM OF PRIOR PERFORMANCE?? -> does this seem right?
# Aggregated problem difficulty ####
#(avg accuracy on problem across students)
length(unique(prl$problem_id))

# exclude rows with NA for problem_first_attempt_correct
prl2 <- prl %>%
  filter(is.na(problem_first_attempt_correct) == F)

# avg accuracy
problem_difficulity <- aggregate(prl2$problem_first_attempt_correct, by = list(prl2$problem_id), FUN = mean)
colnames(problem_difficulity) <- c("problem_id", "problem_avg_accuracy")
table(is.na(problem_difficulity$problem_avg_accuracy))

# number of data points to calc accuracy
prior_num_problem <- data.frame(table(prl2$problem_id))
colnames(prior_num_problem) <- c("problem_id", "num_student_data_to_calc_accuray")
colnames(prior_num_problem)
describe(prior_num_problem$num_student_data_to_calc_accuray)
hist(prior_num_problem$num_student_data_to_calc_accuray, breaks = 100000, xlim= c(0,100))
prior_num_problem$problem_id <- as.integer(as.character(prior_num_problem$problem_id))


# standard error of accuracy
std <- function(x) sd(x)/sqrt(length(x))
problem_difficulity_SE <- aggregate(prl2$problem_first_attempt_correct, by = list(prl2$problem_id), FUN = std)
colnames(problem_difficulity_SE) <- c("problem_id", "problem_accuracy_se")
problem_difficulity_SE$user_id <- as.integer(as.character(problem_difficulity_SE$user_id))


# 
problem_difficulity <- problem_difficulity %>%
  left_join(problem_difficulity_SE,
            by = "problem_id") %>%
  left_join(prior_num_problem,
            by = "problem_id")


table(is.na(problem_difficulity$problem_avg_accuracy))
describe(problem_difficulity$problem_avg_accuracy)
describe(problem_difficulity$problem_accuracy_se)
hist(problem_difficulity$problem_accuracy_se)
plot(problem_difficulity$num_student_data_to_calc_accuray, problem_difficulity$problem_accuracy_se, ylim = c(0,500))

# ASK ADAM -> if i ran a rasch model on these data would I be able to develop metrics of student performance relative to problem difficulty
  
# Merge for Analysis Data Sets #####

ad <- tal %>%
  left_join(
    taf,
    by = c("assigned_tutor_strategy_id"="tutor_strategy_id")
  ) %>%
  left_join(prior_performance,
            by = c("user_id")) %>%
  left_join(pf_performance %>%
              select("problem_id",
                     "average_correctness",
                    "normalized_average_correctness"), ### originally used my own calculations for problem diff measures, but had alot of missing data
            by = c("problem_id"))
  
    
  length(unique(ad$user_id)) # far fewere when 
  
  table(is.na(ad$average_correctness)) # we have missing avg accuracy data
  
  table(is.na(ad$problem_avg_accuracy)) # we have missing avg accuracy data
  
  
  table(is.na(ad$prior_accuracy)) # we have a lot of missing avg accuracy data
  
# #### !! CHANGES 
#   # missing meta data exportation
#   table(ad[is.na(ad$problem_avg_accuracy) == T,]$problem_id)
#     # It seems that a lot of these are problems where students have NA for their first attempt
#     # is also seems like there are no next problems in the TEACHassist log data
#   check <- ad[is.na(ad$problem_avg_accuracy) == T, c("assignment_id","problem_id")] %>%
#     distinct()
#   colnames(check)
# 
#   # Pull in performance data form other data file
#   pf_performance <- read.csv("problem_features.csv")
#   table(is.na(pf_performance$average_correctness))
# 
#   colnames(pf_performance)
# 
#   # !! WILL NEED TO STANDARDIZE HOW I GET PERFORMANCE DATA !!
#   # add data from pf_performance
  
  

# save data 
write.csv(ad, "analysis_data_1.csv")

rm(list=setdiff(ls(), "ad")) # X will remain


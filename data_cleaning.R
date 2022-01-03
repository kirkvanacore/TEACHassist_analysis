# Data Cleaning ####

## Load Packages ####
library(dplyr)
library(psych)

## Read Student Data ####

#### ASSISTments logs ####
al <- read.csv("TEACHassist_data2/assistments_logs.csv")
colnames(al)
head(al)

# logs of ASSISTments started 
    # STRUCTURE:
      # One row per student pre problem 
      # Includes performance data
    # QUESTIONS
        # who is included in this data file?
        # it seems like this is more than just students in the experiment
        # problem_answer_given --> is this a bottom out indicator?
        # is problem_correctness just for the first attempt?
    # there are students in the teacherassist_logs.csv that are not in the assistments_logs.csv (user_id = 1002)

# Data Exploration

# N --> 87,185
length(unique(al$user_id))


# assignment Completion
table(is.na(al$assignment_end_time))/length(al$assignment_end_time)
# 13% of the time the students did not finish the problem

# how many problems did students start
describe(data.frame(table(al$user_id))$Freq)
hist(data.frame(table(al$user_id))$Freq, breaks = 1000, xlim = c(0,500))
# median 16; highly positively skewed

# how many problem ids
length(unique(al$problem_id)) # 22,104 


#### TEACHassits logs ####
tal <- read.csv("TEACHassist_data2/teacherassist_logs.csv")
colnames(tal)
head(tal)
# logs of TEACHassist problems with performance on next problem
    # STRUCTURE: 
        # one row per student/problem in the TEACHassist experiment,
    # QUESTIONS:
        # What does it mean when assigned_tutor_strategy_id == 0 (only occurs in the control)


length(unique(tal$problem_id)) # 13,337 

# create treatment and control variables
tal$treatment <- ifelse(tal$assigned_tutor_strategy_id == 0, 0, 1) 
table(tal$assigned_tutor_strategy_id, tal$treatment)
table(tal$treatment)/length(tal$treatment) # only 6% of students are in to control

# examine different experiments
## treatment vs control
table(tal[tal$randomized_with_control == 1,]$treatment)/length(tal[tal$randomized_with_control == 1,]$treatment)
# 10 % of students are in control

# hints observed -> did only treatment students observe hints?
table(tal$treatment, tal$tutoring_observed) # --> YES, treatment is never observed among control


# is randomization at the student or encounter (student/problem) level --> problem Level
table(tal$randomized_between_tutor_strategies)
table(tal[tal$randomized_with_control == 1,]$user_id, tal[tal$randomized_with_control == 1,]$treatment)

# next problem correctness
table(is.na(tal$next_problem_correctness))
table(is.na(tal$next_problem_correctness))/length(tal$next_problem_correctness)
# 37.9% of students do not complete the problem


#### N RAW DATA ####
exclution <- "Raw Data"
n_problems <- length(tal$id)
n_tc <- length(tal[tal$randomized_with_control == 1, ]$id)
n_bw <- length(tal[tal$randomized_with_control == 0, ]$id)
n_problems_table <- cbind(exclution, n_problems, n_tc, n_bw)
rm(n_problems, n_tc, n_bw)
colnames(n_problems_table) <- c("exclution", "n_problems", "n_tc", "n_bw")

n_students <- length(unique(tal$user_id))
n_tc <- length(unique(tal[tal$randomized_with_control == 1, ]$user_id))
n_bw <- length(unique(tal[tal$randomized_between_tutor_strategies == 1, ]$user_id))
n_students_table <- cbind(exclution, n_students, n_tc, n_bw)
rm(exclution, n_students, n_tc, n_bw)

### EXCLUSION 1: No Post Test (b/c assignment was complete) ####
# NEED TO ID WHEN STUDENTS DON"T HAVE A NEXT PROBLEM TO COMPLETE -->  students who completed there assignments
assignments_complete <- al %>%
  select(user_id, assignment_id, assignment_end_timestamp) %>%
  mutate(assignments_complete = is.na(assignment_end_timestamp) == F) %>%
  distinct()
rm(check)
tal_clean <- tal %>%
  left_join(assignments_complete,
            by = c("user_id", "assignment_id"))

table(is.na(tal_clean$assignments_complete)) / length(is.na(tal_clean$assignments_complete)) ### MISSING DATA FOR 10% of 
table(is.na(tal_clean$assignments_complete), tal_clean$randomized_with_control)

chisq.test(as.matrix(table(tal_clean[tal_clean$randomized_with_control == 1, ]$treatment,is.na(tal_clean[tal_clean$randomized_with_control == 1, ]$assignments_complete))))
# missingness is evenly distributed between treatment and control

# if students have completed their assignment and the next problem_id is null then exclude
# if student have not completed their assignment and the next problem is null --> next_problem_correctness_adjusted == 0
table(tal_clean$assignments_complete)
table(is.na(tal_clean$assignments_complete), is.na(tal_clean$next_problem_id))
table((tal_clean$assignments_complete), is.na(tal_clean$next_problem_id))

tal_clean$flag <- ifelse(
  tal_clean$assignments_complete == T & is.na(tal_clean$next_problem_id) == T, 1,  # the data are not missing, the students didn't have the opportunity for post test
  ifelse(tal_clean$assignments_complete == F & is.na(tal_clean$next_problem_id) == T, 2, # the students did not complete the assignment
         0))
table(tal_clean$flag)
table(is.na(tal_clean$assignments_complete), tal_clean$flag)



tal_clean$next_problem_correctness_adjusted <- ifelse(is.na(tal_clean$next_problem_correctness), 0,
                                                tal_clean$next_problem_correctness)
table(is.na(tal_clean$next_problem_correctness_adjusted), is.na(tal_clean$next_problem_correctness))

table(is.na(tal_clean$next_problem_correctness))/length(tal_clean$next_problem_correctness)
table(is.na(tal_clean$next_problem_correctness_adjusted))/length(tal_clean$next_problem_correctness_adjusted)
table(is.na(tal_clean$next_problem_correctness_adjusted), tal_clean$flag)

chisq.test(as.matrix(table( tal_clean[tal_clean$randomized_with_control == 1, ]$treatment,
                            is.na(tal_clean[tal_clean$randomized_with_control == 1, ]$next_problem_correctness_adjusted))))
# missingness is not sig different between treatment/control condition

# even though the missingness is not different between treatment and control it
# is not missing completely at random because the randomness is caused with
# teachers resetting students to allow them to retake the assignment which is
# most likely negatively correltaed with the  outcome
table(tal_clean$flag == 1)

tal_clean <- tal_clean %>%
  filter(flag != 1)

#### N POST EXECUTION 1 DATA ####
exclution <- "1"
n_problems <- length(tal_clean$id)
n_tc <- length(tal_clean[tal_clean$randomized_with_control == 1, ]$id)
n_bw <- length(tal_clean[tal_clean$randomized_with_control == 0, ]$id)
n_problems_table1 <- cbind(exclution, n_problems, n_tc, n_bw)
n_problems_table <- rbind(n_problems_table, n_problems_table1)
rm(n_problems, n_tc, n_bw, n_problems_table1)

colnames(n_problems_table) <- c("exclution", "n_problems", "n_tc", "n_bw")

n_students <- length(unique(tal_clean$user_id))
n_tc <- length(unique(tal_clean[tal_clean$randomized_with_control == 1, ]$user_id))
n_bw <- length(unique(tal_clean[tal_clean$randomized_between_tutor_strategies == 1, ]$user_id))
n_students_table1 <- cbind(exclution, n_students, n_tc, n_bw)
n_students_table <- rbind(n_students_table, n_students_table1)
rm(exclution, n_students, n_tc, n_bw, n_students_table1)


# Is there only one problem per student?
student_problems <- tal_clean %>%
  select(user_id, problem_id) %>%
  group_by(user_id, problem_id) %>%
  mutate(problem_n = n()) %>%
  distinct()

table(student_problems$problem_n > 1)
table(student_problems$problem_n > 1)/length(student_problems$problem_n)
length(unique(tal_clean$user_id))

### EXCLUSION 2: Repeated Exposure ####
# REMOVE MULTIPLE PROBLEM ATTEMPTS STUDENTS WHO HIT THE A PROBLEM MORE THAN ONCE
# BECAUSE STUDENTS COULD BE IN MULTIPLE CONDITIONS AT DIFFERENT TIMES
tal_clean <- tal_clean %>%
  group_by(user_id, problem_id) %>%
  arrange(timestamp, .by_group =T) %>%
  mutate(prob_attempt_number = seq(n())) %>%
filter(prob_attempt_number == 1) %>%
  select(-prob_attempt_number)
rm(student_problems)

#### N POST EXECUTION 2 DATA ####
exclution <- "2"
n_problems <- length(tal_clean$id)
n_tc <- length(tal_clean[tal_clean$randomized_with_control == 1, ]$id)
n_bw <- length(tal_clean[tal_clean$randomized_with_control == 0, ]$id)
n_problems_table1 <- cbind(exclution, n_problems, n_tc, n_bw)
n_problems_table <- rbind(n_problems_table, n_problems_table1)
rm(n_problems, n_tc, n_bw, n_problems_table1)

n_students <- length(unique(tal_clean$user_id))
n_tc <- length(unique(tal_clean[tal_clean$randomized_with_control == 1, ]$user_id))
n_bw <- length(unique(tal_clean[tal_clean$randomized_between_tutor_strategies == 1, ]$user_id))
n_students_table1 <- cbind(exclution, n_students, n_tc, n_bw)
n_students_table <- rbind(n_students_table, n_students_table1)
rm(exclution, n_students, n_tc, n_bw, n_students_table1)

# number of uses and freq of use
length(unique(tal_clean$user_id))
describe(data.frame(table(tal_clean$user_id))$Freq)
# median 12; highly positively skewed

# variable for sequential exposures
tal_clean<-tal_clean %>%
  group_by(user_id) %>%
  arrange(timestamp, .by_group =T) %>%
  mutate(num_exposures = seq(n()))
table(tal_clean$num_exposures)
check<- data.frame(table(tal_clean[tal_clean$num_exposures == 1, ]$user_id))
table(check$Freq)
rm(check)
# how many problems?
length(unique(tal_clean$problem_id)) # 13337

## Read Meta Data ####
##### Problem Features #####
pf <- read.csv("TEACHassist_data2/problem_features.csv")
colnames(pf)
# QUESTIONS:
    # what does these variables represent?
        # problem type (1-17)
        # Level (k-8, hsa, hsf, hsg, hsm, hsn, hss)
        # subject


table(duplicated(pf$problem_id)) # No Dups yeah!!!

##### TEACHassits (Tutor) Features #####
taf <- read.csv("TEACHassist_data2/tutor_strategy_features.csv")
colnames(taf)
# QUESTIONS:
    # Explanation -> is the a worked problem with the answer?
    # what does the hint flag mean? What are the Tutors that are not hints? --> Hints and expolaintions are mutually exclusiove

  # ADD CREATE DATA

# Thea Durling -> 
# 436143/tdurling@gdrsd.org
# 255574/thea.durling@gmail.com

length(unique(taf$tutor_strategy_id)) # some duplicated ids!!
table(duplicated(taf$tutor_strategy_id))
table(duplicated(taf$tutor_strategy_id))/length(taf$tutor_strategy_id)

taf$dup <- duplicated(taf$tutor_strategy_id)
table(taf$dup)

taf$dup <- duplicated(taf$tutor_strategy_id)
table(taf$dup)

### !!! temporarily remove duplicates ####### ! ADAM -> I fixed this problem in the next script (TEACHERassist_MegaStudy.Rmd)
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
table(taf$text_length <= 3)

# text length by contains video 
describeBy(taf$text_length, taf$contains_video)

table(taf$text_length <= 7, taf$contains_video)

# just the non video text
hist(taf[taf$contains_video == 0, ]$text_length, breaks = 150, xlim = c(0,1000))
describe(taf[taf$contains_video == 0, ]$text_length)
# bit more normal than the total_clean pop

# color use
table(taf$color_use)
table(taf$content_creator_id, taf$color_use) # distributed across teachers


# font/text size --> just indicators (deviations for presets?)
table(taf$font_use)
table(taf$text_size_use)


# Aggregate student performance data ####
# student performance prior to any TEACHassist problems

### !! I AM ASSUMING THAT timestamp AND problem_start_time are in the same format and are sequential variables

# need min time stamp for students 
min_time <- tal_clean %>%
  ungroup() %>%
  select(user_id, timestamp) %>%
  group_by(user_id) %>%
  mutate(timestamp = min(timestamp)) %>%
  distinct()

al2 <- al %>%
  right_join(min_time,
             by =c("user_id"))
length(unique(al2$user_id))

# flag whether the performance was prior to the TEACHassist round
al2$FLAG <- ifelse(al2$problem_start_timestamp < al2$timestamp, 1, 0) # the problem
table(al2$FLAG)

table(is.na(al2$problem_correctness))

al2 <- al2 %>%
  filter(FLAG == 1)

         # for this calculation I am counting instances where students did not complete a problem as a zero
al2$problem_correctness  <-ifelse(is.na(al2$problem_correctness) == T, 0, al2$problem_correctness) 
         
prior_performance <- aggregate(al2$problem_correctness, by = list(al2$user_id), FUN = mean)

colnames(prior_performance) <- c("user_id", "prior_accuracy")
colnames(prior_performance)
table(is.na(prior_performance$prior_accuracy))

prior_num_problem <- data.frame(table(al2$user_id))
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

# no missing data within file 
table(is.na(prior_performance$user_id))
table(is.na(prior_performance$prior_accuracy))
table(is.na(prior_performance$prior_num_problems))

    # NOTE THAT NOT ALL STUDENTS HAVE PRIOR DATA
        # CHECK WITH ETHAN ->this can because the students didn't have any prior data 



# Aggregated problem difficulty ####
#(avg accuracy on problem across students)
length(unique(al$problem_id))

# exclude rows with NA for problem_correctness
al2 <- al %>%
  filter(is.na(problem_correctness) == F)

# avg accuracy
problem_difficulity <- aggregate(al2$problem_correctness, by = list(al2$problem_id), FUN = mean)
colnames(problem_difficulity) <- c("problem_id", "problem_avg_accuracy")
table(is.na(problem_difficulity$problem_avg_accuracy))

# number of data points to calc accuracy
prior_num_problem <- data.frame(table(al2$problem_id))
colnames(prior_num_problem) <- c("problem_id", "num_student_data_to_calc_accuray")
colnames(prior_num_problem)
describe(prior_num_problem$num_student_data_to_calc_accuray)
hist(prior_num_problem$num_student_data_to_calc_accuray, breaks = 100000, xlim= c(0,100))
prior_num_problem$problem_id <- as.integer(as.character(prior_num_problem$problem_id))


# standard error of accuracy
std <- function(x) sd(x)/sqrt(length(x))
problem_difficulity_SE <- aggregate(al2$problem_correctness, by = list(al2$problem_id), FUN = std)
colnames(problem_difficulity_SE) <- c("problem_id", "problem_accuracy_se")
problem_difficulity_SE$problem_id <- as.integer(as.character(problem_difficulity_SE$problem_id))


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
plot(problem_difficulity$num_student_data_to_calc_accuray, problem_difficulity$problem_accuracy_se)

# ASK ADAM -> if i ran a rasch model on these data would I be able to develop metrics of student performance relative to problem difficulty
  
# Check Problem Difficulty
pf_performance <- read.csv("problem_features.csv")
   table(is.na(pf_performance$average_correctness))
# 
   colnames(pf_performance)
   
   problem_difficulity <- problem_difficulity %>% 
     full_join(pf_performance %>%
                 select(problem_id, 
                        problem_completion_percentage,
                        average_correctness),
               by = "problem_id")
   
  t.test(problem_difficulity[problem_difficulity$num_student_data_to_calc_accuray > 5,]$average_correctness, 
         problem_difficulity[problem_difficulity$num_student_data_to_calc_accuray > 5,]$problem_avg_accuracy)
  # marginally non sig differences when the number of students used to calculate problem level accuracy is 
  
  table(is.na(problem_difficulity$average_correctness),
        is.na(problem_difficulity$problem_avg_accuracy))
 
  problem_difficulity$problem_difficulity_est <- ifelse(
    is.na(problem_difficulity$problem_avg_accuracy) == T &
      problem_difficulity$num_student_data_to_calc_accuray > 5,  problem_difficulity$average_correctness,
    problem_difficulity$problem_avg_accuracy
  )
  
   # !! WILL NEED TO STANDARDIZE HOW I GET PERFORMANCE DATA !!
   # add data from pf_performance

  rm(
    pf_performance,
    prior_num_problem,
    problem_difficulity_SE,
    student_problems,
    al2,
    assignments_complete
  )
  


# Merge for Analysis Data Sets #####

table(duplicated(prior_performance$user_id))
table(duplicated(problem_difficulity$problem_id))

ad <- tal_clean %>%
  left_join(
    taf,
    by = c("assigned_tutor_strategy_id"="tutor_strategy_id")
  ) %>%
  left_join(prior_performance,
            by = c("user_id")) %>%
  left_join(problem_difficulity , ### originally used my own calculations for problem diff measures, but had alot of missing data
            by = c("problem_id"))
  
    
  length(unique(ad$user_id)) 
  
  # we have missing problem avg accuracy data -> 5%
  table(is.na(ad$problem_difficulity_est))
  table(is.na(ad$problem_difficulity_est))/length(ad$problem_difficulity_est)
  
  table(is.na(ad$problem_difficulity_est) | ad$num_student_data_to_calc_accuray < 5)
  table(is.na(ad$problem_difficulity_est) | ad$num_student_data_to_calc_accuray < 5)/length(ad$problem_difficulity_est)
  
  table(is.na(ad[ad$randomized_with_control == 1, ]$problem_difficulity_est) | 
          ad[ad$randomized_with_control == 1, ]$num_student_data_to_calc_accuray < 5, 
        ad[ad$randomized_with_control == 1, ]$treatment)
  
  chisq.test(table(is.na(ad[ad$randomized_with_control == 1, ]$problem_difficulity_est) | 
                     ad[ad$randomized_with_control == 1, ]$num_student_data_to_calc_accuray < 5, 
                   ad[ad$randomized_with_control == 1, ]$treatment))
  
    # there is a sig difference in missingness between treatment and control
  
  # we have missing prior avg accuracy data --> 10%
  table(is.na(ad$prior_accuracy)) 
  table(is.na(ad$prior_accuracy))/length(ad$prior_accuracy)
  table(is.na(ad$prior_accuracy), ad$prior_num_problems < 5) 
  table(ad$prior_num_problems < 5)
      chisq.test(table(is.na(ad[ad$randomized_with_control == 1, ]$prior_accuracy) | 
                         ad[ad$randomized_with_control == 1, ]$prior_num_problems < 5, 
                       ad[ad$randomized_with_control == 1, ]$treatment))
      
      # there is NOT a sig difference in missingness between treatment and control
      
      
  table(is.na(ad$prior_accuracy)) 
  table(is.na(ad$prior_accuracy))/length(ad$prior_accuracy)
  table(is.na(ad$prior_accuracy), is.na(ad$prior_num_problems)) 
  
  table(is.na(ad$prior_accuracy), ad$num_exposures == 1) 
  
  ad[ad$num_exposures > 1 & is.na(ad$prior_accuracy), c("user_id")]

  ### Removing problem level missing data
  
  table(is.na(ad$problem_difficulity_est),
        is.na(ad$prior_accuracy) )/length(ad$prior_accuracy)
  # 
  # n_problems_table <- data.frame(n_problems_table) %>% filter(exclution %in% c("Raw Data", "1", "2"))
  # n_students_table <- data.frame(n_students_table) %>% filter(exclution %in% c("Raw Data", "1", "2"))
  # 
  ### EXCLUSION 3: Missing Problem Level ####
  
  ad <- ad %>%
    filter(
      (is.na(problem_difficulity_est) == F) & 
        num_student_data_to_calc_accuray > 5
    )
  
  
  #### N POST EXECUTION 3 DATA ####
  exclution <- "3"
  n_problems <- length(ad$id)
  n_tc <- length(ad[ad$randomized_with_control == 1, ]$id)
  n_bw <- length(ad[ad$randomized_with_control == 0, ]$id)
  n_problems_table1 <- cbind(exclution, n_problems, n_tc, n_bw)
  n_problems_table <- rbind(n_problems_table, n_problems_table1)
  rm(n_problems, n_tc, n_bw, n_problems_table1)
  
  colnames(n_problems_table) <- c("exclution", "n_problems", "n_tc", "n_bw")
  
  n_students <- length(unique(ad$user_id))
  n_tc <- length(unique(ad[ad$randomized_with_control == 1, ]$user_id))
  n_bw <- length(unique(ad[ad$randomized_between_tutor_strategies == 1, ]$user_id))
  n_students_table1 <- cbind(exclution, n_students, n_tc, n_bw)
  n_students_table <- rbind(n_students_table, n_students_table1)
  rm(exclution, n_students, n_tc, n_bw, n_students_table1)
  
  ### EXCLUSION 4: Missing Student Prior Accuracy Level ####
  
  ad <- ad %>%
    filter(
      (is.na(prior_accuracy) == F) &
        prior_num_problems > 5
    )
  
  
  #### N POST EXECUTION 4 DATA ####
  exclution <- "4"
  n_problems <- length(ad$id)
  n_tc <- length(ad[ad$randomized_with_control == 1, ]$id)
  n_bw <- length(ad[ad$randomized_with_control == 0, ]$id)
  n_problems_table1 <- cbind(exclution, n_problems, n_tc, n_bw)
  n_problems_table <- rbind(n_problems_table, n_problems_table1)
  rm(n_problems, n_tc, n_bw, n_problems_table1)
  
  colnames(n_problems_table) <- c("exclution", "n_problems", "n_tc", "n_bw")
  
  n_students <- length(unique(ad$user_id))
  n_tc <- length(unique(ad[ad$randomized_with_control == 1, ]$user_id))
  n_bw <- length(unique(ad[ad$randomized_between_tutor_strategies == 1, ]$user_id))
  n_students_table1 <- cbind(exclution, n_students, n_tc, n_bw)
  n_students_table <- rbind(n_students_table, n_students_table1)
  rm(exclution, n_students, n_tc, n_bw, n_students_table1)
  

  
  ### EXCLUSION 5: exclude when there are problems with few instances ####

# Explore nesting units
  
  ad_problems <- ad %>%
    ungroup() %>%
    select(problem_id) %>%
    group_by(problem_id) %>%
    mutate(num_problem_attempts = n()) %>%
    distinct()
  hist(ad_problems$num_problem_attempts, breaks = 100, xlim = c(0, 2000))
  table(ad_problems$num_problem_attempts <= 20)
  
  ad<-ad %>% 
    left_join(ad_problems,
              by = "problem_id") %>%
    filter(num_problem_attempts >10) %>%
    select(-num_problem_attempts)


#### N POST EXECUTION 5 DATA ####
exclution <- "5"
n_problems <- length(ad$id)
n_tc <- length(ad[ad$randomized_with_control == 1, ]$id)
n_bw <- length(ad[ad$randomized_with_control == 0, ]$id)
n_problems_table1 <- cbind(exclution, n_problems, n_tc, n_bw)
n_problems_table <- rbind(n_problems_table, n_problems_table1)
rm(n_problems, n_tc, n_bw, n_problems_table1)

colnames(n_problems_table) <- c("exclution", "n_problems", "n_tc", "n_bw")

n_students <- length(unique(ad$user_id))
n_tc <- length(unique(ad[ad$randomized_with_control == 1, ]$user_id))
n_bw <- length(unique(ad[ad$randomized_between_tutor_strategies == 1, ]$user_id))
n_students_table1 <- cbind(exclution, n_students, n_tc, n_bw)
n_students_table <- rbind(n_students_table, n_students_table1)
rm(exclution, n_students, n_tc, n_bw, n_students_table1)

# Save Data #### 
write.csv(ad, "TEACHassist_data2/analysis_complete_data.csv")

# separate randomized with control and randomized between exposures

ad_randomized_with_control <- ad %>%
  filter(randomized_with_control == 1)

write.csv(ad_randomized_with_control, "TEACHassist_data2/analysis_data_ad_randomized_with_control.csv")






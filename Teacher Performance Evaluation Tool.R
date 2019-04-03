####Libraries####
library(ggrepel)
library(dplyr)
library(ggplot2)
library(stringi)
library(boot)
library(devtools)
library(ggfortify)
library(cluster)
library(devtools)
library(caret)
setwd("E:/Extracted Files Dec. 13th")
####Loading Data###
cwd <- getwd()
currentdate <- Sys.Date()
####Loading Data####
disc_entry1 <- read.delim(gzfile("disc_entry1"), quote = "", sep = "\t",
                          header = FALSE, stringsAsFactors = FALSE)[,c(1,3,5,6)]
disc_entry2 <- read.delim(gzfile("disc_entry2"), quote = "", sep = "\t",
                          header = FALSE, stringsAsFactors = FALSE)[,c(1,3,5,6)]
disc_entry_full <- rbind(disc_entry1, disc_entry2)
course_score <- read.delim(gzfile("course_score_fact_new"), quote = "", sep = "\t",
                          header = FALSE, stringsAsFactors = FALSE)[,c(2,3,4,5,6,7)]
disc_topic1 <- read.delim(gzfile("disc_topic1"), quote = "", sep = "\t",
                          header = FALSE, stringsAsFactors = FALSE)[,c(1,2,3,5)]
conv_msg_part <- read.delim(gzfile("conv_msg_part1"), quote = "",sep = "\t",
                            header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
conv_msg_part2 <- read.delim(gzfile("conv_msg_part2"), quote = "",sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
conv_msg_part3 <- read.delim(gzfile("conv_msg_part3"), quote = "",sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
conv_msg_part4 <- read.delim(gzfile("conv_msg_part4"),quote = "", sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
conv_msg_part5 <- read.delim(gzfile("conv_msg_part5"), quote = "",sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
conv_msg_part6 <- read.delim(gzfile("conv_msg_part6"),quote = "", sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]

conv_msg_part_full <- rbind(conv_msg_part, conv_msg_part2, conv_msg_part3,
                            conv_msg_part4, conv_msg_part5, conv_msg_part6)

conv_dim <- read.delim(gzfile("conv_dim"), sep = "\t",quote = "", header = FALSE, 
                       stringsAsFactors = FALSE)[,c(1,6)]

convmessagedim <- read.delim(gzfile("conv_msg_dim1"), quote = "",sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,5:9)] 
convmessagedim2 <- read.delim(gzfile("conv_msg_dim2"),quote = "", sep = "\t",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,5:9)] 
convmessagedim3 <- read.delim(gzfile("conv_msg_dim3"), quote = "",sep = "\t",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,5:9)]

convmsgdim_full <- rbind(convmessagedim, convmessagedim2, convmessagedim3)
disc_entry_dim1 <- read.delim(gzfile("disc_entry_dim1"), quote = "",sep = "\t",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,5)]
disc_entry_dim2 <- read.delim(gzfile("disc_entry_dim2"), quote = "",sep = "\t",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,5)]
disc_entry_dim_full <- rbind(disc_entry_dim1, disc_entry_dim2)

disc_topic_dim <- read.delim(gzfile("disc_topic_dim"), quote = "",sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,8)]
##submission comment data##
sub_comm_fact1 <- read.delim(gzfile("sub_comm_fact1"),quote = "", sep = "\t", header = FALSE,
                             stringsAsFactors = FALSE)[,c(1,4,6)]
sub_comm_fact2 <- read.delim(gzfile("sub_comm_fact2"),quote = "", sep = "\t", header = FALSE,
                             stringsAsFactors = FALSE)[,c(1,4,6)]
sub_comm_fact_full <- rbind(sub_comm_fact1, sub_comm_fact2)

sub_comm_dim1 <-  read.delim(gzfile("sub_comm_dim1"),quote = "", sep = "\t", header = FALSE,
                             stringsAsFactors = FALSE)[,c(1,8:11)]
sub_comm_dim2 <-  read.delim(gzfile("sub_comm_dim2"),quote = "", sep = "\t", header = FALSE,
                             stringsAsFactors = FALSE)[,c(1,8:11)]
sub_comm_dim_full <- rbind(sub_comm_dim1, sub_comm_dim2)
ps_dim <- read.delim(gzfile("ps_dim"), sep = "\t",quote = "",
                     header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,7)]
ps_fact <- read.delim(gzfile("ps_fact"), sep = "\t",quote = "",
                      header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
##enrollment and user data##
enrollment_fact <- read.delim(gzfile("enrollment_fact"), sep = "\t",quote = "",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
colnames(ps_dim) <- c("pseudo_id", "user_id", "acct_id","last_login")
colnames(ps_fact) <- c("pseudo_id", "login_ct")
colnames(disc_topic_dim) <- c("Id", "created_at")
colnames(disc_entry_dim_full) <- c("Id", "created_at")
colnames(enrollment_fact) <- c("enrollment_id", "enrollment_term_id")
colnames(disc_entry_full) <- c("Id", "user_id", "course_id", "enrollment_term_id")
colnames(disc_topic1) <- c("Id", "course_id", "enrollment_term_id", "user_id")
colnames(conv_dim) <- c("conversation_id", "course_id")
colnames(sub_comm_fact_full) <- c("submission_comment_id","user_id","course_id")

colnames(sub_comm_dim_full) <- c("submission_comment_id", "comment", 
                                 "author_name", "created_at", "updated_at")

colnames(convmsgdim_full) <- c("conversation_message_id","conversation_id", "user_id",
                               "created_at", "sys.gen", "has_attachments",
                               "has_media_objects", "body")
colnames(conv_msg_part_full) <- c("conversation_message_id", "course_id")

sub_dim1 <- read.delim(gzfile("sub_dim1"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim2 <- read.delim(gzfile("sub_dim2"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim3 <- read.delim(gzfile("sub_dim3"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim4 <- read.delim(gzfile("sub_dim4"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim5 <- read.delim(gzfile("sub_dim5"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim6 <- read.delim(gzfile("sub_dim6"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim7 <- read.delim(gzfile("sub_dim7"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim8 <- read.delim(gzfile("sub_dim8"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim9 <- read.delim(gzfile("sub_dim9"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim10 <- read.delim(gzfile("sub_dim10"), sep = "\t",quote = "", header = FALSE,
                        stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim11 <- read.delim(gzfile("sub_dim11"), sep = "\t",quote = "", header = FALSE,
                        stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim12 <- read.delim(gzfile("sub_dim12"), quote = "", sep = "\t", header = FALSE,
                        stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim13 <-read.delim(gzfile("sub_dim13"), sep = "\t",quote = "", header = FALSE,
                       stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim14 <- read.delim(gzfile("sub_dim14"), sep = "\t",quote = "", header = FALSE,
                        stringsAsFactors = FALSE)[,c(6,15,19,22,26)]
sub_dim15 <- read.delim(gzfile("sub_dim15"), sep = "\t",quote = "", header = FALSE,
                        stringsAsFactors = FALSE)[,c(6,15,19,22,26)]

sub_dim_full <- rbind(sub_dim1,sub_dim2, sub_dim3, sub_dim4, sub_dim5, sub_dim6,
                      sub_dim7, sub_dim8, sub_dim9, sub_dim10, sub_dim11, sub_dim12,
                      sub_dim13, sub_dim14, sub_dim15)
enrollment_term <- read.delim(gzfile("enrollment_term_dim"),quote = "", sep = "\t", header = FALSE,
                              stringsAsFactors = FALSE)[,c(1,4)]
enrollment <- read.delim(gzfile("enrollment_dim"),quote = "", sep = "\t", header = FALSE,
                         stringsAsFactors = FALSE)[,c(6,15,16)]
courses <- read.delim(gzfile("course_dim"),quote = "", sep = "\t", header = FALSE,
                      stringsAsFactors = FALSE)[,c(1,4,5:7)]
user <- read.delim(gzfile("user_dim"),quote = "", sep = "\t",
                   header = FALSE, stringsAsFactors = FALSE)[,c(1,16)]
account <- read.delim(gzfile("account_dim"),quote = "", sep = "\t",
                      header = FALSE, stringsAsFactors = FALSE)[,c(1,3,6)]
assignment_dim <- read.delim(gzfile("assignment_dim"),quote = "", sep = "\t",
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,3,6,11)]
####Finished Loading####

####Column Naming####
colnames(course_score) <- c("canvas_id", "account_id", "course_id", "enrollment_id",
                            "current_score", "final_score")
colnames(courses) <- c("course_id", "account_id",
                       "enrollment_term_id","course_name", "code")
colnames(enrollment_term) <- c("enrollment_term_id", "name")
colnames(enrollment) <- c("type", "course_id", "user_id")
colnames(user) <- c("user_id", "sortable_name")
colnames(account) <-  c("account_id", "account_name", "parent_account")
colnames(assignment_dim) <- c("assignment_id", "course_id", "due_date", "submission_type")
colnames(sub_dim_full) <- c("submitted_at", "graded_at", "assignment_id", "user_id", "grade_state")
####Finished Column Naming####

#Get Courses for the semester#
courses_new <- merge(courses, enrollment_term, by = "enrollment_term_id")
courses_new <- merge(courses_new, account, by = "account_id")
#Get submission comments for semester#
sub_full <- merge(sub_dim_full, assignment_dim, by = "assignment_id")
submissions <- merge(sub_full, courses_new, by = "course_id")
submissions <- submissions[submissions$grade_state != "not_graded",]
#change gradetime to date and time format#
submissions$gradetime <- ifelse(as.POSIXct(submissions$graded_at, format = "%Y-%m-%d %H:%M:%OS") < as.POSIXct(submissions$submitted_at, format = "%Y-%m-%d %H:%M:%OS"), 0, round(difftime(as.POSIXct(submissions$graded_at, format = "%Y-%m-%d %H:%M:%OS"), as.POSIXct(submissions$submitted_at, format = "%Y-%m-%d %H:%M:%OS"), tz = "UTC", units = "days"),2))
submissions <- submissions[!is.na(submissions$gradetime),]
submissions <- submissions[submissions$grade_state != "auto_graded",]
#select appropriate categories and get assignments submitted#
final <- submissions %>%
  group_by(course_id, course_name, account_name, parent_account, name) %>%
  summarise(`Average Grade Time` = as.numeric(round(mean(gradetime), 2)),
            `Assignments` = n())
final$account_name <- gsub("/", " ", final$account_name)
#Assign threshold for grades#
course_score$threshold <- ifelse(course_score$current_score >= 80, 1, 0)
#Find number of students above threshold by course#
course_score <- course_score %>%
  group_by(course_id) %>%
  summarise(threshold = sum(threshold))
#change all dates to date time format#
convmsgdim_full$created_at <- as.Date(as.POSIXct(convmsgdim_full$created_at, format = "%Y-%m-%d"))
sub_comm_dim_full$created_at <- as.Date(as.POSIXct(sub_comm_dim_full$created_at, format = "%Y-%m-%d"))
disc_topic1 <- merge(disc_topic1, disc_topic_dim, by = "Id")
disc_topic1 <- merge(disc_topic1, enrollment_term, by = "enrollment_term_id")
disc_entry_full <- merge(disc_entry_full, disc_entry_dim_full, by = "Id")
disc_topic1$created_at <- as.Date(as.POSIXct(disc_topic1$created_at, format = "%Y-%m-%d"))
disc_entry_full$created_at <- as.Date(as.POSIXct(disc_entry_full$created_at, format = "%Y-%m-%d"))
#get discussion entries for semester#
disc_entry_full <- merge(disc_entry_full, enrollment_term, by = "enrollment_term_id")
#Get number of teachers in a course#
teacheronly <- enrollment[enrollment$type == "TeacherEnrollment",]
teacheronly <- teacheronly %>%
  group_by(course_id, user_id) %>%
  mutate(numteacher = n())
##obtain teacher name##
studentenrollment <- enrollment[enrollment$type == "StudentEnrollment",]
studentenrollment <- studentenrollment %>%
  group_by(course_id) %>%
  summarise(count = n())
teacherenrollment <- merge(teacheronly, user, by = "user_id")
#get interaction counts and merge proper dataframes#
subframe <- merge(sub_comm_fact_full, sub_comm_dim_full, 
                  by = "submission_comment_id")
totalsubframe <- merge(subframe, courses_new, by = "course_id")
subframe <- merge(subframe, courses_new, by = "course_id")
subframe <- subframe[subframe$user_id %in% teacherenrollment$user_id,]
totalconvframe <- merge(convmsgdim_full, conv_msg_part_full, by = "conversation_message_id")
totalconvframe <- merge(totalconvframe, courses_new, by = "course_id")
convframe <- merge(convmsgdim_full, conv_msg_part_full, by = "conversation_message_id")
convframe <- merge(convframe, courses_new, by = "course_id")
convframe <- convframe[convframe$user_id %in% teacherenrollment$user_id,]
#subset by teacher discussion entries#
teacher_disc_start <- disc_topic1[disc_topic1$user_id %in% teacherenrollment$user_id,]
teacher_disc_entry <- disc_entry_full[disc_entry_full$user_id %in% teacherenrollment$user_id,]

total_disc_start <- disc_topic1
total_disc_entry <- disc_entry_full

ps_full <- merge(ps_dim, ps_fact, by = "pseudo_id")
ps_full <- merge(ps_full, user, by = "user_id")
teacher_ps <- ps_full[!ps_full$user_id %in% teacherenrollment$user_id,]
#count interactions by course#
totalsubfirst <- totalsubframe %>%
  group_by(course_id) %>%
  summarise(totalsubcount = n())

totalconvfirst <- totalconvframe %>%
  group_by(course_id) %>%
  summarise(totalconvcount = n())

total_disc_entryfirst <- total_disc_entry %>%
  group_by(course_id) %>%
  summarise(total_disc_entry = n())

total_disc_startfirst <- total_disc_start %>%
  group_by(course_id) %>%
  summarise(total_disc_start = n())

subframefirst <- subframe %>%
  group_by(course_id) %>%
  summarise(subcount = n())

convframefirst <- convframe %>%
  group_by(course_id) %>%
  summarise(convcount = n())

teacher_disc_entryfirst <- teacher_disc_entry %>%
  group_by(course_id) %>%
  summarize(disc_count = n())

teacher_disc_startfirst <- teacher_disc_start %>%
  group_by(course_id) %>%
  summarize(disc_start = n())
#merge all interactions with course dataframes#
totals <- merge(convframefirst, subframefirst, all = TRUE)
totals <- merge(totals, teacher_disc_entryfirst, all = TRUE)
totals <- merge(totals, teacher_disc_startfirst, all = TRUE)
totals[is.na(totals)] <- 0

#generate new totals for interactions and totals for teacher interactions#
totals <- merge(totals, courses_new, by = "course_id")
totals$intcount <- totals$convcount + totals$subcount + totals$disc_count + totals$disc_start
totals$code <- gsub("\\/.*", "", totals$code)
totals$account_name <- gsub("\\/.*", "", totals$account_name)

grandtotals <- merge(totalsubfirst, totalconvfirst, by = "course_id", all = TRUE)
grandtotals <- merge(grandtotals, total_disc_entryfirst, by = "course_id", all = TRUE)
grandtotals <- merge(grandtotals, total_disc_startfirst, by = "course_id", all = TRUE)

studenttotals <- merge(totals, grandtotals, by = "course_id", all = TRUE)
studenttotals[is.na(studenttotals)] <- 0

studenttotals <- studenttotals[studenttotals$name != 0,]
studenttotals$grandtotalint <- studenttotals$totalsubcount + studenttotals$totalconvcount + studenttotals$total_disc_entry + studenttotals$total_disc_start
totals <- studenttotals


####Totaling and finding fraction of interactions####
totals$teacherfraction <- round(totals$intcount / totals$grandtotalint, 4)
totals$teacherconvfrac <- round(totals$convcount / totals$totalconvcount, 4)
totals$teachersubfrac <- round(totals$subcount / totals$totalsubcount, 4)
totals$teacherdiscstartfrac <- round(totals$disc_start / totals$total_disc_start, 4)
totals$teacherdiscentryfrac <- round(totals$disc_count / totals$total_disc_entry, 4)

totals <- merge(totals, teacherenrollment, by = "course_id")
ps_full <- ps_full %>%
  select(user_id, login_ct)
totals <- merge(totals, ps_full, by = "user_id")
totals <- totals %>%
  select(-user_id, -type, -numteacher)
totals <- merge(totals, studentenrollment, by = "course_id")
totals[is.na(totals)] <- 0
final <- ungroup(final)
final <- final %>%
  select(course_id, `Average Grade Time`, Assignments)
totals <- merge(totals, final, by = "course_id")
totals <- merge(totals, course_score, by = "course_id")
totals <- totals %>%
  select(-course_id, -account_id, -enrollment_term_id)
#final naming of columns and output of CSV#
colnames(totals) <- c("Instructor Conversation Messages", "Instructor Submission Comments", "Instructor Discussion Entries", "Instructor Discussion Posts",
                      "Course Name", "Course Code", "Semester", "Department", "College", "Total Interaction Count", 
                      "Total Submission Comment Count", "Total Conversation Message Count", "Total Discussion Entry",
                      "Total Discussion Posts", "Grand Total Interactions", "Total Teacher Fraction of Interactions", 
                      "Total Teacher Fraction of Conversation Messages", "Total Teacher Fraction of Submission Comments",
                      "Total Teacher Fraction of Discussion Entries", "Total Teacher Fraction of Discussion Posts", 
                      "Teacher Name", "Login Count", "Students", "Average Grade Time", "Assignments", "80 or Above")
totals <- totals[,c(21,5:9,26,25,23,24,22,1:4,10:20)]
write.csv(totals, "Canvas Evaluation Historical.csv", row.names = FALSE)

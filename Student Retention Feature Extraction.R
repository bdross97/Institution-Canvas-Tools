library(ggrepel)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringi)
library(boot)
library(devtools)
library(ggfortify)
library(cluster)
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(devtools)
library(caret)
library(zoo)
setwd("E:/March 1st Extracted")
#Set month function to find all interactions contained within 30 days 
#of semester start#
monthfunction <- function(dataframe) {
  dataframe <- dataframe %>%
    group_by(enrollment_term_id) %>%
    mutate(margin = as.Date(as.POSIXct(start_date, format = "%Y-%m-%d")) + 30)
  dataframe <- dataframe[dataframe$created_at >= dataframe$start_date &
                           dataframe$created_at <= dataframe$margin,]
}
#Set current enrollment and start date#
currentenrollment <- "Spring Semester 2019"
startdate <- "2019-01-07"
####Loading Data####
##converasation data##
disc_entry1 <- read.delim(gzfile("disc_entry1"), quote = "", sep = "\t",
                          header = FALSE, stringsAsFactors = FALSE)[,c(1,3,5,6)]
disc_entry2 <- read.delim(gzfile("disc_entry2"), quote = "", sep = "\t",
                          header = FALSE, stringsAsFactors = FALSE)[,c(1,3,5,6)]
disc_entry_full <- rbind(disc_entry1, disc_entry2)

disc_topic_fact <- read.delim(gzfile("disc_topic_fact"), quote = "", sep = "\t",
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
                             header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,5)] 
convmessagedim2 <- read.delim(gzfile("conv_msg_dim2"),quote = "", sep = "\t",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,5)] 
convmessagedim3 <- read.delim(gzfile("conv_msg_dim3"), quote = "",sep = "\t",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,5)]

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
                             stringsAsFactors = FALSE)[,c(1,10)]
sub_comm_dim2 <-  read.delim(gzfile("sub_comm_dim2"),quote = "", sep = "\t", header = FALSE,
                             stringsAsFactors = FALSE)[,c(1,10)]
sub_comm_dim_full <- rbind(sub_comm_dim1, sub_comm_dim2)
ps_dim <- read.delim(gzfile("ps_dim"), sep = "\t",quote = "",
                     header = FALSE, stringsAsFactors = FALSE)[,c(1,3,4,7)]
ps_fact <- read.delim(gzfile("ps_fact"), sep = "\t",quote = "",
                      header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
##enrollment and user data##
enrollment_term <- read.delim(gzfile("enrollment_term_dim"),quote = "", sep = "\t", header = FALSE,
                              stringsAsFactors = FALSE)[,c(1,4)]
enrollment <- read.delim(gzfile("enrollment_dim"),quote = "", sep = "\t", header = FALSE,
                         stringsAsFactors = FALSE)[,c(1,6,15,16)]
courses <- read.delim(gzfile("course_dim"), sep = "\t",quote = "", header = FALSE,
                      stringsAsFactors = FALSE)[,c(1,4,5:7)]
user <- read.delim(gzfile("user_dim"), sep = "\t",quote = "",
                   header = FALSE, stringsAsFactors = FALSE)[,c(1,16)]
account <- read.delim(gzfile("account_dim"), sep = "\t",quote = "",
                      header = FALSE, stringsAsFactors = FALSE)[,c(1,3,6)]
enrollment_fact <- read.delim(gzfile("enrollment_fact"), sep = "\t",quote = "",
                              header = FALSE, stringsAsFactors = FALSE)[,c(1,4)]
colnames(ps_dim) <- c("pseudo_id", "user_id", "acct_id","last_login")
colnames(ps_fact) <- c("pseudo_id", "login_ct")
colnames(disc_topic_dim) <- c("Id", "created_at")
colnames(disc_entry_dim_full) <- c("Id", "created_at")
colnames(enrollment_fact) <- c("enrollment_id", "enrollment_term_id")
colnames(disc_entry_full) <- c("Id", "user_id", "course_id", "enrollment_term_id")
colnames(disc_topic_fact) <- c("Id", "course_id", "enrollment_term_id", "user_id")
colnames(conv_dim) <- c("conversation_id", "course_id")
colnames(user) <- c("user_id", "sortable_name")
colnames(account) <-  c("account_id", "account_name", "parent_account")

colnames(sub_comm_fact_full) <- c("submission_comment_id","user_id","course_id")

colnames(sub_comm_dim_full) <- c("submission_comment_id","created_at")

colnames(convmsgdim_full) <- c("conversation_message_id","conversation_id", "user_id",
                               "created_at")
colnames(conv_msg_part_full) <- c("conversation_message_id", "course_id")


colnames(courses) <- c("course_id", "account_id",
                       "enrollment_term_id","course_name", "code")
colnames(enrollment_term) <- c("enrollment_term_id", "name")
colnames(enrollment) <- c("enrollment_id", "type", "course_id", "user_id")

####finished column naming####
#get current enrollment_id#
enrollment_term <- enrollment_term[enrollment_term$name %in% currentenrollment,]
enrollment_term$start_date <- startdate
enrollment_term$start_date <- as.Date(as.POSIXct(enrollment_term$start_date, format = "%Y-%m-%d"))
#change all datetime stamps to date format#
convmsgdim_full$created_at <- as.Date(as.POSIXct(convmsgdim_full$created_at, format = "%Y-%m-%d"))
sub_comm_dim_full$created_at <- as.Date(as.POSIXct(sub_comm_dim_full$created_at, format = "%Y-%m-%d"))
disc_topic1 <- merge(disc_topic_fact, disc_topic_dim, by = "Id")
disc_topic1 <- merge(disc_topic1, enrollment_term, by = "enrollment_term_id")
disc_entry_full <- merge(disc_entry_full, disc_entry_dim_full, by = "Id")
disc_topic1$created_at <- as.Date(as.POSIXct(disc_topic1$created_at, format = "%Y-%m-%d"))
disc_entry_full <- merge(disc_entry_full, disc_entry_dim_full, by = "Id")
disc_entry_full$created_at <- as.Date(as.POSIXct(disc_entry_full$created_at, format = "%Y-%m-%d"))
#Get enrollment and discussions for current semester#
enrollment <- merge(enrollment, enrollment_fact, by = "enrollment_id")
disc_entry_full <- merge(disc_entry_full, enrollment_term, by = "enrollment_term_id")
#obtain number of teachers in a course and number of students#
teacheronly <- enrollment[enrollment$type == "TeacherEnrollment",]
teacheronly <- teacheronly %>%
  group_by(course_id, user_id, enrollment_term_id) %>%
  mutate(numteacher = n())

students <- enrollment %>%
  group_by(type, course_id, enrollment_term_id) %>%
  summarise(count = n())
studentenrollment <- students[students$type == "StudentEnrollment",]
##obtain teacher name##
teacherenrollment <- merge(teacheronly, user, by = "user_id")
##merge courses and sub and conv frames##
courses_new <- merge(courses, enrollment_term, by = "enrollment_term_id")
courses_new <- merge(courses_new, account, by = "account_id")

subframe <- merge(sub_comm_fact_full, sub_comm_dim_full, 
                  by = "submission_comment_id")
totalsubframe <- merge(subframe, courses_new, by = "course_id")
subframe <- merge(subframe, courses_new, by = "course_id")
studentsub <- subframe[!subframe$user_id %in% teacherenrollment$user_id,]
subframe <- subframe[subframe$user_id %in% teacherenrollment$user_id,]
totalconvframe <- merge(convmsgdim_full, conv_msg_part_full, by = "conversation_message_id")
totalconvframe <- merge(totalconvframe, courses_new, by = "course_id")
convframe <- merge(convmsgdim_full, conv_msg_part_full, by = "conversation_message_id")
convframe <- merge(convframe, courses_new, by = "course_id")
studentconv <- convframe[!convframe$user_id %in% teacherenrollment$user_id,]
convframe <- convframe[convframe$user_id %in% teacherenrollment$user_id,]
#Extract teacher discussion entries and topics#
teacher_disc_start <- disc_topic1[disc_topic1$user_id %in% teacherenrollment$user_id,]
teacher_disc_entry <- disc_entry_full[disc_entry_full$user_id %in% teacherenrollment$user_id,]
student_disc_start <- disc_topic1[!disc_topic1$user_id %in% teacherenrollment$user_id,]
student_disc_entry <- disc_entry_full[!disc_entry_full$user_id %in% teacherenrollment$user_id,]

total_disc_start <- disc_topic1
total_disc_entry <- disc_entry_full
#Apply month function to all data frames to subset proper data#
totalconvfirst <- monthfunction(totalconvframe)
totalsubfirst <- monthfunction(totalsubframe)
total_disc_entryfirst <- monthfunction(total_disc_entry)
total_disc_startfirst <- monthfunction(total_disc_start)

convframefirst <- monthfunction(convframe)
subframefirst <- monthfunction(subframe)
teacher_disc_startfirst <- monthfunction(teacher_disc_start)
teacher_disc_entryfirst <- monthfunction(teacher_disc_entry)

studentconvfirst <- monthfunction(studentconv)
studentsubfirst <- monthfunction(studentsub)
student_disc_entryfirst <- monthfunction(student_disc_entry)
student_disc_startfirst <- monthfunction(student_disc_start)
#get login counts#
ps_full <- merge(ps_dim, ps_fact, by = "pseudo_id")
ps_full <- merge(ps_full, user, by = "user_id")

####Month Interaction Counting####
totalsubfirst <- totalsubfirst %>%
  group_by(course_id) %>%
  summarise(totalsubcount = n())

totalconvfirst <- totalconvfirst %>%
  group_by(course_id) %>%
  summarise(totalconvcount = n())

total_disc_entryfirst <- total_disc_entryfirst %>%
  group_by(course_id) %>%
  summarise(total_disc_entry = n())

total_disc_startfirst <- total_disc_startfirst %>%
  group_by(course_id) %>%
  summarise(total_disc_start = n())

subframefirst <- subframefirst %>%
  group_by(course_id) %>%
  summarise(subcount = n())

studentconvfirst <- studentconvfirst %>%
  group_by(course_id, user_id) %>%
  summarise(student_convcount = n())

studentsubfirst <- studentsubfirst %>%
  group_by(course_id, user_id) %>%
  summarise(student_subcount = n())

student_disc_entryfirst <- student_disc_entryfirst %>%
  group_by(course_id, user_id) %>%
  summarize(student_disc_count = n())

student_disc_startfirst <- student_disc_startfirst %>%
  group_by(course_id, user_id) %>%
  summarize(student_disc_start = n())

convframefirst <- convframefirst %>%
  group_by(course_id) %>%
  summarise(convcount = n())

teacher_disc_entryfirst <- teacher_disc_entryfirst %>%
  group_by(course_id) %>%
  summarize(disc_count = n())

teacher_disc_startfirst <- teacher_disc_startfirst %>%
  group_by(course_id) %>%
  summarize(disc_start = n())
#Merging total data frames to find interactions#
totals <- merge(convframefirst, subframefirst, all = TRUE)
totals <- merge(totals, teacher_disc_entryfirst, all = TRUE)
totals <- merge(totals, teacher_disc_startfirst, all = TRUE)
totals[is.na(totals)] <- 0


totals <- merge(totals, courses_new, by = "course_id")
totals <- merge(totals, studentenrollment, by = "course_id")
totals$intcount <- totals$convcount + totals$subcount + totals$disc_count + totals$disc_start
totals$intperstudent <- round(totals$intcount / totals$count,2)
totals$code <- gsub("\\/.*", "", totals$code)
totals$account_name <- gsub("\\/.*", "", totals$account_name)

grandtotals <- merge(totalsubfirst, totalconvfirst, by = "course_id", all = TRUE)
grandtotals <- merge(grandtotals, total_disc_entryfirst, by = "course_id", all = TRUE)
grandtotals <- merge(grandtotals, total_disc_startfirst, by = "course_id", all = TRUE)
grandtotals[is.na(grandtotals)] <- 0
studentsubtotals <- merge(student_disc_entryfirst, student_disc_startfirst, by = c("course_id", "user_id"), all = TRUE)
studenttotals <- merge(studentsubfirst, studentconvfirst, by = c("course_id", "user_id"), all = TRUE)
studenttotals <- merge(studenttotals, studentsubtotals, by = c("course_id", "user_id"), all = TRUE)
studenttotals[is.na(studenttotals)] <- 0
studenttotals$course_id <- gsub(".*N.*", 0, studenttotals$course_id)
studenttotals$user_id <- gsub(".*N.*", 0, studenttotals$user_id)
studenttotals <- studenttotals[studenttotals$course_id != 0,]
studenttotals <- studenttotals[studenttotals$user_id != 0,]
studenttotals$studentintcount <- studenttotals$student_convcount + studenttotals$student_disc_count + studenttotals$student_disc_start + studenttotals$student_subcount

studenttotals <- merge(studenttotals, grandtotals, by = "course_id", all = TRUE)
studenttotals[is.na(studenttotals)] <- 0
studenttotals <- studenttotals[studenttotals$user_id != 0,]

studenttotals <- merge(studenttotals, totals, by = "course_id", all = TRUE)
studenttotals <- studenttotals %>%
  select(-start_date)
studenttotals[is.na(studenttotals)] <- 0

studenttotals <- studenttotals[studenttotals$name != 0,]
studenttotals$grandtotalint <- studenttotals$totalsubcount + studenttotals$totalconvcount + studenttotals$total_disc_entry + studenttotals$total_disc_start
totals <- studenttotals


####.####
#Find fraction of interactions by instructor and student#
totals$studentfraction <- round(totals$studentintcount / totals$grandtotalint, 4)
totals$teacherfraction <- round(totals$intcount / totals$grandtotalint, 4)
totals$teacherconvfrac <- round(totals$convcount / totals$totalconvcount, 4)
totals$studentconvfrac <- round(totals$student_convcount / totals$totalconvcount, 4)
totals$teachersubfrac <- round(totals$subcount / totals$totalsubcount, 4)
totals$studentsubfrac <- round(totals$student_subcount / totals$totalsubcount)
totals$teacherdiscstartfrac <- round(totals$disc_start / totals$total_disc_start, 4)
totals$studentdiscstartfrac <- round(totals$student_disc_start / totals$total_disc_start, 4)
totals$teacherdiscentryfrac <- round(totals$disc_count / totals$total_disc_entry, 4)
totals$studentdiscentryfrac <- round(totals$student_disc_count / totals$total_disc_entry, 4)
#Feature extraction#
totals <- totals %>%
  group_by(course_id) %>%
  mutate(minstudentint = min(studentintcount),
         maxstudentint = max(studentintcount),
         medianstudentint = median(studentintcount),
         meanabsdev = mad(studentintcount),
         meanstudentint = mean(studentintcount),
         minstudentconv = min(student_convcount),
         maxstudentint = max(student_convcount),
         medianstudentconv = median(student_convcount),
         meanabsdevconv = mad(student_convcount),
         meanstudentconv = mean(student_convcount),
         minstudentsub = min(student_subcount),
         maxstudentsub = max(student_subcount),
         medianstudentsub = median(student_subcount),
         meanabsdevsub = mad(student_subcount),
         meanstudentsub = mean(student_subcount),
         minstudentdiscentry = min(student_disc_count),
         maxstudentdiscentry = max(student_disc_count),
         medianstudentdiscentry = median(student_disc_count),
         meanabsdevdiscentry = mad(student_disc_count),
         meanstudentdiscentry = mean(student_disc_count),
         minstudentdiscstart = min(student_disc_start),
         maxstudentdiscstart = max(student_disc_start),
         medianstudentdiscstart = median(student_disc_start),
         meanabsdevdiscstart = mad(student_disc_start),
         meanstudentdiscstart = mean(student_disc_start))
totals <- totals %>%
  select(-enrollment_term_id.x, -type, -account_id)
#Looping to determine proper interaction threshold for average in each college#
finalfinal <- totals
finalfinal$level <- stri_extract_first_regex(finalfinal$course_name, "[0-9]")
finalfinal <- finalfinal[finalfinal$user_id != 0,]
finalfinal$level <- paste0(finalfinal$level, "000")
deptlist <- unique(finalfinal$account_name)
levellist <- unique(finalfinal$level)
meandf <- NULL
#Utilising bootstrapping to find standard error around mean for each college
for (i in 1:length(deptlist)) {
  df <- finalfinal[finalfinal$account_name == deptlist[i],]
  account_name <- deptlist[i]
  for(i in 1:length(levellist)) {
    df2 <- df[df$level == levellist[i],]
    avgint <- round(mean(df2$intperstudent),2)
    if(nrow(df2) > 0){
      level <- levellist[i]
      boot1 <- boot(df2$intperstudent, function(x, index){mean(x[index])}, R = 1000)
      seint <- round(sd(boot1$t),2)
      meandf <- rbind(meandf, data.frame(account_name,level, avgint, seint))
    }
  }
}
agg <- meandf
finalfinal <- merge(finalfinal, agg, by = c("account_name", "level"))
finalfinal$avgintfloor <- finalfinal$avgint - finalfinal$seint
finalfinal$avgintbin <- ifelse(finalfinal$intperstudent >= finalfinal$avgintfloor, 1, 0)
binarydf <- finalfinal %>% 
  select(account_name, parent_account, name, code, course_id, avgintbin)
binarydf2 <- binarydf[!duplicated(binarydf),]
newdf <- merge(totals, binarydf2, by = "course_id")
newdf <- newdf %>%
  group_by(user_id) %>%
  mutate(avgintsum = sum(avgintbin))
newdf2 <- newdf %>%
  select(-course_id, -code.x, -name.x, -account_name.x, -parent_account.x, 
         -account_name.y, -parent_account.y, -name.y, -code.y,  -course_name)
newdf2 <- newdf2 %>%
  group_by(user_id) %>%
  summarise_all(.funs = funs(mean, min, max))
logins <- ps_full %>%
  select(login_ct, user_id)
newdf2 <- merge(newdf2, logins, by = "user_id", all = TRUE)
newdf2 <- newdf2[!is.na(newdf2$student_subcount_mean),]
newdf2$login_ct[is.na(newdf2$login_ct)] <- 0
newdf2[is.na(newdf2)] <- 0
write.csv(newdf2, "First Month Subset Final.csv", row.names = F)

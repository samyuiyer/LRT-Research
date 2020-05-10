df = read.csv("ECE203_2020_Spring.csv")
no_tutor = df[(df$Student_Response_Subtype != "tutor-performed"),]
filtered_df = no_tutor[no_tutor$Step_Name != "done ButtonPressed" 
                       & no_tutor$Step_Name != "doneReading ButtonPressed" 
                       & no_tutor$Step_Name != "doneReading1 ButtonPressed" 
                       & no_tutor$Step_Name != "doneReading2 ButtonPressed",]
filtered_df = filtered_df[filtered_df$Problem_Name != "4" 
                          & filtered_df$Problem_Name != "5" 
                          & filtered_df$Problem_Name != "1_1" 
                          & filtered_df$Problem_Name != "1_2" 
                          & filtered_df$Problem_Name != "2_2" 
                          & filtered_df$Problem_Name != "2_3" 
                          & filtered_df$Problem_Name != "3_1" 
                          & filtered_df$Problem_Name != "3_2" 
                          & filtered_df$Problem_Name != "3_3" 
                          & filtered_df$Problem_Name != "3_4" 
                          & filtered_df$Problem_Name != "get_student_id" 
                          & filtered_df$Problem_Name != "intro_1" 
                          & filtered_df$Problem_Name != "intro_2",]

filtered_df = filtered_df[filtered_df$Level_ProblemSet != "Daylight Problems"
                          & filtered_df$Level_ProblemSet != "Signals Tutor - Daylight Problems Pilot 5 - Daylight",]

stud_IDs = unique(filtered_df$Anon_Student_Id)
num_correct<-c()
for(id in stud_IDs){
  len = length((filtered_df[filtered_df$Anon_Student_Id == id,]$Outcome == "CORRECT")[(filtered_df[filtered_df$Anon_Student_Id == id,]$Outcome == "CORRECT") == TRUE])
  #print(len)
  num_correct<-c(num_correct, len)
}
length(num_correct)
#num_correct = data.frame(num_correct[num_correct >= 100])
tutor_data = data.frame(stud_IDs, num_correct)
nrow(tutor_data[tutor_data$num_correct == 287,])
#student with 287 -- Stu_17ddcb3005b094160d5053d3ed0aa797	
#student with 289 -- 	Stu_19a341e455b92c499727e32433258af8

student287 = filtered_df[filtered_df$Anon_Student_Id == "Stu_17ddcb3005b094160d5053d3ed0aa797",]
student289 = filtered_df[filtered_df$Anon_Student_Id == "Stu_19a341e455b92c499727e32433258af8",]
student287_prob = unique(student287$Problem_Name)
student289_prob = unique(student289$Problem_Name)

dates = (strsplit(as.character(filtered_df$Time), ' '))
length(dates)
nrow(filtered_df)
for(i in 1:59329){
  filtered_df$mdate = as.POSIXlt(as.Date((strsplit(as.character(filtered_df$Time), ' '))[[i]][1], format = '%m/%d/%y'), format = '%m/%d/%y %hour:%min:%sec')
}

filtered_df$mdate = (strsplit(as.character(filtered_df$Time), ' '))
filtered_df$mdate =as.POSIXlt(as.Date(filtered_df$mdate, format = '%m/%d/%y'))

(filtered_df$mdate)

filtered_df[2,]
as.POSIXlt(as.Date((strsplit(as.character(filtered_df$Time), ' '))[[5]][1], format = '%m/%d/%y'), format = '%m/%d/%y %hour:%min:%sec')
filtered_df[3,]$mdate
as.POSIXlt(filtered_df[3,]$mdate, )
#filtered_df[filtered_df$mdate$monn

d1 = as.Date(dates[[1]][1], format = '%m/%d/%y')
d
filtered_df$Time[[1]][1]
d2 = as.POSIXlt(as.Date(dates[[1]][1], format = '%m/%d/%y'), format = '%m/%d/%y %hour:%min:%sec')
d2$mon

d1
d2$mday
d2$mon
filtered_df$Time[1]
nrow(tutor_data[tutor_data$num_correct < 287,])


















# Test Items grading script

df = read.csv("ECE203_2020_Spring_identified test accounts.csv")

# filter irrelevant roles
df = df[df$Role == "student",]


# IF we wanted to sort by time in tool_event_time, uncomment the line below!
#df = df[order(df$CF..tool_event_time.), ]


# list of all unique student IDs
students = unique(df$Anon.Student.Id)

# list of correct answers for future comparisons
correct_answers = c("target_4:", "target_1:", "target_0:", "target_2:", "target_4:", "target_0:", "target_1:", "target_1:", "target_2:", "target_0:", "target_2:", "target_0:", "target_1:", "target_3:", "target_4:", "target_2:", "target_2:", "target_4:", "target_0:", "target_2:", "target_1:")

# setting up data frames for later
ans_df = data.frame()
binary_df = data.frame()

# list of problem names
problem_names = c("A1a", "A1b", "A2a", "A2b", "A2c", "A3a", "A4a", "A4b", "A4c", "A4d", "A5a", "Ab5", "A5c", 
                  "A6a", "A6b", "A6c", "A7a", "A8a", "A9a", "A10a", "A10b")

target_indices = c()

correct_counts = c()
for(student in students){
  corr_count = 0

  # narrow to just this student's data
  student_df = df[df$Anon.Student.Id == student,]

  # Why is this commented out?
  #student_df = student_df[student_df$Problem.Name != "A0"]

  # which() gives indices which are TRUE, so this gives us indices where the Selection is "radio_button_group"
  # Ultimately, it looks like this will be a list of indices for the student's *actual* answer.
  new_indices = which(student_df$Selection == "radio_button_group")
  
  # The below doesn't take tool_event_time into account right now.
  # The rows are not sorted by tool_event_time, so this is probably not correct.
  # Probably want to sort by tool_event_time and then use unique(fromLast=TRUE) with fromLast argument.

  # ? Compare all un-equal indices and eliminate duplicates, leaving the last one?
  # NOTE R's for-in loop uses a copy of the array. 
  # The outer array will not change, but the inner array will update each time.
  for(index in new_indices){
    for(index2 in new_indices){
      if(index2 != index){
        # if eg "D1" == "D2" and "target_4:" != "-1"
        # ? Not sure why -1 is compared, I don't see any -1 values for Input when Selection is radio_button_group
        if(student_df$Problem.Name[index] == student_df$Problem.Name[index2] & (student_df$Input[index2] != "-1")){
          # We remove this index from new_indices.
          new_indices = new_indices[new_indices != index]
        }
      }
    }
  }
  
  # Ultimately, we compare the student's answer with the correct answer.
  ans_vec = c()
  binary_vec = c()
  for(i in 1:21){
    if(student_df$Input[new_indices][i] == "target_0:"){
      target = 1
    }
    if(student_df$Input[new_indices][i] == "target_1:"){
      target = 2
    }
    if(student_df$Input[new_indices][i] == "target_2:"){
      target = 3
    }
    if(student_df$Input[new_indices][i] == "target_3:"){
      target = 4
    }
    if(student_df$Input[new_indices][i] == "target_4:"){
      target = 5
    }
    ans_vec = append(ans_vec, (target))
    if(correct_answers[i] == student_df$Input[new_indices][i]){
      corr_count = corr_count+1
      binary_vec = append(binary_vec, 1)
    }
    else{
      binary_vec = append(binary_vec, 0)
    }
  }
  ans_df <- rbind(ans_df, "student" = ans_vec)
  binary_df <- rbind(binary_df, binary_vec)
  correct_counts = c(correct_counts, corr_count)
}
final_df = data.frame(students, correct_counts)

rownames(ans_df) <- students
colnames(ans_df) <- problem_names

rownames(binary_df) <- students
colnames(binary_df) <- problem_names

student_df$Input[new_indices][i]

write.csv(final_df, "/Users/samyuiyer/Desktop/Reseach LRT/StudentCorrectCounts.csv")
write.csv(ans_df, "/Users/samyuiyer/Desktop/Reseach LRT/AllStudentAnswers.csv")
write.csv(binary_df, "/Users/samyuiyer/Desktop/Reseach LRT/BinaryStudentAnswers.csv")


# > A = c(1, 2, 1, 3, 3, 4, 5, 4, 6, 6)
# > unique(A, fromLast=TRUE)
# [1] 2 1 3 5 4 6


# > A = c(1, 2, 3, 4, 5, 6)
# > for (i in A) { A=c(0); print(i)}
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5
# [1] 6
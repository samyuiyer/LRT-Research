df = read.csv("ECE203_2020_Spring_identified test accounts.csv")
df = df[df$Role == "student",]

df[order(df$CF..tool_event_time.), ]
students = unique(df$Anon.Student.Id) #list of all unique student IDs
correct_answers = c("target_4:", "target_1:", "target_0:", "target_2:", "target_4:", "target_0:", "target_1:", "target_1:", "target_2:", "target_0:", "target_2:", "target_0:", "target_1:", "target_3:", "target_4:", "target_2:", "target_2:", "target_4:", "target_0:", "target_2:", "target_1:")
questions = c(1:21)
student_resp <- matrix(ncol=21, nrow=length(students))
problem_names = unique(df$Problem.Name)
problem_names = problem_names[problem_names != "A0"]
length(problem_names)

target_indices = c()
correct_counts = c()

for(s in 1:length(students)){
  corr_count = 0
  student_df = df[df$Anon.Student.Id == student,]
  new_indices = which(student_df$Selection == "radio_button_group")
  for(index in new_indices){
    for(index2 in new_indices){
      if(index2 != index){
        if(student_df$Problem.Name[index] == student_df$Problem.Name[index2] & (student_df$Input[index2] != "-1")){
          new_indices = new_indices[new_indices != index]
        }
      }
    }
  }
  
    for(i in 1:21)
      student_resp[s,i] = student_df$Input[new_indices][i]
  
  for(i in 1:21){
    if(correct_answers[i] == student_df$Input[new_indices][i]){
      corr_count = corr_count+1
    }
  }
  correct_counts = c(correct_counts, corr_count)
  
}
print(correct_counts)
final_df = data.frame(students, correct_counts)
#write.csv(final_df, "/Users/samyuiyer/Desktop/Reseach LRT/CorrectAnswers.csv")

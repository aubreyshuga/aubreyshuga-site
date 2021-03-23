##--------------- DATA WRANGLING ---------------##

library(tidyverse)
library(readxl)

#setwd(here::here())
#Read in the data from the client's excel sheet
Mentored <- read_excel("StudentGPAs.xlsx", sheet = "Tier 3 Mentored", skip = 1)
Control <- read_excel("StudentGPAs.xlsx", sheet = "Tier 3 Control", skip = 1)
Referrals <- read_excel("StudentGPAs.xlsx", sheet = "Referrals", skip = 1)


#Add a column to specify which group that student belongs to.
Mentored <- Mentored %>%
  mutate("Group" = "Mentored")

Control <- Control %>%
  mutate("Group" = "Control")

Referrals <- Referrals %>%
  mutate("Group" = "Mentored")


#Combine both groups into one data frame
Students <- rbind(Mentored, Control, Referrals) %>%
  rename(semester_mentored = `Semester Mentored...1`,
         `Pre_1` = `Pre 1`, 
         `During` = `Semester Mentored...7`, 
         `Post_1` = `Post 1`, 
         `Post_2` = `Post 2`,
         `Student_Type` = `Student Type`)




#Remove rows that do not have any actual mentee info
Students <- Students %>%
  filter(`Mentee Name` != "NA")

#Reorder the status values so they are in the order we want for plots
Students <- Students %>%
  mutate(Status = fct_relevel(Status,
                              "Attempted",
                              "Fulfilled Program",
                              "Dropped",
                              "Referred to HJG/LS",
                              "Active Mentoring"))


## Type Sizes ##

#Number of students in each Type
sample_size_type = Students %>%
  group_by(Student_Type) %>%
  summarise(Type_size = n())

#Number of students in each Type/semester mentored
sample_size_type_sem = Students %>%
  group_by(Student_Type, semester_mentored) %>%
  summarise(Type_sem_size = n())



## Group Sizes ##

#Number of students in each Group
sample_size_group = Students %>%
  group_by(Group) %>%
  summarise(Group_size = n())

#Number of students in each Group/semester mentored
sample_size_group_sem = Students %>%
  group_by(Group, semester_mentored) %>%
  summarise(Group_sem_size = n())



## Status Sizes ##

#Number of students in each Status
sample_size_status = Students %>%
  group_by(Status) %>%
  summarise(Status_size = n())

#Number of students in each Status/semester mentored
sample_size_status_sem = Students %>%
  group_by(Status, semester_mentored) %>%
  summarise(Status_sem_size = n())



#### Add sample size labels to my dataset ####

Students <- Students %>%
  
  ## Student Type ##
  
  #Add on Type sizes
  left_join(sample_size_type) %>%
  
  #make new column with Type size labels
  mutate(Type_size_label = paste0(Student_Type, "\n", "n = ", Type_size)) %>%
  
  #Add on Type size / semester mentored
  left_join(sample_size_type_sem) %>%
  
  #make new column with Type size/semester mentored labels
  mutate(Type_sem_size_label = paste0(Student_Type, "\n", semester_mentored, "\n", "n = ", Type_sem_size)) %>%
  
  
  ## Group ##
  
  #Add on Group sizes
  left_join(sample_size_group) %>%
  
  #make new column with Group size labels
  mutate(Group_size_label = paste0(Group, "\n", "n = ", Group_size)) %>%
  
  #Add on Groups size / semester mentored
  left_join(sample_size_group_sem) %>%
  
  #make new column with Group size/semester mentored labels
  mutate(Group_sem_size_label = paste0(Group, "\n", semester_mentored, "\n", "n = ", Group_sem_size)) %>%
  
  
  ## Status ##
  
  #Add on Status sizes
  left_join(sample_size_status) %>%
  
  #make new column with Status size labels
  mutate(Status_size_label = paste0(Status, "\n", "n = ", Status_size)) %>%
  
  #reorder the size labels to match the status labels
  #https://stackoverflow.com/questions/10758243/sort-a-factor-based-on-value-in-one-or-more-other-columns
  mutate(Status_size_label = factor(
    Status_size_label, 
    levels = unique(Status_size_label[order(Status,Status_size_label)]), 
    ordered = TRUE)
    ) %>%
  
  #Add on Status sizes / semester mentored
  left_join(sample_size_status_sem) %>%
  
  #make new column with Status size labels
  mutate(Status_sem_size_label = paste0(Status, "\n", semester_mentored, "\n", "n = ", Status_sem_size))



### Remove the sample size datasets to clean up my environment ###
rm(sample_size_type,
   sample_size_type_sem,
   sample_size_group,
   sample_size_group_sem,
   sample_size_status,
   sample_size_status_sem)

#Now I want to just keep the columns that are useful to me
Students_new <- Students %>%
  select(Pre_1,
         During,
         Post_1,
         Post_2,
         semester_mentored,
         Student_Type,
         Group,
         Status,
         Type_size_label,
         Type_sem_size_label,
         Group_size_label,
         Group_sem_size_label,
         Status_size_label,
         Status_sem_size_label)

# Wide format data with a new column that is the change in GPA from pre1 to post 1
Students_wide <- Students_new %>%
  mutate(Change = Post_1 - Pre_1)

#We want pivot the data so that each row is only one GPA measurement (one student+one semester)
Students_long <- Students_new  %>%
  pivot_longer(cols = c(`Pre_1`, `During`, `Post_1`, `Post_2`), names_to = "semester", values_to = "GPA")

#Reorder the semester values so they are in the right order
Students_long <- Students_long %>%
  mutate(semester = fct_relevel(semester,
                                "Pre_1",
                                "During",
                                "Post_1",
                                "Post_2"))

## Because most students did not attend the `Post_2` semester, I do not want to include it in this analysis. It would be interesting to use for further analysis after collecting more data.

Students_wide <- Students_wide %>%
  select(-("Post_2"))

Students_long <- Students_long %>%
  filter(semester != "Post_2")

## For our hypothesis testing, we want to only compare Tier 3 students with each other

Tier3_wide <- Students_wide %>%
  filter(Student_Type == "Tier 3")
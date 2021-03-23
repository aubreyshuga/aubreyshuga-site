##--------------- RAINCLOUDPLOTS DATAFRAMES AND PLOTS ---------------##

#source("wrangling.R")
source("colors.R")

library(raincloudplots)

# Create Mentored Pre-Post 1x1
Mentored_pre_post <- data_1x1( 
  array_1 = Mentored$`Pre 1`, #first set of values
  array_2 = Mentored$`Post 1`, #second set of values
  jit_distance = .09,
  jit_seed = 321) 
# Mentored Pre-Post 1x1 raincloud plot - ties
M_pre_post_ties <- raincloud_1x1_repmes(
  data = Mentored_pre_post,
  colors = (c(m_pre, m_post)), 
  fills = (c(m_pre, m_post)), 
  line_color = 'gray',
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Pre 1", "Post 1"), limits=c(0, 3)) +
  xlab("Semester") + 
  ylab("GPA") +
  labs(title = "Pre and Post GPAs of Mentored Students") +
  theme_classic()


# Create Control Pre-Post 1x1
Control_pre_post <- data_1x1( 
  array_1 = Control$`Pre 1`, #first set of values
  array_2 = Control$`Post 1`, #second set of values
  jit_distance = .09,
  jit_seed = 321) 
# Control Pre-Post 1x1 raincloud plot - ties
C_pre_post_ties <- raincloud_1x1_repmes(
  data = Control_pre_post,
  colors = (c(c_pre, c_post)), 
  fills = (c(c_pre, c_post)), 
  line_color = 'gray',
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Pre 1", "Post 1"), limits=c(0, 3)) +
  xlab("Semester") + 
  ylab("GPA") +
  labs(title = "Pre and Post GPAs of Control Students") +
  theme_classic()

# Create Whole Pre-Post 1x1
Whole_pre_post <- data_1x1( 
  array_1 = Students$`Pre_1`, #first set of values
  array_2 = Students$`Post_1`, #second set of values
  jit_distance = .09,
  jit_seed = 321) 
# Whole Pre-Post 1x1 raincloud plot - ties
W_pre_post_ties <- raincloud_1x1_repmes(
  data = Whole_pre_post,
  colors = (c("purple", "purple4")), 
  fills = (c("purple", "purple4")), 
  line_color = 'gray',
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Pre 1", "Post 1"), limits=c(0, 3)) +
  xlab("Semester") + 
  ylab("GPA") +
  labs(title = "Pre and Post GPAs of All Tier 3 Students in the Dataset") +
  theme_classic()

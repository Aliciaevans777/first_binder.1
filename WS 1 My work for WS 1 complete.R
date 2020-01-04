install.packages("tidyverse")
install.packages("gganimate")
install.packages("NNHANES")

library(tidyverse)
library(NHANES)
colnames(NHANES)
nrow(NHANES)
head(NHANES)

NHANES %>%
  select(ID) %>%
  n_distinct()

NHANES_tidied <- NHANES %>%
  distinct(ID, .keep_all = TRUE)
nrow(NHANES_tidied)
#so have reMoved duplicates, new data set NHANES tidies ony has the 6779 distinct rows
NHANES_tidied
#Histogram BMI
NHANES_tidied %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(bins = 100, na.rm = TRUE)
#na.rm tells R to ignore rows with no data
#Does BMI vary with educatio level
#Median BMI for each education group
NHANES_tidied %>%
  group_by(Education) %>%
  summarise(median = median(BMI, na.rm = TRUE))
#make sure TRUE is capitalised
#na.rm removes missing values frm calculation
#is.na removes missing data from graph
#Filter allows us to only use cases where BMI is not misiing AND education is not missing
#so ggplot has no data sets with missing key variables
#Rearranged order the seperate boxplots are shown, based on median BMI
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>%
  ggplot(aes(x = fct_reorder(Education, BMI, median), y = BMI, colour = Education)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5) +
  
  labs(title = "Examining the effect of education level on median BMI", 
       x = "Education level",
       y = "BMI")

#Can use facet wrap to plot histograms of BMI seperately
NHANES_tidied %>%
  filter(!is.na(Education)) %>%
  group_by(Education) %>%
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram() +
  guides(fill = FALSE) +
  labs(title = "Examining the effect of educational level on BMI",
       x = "BMI",
       y = "Number of cases") +
  facet_wrap(~ Education, scales = "free")

#different scales make easier comparisons
#Instead of generating histograms using a count; usea density function, with a density curve
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>%
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = FALSE) + 
  labs( title = "Examining the effect of education level on BMI",
        x = "BMI",
        y = "Density") +
  facet_wrap(~ Education)
#The following code return if people work and their BMI into the console
NHANES_tidied %>%
  group_by(Work) %>%
  summarise(median = median(BMI, na.rm = TRUE))
#Can see that those looking for work have the highest BMI. Those working have the lowest
#Now group by education and work, returns into console
NHANES_tidied %>%
  group_by(Education, Work) %>%
  summarise(median = median(BMI, na.rm = TRUE))
#Hard to read so will filter out cases of missing data[NA]
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Work)) %>%
  group_by(Education, Work) %>%
  summarise(median = median(BMI, na.rm = TRUE))
#So have excluded data sets where had no data for our two factors
#Order from highest median BMI using arrange()
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Work)) %>%
  group_by(Education, Work) %>%
  summarise(median = median(BMI, na.rm = TRUE)) %>%
  arrange(desc(median))

#Those with the highest BMI have some college education and are looking for jobs
#Lowest BMI's are for college grads in work

#Graph this:using filter() so only include sets not missing Work AND not missing education AND not missing BMI
#Boxplot, arrange order of boxplot by based on median BMI, coord flip flips the axies for readability
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI)) %>%
  ggplot(aes(x = fct_reorder(Education:Work, BMI, median),
             y = BMI,
             colour = Education:Work)) +
  geom_boxplot() +
  coord_flip() +
  guides(colour = FALSE) +
  labs(title = "Examining the effect of education level and employment \n status on BMI",
       x = "Education X Working",
       y = "BMI")
#Hard to see trends, so simplify by lookng at work first
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI)) %>%
  ggplot(aes(x = fct_reorder(Work, BMI, median),
             y = BMI, 
             colour = Work)) +
  geom_boxplot() +
  coord_flip() +
  guides(colour = FALSE) +
  labs(title = "Examining the effect of employment status on BMI",
       X = "Working",
       y = "BMI")
#As we can see thoe looking for work have the highest BMI's and in work have the lowest
#Now have a look at median BMI boxplots grouped by education
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI)) %>%
  ggplot(aes(x = fct_reorder(Education, BMI, median),
             y = BMI,
             colour = Education)) +
  geom_boxplot() +
  coord_flip() +
  guides(colour = FALSE) + 
  labs(title = "Examining the effect of education level on BMI",
       x = "Education",
       y = "BMI")

#cOLOUR = x - means diff colour for the diff types of x
#Lowest median BMI group is the college grad

#Investigate a new variable: race
#How many race variables, rem %>% means 'and then'
NHANES_tidied %>%
  select(Race1) %>%
  distinct()
#So 5 races, filter out the other category. Plot BMI as a function of race
#Use != not equal to other to remove the variable
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI) & Race1 != "Other") %>%
  ggplot(aes(x = fct_reorder(Race1, BMI, median),
             y = BMI, 
             colour = Race1)) +
  geom_boxplot() +
  guides(colour = FALSE) +
  labs(title = "Examining Race and BMI",
       x = "Race",
       y = "BMI")
#Can see that Black people have the highest BMI
#BMI plot against gender
NHANES_tidied %>%
  select(Gender) %>%
  distinct()
#Two genders
#Plot vs BMI

NHANES_tidied %>% 
  filter(!is.na(BMI) & !is.na(Gender)) %>%
  ggplot(aes(x = fct_reorder(Gender, BMI, median),
             y = BMI, 
             colour = Gender)) +
  geom_boxplot() + 
  guides(colour = FALSE) +
  labs(title = "Examining Gender and BMI",
       x = "Gender", 
       y = "BMI") 

#Hence in this data Males tended to have higher BMI's
#How does race and gender affect BMI
NHANES_tidied %>% 
  filter(!is.na(BMI) & !is.na(Gender)) %>%
  ggplot(aes(x = fct_reorder(Race1:Gender, BMI, median),
             y = BMI, 
             colour = Race1:Gender)) +
  geom_boxplot() + 
  guides(colour = FALSE) +
  labs(title = "Examining Gender and Race influence on BMI",
       x = "Race1:Gender", 
       y = "BMI") 
facet_wrap(~ Race1:Gender)

#So black females tend to have the highest BMI
#Can't seem to get facet wrap to work

#Would need a bar chart for investigating gender differences in education
#Could return a tibble
NHANES_tidied %>%
  filter(!is.na(Education)) %>%
  group_by(Gender, Education)
#tried, maybe ask: want a boxplot of seperate education groups, both genders in each with y varibale of number of each gender

#Now for some animation
library(gganimate)
install.packages("gifski")
install.packages("png")
#Plot histograms of BMI animated by age decade. The trasition states function defines the operation
#of our animate with ease function defining how the frames transition
NHANES_tidied %>%
  filter(!is.na(BMI) & !is.na(AgeDecade)) %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(bins = 100) +
  transition_states(AgeDecade, transition_length = 2, state_length = 2) +
  ease_aes("linear") +
  labs(title = "Age decade: {closest_state}")
#So can see the pattern of BMI as age increases
#Pattern seems to show as age increases so does BMI
#Now plot BMI by Education level, with animation describing the differences between the races
#Graph changes with time allow the plotting of three variables
#Using violin boxplots as third variable is not count [as before]
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>%
  ggplot(aes(x = fct_reorder(Education, BMI, median), y = BMI, colour = Education)) +
  geom_violin() +
  geom_boxplot(alpha = .5) +
  guides(colour = FALSE) +
  labs(title = "Examining the effect of education level on median BMI for Race = {closest_state}",
       x = "Education level",
       y = "BMI") +
  transition_states(Race1, transition_length = 2, state_length = 2) +
  ease_aes("linear")
#Can save the graphs
anim_save(filename = "my_plot.gif")



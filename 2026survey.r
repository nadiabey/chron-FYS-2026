library('dplyr')
library('labelled')
data <- read.csv('Survey Data! - Final Data.csv')
header <- c(data[1,]) # question text
survey <- data[2:364,] # responses
final <- set_variable_labels(survey, .labels = header) # set questions as labels

#race/ethnicity analysis
#race <- select(final, starts_with('Q4_')) # only responses to race/ethnicity
white <- filter(final, Q4_1 == 'White') # all students that ided as white
# students that only chose white
white_only <- filter(white, Q4_2 == '-99', Q4_3 == '-99', Q4_4 == '-99',
                      Q4_5 == '-99', Q4_6 == '-99', Q4_7 == '-99')
latinx <- filter(final, Q4_2 == 'Hispanic/Latinx') # all students that ided as latine
# students that only chose latine
latinx_only <- filter(latinx, Q4_1 == '-99', Q4_3 == '-99', Q4_4 == '-99',
                     Q4_5 == '-99', Q4_6 == '-99', Q4_7 == '-99')
# same thing for rest of ethnic groups
black <- filter(final, Q4_3 == 'Black or African American')
black_only <- filter(black, Q4_1 == '-99', Q4_2 == '-99', Q4_4 == '-99',
                      Q4_5 == '-99', Q4_6 == '-99', Q4_7 == '-99')
aian <- filter(final, Q4_4 == 'American Indian or Alaska Native')
# aian_only = 0
asian <- filter(final, Q4_5 == 'Asian')
asian_only <- filter(asian, Q4_1 == '-99', Q4_3 == '-99', Q4_4 == '-99',
                      Q4_2 == '-99', Q4_6 == '-99', Q4_7 == '-99')
nhpi <- filter(final, Q4_6 == 'Native Hawaiian or Pacific Islander')
nhpi_only <- filter(nhpi, Q4_1 == '-99', Q4_3 == '-99', Q4_4 == '-99',
                      Q4_5 == '-99', Q4_2 == '-99', Q4_7 == '-99')
other <- filter(final, Q4_7 == 'Other')
other_only <- filter(other, Q4_1 == '-99', Q4_3 == '-99', Q4_4 == '-99',
                      Q4_5 == '-99', Q4_6 == '-99', Q4_2 == '-99')
# how many students chose two or more ethnic groups; sub only groups from all
multiracial <- nrow(final) - (nrow(white_only) + nrow(black_only) + 
                                nrow(latinx_only) + nrow(asian_only) +
                                nrow(nhpi_only) + nrow(other_only))

# health status
disabled <- filter(final, Q47 == 'Yes')
immune <- filter(final, Q8 == 'I am immunocompromised')

# domestic/intl
domestic <- filter(final, Q11 == 'No')
intl <- filter(final, Q11 == 'Yes')
community <- final %>% group_by(Q15) %>% summarize(n = n())

# income
# group responses by income and report number of students
income <- final %>% group_by(Q16) %>% summarize(n = n())
finaid <- filter(final, Q17 == 'Yes')

# standardized tests
std_test <- final %>% group_by(Q76) %>% summarize(n = n())
sat <- filter(final, Q76 == 'SAT' | Q76 == 'Both')
act <- filter(final, Q76 == 'ACT' | Q76 == 'Both')
sat_scores <- sat %>%
  summarize(mean = mean(as.integer(Q77), na.rm=TRUE), n = n(), 
            qs = quantile(as.integer(Q77), c(0.25, 0.75), prob = c(0.25, 0.75),
                          na.rm = TRUE))
act_scores <- act %>%
  summarize(mean = mean(as.integer(Q78), na.rm=TRUE), n = n(), 
            qs = quantile(as.integer(Q78), c(0.25, 0.75), prob = c(0.25, 0.75),
                          na.rm = TRUE))

# schools
# by this point i got lazy and realized it would be easier to summarize, plus denominator kept changing
school_type <- final %>% group_by(Q79) %>% summarize(n = n())
residential <- filter(final, Q104 == 'Yes')
ed <- final %>% group_by(Q83) %>% summarize(n = n())
focus <- final %>% group_by(Q27) %>% summarize(n = n())


gender <- final %>% group_by(Q5) %>% summarize(n = n())
gen <- final %>% group_by(Q9) %>% summarize(n = n())
orientation <- final %>% group_by(Q7) %>% summarize(n = n())

#Reading the data file
df = read.csv('Data.csv')

#Looking at the sample of data
head(df)

#Installing dplyr
install.packages('dplyr')
library(dplyr)

#Seperating the adopters and non-adopters into distinct dataframes
df0 <- filter(df,adopter == 0) 
df1 <- filter(df,adopter == 1) 

#Checking the dimesnions of the two sepearted dataframes
dim(df0)
dim(df1)
colnames(df0)

#Selecting the key columns
df0_0 = df0[,c('age','male','friend_cnt','avg_friend_age','friend_country_cnt','subscriber_friend_cnt',
                'songsListened','lovedTracks','posts','playlists','shouts','tenure')]

df1_1 = df1[,c('age','male','friend_cnt','avg_friend_age','friend_country_cnt','subscriber_friend_cnt',
               'songsListened','lovedTracks','posts','playlists','shouts','tenure')]

#Installing package for descriptive stats
install.packages('psych')
library(psych)
#Checking descriptive stats for non_adopters
a0 = describe(df0_0)
#Checking descriptive stats for adopters
a1 = describe(df1_1)

#Creating a descriptive stats file so that only essential columns can be included
write.csv(a0,'Non-adopters.csv')
write.csv(a1,'Adopters.csv')
write.csv(df0,'NA.csv')
write.csv(df1,'A.csv')

#Propensity Score Matching
head(df)
# Pre-analysis using non-matched data
#Creating a column 'test' where 1 indicates treatment group and 0 indicates control group.
df$test <- ifelse(df$subscriber_friend_cnt >= 1,1,0)

head(df$test)

#To check the number of observations for treatment and control group respectively.
table(df$test)

#To check the mean effect of treatment and control group across adopters
df %>% group_by(test) %>% summarise(mean_adopter = mean(adopter))

#T-test to check statistical significance between the two groups 
with(df, t.test(adopter ~ test))

# Difference-in-means: pre-treatment covariates
#age : Age of the user
#male : if 1 then male else female
#friend_cnt : number of friends
#avg_friend_age : average age of the friends
#avg_friend_male : what proportion of friends are male
#friend_country_cnt :	Number of different countries this user's friends are from
#subscriber_friend_cnt : number of friends who are premium subscribers
#songsListened : "cumulative number of songs listened til the beginning of the current period"
#lovedTracks : number of tracks loved at the start of CURRENT
#posts : number of Q&A forum posts made at the start of CURRENT
#playlists : number of playlists made till the current period
#shouts :	number of shouts received from other users till the current period
#tenure :	how long has the user been on the site (in months)
#good_country :	= 1 if from US, UK or Germany, otherwise rest of the world

#Calculating the mean for each covariate by test status
df_c = c('age','male','friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt',
         'songsListened','lovedTracks','posts','playlists','shouts','tenure','good_country')

df %>%
  group_by(test) %>%
  select(one_of(df_c)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Propensity score estimation
# We estimate the propensity score by running a logit model.
lapply(df_c, function(v) {
  t.test(df[, v] ~ df[, 'test'])
})

#Logit model to check the significant variables
model <- glm(test ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
         songsListened + lovedTracks + posts + playlists + shouts + tenure + good_country,
            family = binomial(), data = df)
summary(model)

#Logistic regression with significant variables
#‘male’, ‘posts’, ‘playlists’, ‘shouts’ and ‘good_country’
model1 <- glm(test ~ age + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
               songsListened + lovedTracks + tenure,
             family = binomial(), data = df)
summary(model1)

#Now predicting the propensity scores
# Using this model, we can now calculate the propensity score for each user. 
# It is simply the user’s predicted probability of being Treated, 
# given the estimates from the logit model.
prs_df <- data.frame(pr_score = predict(model1, type = "response"),
                     test = model1$model$test)
head(prs_df)

# Examining the region of common support
# After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status
labs <- paste("Actual Test group:", c("Treatment", "Control"))
prs_df %>%
  mutate(test = ifelse(test == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~test) +
  xlab("Probability of Test group") +
  theme_bw()

#Installing the matchit library
install.packages("MatchIt")
library(MatchIt)

# The method we use below is to find pairs of observations that have very similar propensity scores, 
# but that differ in their treatment status. We use the package MatchIt for this. 
# This package estimates the propensity score in the background and then matches observations based 
# on the method of choice (“nearest” in this case).
#Executing a matching algorithm
df_nomiss <- df %>%  # MatchIt does not allow missing values
  select(adopter, test, one_of(df_c)) %>%
  na.omit()

mod_match <- matchit(test ~ age + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
                       songsListened + lovedTracks + tenure,
                     method = "nearest", data = df_nomiss)

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)
summary(mod_match)
plot(mod_match)

# To create a dataframe containing only the matched observations, use the match.data() function
dta_m <- match.data(mod_match)
dim(dta_m)

# The final dataset contains a variable called distance, which is the propensity score.
# Examining covariate balance in the matched sample
# Visual Inspection

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$test <- as.factor(dta$test)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = test)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

install.packages("gridExtra")
library(gridExtra)
grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "friend_cnt") + theme(legend.position = "none"),
  fn_bal(dta_m, "avg_friend_age"),
  fn_bal(dta_m, "avg_friend_male") + theme(legend.position = "none"),
  fn_bal(dta_m, "friend_country_cnt"),
  fn_bal(dta_m, "songsListened"),
  fn_bal(dta_m, "lovedTracks"),
  fn_bal(dta_m, "tenure"),
  nrow = 4, widths = c(1, 0.8)
)

# Difference of means
df_sign = c('age','friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt',
          'songsListened','lovedTracks','tenure')
dta_m %>%
  group_by(test) %>%
  select(one_of(df_sign)) %>%
  summarise_all(funs(mean))

#T-test
lapply(df_sign, function(v) {
  t.test(dta_m[, v] ~ dta_m$test)
})

# Estimating treatment effects
# Estimating the treatment effect is simple once we have 
# a matched sample that we are happy with. We can use a t-test:

with(dta_m, t.test(adopter ~ test))

head(dta_m)
write.csv(dta_m,'Logreg.csv')

#Logistic regression with matched data -
mat <- glm(adopter ~ . ,family = binomial(), data = dta_m)
summary(mat)
odd = exp(mat$coefficients)
odd = as.data.frame(odd)
odd

#Logitic regression - 
colnames(df)
logreg <- glm(adopter ~ age + male + friend_cnt + avg_friend_male + friend_country_cnt + subscriber_friend_cnt+
                  songsListened + lovedTracks + posts + playlists + shouts + tenure + good_country,
                family = binomial(), data = df)
summary(logreg)

#Logistic regression with significant variables - 
logreg1 <- glm(adopter ~ age + male + friend_cnt + avg_friend_male + friend_country_cnt + subscriber_friend_cnt+
                songsListened + lovedTracks + playlists + tenure + good_country,
              family = binomial(), data = df)
summary(logreg1)

#Finding odds for the variables
odds = exp(logreg1$coefficients)
odds = as.data.frame(odds)
print(odds)


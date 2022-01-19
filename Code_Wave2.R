###Wave 2

##read important variables
imp_vars <- read.csv("important_variables.csv")

##this is hardcoded, need to be changed
imp_vars2 <- imp_vars[2:28,6]


##reading merged data
df_2 <- read.spss("W2 Merged Data/2w-3rd_release_all/merge/Wave2_20170724.sav", to.data.frame=TRUE)
df_w2 <- df_2[,imp_vars2]

##looking at the summary of the dataset
summary(df_w2)

#Remove values for Mainland China
#df_w2 <- df_w2[df_w2$country == "Thailand",]
df_w2 <- df_w2[df_w2$country != "Mainland China",]
unique(df_w2$country)


# Remove colmumns with only i distinct value
fea_uniq_values <- sapply(df_w2, n_distinct)
fea_del <- names(fea_uniq_values[fea_uniq_values == 1])
var_out <- !colnames(df_w2) %in% fea_del
df_w2 <- df_w2[,var_out]

##check the percentage of missing values in each column
sapply(df_w2, function(x) 100*sum(is.na(x))/nrow(df_w2))

##removing missing values
comp_cases <- complete.cases(df_w2)
df_w2 <- df_w2[comp_cases,]

##scoring gender column
df_w2 <- df_w2 %>% mutate(male = case_when(
  se2 == "Male" ~ 1,
  se2 == "Female" ~0
))

###scoring for education
df_w2 <- df_w2 %>%
  mutate(education_num = case_when(
    se5 == 'No formal education' ~ 1,
    se5 == 'Incomplete primary/elementary' ~ 2,
    se5 == 'Complete primary/elementary' ~ 3,
    se5 == 'Incomplete secondary/high school: technical/vocational type' ~ 4,
    se5 == 'Complete secondary/high school: technical/vocational type' ~ 5,
    se5 == 'Incomplete secondary/high school' ~ 6,
    se5 == 'Complete secondary/high school' ~ 7,
    se5 == 'Some university education' ~ 8,
    se5 == 'University education completed' ~ 9,
    se5 == 'Post-graduate degree' ~ 10)) 

#scoring for income
levels(df_w2$se9)
df_w2 <- df_w2 %>%
  mutate(income_level = case_when(
    se9 == 'lowest quintile' ~ 1,
    se9 == '2nd quintile' ~ 2,
    se9 == '3rd quintile' ~ 3,
    se9 == '4th quintile' ~ 4,
    se9 == 'top quintile' ~ 5,
    se9 == 'Decline to answer' ~ 0))

#age to numeric
df_w2$age <- as.numeric(as.character(df_w2$se3a))

#scoring for national government
levels(df_w2$q8)
df_w2 <- df_w2 %>%
  mutate(trust_in_capital = case_when(
    q8 == 'None at all' ~ 1,
    q8 == 'Not Very Much Trust' ~ 2,
    q8 == 'Quite a Lot of Trust' ~ 3,
    q8 == 'A Great Deal of Trust' ~ 4,
    q8 == 'Do not understand the question' ~ 97,
    q8 == 'Can\'t choose' ~ 98,
    q8 == 'Decline to answer' ~ 99
    ))


#scoring for trust in political parties and civil service
df_w2 <- df_w2 %>%
  mutate(trust_in_pp = case_when(
    q9 == 'None at all' ~ 1,
    q9 == 'Not Very Much Trust' ~ 2,
    q9 == 'Quite a Lot of Trust' ~ 3,
    q9 == 'A Great Deal of Trust' ~ 4,
    q9 == 'Do not understand the question' ~ 97,
    q9 == 'Can\'t choose' ~ 98,
    q9 == 'Decline to answer' ~ 99
  ),
  trust_in_civil = case_when(
    q12 == 'None at all' ~ 1,
    q12 == 'Not Very Much Trust' ~ 2,
    q12 == 'Quite a Lot of Trust' ~ 3,
    q12 == 'A Great Deal of Trust' ~ 4,
    q12 == 'Do not understand the question' ~ 97,
    q12 == 'Can\'t choose' ~ 98,
    q12 == 'Decline to answer' ~ 99
  ),
  trust_in_military = case_when(
    q13 == 'None at all' ~ 1,
    q13 == 'Not Very Much Trust' ~ 2,
    q13 == 'Quite a Lot of Trust' ~ 3,
    q13 == 'A Great Deal of Trust' ~ 4,
    q13 == 'Do not understand the question' ~ 97,
    q13 == 'Can\'t choose' ~ 98,
    q13 == 'Decline to answer' ~ 99
  )
  )

#scoring for trust in economic conditions
levels(df_w2$q6)
df_w2 <- df_w2 %>%
  mutate(trust_in_ec = case_when(
    q6 == 'Much worse' ~ 1,
    q6 == 'A little worse' ~ 2,
    q6 == 'About the same' ~ 3,
    q6 == 'A little better' ~ 4,
    q6 == 'Much better' ~ 5,
    q6 == 'Can\'t choose' ~ 8,
    q6 == 'Decline to answer' ~ 9))

#scoring for satisfaction with democracy
levels(df_w2$q93)
df_w2 <- df_w2 %>%
  mutate(satisfaction_demo = case_when(
    q93 == 'Not at all satisfied' ~ 1,
    q93 == 'Not very satisfied' ~ 2,
    q93 == 'Fairly satisfied' ~ 3,
    q93 == 'Very satisfied' ~ 4,
    q93 == 'Can\'t choose' ~ 8,
    q93 == 'Decline to answer' ~ 9))

#scoring for trust in economic conditions
levels(df_w2$q5)
df_w2 <- df_w2 %>%
  mutate(current_eco = case_when(
    q5 == 'Much worse now' ~ 1,
    q5 == 'A little worse now' ~ 2,
    q5 == 'About the same' ~ 3,
    q5 == 'A little better now' ~ 4,
    q5 == 'Much better now' ~ 5,
    q5 == 'Can\'t choose' ~ 8,
    q5 == 'Decline to answer' ~ 9))

#scoring for court_fairness, change_govt, influence_decisions, freedom_speech, equality, help_people
df_w2 <- df_w2 %>%
  mutate(court_fairness = case_when(
    q104 == 'Strongly agree' ~ 1,
    q104 == 'Somewhat agree' ~ 2,
    q104 == 'Somewhat disagree' ~ 3,
    q104 == 'Strongly disagree' ~ 4,
    q104 == 'Do not understand the question' ~ 97,
    q104 == 'Can\'t choose' ~ 98,
    q104 == 'Decline to answer' ~ 99
  ),
  change_govt = case_when(
    q103 == 'Strongly agree' ~ 1,
    q103 == 'Somewhat agree' ~ 2,
    q103 == 'Somewhat disagree' ~ 3,
    q103 == 'Strongly disagree' ~ 4,
    q103 == 'Do not understand the question' ~ 97,
    q103 == 'Can\'t choose' ~ 98,
    q103 == 'Decline to answer' ~ 99
  ),
  influence_decisions = case_when(
    q128 == 'Strongly agree' ~ 1,
    q128 == 'Somewhat agree' ~ 2,
    q128 == 'Somewhat disagree' ~ 3,
    q128 == 'Strongly disagree' ~ 4,
    q128 == 'Do not understand the question' ~ 97,
    q128 == 'Can\'t choose' ~ 98,
    q128 == 'Decline to answer' ~ 99
  ),
  freedom_speech = case_when(
    q110 == 'Strongly agree' ~ 1,
    q110 == 'Somewhat agree' ~ 2,
    q110 == 'Somewhat disagree' ~ 3,
    q110 == 'Strongly disagree' ~ 4,
    q110 == 'Do not understand the question' ~ 97,
    q110 == 'Can\'t choose' ~ 98,
    q110 == 'Decline to answer' ~ 99
  ),
  equality = case_when(
    q108 == 'Strongly agree' ~ 1,
    q108 == 'Somewhat agree' ~ 2,
    q108 == 'Somewhat disagree' ~ 3,
    q108 == 'Strongly disagree' ~ 4,
    q108 == 'Do not understand the question' ~ 97,
    q108 == 'Can\'t choose' ~ 98,
    q108 == 'Decline to answer' ~ 99
  ),
  help_people = case_when(
    q130 == 'Strongly agree' ~ 1,
    q130 == 'Somewhat agree' ~ 2,
    q130 == 'Somewhat disagree' ~ 3,
    q130 == 'Strongly disagree' ~ 4,
    q130 == 'Do not understand the question' ~ 97,
    q130 == 'Can\'t choose' ~ 98,
    q130 == 'Decline to answer' ~ 99
  )
  )

#ranking for voting
levels(df_w2$q38)
df_w2 <- df_w2 %>%
  mutate(voted_last = case_when(
    q38 == 'Not applicable' ~ 0,
    q38 == 'No' ~ 0,
    q38 == 'Yes' ~ 1,
    q38 == 'Can\'t choose' ~ 0,
    q38 == 'Decline to answer' ~ 0))

#ranking for voting
levels(df_w2$q40)
df_w2 <- df_w2 %>%
  mutate(attend_rally = case_when(
    q40 == 'Not applicable' ~ 0,
    q40 == 'No' ~ 0,
    q40 == 'Yes' ~ 1,
    q40 == 'Can\'t choose' ~ 0,
    q40 == 'Decline to answer' ~ 0))

df_w2 <- df_w2 %>%
  mutate(army_govt = case_when(
    q126 == 'Strongly approve' ~ 1,
    q126 == 'Approve' ~ 2,
    q126 == 'Disapprove' ~ 3,
    q126 == 'Strongly disapprove' ~ 4,
    q126 == 'Do not understand the question' ~ 97,
    q126 == 'Can\'t choose' ~ 98,
    q126 == 'Decline to answer' ~ 99
  ),
  rid_elections = case_when(
    q124 == 'Strongly approve' ~ 1,
    q124 == 'Approve' ~ 2,
    q124 == 'Disapprove' ~ 3,
    q124 == 'Strongly disapprove' ~ 4,
    q124 == 'Do not understand the question' ~ 97,
    q124 == 'Can\'t choose' ~ 98,
    q124 == 'Decline to answer' ~ 99
  ))

#scoring for democracy
df_w2 <- df_w2 %>%
  mutate(democracy = case_when(
    q94 == 'A full democracy' ~ 1,
    q94 == 'A democracy, but with minor problems' ~ 2,
    q94 == 'A democracy, with major problems' ~ 3,
    q94 == 'Not a democracy' ~ 4,
    q94 == 'Don\'t understand question' ~ 97,
    q94 == 'Can\'t choose' ~ 98,
    q94 == 'Decline to answer' ~ 99
  ))
#scoring for interest_politics
df_w2 <- df_w2 %>%
  mutate(interest_politics = case_when(
    q49 == 'Not at all interested' ~ 1,
    q49 == 'Not very interested' ~ 2,
    q49 == 'Somewhat interested' ~ 3,
    q49 == 'Very interested' ~ 4,
    q49 == 'No answer' ~ 97,
    q49 == 'Can\'t choose' ~ 98,
    q49 == 'Decline to answer' ~ 99
  ))
#scoring for curr_politics
df_w2 <- df_w2 %>%
  mutate(curr_politics = case_when(
    q48 == 'Very good' ~ 1,
    q48 == 'Good' ~ 2,
    q48 == 'Average' ~ 3,
    q48 == 'Bad' ~ 4,
    q48 == 'Very bad' ~ 97,
    q48 == 'Can\'t choose' ~ 98,
    q48 == 'Decline to answer' ~ 99
  ))
#scoring for safety
df_w2 <- df_w2 %>%
  mutate(safety = case_when(
    q32 == 'Very safe' ~ 1,
    q32 == 'Safe' ~ 2,
    q32 == 'Unsafe' ~ 3,
    q32 == 'Very unsafe' ~ 4,
    q32 == 'Can\'t choose' ~ 98,
    q32 == 'Decline to answer' ~ 99
  ))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for(i in 29:50)
{
  df_w2[is.na(df_w2[,i]),i] <- getmode(df_w2[,i])
}

for(i in 29:50)
{
  if(i != 31){
    df_w2[df_w2[,i] > 5 ,i] <- getmode(df_w2[,i])
  }
}

#keep only numeric columns from df_w2
nums <- unlist(lapply(df_w2, is.numeric))
df_w2_num <- df_w2[,nums]
df_w2_num$age <- as.integer(df_w2_num$age/20)
#PCA analysis
df_w2_scaled_PCA <- data.frame(t(na.omit(t(df_w2_num))))
pca_df_w2_num <- prcomp(df_w2_scaled_PCA, retx=TRUE,scale= TRUE)
#plot PCA factors
par(mar=c(4,4,4,4)+0.3)
plot(pca_df_w2_num,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

#loading 1 for PCA
loadings <- pca_df_w2_num$rotation[,1:3]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(df_w2_scaled_PCA)],1]
loadingfit <- lapply(1:ncol(df_w2_scaled_PCA), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#loading 2 for PCA
loadings <- pca_df_w2_num$rotation[,1:3]
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(df_w2_scaled_PCA)],2]
loadingfit <- lapply(1:ncol(df_w2_scaled_PCA), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#loading 3 for PCA
loadings <- pca_df_w2_num$rotation[,1:3]
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(df_w2_scaled_PCA)],3]
loadingfit <- lapply(1:ncol(df_w2_scaled_PCA), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]



#installing multinomial regression package
library(nnet)

### Turn rating into factor
df_w2_num$trust_in_capital <- factor(df_w2_num$trust_in_capital)
### We call the multinomial logistic regression
modeltest.multinomial <- multinom(trust_in_capital ~ ., data = df_w2_num )
### Lets look at the summary. How many coefficients?
summary(modeltest.multinomial)
###


####### K FOLD #######
library(randomForest)


n <- nrow(df_w2_num)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(multinomial = rep(NA,nfold),random = rep(NA,nfold)) 

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit the two regressions and null model
  model.multinomial <- multinom(trust_in_capital ~ ., data=df_w2_num, subset=train)
  model.random <- randomForest(trust_in_capital ~ .,data = df_w2_num,subset = train)
  
  ## get predictions: type=response so we have probabilities
  pred.multinomial <- predict(model.multinomial, newdata=df_w2_num[-train, ], type="class")
  pred.random <- predict(model.random, newdata =  df_w2_num[-train,], type = "response")
  
  ## calculate and log accuracy
  # multinomial
  OOS$multinomial[k] <- 100 * sum(df_w2_num$trust_in_capital[-train] == pred.multinomial)/NROW(df_w2_num$trust_in_capital[-train])
  OOS$multinomial[k]
  
  OOS$random[k] <- 100 * sum(df_w2_num$trust_in_capital[-train] == pred.random)/NROW(df_w2_num$trust_in_capital[-train])
  OOS$random[k]
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(Datathon code is brewing)"))
}


colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0.0),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

###checking in sample accuracy
model.random <- randomForest(trust_in_capital ~ .,data=df_w2_num)
values_fitted <- predict(model.random,newdata = df_w2_num,type = "response")

acc <- 100 * sum(df_w2_num$trust_in_capital == values_fitted)/NROW(df_w2_num$trust_in_capital)
acc

###checking in sample accuracy
model.multinomial <- multinom(trust_in_capital ~ .,data=df_w2_num)
values_fitted <- predict(model.multinomial,newdata = df_w2_num,type = "class")

acc <- 100 * sum(df_w2_num$trust_in_capital == values_fitted)/NROW(df_w2_num$trust_in_capital)
acc


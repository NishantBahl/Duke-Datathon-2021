##loading the libraries required
library(dplyr)
library(haven)
library(foreign)
library(inspectdf)
library(ggplot2)
library(sjlabelled)

##reading thailand data
data <- read.spss("data/W1 Merged Data/Wave.1_Data/Thailand/thailand v4.2.sav", 
                  to.data.frame=TRUE)
attach(data)
data %>%inspect_types()

cols <- colnames(data)

##read important variables
imp_vars <- read.csv("important_variables.csv")

##this is hardcoded, need to be changed
imp_vars2 <- imp_vars[2:22,4]

####
# important_variables <- append(imp_vars$ABS1_Coding_name,colnames(data)[1:11])
# ##filter data to only include important variables
# variables <- as.data.frame(t(data[,colnames(data) %in% important_variables]))
# variables$questions = rownames(variables)
# colnames(imp_vars)[4] <- 'questions'
# #left join
# data1 <- merge(x = variables, y=imp_vars, by = "questions", all.x=TRUE)
# #OPTIONAL TO TRANSPOSE DATA
# data1 <- t(data1)
# colnames(data1) <-data1[1,]
# data1 <- data[-1,]

data1 <- data[,imp_vars2]

##checking gender distribution
counts <- table(data1$se002)
barplot(counts, main="Gender Distribution")

##checking marital status distribution
g <- ggplot(data1, aes(se004))
# Number of cars in each class:
g + geom_bar()
##Majority married

##checking Education distribution
g <- ggplot(data1, aes(se005))
# Number of cars in each class:
g + geom_bar()
##majority incomplete primary

##reading merged data
df <- read.spss("data/W1 Merged Data/Wave.1_Data/Merge/Wave1_20170906.sav", 
                  to.data.frame=TRUE)
df1 <- df[,imp_vars2]
##adding the column trust in police   
df1$police_trust <- df$q013
df1$people_trust <- df$q024


#Keep values only for Thailand
df1 <- df1[df1$country != "Mainland China",]


# Remove colmumns with only i distinct value
fea_uniq_values <- sapply(df1, n_distinct)
fea_del <- names(fea_uniq_values[fea_uniq_values == 1])
var_out <- !colnames(df1) %in% fea_del
df1 <- df1[,var_out]
#No columns with single value

##check the percentage of missing values in each column
sapply(df1, function(x) sum(is.na(x))*100/nrow(df1))

##q028 has 56% missing values and q121 has 29.3% missing values hence excluding them from the analysis
df1 <-select(df1, -c(q028,q121))

##if we try to remove all the missing values the dataset will have less than 50% of the records
##hence need to do data cleaning for each column
df2 <- df1
nrow(df2[complete.cases(df2),])

##only 8 rows have missing values in gender column. hence removing them from the dataset
df1 <- df1[!is.na(df1$se002),]

#dummy variables for Gender
male <- c()
for(i in 1:nrow(df1))
{
  if(df1$se002[i] == "male")
  {
    male <- append(male,1)
  }
  else if(df1$se002[i] == "female")
  {
    male <- append(male,0)
  }
}
df1$male <- male

#scoring for education
df1 <- df1 %>%
  mutate(education_num = case_when(
    se005 == 'No formal education' ~ 1,
    se005 == 'Incomplete elementary school' ~ 2,
    se005 == 'Complete elementary school' ~ 3,
    se005 == 'Incomplete secondary school' ~ 4,
    se005 == 'Incomplete high school' ~ 4,
    se005 == 'Complete high school' ~ 5,
    se005 == 'Complete secondary school' ~ 5,
    se005 == 'Some university_college education' ~ 6,
    se005 == 'University_college degree' ~ 7,
    se005 == 'Post graduate degree' ~ 8)) 

#scoring for income
levels(df1$se009)
df1 <- df1 %>%
  mutate(income_level = case_when(
    se009 == 'lowest quintile' ~ 1,
    se009 == '2nd quintile' ~ 2,
    se009 == '3rd quintile' ~ 3,
    se009 == '4th quintile' ~ 4,
    se009 == '5th quintile' ~ 5)) 

#age to numeric
df1$age <- as.numeric(as.character(df1$se003a))

#scoring for trust in courts
levels(df1$q007)
df1 <- df1 %>%
  mutate(trust_in_courts = case_when(
    q007 == 'Not sure' ~ 1,
    q007 == 'None at all' ~ 1,
    q007 == 'Not very much trust' ~ 2,
    q007 == 'Quite a lot of trust' ~ 3,
    q007 == 'A great deal of trust' ~ 4))

#scoring for national government
levels(df1$q008)
df1 <- df1 %>%
  mutate(trust_in_capital = case_when(
    q008 == 'Not sure' ~ 1,
    q008 == 'None at all' ~ 1,
    q008 == 'Not very much trust' ~ 2,
    q008 == 'Quite a lot of trust' ~ 3,
    q008 == 'A great deal of trust' ~ 4))

#scoring for trust in political parties
levels(df1$q009)
df1 <- df1 %>%
  mutate(trust_in_pp = case_when(
    q009 == 'Not sure' ~ 1,
    q009 == 'None at all' ~ 1,
    q009 == 'Not very much trust' ~ 2,
    q009 == 'Quite a lot of trust' ~ 3,
    q009 == 'A great deal of trust' ~ 4))

#scorng for trust in Parliament
levels(df1$q010)
df1 <- df1 %>%
  mutate(trust_in_parliament = case_when(
    q010 == 'Not sure' ~ 1,
    q010 == 'None at all' ~ 1,
    q010 == 'Not very much trust' ~ 2,
    q010 == 'Quite a lot of trust' ~ 3,
    q010 == 'A great deal of trust' ~ 4))

#scoring for trust in economic conditions
levels(df1$q006)
df1 <- df1 %>%
  mutate(trust_in_ec = case_when(
    q006 == 'Much Worse' ~ 1,
    q006 == 'A little Worse' ~ 2,
    q006 == 'About the same' ~ 3,
    q006 == 'A little Better' ~ 4,
    q006 == 'Much Better' ~ 5))


##nation run by few powerful (ordinary can't do anything) encoding
df1 <- df1 %>% mutate(nation_runby_powerful = case_when(
  q098 == 'Not at all satisfied' ~ 1,
  q098 == 'Not very satisfied' ~ 2,
  q098 == 'Fairly satisfied' ~ 3,
  q098 == 'Very satisfied' ~ 4
))

##nation run by few powerful (ordinary can't do anything) encoding
df1 <- df1 %>% mutate(nation_runby_powerful = case_when(
  q128 == 'Strongly agree' ~ 1,
  q128 == 'Somewhat agree' ~ 2,
  q128 == 'Somewhat disagree' ~ 3,
  q128 == 'Strongly disagree' ~ 4
))

##current_economic condition encoding
df1 <- df1 %>% mutate(current_econ = case_when(
  q005 == 'Much worse now' ~ 1,
  q005 == 'A little worse now' ~ 2,
  q005 == 'About the same' ~ 3,
  q005 == 'A little better now' ~ 4,
  q005 == 'Much better now' ~ 5
))

##freedom_speech encoding
df1 <- df1 %>% mutate(freeedom_speech = case_when(
  q105 == 'Much worse' ~ 1,
  q105 == 'Somewhat worse' ~ 2,
  q105 == 'Much the same' ~ 3,
  q105 == 'Somewhat better' ~ 4,
  q105 == 'Much better than before' ~ 5
))

##treated equal encoding
df1 <- df1 %>% mutate(treated_equal = case_when(
  q106 == 'Much worse' ~ 1,
  q106 == 'Somewhat worse' ~ 2,
  q106 == 'Much the same' ~ 3,
  q106 == 'Somewhat better' ~ 4,
  q106 == 'Much better than before' ~ 5
))

##military govern encoding
df1 <- df1 %>% mutate(military_govern = case_when(
  q123 == 'Strongly agree' ~ 1,
  q123 == 'Somewhat agree' ~ 2,
  q123 == 'Somewhat disagree' ~ 3,
  q123 == 'Strongly disagree' ~ 4
))

##police trust encoding
df1 <- df1 %>% mutate(policetrust = case_when(
  police_trust == 'A great deal of trust' ~ 1,
  police_trust == 'Quite a lot of trust' ~ 2,
  police_trust == 'Not very much trust' ~ 3,
  police_trust == 'None at all' ~ 4
))

##poeple trust encoding
df1 <- df1 %>% mutate(peopletrust = case_when(
  people_trust == "One can't be too careful in dealing with them" ~ 2,
  people_trust == "Most people can be trusted" ~ 1
  
))

##q127 encoding
df1 <- df1 %>% mutate(politics_complicated = case_when(
  q127 == 'Strongly agree' ~ 1,
  q127 == 'Somewhat agree' ~ 2,
  q127 == 'Somewhat disagree' ~ 3,
  q127 == 'Strongly disagree' ~ 4
  
))

##participated in last election encoding
df1 <- df1 %>% mutate(prev_election_p = case_when(
  q027 == 'No' ~ 0,
  q027 == 'Yes' ~ 1
))


##Replacing blanks in marital status
levels(df1$se004) <- c("Married","Living-in as married","Widowed","Separated","Divorced","Single_Never married","Unknown")
df1$se004[is.na(df1$se004)] <- "Unknown"

##creating dummy variables for marital status column
df1 <- df1 %>% mutate(married = ifelse(se004=="Married",1,0),
                      living_in = ifelse(se004=="Living-in as married",1,0),
                      widowed = ifelse(se004=="Widowed",1,0),
                      separated = ifelse(se004=="Separated",1,0),
                      divorced = ifelse(se004=="Divorced",1,0),
                      single = ifelse(se004=="Single_Never married",1,0))


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for(i in 22:39)
{
  df1[is.na(df1[,i]),i] <- getmode(df1[,i])
}

#keep only numeric columns from df1
nums <- unlist(lapply(df1, is.numeric))
df1_num <- df1[,nums]
df1_num$age <- as.integer(df1_num$age/20)
# #remove age and check
# df1_num <- select(df1_num, -c(age))
#PCA analysis
df1_scaled <- data.frame(t(na.omit(t(df1_num))))
pca_df1_num <- prcomp(df1_scaled, retx=TRUE,scale. = TRUE)
#plot PCA factors
par(mar=c(4,4,4,4)+0.3)
plot(pca_df1_num,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
#loading 1 for PCA
loadings <- pca_df1_num$rotation[,1:3]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(df1_scaled)],1]
loadingfit <- lapply(1:ncol(df1_scaled), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#First factor comprises high trust in government agencies category.
#loading 2 for PCA
loadings <- pca_df1_num$rotation[,1:3]
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(df1_scaled)],2]
loadingfit <- lapply(1:ncol(df1_scaled), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#Second factor comprises poorer, older, non-single people with low trust in freedom of speech and their future economic condition
#loading 3 for PCA
loadings <- pca_df1_num$rotation[,1:3]
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(df1_scaled)],3]
loadingfit <- lapply(1:ncol(df1_scaled), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#Third factor comprises married citizens who believe freedom of speech and equality has improved


#installing multinomial regression package
library(nnet)

### Turn rating into factor
df1_num$trust_in_capital <- factor(df1_num$trust_in_capital)
### We call the multinomial logistic regression
modeltest.multinomial <- multinom(trust_in_capital ~ ., data = df1_num )
### Lets look at the summary. How many coefficients?
summary(modeltest.multinomial)
###


####### K FOLD #######
library(randomForest)

n <- nrow(df1_num)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(multinomial = rep(NA,nfold),random = rep(NA,nfold)) 

nums <- complete.cases(df1_num)
length(nums)
df1_num <- df1_num[nums,]

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit the two regressions and null model
  model.multinomial <- multinom(trust_in_capital ~ ., data=df1_num, subset=train)
  model.random <- randomForest(trust_in_capital ~ .,data = df1_num,subset = train,na.action=na.roughfix)
  
  ## get predictions: type=response so we have probabilities
  pred.multinomial <- predict(model.multinomial, newdata=df1_num[-train, ], type="class")
  pred.random <- predict(model.random, newdata =  df1_num[-train,], type = "response")
  
  ## calculate and log accuracy
  # multinomial
  OOS$multinomial[k] <- 100 * sum(df1_num$trust_in_capital[-train] == pred.multinomial,na.rm = TRUE)/NROW(df1_num$trust_in_capital[-train])
  OOS$multinomial[k]
  
  OOS$random[k] <- 100 * sum(df1_num$trust_in_capital[-train] == pred.random)/NROW(df1_num$trust_in_capital[-train])
  OOS$random[k]
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(Datathon code is brewing)"))
}


#plot the OOS accuracy for each fold
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0.0),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

###checking in sample accuracy
model.random <- randomForest(trust_in_capital ~ .,data=df1_num)
values_fitted <- predict(model.random,newdata = df1_num,type = "response")

acc <- 100 * sum(df1_num$trust_in_capital == values_fitted)/NROW(df1_num$trust_in_capital)
acc

###checking in sample accuracy
model.multinomial <- multinom(trust_in_capital ~ .,data=df1_num)
values_fitted <- predict(model.multinomial,newdata = df1_num,type = "class")
summary(model.multinomial)

acc <- 100 * sum(df1_num$trust_in_capital == values_fitted)/NROW(df1_num$trust_in_capital)
acc








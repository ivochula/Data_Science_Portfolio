###### START ######
setwd('C:/Projetos/Titanic')
getwd()

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(caret)
library(corrplot)
library(corrgram)
library(randomForest)
library(e1071)

train = fread("C:/Projetos/Titanic/train.csv")
test = fread("C:/Projetos/Titanic/test.csv")

df = bind_rows(train, test) # bind training & test data 

# Let's check the type of each variable
str(df)

# Checking the amount of people which survived the Titanic disaster
survived.matrix <- matrix( c(table(df$Survived)), nrow = 2, byrow = F)
rownames(survived.matrix) <- c('Died', 'Survived')
colnames(survived.matrix) <- c('died/survived')
survived.matrix
barplot(survived.matrix, legend.text = c('Died', 'Survived'), 
        beside = T, col = c('red','green'),main = 'Number of peole who died/survived at Titanic Disaster',
        ylim = c(0, 600))

survived.prop <- round(prop.table(table(df$Survived)), 3) * 100 # Proportion
survived.prop.matrix <- matrix(survived.prop , nrow = 2, byrow = F)
rownames(survived.prop.matrix) <- c('Died', 'Survived')
colnames(survived.prop.matrix) <- c('died/survived')
survived.prop.matrix

#Checking the classes, 1st, 2nd and 3rd class
data.table(table(df$Pclass))
classes <- round(prop.table(table(df$Pclass)), 3) * 100 # Proportion
rownames(classes) <- c('1st Class', '2nd Class', '3rd Class')
classes

# Checking how many 1st, 2nd and 3rd class survived

df_1st = filter(df, Pclass == 1) 
data.table(table(df_1st$Survived))
firsty <- round(prop.table(table(df_1st$Survived)), 3) * 100
rownames(firsty) <- c('Died', 'Survived')
firsty
# 63% of 1st class people survived

df_2nd = filter(df, Pclass == 2) 
data.table(table(df_2nd$Survived)) 
secondy <- round(prop.table(table(df_2nd$Survived)), 3) * 100
rownames(secondy) <- c('Died', 'Survived')
secondy
# 47.3% of 2nd class people survived

df_3rd = filter(df, Pclass == 3) 
data.table(table(df_3rd$Survived)) 
thirdy <- round(prop.table(table(df_3rd$Survived)), 3) * 100
rownames(thirdy) <- c('Died', 'Survived')
thirdy
# only 24.2% of 3rd class people survived

# Using values from 1st, 2nd and 3rd class survival rate and insert into a matrix
a = data.table(table(df_1st$Survived))[1,2]
b = data.table(table(df_1st$Survived))[2,2]
c = data.table(table(df_2nd$Survived))[1,2]
d = data.table(table(df_2nd$Survived))[2,2]
e = data.table(table(df_3rd$Survived))[1,2] 
f = data.table(table(df_3rd$Survived))[2,2]

matrix_class = matrix(c(a, b, c, d, e, f),nrow = 2, byrow = F)
rownames(matrix_class) = c('Died', 'Survived')
colnames(matrix_class) = c('1st Class', '2nd Class', '3rd Class')

barplot(matrix_class, legend.text = c('Died', 'Survived'), names.arg = NULL, 
        angle = c(45, 315), density = c(20,20), col = c('red','green'), border =T,
        main = 'Number of survivers per Class', ylab = 'Number of Passengers', 
        sub = 'Classes', ylim = c(0, 500), col.main = 52, cex.main = 2,
        col.sub = 52, cex.sub = 1.5)

matrix_class_died = matrix(c(a, c, e),nrow = 3)
lines(matrix_class_died, col = 'blue',lwd = 3)

#By this data we can inform that people which travelled in first class had more chances of surviving 
#than the others classes

# We will check the surviving rate for young and old people
df_young = filter(df, Age < 18)
data.table(table(df_young$Survived)) 
young <- round(prop.table(table(df_young$Survived)), 3) * 100
rownames(young) <- c('Died', 'Survived')
young
# 54% of young people survived

df_old = filter(df, Age >= 65)
data.table(table(df_old$Survived)) 
old <- round(prop.table(table(df_old$Survived)), 3) * 100
rownames(old) <- c('Died', 'Survived')
old
# only 9.1% of old people survived

young_old <- matrix(c(young, old), nrow = 2, byrow = T)
colnames(young_old) <- c('Died', 'Survived')
rownames(young_old) <- c('Young (< 18)  ', 'Old   (>= 65) ')
young_old
# 

#Lets also check if youngsters from 1st, 2nd and 3rd classes had the same chances of surviving

df_young1 = filter(df, (Age < 18) & (Pclass == 1))
data.table(table(df_young1$Survived)) 
young1 <- round(prop.table(table(df_young1$Survived)), 3) * 100
# 91.7% of youngsters in class 1 survived

df_young2 = filter(df, (Age < 18) & (Pclass == 2))
data.table(table(df_young2$Survived)) 
young2 <- round(prop.table(table(df_young2$Survived)), 3) * 100
# 91.3% of youngsters in class 2 survived

df_young3 = filter(df, (Age < 18) & (Pclass == 3))
data.table(table(df_young3$Survived)) 
young3 <- round(prop.table(table(df_young3$Survived)), 3) * 100
# only 37.2% of youngsters lived

young_final <- matrix(c(young1, young2, young3), nrow = 3, byrow = T)
colnames(young_final) <- c('Died', 'Survived')
rownames(young_final) <- c('1st Class', '2nd Class', '3rd Class')
young_final
# Youngsters from 3rd class had only 37% chances of surviving against the >90% chance of 1st and 2nd class


####REPLACE MISSING VALUES#####


hist(df_1st$Age)
hist(df_2nd$Age)
hist(df_3rd$Age)
# We can observe that the age value has a normal distribution along the classes

# We will fill NA values according to MEDIAN age by class
median_1st <- median.default(df_1st$Age, na.rm = T)
median_2nd <- median.default(df_2nd$Age, na.rm = T)
median_3rd <- median.default(df_3rd$Age, na.rm = T) 

# Replacing NA values in AGE colummn by median of each class

df_na <- filter(df, is.na(df$Age)) #subset with NA values on AGE column
df_origin <- filter(df, !is.na(df$Age)) #removing NA rows from AGE column

df_na$Age <- ifelse(df_na$Pclass == 1 , median_1st,
                    ifelse(df_na$Pclass == 2, median_2nd,
                           ifelse(df_na$Pclass == 3 , median_3rd, df_na$Age)))

df <-  bind_rows(df_na, df_origin) #binding the two dataframes
df <- arrange(df, df$PassengerId) #arrange the order by passengerID (crescent order)
sum(is.na(df$Age)) # checking for any NA Value in the column


# Automatically checking number of NA values on each column

len_colnames <- length(colnames(df))
for (i in 1:len_colnames){print(sum(is.na(df[,i])))}

#418 values on 2nd column are the missing values from the test dataset
# One Missing Value in Fare Column

which(is.na(df$Fare)) # Finding position of missing value

# Checking 3rd class values
summary(df_3rd$Fare)
# The values are quite different, lets check by a boxplot to see the big picture for the 3rd class fare

boxplot(df_3rd$Fare)
# there are many outliers, the prices vary too much
# I will choose a median value between the 1st and 3rd quartile to have an average price for the ticket.
# The outliers probably were cause by last minute purchased tickets or discount.
fare_q <- filter(df, (Fare >= 7.75) & (Fare <= 15.25))
df$Fare[is.na(df$Fare)] <- median(fare_q$Fare)


################################ FEATURE ENGINEERING ################

# Spliting Name column to Name, Title, Surname columns

df$Title <- gsub('(.+, )|(\\..+)', '', df$Name)
df$Surname <- gsub('(.+, )|(.+. )|[[:punct:]]', '', df$Name)
df$Name <- gsub('(\\,.+)', '', df$Name)

#Rearranging the Titles
other_title <- c('Capt','Col', 'Don', 'Dona', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Sir', 'Rev', 'the Countess')
df$Title[df$Title == 'Mlle'] <- 'Miss' 
df$Title[df$Title == 'Ms']  <- 'Miss'
df$Title[df$Title == 'Mme'] <- 'Mrs' 
df$Title[df$Title %in% other_title]  <- 'Other Title'

# Show title counts by sex
table(df$Sex, df$Title)

# Family relation of survival 
table(df$SibSp)
table(df$Parch)

df$Family <- df$SibSp + df$Parch + 1 # Number of relatives plus the person himself/herself
df <- subset(df, select = -c(SibSp,Parch)) # Removing columns

#Reorganize the new columns
df_clean <- df[, c(1,3,11,4,12,13,5:10,2)]


############################## PREPARING FOR ML ############

# Removing Features 
df_clean <- subset(df_clean, select = -c(Name, Surname, Ticket, Cabin))


# Passing Characters Feature to Numeric
df_clean$Title[df_clean$Title == 'Mrs'] <- 1
df_clean$Title[df_clean$Title == 'Miss'] <- 2
df_clean$Title[df_clean$Title == 'Master']  <- 3
df_clean$Title[df_clean$Title == 'Mr']  <- 4
df_clean$Title[df_clean$Title %in% "Other Title"]  <- 5
df_clean$Title <- as.numeric(df_clean$Title) 

df_clean$Sex[df_clean$Sex == 'male'] <- 0
df_clean$Sex[df_clean$Sex == 'female'] <- 1
df_clean$Sex <- as.numeric(df_clean$Sex) 

df_clean$Embarked[df_clean$Embarked == 'C'] <- 0
df_clean$Embarked[df_clean$Embarked == 'Q'] <- 1
df_clean$Embarked[df_clean$Embarked == 'S'] <- 2
df_clean$Embarked <- as.numeric(df_clean$Embarked) 

df_clean$Survived <- as.factor(df_clean$Survived)

# Removing Rows 62 and 830 because they have "" insteed of 'C', 'Q' or 'S'
df_clean <- df_clean[-c(62, 830),] 

# Normalize the values, putting all to the same scale
scale.features <- function(df, variables){
        for (variable in variables){
                df[[variable]] <- scale(df[[variable]], center=T, scale=T)
        }
        return(df)
}

numeric_values <- c('Pclass', 'Title', 'Family', 'Age', 'Fare', 'Embarked')
df_norm <- scale.features(df_clean, numeric_values)

# Spliting train and test
train_final <- df_norm[1:889,] 
test_final <- df_norm[890:1307,]

#Correlation and importance between features

correlation <- cor(train_final, method = c('pearson'))
corrplot(correlation, method = c('number'), type = c('lower'))

train_final_imp <- randomForest(Survived ~ Pclass + Title + Family + Sex + Age + Fare, train_final, 
                                ntree=1000, importance=TRUE, na.action=na.fail)

importance(train_final_imp)
varImpPlot(train_final_imp, sort=TRUE)

#################### CREATING MACHINE LEARNING MODEL ##################

#### KNN ####

ctrl <- trainControl(method = "repeatedcv", repeats = 3) 

model_V1 <- train(Survived ~ Pclass + Title + Family + Sex + Age + Fare + Embarked, train_final, method = "knn", 
                trControl = ctrl, tuneLength = 20)
predict_knn <- predict(model_V1, test_final)

solution_knn <- data.frame(PassengerID = test_final$PassengerId, Survived = predict_knn)#0.77751 score Kaggle


#### Naive Bayes ####

model_V2 <- naiveBayes(Survived ~ Pclass + Title + Family + Sex + Age + Fare, train_final)
predict_nb <- predict(model_V2, test_final)

solution_nb <- data.frame(PassengerID = test_final$PassengerId, Survived = predict_nb) #0.75598 score Kaggle

# Embarked feature presents a lower score

#### Random Forest ####

model_V3 <- randomForest(Survived ~ Pclass + Title + Family + Sex + Age + Fare + Embarked, train_final, ntree=1000)
varImp(model_V3)
predict_rf <- predict(model_V3, test_final)

solution_rf <- data.frame(PassengerID = test_final$PassengerId, Survived = predict_rf) #0.78468 score Kaggle

#Compare prediction values of the 3 predictive models

solution_all <- data.frame(PassengerID = test_final$PassengerId, KNN = predict_knn, NB = predict_nb, RF = predict_rf)

table_knn <-  table(solution_all$KNN)
table_nb <-  table(solution_all$NB)
table_rf <-  table(solution_all$RF)

compare_list = list(table_knn, table_nb, table_rf)
names(compare_list) <- c("KNN", "Naive Bayes", "RandomForest")
compare_list

# Write the solution to file
write.csv(solution_knn, file = 'knn_mod_Solution.csv', row.names = F)
write.csv(solution_nb, file = 'nb_mod_Solution.csv', row.names = F)
write.csv(solution_rf, file = 'rf_mod_Solution.csv', row.names = F)

setwd('C:/Projetos/Titanic')
getwd()

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

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


#REPLACE MISSING VALUES


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


# NEW INSIGHTS


# Spliting Name column to Name, Title, Surname columns

df$Title <- gsub('(.+, )|(\\..+)', '', df$Name)
df$Surname <- gsub('(.+, )|(.+. )|[[:punct:]]', '', df$Name)
df$Name <- gsub('(\\,.+)', '', df$Name)

#Reorganize the new columns
df <- df[, c(1,3,13,4,14,5:12,2)]

#Rearranging the Titles
other_title <- c('Capt','Col', 'Don', 'Dona', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Sir', 'Rev', 'the Countess')
df_teste$Title[df_teste$Title == 'Mlle'] <- 'Miss' 
df_teste$Title[df_teste$Title == 'Ms']  <- 'Miss'
df_teste$Title[df_teste$Title == 'Mme'] <- 'Mrs' 
df_teste$Title[df_teste$Title %in% other_title]  <- 'Other Title'

# Show title counts by sex
table(df_teste$Sex, df_teste$Title)



# Spliting train and test
train <- df[1:891,] 
test <- df[892:1309,]


################################ WORK IN PROGRESS ##########
df_teste <- df 

View(df_teste)

# Analise de pessoas da mesma familia



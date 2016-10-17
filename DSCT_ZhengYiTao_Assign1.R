#INTRO TO R ASSIGNMENT
#DONE BY: ZHENG YI TAO

# Importing csv file with ONLY the columns required.
surveyData <- read.csv("D:\\Assignment 1\\Survey_Data.csv", header = TRUE, sep=";")

# ***Question 1***
# Yes, There are missing values in the dataset. 
#According to the codebook, missing values are denoted as the following: 
# "8", "98" or "999998" as "Don't Know"
# "9", "99" or "999999" as "No answer"
# "0" as "Inapplicable"
# The code below shows the list of rows that have missing values:

missingVal <- surveyData[rowSums(surveyData == "8" | 
                                   surveyData == "9" |
                                   surveyData == "98" |
                                   surveyData == "99" |
                                   surveyData == "999998" |
                                   surveyData == "999999" |
                                   surveyData == "0") > 0,]

#***Question 2***
surveyDataNA <- surveyData
surveyDataNA[surveyDataNA == "8" | 
               surveyDataNA == "9" |
               surveyDataNA == "98" |
               surveyDataNA == "99" |
               surveyDataNA == "999998" |
               surveyDataNA == "999999" |
               surveyDataNA == "0"] <- NA

#***Question 3***
fSurveyData <- surveyDataNA

#Convert those columns with label into factor
fSurveyData[,c(1,2,3,5,6,7)] = lapply(fSurveyData[,c(1,2,3,5,6,7)], factor)

#Check to see the type of each column
#str(fSurveyData)

#Labels
levels(fSurveyData$MARITAL) = c("Married", "Widowed", "Divorced", "Separated", "Never Married")
levels(fSurveyData$AGE)[levels(fSurveyData$AGE)  == "89"] <- "89 or older"
levels(fSurveyData$GENDER1) = c("Male", "Female")
levels(fSurveyData$HAPPY) = c("Very happy", "Pretty happy", "Not too happy")
levels(fSurveyData$HEALTH) = c("Excellent", "Good", "Fair", "Poor")
levels(fSurveyData$SATJOB) = c("Very satisfied", "Moderately satisfied", "A little dissatisfied", "Very dissatisfied")


#***Question 4***
capSurveyData <- fSurveyData

#percapinc = total household income / no. of family members
capSurveyData <- transform(capSurveyData, PERCAPINC = REALINC / HOMPOP)


#***Question 5***
summary(capSurveyData)
describe(capSurveyData)

#cor(capSurveyData$PERCAPINC, capSurveyData$REALINC, use = "complete")

#***Question 6***
healthGenderTbl <- table(fSurveyData$HAPPY, fSurveyData$GENDER1)

barplot(prop.table(healthGenderTbl, 2)*100, beside = T, col=c("yellow", "blue", "red"), 
        legend = rownames(healthGenderTbl), 
        main = "Comparison of Happiness Level between Genders",
        xlab = "Genders", ylab = "% of Genders")

chisq.test(prop.table(healthGenderTbl, 2)*100)

#***Question 7***
catAge <- cut(as.numeric(levels(fSurveyData$AGE))[fSurveyData$AGE], 
               breaks = c(0,25,50,100),
               labels = c("Young", "Middle", "Old"))

catAgeHap <- table(fSurveyData$HAPPY, catAge)

barplot(prop.table(catAgeHap, 2)*100, beside = T, col = c("yellow", "blue", "red"),
        main = "Comparison of Happiness Level between Age Ranges",
        xlab = "Age Ranges", ylab = "% of No. of People")
legend("topright", fill = c("yellow", "blue", "red"), rownames(catAgeHap))

#***Question 8***
aggregate(REALINC ~ HAPPY, fSurveyData, median)


#***Question 9***
aggregate(REALINC ~ HEALTH, fSurveyData, median)

#***Question 10***
#Summarising Linear Model
model <- lm(HAPPY ~ MARITAL, data=surveyDataNA)
abline(model, col="red")

summary(model)

#Build Bar Plot for comparison
HapMarTbl <- table(fSurveyData$HAPPY, fSurveyData$MARITAL)
barplot(prop.table(HapMarTbl,2), beside = T, col=c("yellow", "blue", "red"), 
        main = "Happinesss depend on Marital status?",
        xlab = "Marital Status", ylab = "Count of Marital status and happiness")
legend("topright", fill = c("yellow", "blue", "red"), rownames(catAgeHap), cex = 0.5)

#***Question 11***
surveyDataRACE <- read.csv("D:\\Assignment 1\\Survey_Data_RACE.csv", header = TRUE, sep=",")

raceFactor <- as.factor(surveyDataRACE[,1])
levels(raceFactor) = c("White", "Black", "Other")

fSurveyData$RACE <- raceFactor

aggregate(REALINC ~ RACE, fSurveyData, median)

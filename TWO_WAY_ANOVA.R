#IMPORT THE DATA SET 

data <- read.csv(file.choose())

attach(data)
names(data)
str(data)

#CONVERT THE GROUP VARIABLE IN FACTOR 

data$type <- as.factor(data$type)
data$season <- as.factor(data$season)

str(data)

data$type = factor(data$type,labels = c("car", "jeep", "truck"))
data$season = factor(data$season, labels = c("summer","winter"))

str(data)

############## CHECK FOR THE ASSUMPTION ################

# 1. ALL SAMPLES ARE INDEPENDENT, 
# AND MORE THAN 2 CATEGORICAL GROUP

summary(data$type)

summary(data$season)

#2. DEPENDENT VARIABLE IS CONTINUOUS

#3. CHECK FOR OUTLIERS

Group1 <- subset(data, type == "car")
Group2 <- subset(data, type == "jeep")
Group3 <- subset(data, type == "truck")

boxplot(Group1$sales)
boxplot(Group2$sales)
boxplot(Group3$sales)

#TREATMENT OF OUTLIER FOR GROUP - 1


summary(Group1$sales)
upper <- 63.26 + 1.5 * IQR(Group1$sales)
upper

Group1$sales[Group1$sales > upper] <- upper
boxplot(Group1$sales)

#TREATMENT OF OUTLIER FOR GROUP - 3

summary(Group3$sales)
upper <-  67.72 + 1.5 * IQR(Group1$sales)

upper

Group3$sales[Group3$sales > upper] <- upper
boxplot(Group3$sales)

#4. NORMAL DISTRIBUTIONS OF EACH GROUP 

qqnorm(Group1$sales)
qqline(Group1$sales)


qqnorm(Group2$sales)
qqline(Group2$sales)

qqnorm(Group3$sales)
qqline(Group3$sales)



############# ANOVA TEST #######################


model <- aov(sales ~ type * season, data = data)
summary(model)

#POST-HOC TEST - WHICH OF THE GROUP HAVE DIFF MEANS

TukeyHSD(model)

# DATA VISUALISATIOIN


x <- TukeyHSD(model)
plot(x)

 

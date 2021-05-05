library(foreign)
library(tidyverse)
library(reshape2)
library(lubridate)
library(readxl)
library(rsample)
library(caret)
library(tree)
library(rpart)
library(rpart.plot)


# read data
alldata <- read.spss(file = "Daten_bereinigt_Modell.sav", to.data.frame = TRUE)

# date to GMT format
alldata$Date <- as.Date(as.POSIXct(alldata$Datum, origin = "1582-10-14", tz = "GMT"))

# read data for feature "leaving Old Piste"
loldp <- read_excel("ID_Tourenbereich_0_nicht_1_gefährlich.xlsx")

# merge the data and delete the 4 four observations
mydata <- full_join(loldp, alldata, by = "ID")
mydata <- mydata %>%
  slice(-139,
        -140,
        -303,
        -304)

# define feature (Leaving old Piste)
mydata <- mydata %>%
  mutate("Leaving Old Piste" = Tourenbereich)
mydata$`Leaving Old Piste` <- as.factor(mydata$`Leaving Old Piste`)
levels(mydata$`Leaving Old Piste`) <- c("no", "yes")

# read weather data
wdata <- read_excel("Wetterdaten.xlsx", sheet = "AufbereiteteTageswerte")

wdata$Datum <- force_tz(wdata$Datum, "GMT")
wdata$Date <- wdata$Datum
wdata <- wdata %>%
  filter(Date > "2020-02-18 00:00:00") %>%
  dplyr::select(-Datum,
                -avalanche_report_down)

mydata <- full_join(wdata, mydata, by = "Date")


# define a new target feature (Equipment)
mydata <- mydata %>%
  mutate(Equipment = ifelse(v_6_D_1 == "quoted" & v_6_D_2 == "quoted" & v_6_D_3 == "quoted", 1,0))

# control if ok!! check...
mydata %>% select(v_6_D_1,
                  v_6_D_2,
                  v_6_D_3,
                  Equipment)
mydata$Equipment <- as.factor(mydata$Equipment)
levels(mydata$Equipment) <- c("Not Carrying", "Carrying Along")

# define feature (Familarity)
mydata$Familarity <- mydata$v_3
levels(mydata$Familarity) <- c("5","4","3","2","1")
mydata$Familarity <- factor(mydata$Familarity, levels = c("1", "2", "3", "4", "5"))
mydata$Familarity <- as.numeric(mydata$Familarity)
mydata$Familarity[is.na(mydata$Familarity)] <- 0
mydata$v_4_2[is.na(mydata$v_4_2)] <- 0
mydata$Familarity <- mydata$Familarity * mydata$v_4_2

# define feature (Incompany)
mydata$Incompany <- mydata$v_7_3
levels(mydata$Incompany) <- c("no", "yes")

# define feature (Avalanche Danger Level)
mydata$"Avalanche Danger Level" <- mydata$v_7_2
levels(mydata$"Avalanche Danger Level") <- c("no", "yes")

# define feature (Tour Specific)
mydata$"Tour Specific" <- mydata$v_7_4
levels(mydata$"Tour Specific") <- c("no", "yes")

# define feature (Terrain Information) 0 = unserious, 1 = serious Terrain Information
mydata <- mydata %>%
  mutate("Terrain Information" = ifelse(v_8_1_bereinigt == "quoted" |
                                          v_8_2_bereinigt == "quoted" |
                                          v_8_3_bereinigt == "quoted", 1,0))
mydata %>%
  select(v_8_4_bereinigt,
         v_8_6_bereinigt,
         v_8_5_bereinigt,
         v_8_1_bereinigt,
         v_8_2_bereinigt,
         v_8_3_bereinigt,
         "Terrain Information")
mydata$"Terrain Information" <- as.factor(mydata$"Terrain Information")
levels(mydata$"Terrain Information") <- c("unserious", "serious")

# define feature (Avalanche Information)
mydata <- mydata %>%
  mutate("Avalanche Information" = ifelse(v_9_1 == "quoted" |
                                            v_9_2 == "quoted", 1,0))
mydata %>%
  select(v_9_1,
         v_9_2,
         "Avalanche Information")

mydata$"Avalanche Information" <- as.factor(mydata$"Avalanche Information")
levels(mydata$"Avalanche Information") <- c("no", "yes")

# define feature (Group Size)
levels(mydata$v_10) <- c("alone", "small group", "big group")
mydata$"Group Size" <- mydata$v_10

# define feature (Always Avalanche Information)
mydata <- mydata %>%
  mutate("Always Avalanche Information" = ifelse(v_11 == "Fünfmal von fünf", 1,0))
mydata$"Always Avalanche Information" <- as.factor(mydata$"Always Avalanche Information")
levels(mydata$"Always Avalanche Information") <- c("no", "yes")

# define feature (Avalanche Education)
mydata <- mydata %>%
  mutate("Avalanche Education" = ifelse(v_12 == "ja" & v_12_1_J_bereinigt >= 2014, 1,0))
mydata$"Avalanche Education" <- as.factor(mydata$`Avalanche Education`)
levels(mydata$"Avalanche Education") <- c("no", "yes")

# define feature (DAV Snowcard)
mydata$"DAV Snowcard" <- mydata$v_14_neu
levels(mydata$"DAV Snowcard") <- c("yes", "no")

# define feature (Filter Method)
mydata$"Filter Method" <- mydata$v_15
levels(mydata$"Filter Method") <- c("yes", "no")

# define feature (Expertise)
mydata$yy <- mydata$v_18_Y
mydata$yy[is.na(mydata$yy)] <- 1
mydata$yyy <- mydata$v_19
levels(mydata$yyy) <- c("5", "4", "3", "2", "1")
mydata$yyy <- factor(mydata$yyy, levels = c("1", "2", "3", "4", "5"))
mydata$yyy <- as.numeric(mydata$yyy)
mydata$yyy[is.na(mydata$yyy)] <- 0
mydata <- mydata %>%
  mutate(Expertise = mydata$yy * mydata$yyy)

# define feature (Direct Avalanche Involvement)
mydata <- mydata %>%
  mutate("Avalanche Involvement" = v_20)
levels(mydata$"Avalanche Involvement") <- c("direct experience",
                                            "indirect experience",
                                            "no experience")

# define feature (Selfassessmnet Experinece)
mydata$"Selfassessmnet Experinece" <- as.numeric(mydata$v_22)
mydata$"Selfassessmnet Experinece" <- mydata$"Selfassessmnet Experinece" - 1
mydata$"Selfassessmnet Experinece"[is.na(mydata$"Selfassessmnet Experinece")] <- 0

# define feature (Selfassessmnet Riskiness)
mydata$"Selfassessmnet Riskiness" <- as.numeric(mydata$v_23)
mydata$"Selfassessmnet Riskiness" <- mydata$"Selfassessmnet Riskiness" - 1
mydata$"Selfassessmnet Riskiness"[is.na(mydata$"Selfassessmnet Riskiness")] <- 0

# define feature (Alpine Education)
mydata <- mydata %>%
  mutate("Alpine Education" = ifelse(v_24_1 == "quoted" |
                                       v_24_2 == "quoted" |
                                       v_24_4 == "quoted" |
                                       v_24_5 == "quoted", 1,0))
mydata %>%
  select(v_24_1,
         v_24_2,
         v_24_3,
         v_24_4,
         v_24_5,
         v_24_6,
         "Alpine Education")
mydata$"Alpine Education" <- as.factor(mydata$"Alpine Education")
levels(mydata$"Alpine Education") <- c("no", "yes")

# define feature (Climate Change Perception)
mydata <- mydata %>%
  mutate("Climate Change Perception" = ifelse(V_25_neu == "Groß" |
                                                V_25_neu == "Sehr groß", 1,0))
mydata %>%
  select(V_25_neu,
         "Climate Change Perception") %>%
  cbind()
mydata$"Climate Change Perception" <- as.factor(mydata$"Climate Change Perception")
levels(mydata$"Climate Change Perception") <- c("no", "yes")

# define feature (University Degree)
mydata <- mydata %>%
  mutate("University Degree" = ifelse(v_29 == "Hochschulabschluss", 1,0))

mydata %>%
  select(v_29,
         "University Degree") %>%
  cbind()
mydata$"University Degree" <- as.factor(mydata$"University Degree")
levels(mydata$"University Degree") <- c("no", "yes")

# define feature (Minors in Haushold)
mydata <- mydata %>%
  mutate("Minors in Haushold" = ifelse(v_30_2 == 0, 0,1))
mydata$"Minors in Haushold"[is.na(mydata$"Minors in Haushold")] <- 0
mydata$`Minors in Haushold` <- as.factor(mydata$`Minors in Haushold`)
levels(mydata$`Minors in Haushold`) <- c("no", "yes")

# define feature (Age)
mydata$Age <- year(today()) - mydata$v_31

# define feature (Gender)
mydata$Gender <- mydata$v_33
levels(mydata$Gender) <- c("male", "female", NA)

# define feature (Residency)
names(mydata)
sum(table(mydata$v_173))
mydata <- mydata %>%
  mutate(Residency = ifelse(v_173 == 83703 |
                              v_173 == 83684 |
                              v_173 == 83714 |
                              v_173 == 83734 |
                              v_173 == 83727 |
                              v_173 == 83730 |
                              v_173 == 83075, 1,0))

mydata %>%
  select(v_173,
         Residency) %>%
  cbind()
mydata$Residency <- as.factor(mydata$Residency)
levels(mydata$Residency) <- c("farway", "nearby")

# rename some features
mydata <- mydata %>%
  mutate(Temprature = Max_temp_d,
         Snowfall = Sum_snow_d,
         Wind = Avg_wind_d,
         Cloudiness = Avg_6t8_cloud_cover_total)

# select the relevant features and drop NA obeservations
mydata <- mydata %>%
  select(`Avalanche Assessment`,
         Temprature,
         Snowfall,
         Wind,
         Cloudiness,
         Residency,
         Gender,
         Age,
         `Minors in Haushold`,
         `University Degree`,
         `Climate Change Perception`,
         `Alpine Education`,
         `Selfassessmnet Riskiness`,
         `Selfassessmnet Experinece`,
         `Leaving Old Piste`,
         `Avalanche Involvement`,
         Expertise,
         `Filter Method`,
         `DAV Snowcard`,
         `Avalanche Education`,
         `Always Avalanche Information`,
         `Group Size`,
         `Avalanche Information`,
         `Terrain Information`,
         `Tour Specific`,
         `Avalanche Danger Level`,
         Incompany,
         Familarity,
         Equipment) %>%
  drop_na()

# Droping NAs => overall 319 Observations for model building

# Set seed and split the data into training/test data
set.seed(1234)
data.tree.split <- initial_split(mydata, prop = 0.7)
data.tree.train <- training(data.tree.split)
data.tree.test <- testing(data.tree.split)

# Fit the tree model
mod.tree <- rpart(Equipment ~.,
                  minsplit = 8,
                  minbucket = round(8/3),
                  cp = 0.01,
                  method = "class",
                  data = data.tree.train)

# Feature importance
mod.tree$variable.importance
plotdf <- data.frame("Var_Imp" = mod.tree$variable.importance)
#pdf(file = "~/Desktop/Taubenstein/varimportanceA.pdf", width = 8, height = 5)
ggplot(plotdf, aes(x = rownames(plotdf), y = Var_Imp/100)) +
  geom_bar(stat = "identity") +
  ylab("") +
  xlab("") +
  coord_flip()
#dev.off()
rownames(plotdf)

# Choose best cp and prune the tree
printcp(mod.tree)
mod.tree <- prune(mod.tree, cp = 0.013793)

# Plot the tree
#pdf(file = "tree.pdf", width = 8, height = 5)
rpart.plot(mod.tree, type = 4, clip.right.labs = FALSE, branch = .3, under = TRUE)
#dev.off()

# 10 k cross validation
set.seed(123)
accuracy <- rep(0, 10)
obs <- matrix(ncol = 10, nrow = 22)
preds <- matrix(ncol = 10, nrow = 22)

for (i in 1:10) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/10)*nrow(data.tree.train))) + 1):((i*round((1/10) * nrow(data.tree.train))))

  # Exclude them from the train set
  train <- data.tree.train[-indices,]

  # Include them in the test set
  test <- data.tree.train[indices,]
  obs[,i] <- test$Equipment

  # A model is learned using each training set
  tree <- rpart(Equipment ~ ., train,
                minsplit = 8,
                minbucket = round(8/3),
                cp = 0.01,
                method = "class")

  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type = "class")
  preds[,i] <- pred

  # Assign the confusion matrix to conf
  conf <- confusionMatrix(pred, as.factor(test$Equipment))

  # Assign the accuracy of this model to the ith index in accuracy vector
  accuracy[i] <- conf$overall[1]
}

mean(accuracy)

# sensitivity analysis
set.seed(123)
# pdf(file = "roc.pdf", width = 8, height = 5)
roc_obj <- roc(data.tree.test$Equipment, as.numeric(tree.pred),
               smoothed = TRUE,
               percent = TRUE,
               # arguments for ci
               ci = TRUE, ci.alpha = 0.9, stratified = FALSE,
               # arguments for plot
               plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE,
               grid = TRUE, print.auc = TRUE, show.thres = TRUE)
# dev.off()

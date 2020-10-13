library(foreign)
library(tidyverse)
library(reshape2)
library(lubridate)
library(readxl)
library(mgcv)
library(rsample)
library(caret)
library(tree)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)

# read data 
alldata <- read.spss(file = "~/Desktop/Taubenstein/survey/Daten_bereinigt_Modell.sav",
                     to.data.frame = TRUE)

# date to GMT format
alldata$Date <- as.Date(as.POSIXct(alldata$Datum, origin = "1582-10-14", tz = "GMT"))

# read data for feature "leaving Old Piste"
loldp <- read_excel("~/Desktop/Taubenstein/survey/ID_Tourenbereich_0_nicht_1_gefährlich.xlsx")

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
wdata <- read_excel("~/Desktop/Taubenstein/survey/Wetterdaten.xlsx",
                    sheet = "AufbereiteteTageswerte")
head(wdata)
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
head(mydata$Equipment)
# control if ok!! check...
mydata %>% select(v_6_D_1,
                  v_6_D_2,
                  v_6_D_3,
                  Equipment) %>% 
  head()
mydata$Equipment <- as.factor(mydata$Equipment)
levels(mydata$Equipment) <- c("Not Carrying", "Carrying Along") 
table(mydata$Equipment)  

# define feature (Familarity) 
mydata$Familarity <- mydata$v_3
levels(mydata$Familarity) <- c("5","4","3","2","1")
mydata$Familarity <- factor(mydata$Familarity, levels = c("1", "2", "3", "4", "5"))
mydata$Familarity <- as.numeric(mydata$Familarity)
mydata$Familarity[is.na(mydata$Familarity)] <- 0
mydata$v_4_2[is.na(mydata$v_4_2)] <- 0
mydata$Familarity <- mydata$Familarity * mydata$v_4_2


# define feature (Incompany) Incompany
mydata$Incompany <- mydata$v_7_3
levels(mydata$Incompany) <- c("no", "yes")

# define feature (Avalanche Danger Level) Avalanche Danger Level
mydata$"Avalanche Danger Level" <- mydata$v_7_2
levels(mydata$"Avalanche Danger Level") <- c("no", "yes")

# define feature (Tour Specific) Tour Specific
mydata$"Tour Specific" <- mydata$v_7_4
levels(mydata$"Tour Specific") <- c("no", "yes")
table(mydata$"Tour Specific")

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
         "Terrain Information") %>% 
  head()
mydata$"Terrain Information" <- as.factor(mydata$"Terrain Information")
levels(mydata$"Terrain Information") <- c("unserious", "serious")
table(mydata$"Terrain Information")

# define feature (Avalanche Information) Avalanche Information
mydata <- mydata %>% 
  mutate("Avalanche Information" = ifelse(v_9_1 == "quoted" |
                                            v_9_2 == "quoted", 1,0))
mydata %>% 
  select(v_9_1,
         v_9_2,
         "Avalanche Information") %>% 
  head()
mydata$"Avalanche Information" <- as.factor(mydata$"Avalanche Information")
levels(mydata$"Avalanche Information") <- c("no", "yes")
table(mydata$"Avalanche Information")

# define feature (Group Size) Groupsize
levels(mydata$v_10) <- c("alone", "in group", "in group")
mydata$"Group Size" <- mydata$v_10

# define feature (Always Avalanche Information) Always Avalanche Information
mydata <- mydata %>% 
  mutate("Always Avalanche Information" = ifelse(v_11 == "Fünfmal von fünf", 1,0))
mydata$"Always Avalanche Information" <- as.factor(mydata$"Always Avalanche Information")
levels(mydata$"Always Avalanche Information") <- c("no", "yes")
table(mydata$"Always Avalanche Information")

# define feature (Avalanche Education) Avalanche Education
mydata$"Avalanche Education" <- mydata$v_12
levels(mydata$"Avalanche Education") <- c("yes", "no")

# define feature (DAV Snowcard) DAV Snowcard 
mydata$"DAV Snowcard" <- mydata$v_14_neu
levels(mydata$"DAV Snowcard") <- c("yes", "no")
table(mydata$`DAV Snowcard`)

# define feature (Filter Method) Filter Method
mydata$"Filter Method" <- mydata$v_15
levels(mydata$"Filter Method") <- c("yes", "no")

# define feature (Expertise) Expertise
mydata$yy <- mydata$v_18_Y
mydata$yy[is.na(mydata$yy)] <- 1
mydata$yyy <- mydata$v_19
levels(mydata$yyy) <- c("5", "4", "3", "2", "1")
mydata$yyy <- factor(mydata$yyy, levels = c("1", "2", "3", "4", "5"))
mydata$yyy <- as.numeric(mydata$yyy)
mydata$yyy[is.na(mydata$yyy)] <- 0
mydata <- mydata %>% 
  mutate(Expertise = mydata$yy * mydata$yyy)
boxplot(mydata$Expertise)

# define feature (Direct Avalanche Involvement)   Avalanche Involvement #####-----------
sum(table(mydata$v_20)) # Ausprägungen direct experience = selber, indirect experience = beobachtet, no experience = nein
mydata <- mydata %>% 
  mutate("Avalanche Involvement" = v_20)
levels(mydata$"Avalanche Involvement") <- c("direct experience", "indirect experience", "no experience")
table(mydata$"Avalanche Involvement")

# define feature (Selfassessmnet Experinece) Selfassessmnet Experinece
mydata$"Selfassessmnet Experinece" <- as.numeric(mydata$v_22)
mydata$"Selfassessmnet Experinece" <- mydata$"Selfassessmnet Experinece" - 1
mydata$"Selfassessmnet Experinece"[is.na(mydata$"Selfassessmnet Experinece")] <- 0

# define feature (Selfassessmnet Riskiness) Selfassessmnet Riskiness
mydata$"Selfassessmnet Riskiness" <- as.numeric(mydata$v_23)
mydata$"Selfassessmnet Riskiness" <- mydata$"Selfassessmnet Riskiness" - 1
mydata$"Selfassessmnet Riskiness"[is.na(mydata$"Selfassessmnet Riskiness")] <- 0

# define feature (Alpine Education) Alpine Education
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
         "Alpine Education") %>% 
  head()
mydata$"Alpine Education" <- as.factor(mydata$"Alpine Education")
levels(mydata$"Alpine Education") <- c("no", "yes")
table(mydata$"Alpine Education")

# define feature (Climate Change Perception) Climate Change Perception ----- eventuell Input Max
mydata <- mydata %>% 
  mutate("Climate Change Perception" = ifelse(V_25_neu == "Groß" |
                                                V_25_neu == "Sehr groß", 1,0))
mydata %>% 
  select(V_25_neu,
         "Climate Change Perception") %>% 
  cbind()
mydata$"Climate Change Perception" <- as.factor(mydata$"Climate Change Perception")
levels(mydata$"Climate Change Perception") <- c("no", "yes")
table(mydata$"Climate Change Perception")

# define feature (University Degree) University Degree
mydata <- mydata %>% 
  mutate("University Degree" = ifelse(v_29 == "Hochschulabschluss", 1,0))

mydata %>% 
  select(v_29,
         "University Degree") %>% 
  cbind()
mydata$"University Degree" <- as.factor(mydata$"University Degree")
levels(mydata$"University Degree") <- c("no", "yes")
table(mydata$"University Degree")

# define feature (Minors in Haushold) Minors in Haushold (yes or no)
mydata <- mydata %>% 
  mutate("Minors in Haushold" = ifelse(v_30_2 == 0, 0,1))
mydata$"Minors in Haushold"[is.na(mydata$"Minors in Haushold")] <- 0
mydata$`Minors in Haushold` <- as.factor(mydata$`Minors in Haushold`)
levels(mydata$`Minors in Haushold`) <- c("no", "yes")
table(mydata$`Minors in Haushold`)

# define feature (Age)
mydata$Age <- year(today()) - mydata$v_31
boxplot(mydata$Age)

# define feature (Gender)
mydata$Gender <- mydata$v_33
levels(mydata$Gender) <- c("male", "female", NA)
table(mydata$Gender)

# define feature (Residency) Residency (nearby farway) die andere Variante ausprobieren
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
table(mydata$Residency)

# rename some features
mydata <- mydata %>% 
  mutate(Temprature = Max_temp_d,
         Snowfall = Sum_snow_d,
         Wind = Avg_wind_d,
         Cloudiness = Avg_6t8_cloud_cover_total)

# define feature Avalanche Assessment
mydata <- mydata %>% 
  mutate("Avalanche Assessment" = v_5)
levels(mydata$`Avalanche Assessment`) <- c("1","2","3","4","5")
mydata$`Avalanche Assessment` <- as.numeric(mydata$`Avalanche Assessment`)
mydata$`Avalanche Assessment` <- mydata$avanlache_report_top - mydata$`Avalanche Assessment`
mydata$`Avalanche Assessment` <- as.factor(mydata$`Avalanche Assessment`)
levels(mydata$`Avalanche Assessment`) <- c("overestimate", "overestimate", 
                                           "realistic", "underestimate")
table(mydata$`Avalanche Assessment`)

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
         Familarity, # out of cv.test
         Equipment) %>% 
  drop_na()

# Droping NAs => overall 328 Observations for model building
# checking for features to be independent 

# Cramer's V -- small = .10, medium = .30, large =.50 literature 
# Cohen, J(1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, N.J.:L Erlbaum Associates
cv.test <- function(x,y){
  cv = sqrt(chisq.test(x,y, correct = FALSE)$statistic /
              (length(x) * (min(length(unique(x)), length(unique(y))) -
                              1)))
  print.noquote("Cramér V:")
  return(as.numeric(cv))
}
cv.test(mydata$Incompany, mydata$`Avalanche Situation`) # .24
cv.test(mydata$Incompany, mydata$`Tour Specific`) # .19
cv.test(mydata$Incompany, mydata$`Terrain Information`) # .09
cv.test(mydata$Incompany, mydata$`Avalanche Information`) # .03
cv.test(mydata$Incompany, mydata$`Group Size`) # 0.05
cv.test(mydata$`Avalanche Education`, mydata$`DAV Snowcard`) # .41
cv.test(mydata$`Avalanche Education`, mydata$`Avalanche Information`) # .14

set.seed(1234)
data.tree.split <- initial_split(mydata, prop = 0.7)
data.tree.train <- training(data.tree.split)
data.tree.test <- testing(data.tree.split)

# fit tree model
mod.tree <- rpart(Equipment ~.,
                  minsplit = 8, #10
                  minbucket = round(8/3), #round(10/3)
                  cp = 0.01,
                  method = "class",
                  data = data.tree.train)

fancyRpartPlot(mod.tree, caption = NULL)
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
# choose best cp and prune the tree
printcp(mod.tree)
mod.tree <- prune(mod.tree, cp = 0.016279)
fancyRpartPlot(mod.tree, caption = NULL)
#pdf(file = "~/Desktop/Taubenstein/treeA.pdf", width = 8, height = 5)
rpart.plot(mod.tree, type = 4, clip.right.labs = FALSE, branch = .3, under = TRUE)
#dev.off()
#prp(tree, extra = 6, box.col = c("pink", "palegreen3"))
# predict 
tree.pred <- predict(mod.tree, data.tree.test, type = "class")

# validate the model
confmat.tree <- confusionMatrix(tree.pred, as.factor(data.tree.test$Equipment))
#save.image(file = "~/Desktop/Taubenstein/analysisA.RData")


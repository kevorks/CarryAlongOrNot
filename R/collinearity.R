install.packages("vcd")
install.packages("corrplot")
library(vcd)
library(corrplot)
# Data prep for collinearity test (categorical features)
mydata.cv <- mydata %>% 
  select(`Avalanche Assessment`,
         Residency,
         Gender,
         `Minors in Haushold`,
         `University Degree`,
         `Climate Change Perception`,
         `Alpine Education`,
         `Leaving Old Piste`,
         `Avalanche Involvement`,
         `Filter Method`,
         `DAV Snowcard`,
         `Avalanche Education`,
         `Always Avalanche Information`,
         `Group Size`,
         `Avalanche Information`,
         `Terrain Information`,
         `Tour Specific`,
         `Avalanche Danger Level`,
         Incompany)
# Initialize empty matrix to store coefficients
empty.m <- matrix(ncol = length(mydata.cv),
                  nrow = length(mydata.cv),
                  dimnames = list(names(mydata.cv), 
                                  names(mydata.cv)))
# Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate.cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor.matrix <- calculate.cramer(empty.m ,mydata.cv)

#pdf(file = "~/Desktop/Taubenstein/cramers_v.pdf", width = 8, height = 5)
corrplot(cor.matrix, type = "upper", method = "number", number.cex = .4)
#dev.off()

# Data prep for collinearity test (metric features)
mydata.col <- mydata %>% 
  select(Temprature,
         Snowfall,
         Wind,
         Cloudiness,
         Age,
         `Selfassessmnet Experinece`,
         `Selfassessmnet Riskiness`,
         Expertise,
         Familarity)

corr.mat <- cor(mydata.col)
#pdf(file = "~/Desktop/Taubenstein/corr_met.pdf", width = 8, height = 5)
corrplot(corr.mat, type = "upper", method = "number", number.cex = .4)
#dev.off()

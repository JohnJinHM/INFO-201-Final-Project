library(stringr)
library(dplyr)
library(ggplot2)

df1 <- read.csv("Global Economy Indicators.csv")
# Clean df1
# df1 <- mapply(str_trim,df1)
# write.csv(df1,"Global Economy Indicators.csv",row.names = FALSE)

df2 <- read.csv("WLD_RTFP_country_2023-10-02.csv")

df1$Year <- as.numeric(df1$Year)
df1 <- df1[df1$Year>=2007&df1$Year<=2021,]

countries <- unique(df2$country)
df2_processed <- data.frame(Year=rep(sort(unique(df1$Year)),each=25),Country=rep(countries))

value_year_country <- function(year,country){
    same_country <- df2[df2$country==country,]
    return(colMeans(same_country[str_detect(same_country$date,as.character(year)),][,1:5], na.rm = TRUE))
}

temp <- mapply(value_year_country,df2_processed$Year,df2_processed$Country)
df2_processed[,3:7] <- t(temp)
colnames(df2_processed)[3:7] <- rownames(temp)

# Full Data
df <- merge(x=df1,y=df2_processed,by=c("Country", "Year"),all.y=TRUE)

# Useful Data
df <- df[,-c(3:5,7,9:24)]

# write.csv(df,"cleaned_dataset.csv",row.names = FALSE)

# Data Cleaning & Augmentation
# Categorical Variable
avg_GNI <- mean(df$Per.capita.GNI,na.rm = TRUE)
df$Above_avg_economy <- df$Per.capita.GNI>avg_GNI
# Numerical Variable
df$Mean_Food_Price_Estimates <- rowMeans(df[,c(7,10)],na.rm = TRUE)
# Summarization data frame: df2_processed contains the mean monthly food price estimates each year of each country ("Open","High","Low","Close","Inflation")
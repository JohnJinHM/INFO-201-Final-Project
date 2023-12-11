library(stringr)
library(dplyr)
library(ggplot2)

df1 <- read.csv("Datasets/Global_Economy_Indicators_cleaned.csv")
# Clean df1
# df1 <- mapply(str_trim,df1)
# write.csv(df1,"Global_Economy_Indicators_cleaned.csv",row.names = FALSE)

df2 <- read.csv("Datasets/wfpvam_foodprices_cleaned.csv")
# Clean df2
# Original file of df2 is too large to be uploaded! (215mb)
# It can be retrieved from https://data.humdata.org/dataset/wfp-food-prices/resource/12d7c8e3-eff9-4db0-93b7-726825c4fe9a
# df2 <- summarise(group_by(df2,adm0_name,mp_year,cm_name),mean_price=mean(mp_price,na.rm = TRUE))
# write.csv(df2,"wfpvam_foodprices_cleaned.csv",row.names = FALSE)
# df2 <- summarise(group_by(df2,adm0_name,mp_year,cm_name,um_name),mean_price=mean(mp_price,na.rm = TRUE))
# write.csv(df2,"wfpvam_foodprices_cleaned.csv",row.names = FALSE)

df1$Year <- as.numeric(df1$Year)

df1_countries <- unique(df1$Country)
df2_countries <- unique(df2$adm0_name)
df2_processed <- data.frame(Year=rep(sort(unique(df1$Year)),each=length(df2_countries)),Country=rep(df2_countries))


df <- merge(x=df1,y=df2,by.x=c("Country", "Year"),by.y=c("adm0_name","mp_year"),all.y=TRUE)

# Filter useful columns
df <- df[,-c(3:5,7,9:24)]

write.csv(df,"Datasets/MergedDataset.csv",row.names = FALSE)

# Data Cleaning & Augmentation
# Categorical Variable
avg_GNI <- mean(df$Per.capita.GNI,na.rm = TRUE)
df$Above_avg_economy <- df$Per.capita.GNI>avg_GNI
# Numerical Variable
df$Food_Price_vs_income <- df$mean_price/df$Per.capita.GNI
# Summarization data frame: df2_processed contains the mean monthly food price estimates each year of each country ("Open","High","Low","Close","Inflation")
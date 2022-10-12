library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("hacksaw")
library(lubridate)
library(hacksaw)

# IMPORT DATA
setwd("C://Users//yudhi//OneDrive//Desktop//#100_Data_Viz//Unicorn Startups//Data")
df = read.csv("unicorns till sep 2022.csv", na.strings = c(""))
head(df)
str(df)
summary(df)

#Check for any missing rows
df[!complete.cases(df), ]


#########---------------------------------------------------DATA CLEANING--------------------------------------------------------------------###


#Mismatched data
df[df$Country == 'Singapore',]
df[df$Country == 'Hong Kong' & df$City. == 'Fintech',]
df[df$Country == 'Hong Kong' & df$City. == 'E-commerce & direct-to-consumer',]
df[df$Country == 'Bahamas',]


#-------Shift --------
#Get index of rows
index1 <- as.numeric(rownames(df[df$Country == 'Hong Kong' & df$City. == 'Fintech',]))
index2 <- as.numeric(rownames(df[df$Country == 'Hong Kong' & df$City. == 'E-commerce & direct-to-consumer',]))
index3 <- as.numeric(rownames(df[df$Country == 'Singapore',]))
index4 <- as.numeric(rownames(df[df$Country == 'Bahamas',]))
index <- c(index1, index2, index3, index4)
paste(index)

# Loop to shift rows
for (i in index) {
  df[i,c(5:7)] <- shift_row_values(df[i,c(5:7)],.dir = "right")
}

#Check for any missing rows
df[!complete.cases(df), ]



##---------------------------Formatting

#rename columns
colnames(df) <- c("Company","Valuation", "Year", "Country", "City", "Industry", "Investors")

#---Remove Dollar sign ($)
df$Valuation <- gsub("\\$", "", df$Valuation)

#Converting Character to Numeric
df$Valuation <- as.numeric(df$Valuation)

#CONVERTING DATETIME
as.POSIXct(df$Year, format="%m/%d/%Y")

df$Year <- as.POSIXct(df$Year, format="%m/%d/%Y")
summary(df)

# Convert to to Factor
df$Company <- factor(df$Company)
df$Country <- factor(df$Country)
df$City. <- factor(df$City)
df$Industry <- factor(df$Industry)
df$Year <- factor(df$Year)

# Check
summary(df)


# Select important columns only
clean_df <- df %>%
  select(Year, Company, Country, Industry, Valuation) %>%
  arrange(Year)

#Separate Year intoYear, Month and Day
clean_df <- clean_df %>%
  separate(data=., col=Year, into=c("Year", "Month", "Day"), sep="-")

clean_df$Year <- factor(clean_df$Year)
clean_df$Month <- factor(clean_df$Month)

# write.csv(clean_df,"C:\\Users\\yudhi\\OneDrive\\Desktop\\#100_Data_Viz\\Unicorn Startups\\Data\\clean_df.csv", row.names = FALSE)


library(ggraph)
library(igraph)
library(tidyverse)

# create an edge list data frame giving the hierarchical structure of your individuals
d1 <- data.frame(from="origin", to=paste("group", seq(1,5), sep=""))
d2 <- data.frame(from=rep(d1$to, each=5), to=paste("subgroup", seq(1,25), sep="_"))
edges <- rbind(d1, d2)

# Create a graph object 
mygraph <- graph_from_data_frame( edges )

# Basic tree
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()



warpbreaks=warpbreaks




































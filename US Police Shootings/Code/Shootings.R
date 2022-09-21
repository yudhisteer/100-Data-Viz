install.packages("magrittr")
install.packages("dplyr")  
library(lubridate)
install.packages("ggchicklet", repos = "https://cinc.rud.is")
library("ggchicklet")
library(magrittr)
library(dplyr)


#---------------- IMPORT DATA
setwd("C://Users//yudhi//OneDrive//Desktop//#100_Data_Viz//US Police Shootings//Data")
df = read.csv("US Police shootings in from 2015-22.csv", na.strings = c(""))
#rm(df)


#---------------- EXPLORE DATA
head(df)
summary(df)
str

#---------------- FACTORIZE
df$manner_of_death <- factor(df$manner_of_death)
df$id <- factor(df$id)
df$name <- factor(df$name)
df$armed <- factor(df$armed)
df$gender <- factor(df$gender)
df$race <- factor(df$race)
df$city <- factor(df$city)
df$state <- factor(df$state)
df$threat_level <- factor(df$threat_level)
df$flee <- factor(df$flee)
df$longitude <- factor(df$longitude)
df$latitude <- factor(df$latitude)
summary(df)


cpal = c("#F8766D", "#D89000", "#A3A500", "#39B600", "#00BF7D",
         "#00BFC4", "#00B0F6", "#9590FF", "#E76BF3", "#FF62BC")





---------------------------------------------------------------------------------------------------------------


#---------------- LOCATE MISSING DATA
df[!complete.cases(df),]



#---------------- REPLACING MISSING VALUES: FACTUAL ANALYSIS (GENDER)
df[is.na(df$gender),]

# rows where gender = NA AND name != NA
df[is.na(df$gender) & !(is.na(df$name)),] 
df[is.na(df$gender) & !(is.na(df$name)),'gender'] <- 'M'

#check
df[c(2653, 7314, 7322, 7455, 7462),]



#---------------- REPLACING MISSING VALUES: FACTUAL ANALYSIS (ARMED)
df[is.na(df$armed),]

# rows where gender = NA AND name != NA
df[is.na(df$armed),'armed'] <- 'undetermined'

#check
df[is.na(df$armed),]




#---------------- REPLACING MISSING VALUES: FACTUAL ANALYSIS (FLEE)
df[is.na(df$flee),]

# rows where gender = NA AND name != NA
df[is.na(df$flee),'flee'] <- 'Other'

#check
df[is.na(df$flee),]




#---------------- REPLACING MISSING VALUES: FACTUAL ANALYSIS (LONGITUDE & LATITUDE) ***
df[is.na(df$longitude),]

# rows where gender = NA AND name != NA
df[is.na(df$longitude) & !(is.na(df$city)),] 







#---------------- REMOVE MISSING DATA (GENDER)
df_backup <- df

df[!complete.cases(df),]
df[is.na(df$gender),] #15 missing rows
df[!is.na(df$gender),] #non-missing rows

df <- df[!is.na(df$gender),] #update df with non-missing rows
df

#check
summary(df)
df[is.na(df$gender),]


--------------------------------------------------------------------------------------------------------


#---------------- GROUP WEAPONS
df$armed <- gsub("vehicle and machete", "vehicle", df$armed)
df$armed <- gsub("vehicle and gun", "vehicle", df$armed)
df$armed <- gsub("vehicle and machete", "vehicle", df$armed)
df$armed <- gsub("car, knife and mace" , "vehicle", df$armed)

df$armed <- gsub("baseball bat and bottle", "baseball bat", df$armed)
df$armed <- gsub("baseball bat and knife", "baseball bat", df$armed)
df$armed <- gsub("baseball bat and fireplace poker", "baseball bat", df$armed)

df$armed <- gsub("gun and car", "gun", df$armed)
df$armed <- gsub("gun and machete", "gun", df$armed)
df$armed <- gsub("gun and vehicle", "gun", df$armed)
df$armed <- gsub("gun and knife", "gun", df$armed)
df$armed <- gsub("gun and sword", "gun", df$armed)
df$armed <- gsub("gun and explosives", "gun", df$armed)
df$armed <- gsub("guns and explosives", "gun", df$armed)

df$armed <- gsub("metal hand tool", "metal object", df$armed)
df$armed <- gsub("metal pipe", "metal object", df$armed)
df$armed <- gsub("metal rake", "metal object", df$armed)
df$armed <- gsub("metal pole", "metal object", df$armed)
df$armed <- gsub("metal stick", "metal object", df$armed)
df$armed <- gsub("pole and knife" , "metal object", df$armed)
df$armed <- gsub("pole" , "metal object", df$armed)
df$armed <- gsub("pipe" , "metal object", df$armed)

df$armed <- gsub("ax and machete" , "ax", df$armed)
df$armed <- gsub("ax and gun" , "ax", df$armed)
df$armed <- gsub("hatchet" , "ax", df$armed)

df$armed <- gsub("BB gun and vehicle", "BB gun", df$armed)

df$armed <- gsub("knife and vehicle", "knife", df$armed)
df$armed <- gsub("knife, hammer and gasoline can", "knife", df$armed)

df$armed <- gsub("beer bottle", "bottle", df$armed)

df$armed <- gsub("machete and gun", "machete", df$armed)
df$armed <- gsub("machete and hammer" , "machete", df$armed)

df$armed <- gsub("chain saw", "chainsaw", df$armed)

df$armed <- gsub("hand torch" , "flashlight", df$armed)

df$armed <- gsub("hammer and garden tool" , "hammer", df$armed)

df$armed <- gsub("samurai sword" , "sword", df$armed)

df$armed <- gsub("bow and arrow" , "crossbow", df$armed)


#Factor Again
df$armed <- factor(df$armed)

#check
summary(df)




#---------------- GROUP BY ARMED
library(dplyr)
library(magrittr)

#Separate df from Armed with frequency
armed_df <- df %>% 
  group_by(armed) %>%
  summarise(Count = n())  %>%
  arrange(armed) %>%
  mutate(id = seq(1,105))

head(armed_df)
summary(armed_df)

armed_df[!complete.cases(armed_df),]
armed_df[is.na(armed_df$Count),]


#---------------- VISUALIZATION

# We observe we have very low values for most of the variables
# and a few high values for some few variables
# Does not display a proper bar chart
ggplot(armed_df, aes(x=armed, y=Count)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7))

#---------------- LOG TRANSFORMATION

armed_df$log <- log(armed_df$Count + 1)
head(armed_df)

armed_df[!complete.cases(armed_df),]
armed_df[is.na(armed_df$log),]

#---------------- BARCHART
# We observe a more proper barchart after transforming our data
ggplot(armed_df, aes(x=armed, y=log)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7))


#---------------- CIRCULAR BARCHART

# Too small
ggplot(armed_df, aes(x=armed, y=log)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  coord_polar(start = 0)

#Dirty labels
ggplot(armed_df, aes(x=armed, y=log)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  coord_polar(start = 0) +
  ylim(-10,10)


#Removing Labels
ggplot(data=armed_df, aes(x=armed, y=log)) + 
  geom_bar(stat="identity", col = "darkblue", fill=alpha("skyblue", 0.7)) +
  coord_polar(start = 0) +
  ylim(-10,10) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")) 




#Make labels
label_data <- armed_df
label_data
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar 
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#make graph
armed_df %>%
  ggplot(aes(x = armed, y = log)) + 
  geom_bar(stat = "identity", col = "darkblue", fill = "skyblue", alpha = 0.7) +
  ylim(-10,10)  + 
  coord_polar(start = 0) + 
  
  geom_text(data=armed_df,aes(label=armed, x=id, y=log+3),
            hjust = label_data$hjust,
            color = "black",
            fontface = "bold",
            alpha = 0.6,
            size = 2.5,
            angle = label_data$angle,
            inherit.aes = FALSE) +

  theme_minimal() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank(),
         plot.margin = unit(rep(-1,4), "cm")) 



--------------------------------------------------------------------------------------------------------

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")  
install.packages("treemap")
library(treemap)
library(magrittr)
library(dplyr)

police_df <- df %>% 
  group_by(manner_of_death) %>%
  summarise(Count = n())  %>%
  arrange(manner_of_death)



#---------------- POLICE GROUP TREE MAP
#create empty column
police_df$subgroup <- NA
police_df[police_df$manner_of_death == 'shot','subgroup'] <- 1
police_df[police_df$manner_of_death == 'shot and Tasered','subgroup'] <- 2
police_df$manner_of_death <- gsub('shot and Tasered' , 'shot & tasered', police_df$manner_of_death) #rename

police_df


png(filename="pol_gr.png",width=1500, height=800)
treemap(police_df,
        index=c("manner_of_death","subgroup"),
        vSize="Count",
        type="index",
        fontsize.labels=c(22),
        fontcolor.labels=c("black"), 
        overlap.labels=0.5,  
        inflate.labels=F,
        
        palette = c("#FFC519", #gold
                    "#667b68", #green
                    "#f0f0f0"  ##white
        ),
        title="Manner of death of assailants",
        fontsize.title=30)
dev.off()





#---------------- POLICE UNGROUP TREE MAP
png(filename="pol_ung.png",width=1200, height=800)
treemap(police_df,
        index="manner_of_death",
        vSize="Count",
        type="index",
        fontsize.labels=c(13),
        fontcolor.labels=c("black"), 
        overlap.labels=0.5,  
        inflate.labels=F,
        
        palette = c("#E7298A", #red
                    "#D95F02", #orange
                    "#FFC519", #gold
                    "#1F78B4", #blue
                    "#1B9E77", #green
                    "#667b68", #green
                    "#7570B3", #purple
                    "#f0f0f0",  ##white
                    "#967259" #brown         
        ),
        title=" ",
        fontsize.title=1)
dev.off()








#---------------- VICTIM UNGROUP TREE MAP

#Separate df from Armed with frequency
victim_df <- df %>% 
  group_by(armed) %>%
  summarise(Count = n())  %>%
  arrange(armed)

# treemap
png(filename="vic_ung.png",width=1200, height=800)
treemap(victim_df,
        index="armed",
        vSize="Count",
        type="index",
        fontsize.labels=c(15),
        fontcolor.labels=c("black"), 
        overlap.labels=0.5,  
        inflate.labels=F,
        
        palette = c("#E7298A", #red
                    "#D95F02", #orange
                    "#FFC519", #gold
                    "#1F78B4", #blue
                    "#1B9E77", #green
                    "#667b68", #green
                    "#7570B3", #purple
                    "#f0f0f0",  ##white
                    "#967259" #brown
                    
        ),
        title=" ",
        fontsize.title=12)
dev.off()


#---------------- VICTIM GROUP TREE MAP

#create empty column
victim_df$subgroup <- NA

victim_df[victim_df$armed == 'gun','subgroup'] <- 1
victim_df[victim_df$armed == "gun and car",'subgroup'] <- 1
victim_df[victim_df$armed == "gun and machete",'subgroup'] <- 1
victim_df[victim_df$armed == "gun and knife",'subgroup'] <- 1
victim_df[victim_df$armed == "gun and sword",'subgroup'] <- 1
victim_df[victim_df$armed == "gun and explosives",'subgroup'] <- 1
victim_df[victim_df$armed == "gun and vehicle",'subgroup'] <- 1
victim_df[victim_df$armed == "guns and explosives",'subgroup'] <- 1

victim_df[victim_df$armed == "metal object",'subgroup'] <- 2
victim_df[victim_df$armed == "metal hand tool",'subgroup'] <- 2
victim_df[victim_df$armed == "metal pipe",'subgroup'] <- 2
victim_df[victim_df$armed == "metal pole",'subgroup'] <- 2
victim_df[victim_df$armed == "metal stick",'subgroup'] <- 2
victim_df[victim_df$armed == "metal rake",'subgroup'] <- 2
victim_df[victim_df$armed == "pole and knife",'subgroup'] <- 2
victim_df[victim_df$armed == "pole",'subgroup'] <- 2
victim_df[victim_df$armed == "pipe",'subgroup'] <- 2

victim_df[victim_df$armed == "vehicle",'subgroup'] <- 3
victim_df[victim_df$armed == "vehicle and machete",'subgroup'] <- 3
victim_df[victim_df$armed == "vehicle and gun",'subgroup'] <- 3
victim_df[victim_df$armed == "vehicle and machete",'subgroup'] <- 3
victim_df[victim_df$armed == "car, knife and mace",'subgroup'] <- 3
victim_df[victim_df$armed == "motorcycle",'subgroup'] <- 3

victim_df[victim_df$armed == "baseball bat",'subgroup'] <- 4
victim_df[victim_df$armed == "baseball bat and bottle",'subgroup'] <- 4
victim_df[victim_df$armed == "baseball bat and knife",'subgroup'] <- 4
victim_df[victim_df$armed == "baseball bat and fireplace poker",'subgroup'] <- 4

victim_df[victim_df$armed == "ax",'subgroup'] <- 5
victim_df[victim_df$armed == "ax and machete",'subgroup'] <- 5
victim_df[victim_df$armed == "ax and gun",'subgroup'] <- 5
victim_df[victim_df$armed == "hatchet",'subgroup'] <- 5

victim_df[victim_df$armed == "unarmed",'subgroup'] <- 6
victim_df[victim_df$armed == "undetermined",'subgroup'] <- 6
victim_df[victim_df$armed == "unknown weapon",'subgroup'] <- 6

victim_df[victim_df$armed == "knife",'subgroup'] <- 7
victim_df[victim_df$armed == "knife and vehicle",'subgroup'] <- 7
victim_df[victim_df$armed == "knife, hammer and gasoline can",'subgroup'] <- 7

victim_df[victim_df$armed == "machete",'subgroup'] <- 8
victim_df[victim_df$armed == "machete and gun",'subgroup'] <- 8
victim_df[victim_df$armed == "machete and hammer",'subgroup'] <- 8

victim_df[victim_df$armed == "BB gun",'subgroup'] <- 9
victim_df[victim_df$armed == "BB gun and vehicle",'subgroup'] <- 9

victim_df[victim_df$armed == "chain saw",'subgroup'] <- 10
victim_df[victim_df$armed == "chainsaw",'subgroup'] <- 10

victim_df[victim_df$armed == "bottle",'subgroup'] <- 11
victim_df[victim_df$armed == "beer bottle",'subgroup'] <- 11

victim_df[victim_df$armed == "flashlight",'subgroup'] <- 12
victim_df[victim_df$armed == "hand torch",'subgroup'] <- 12

victim_df[victim_df$armed == "hammer",'subgroup'] <- 13
victim_df[victim_df$armed == "hammer and garden tool",'subgroup'] <- 13

victim_df[victim_df$armed == "sword",'subgroup'] <- 14
victim_df[victim_df$armed == "samurai sword",'subgroup'] <- 14

victim_df[victim_df$armed == "crossbow",'subgroup'] <- 15
victim_df[victim_df$armed == "bow and arrow",'subgroup'] <- 15

victim_df[is.na(victim_df$subgroup),'subgroup'] <-17

victim_df[victim_df$armed == "toy weapon",'subgroup'] <- 18



png(filename="vic_gr0.png",width=1500, height=800)
treemap(victim_df,
        index=c("subgroup","armed"),
        vSize="Count",
        type="index",
        fontsize.labels=c(1,22),
        fontcolor.labels=c("transparent","black"),
        fontface.labels=c(2,1),
        align.labels=list(
          c("right", "bottom"), 
          c("center", "center")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        palette = c("#E7298A", #red
                    "#FFC519", #gold
                    "#D95F02", #orange
                    "#1F78B4", #blue
                    "#1B9E77", #green
                    "#7570B3", #purple
                    "#f0f0f0",  ##white
                    "#667b68", #darkgreen
                    "#967259" #brown
        ),
        title="Range of weapons of assailants",
        fontsize.title=30)
dev.off()




#--------------------------------------------------------------------------------------------------------
  

#---------------- KILLINGS OVER THE YEARS
install.packages("magrittr")
install.packages("dplyr")  
library(lubridate)
install.packages("ggchicklet", repos = "https://cinc.rud.is")
library("ggchicklet")
library(magrittr)
library(dplyr)

killings_df <- df[,3, drop=FALSE]
killings_df


#Group killings by date
gro_kil_df <- killings_df %>% 
  group_by(date) %>%
  summarise(Count = n()) %>%
  arrange(mdy(date))




#---------------- UNGROUP BARCHART FOR 2015

#gro_kil_df$date <- factor(gro_kil_df$date,levels = gro_kil_df$date[order(gro_kil_df$CUMFREQ, decreasing = F)])

#subset 100 of 2015 data only. Full = 341
df_2015 = gro_kil_df[c(1:341),]

#convert datetime to character
df_2015$mo <- as.character(df_2015$date)

png(filename="2015.png",width=1200, height=800)
color <- "#d20000"
ggplot(df_2015, aes(x=mo, y=Count)) + 
  geom_chicklet(radius = grid::unit(1.5, "mm"),color=color, fill=color, width=3)+
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()) +
    scale_y_reverse() + 
    geom_col(aes(y = Count / 100),color=color, fill=color, width=1)
dev.off()




#----------------GROUP BY MONTH

#convert character to datetime
gro_kil_df$date <- strptime(gro_kil_df$date, format = "%m/%d/%Y")

#group by month
mo_df <- gro_kil_df %>% 
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  summarize(sum_count = sum(Count))

#convert datetime to character
mo_df$mo <- as.character(mo_df$month)


mo_df[c(1:12),'year'] <- '2015'
mo_df[c(13:24),'year'] <- '2016'
mo_df[c(25:36),'year'] <- '2017'
mo_df[c(37:48),'year'] <- '2018'
mo_df[c(49:60),'year'] <- '2019'
mo_df[c(61:72),'year'] <- '2020'
mo_df[c(73:84),'year'] <- '2021'
mo_df[c(85:93),'year'] <- '2022'


color <- "#d20000"
o <- ggplot(mo_df, aes(x=mo, y=sum_count)) + 
  geom_chicklet(radius = grid::unit(2, "mm"),color=color, fill=color, width=.95)+
  theme_minimal() +
  ylab("Monthly Deaths") +
  theme(axis.text.y = element_text(color='black',size=20),
        axis.title.y = element_text(color='black',size=20, vjust=8, hjust=.6, 
                                    face =2),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 50)) +
  scale_y_reverse() + 
  #scale_x_discrete(position = "top") +
  geom_col(aes(y = sum_count/10),color=color, fill=color, width=1) +
  annotate("text", x = c(12,24,36,48,60,72,84), y=-2.5, label = "―", 
           angle=90, fontface =2, size=5) +
  annotate("text", x = c(6,18,30,42, 54, 66, 78, 88), y=-2.5, 
           label = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
           size=8, vjust=-.5)

#VIEW GRAPH
o

#DOWNLOAD GRAPH
png(filename="summary.png",width=1500, height=800)
o
dev.off()




#----------------FACET BARCHART

gro_kil_df$year <- NA

gro_kil_df[c(1:341),'year'] <- '2015'
gro_kil_df[c(342:681),'year'] <- '2016'
gro_kil_df[c(682:1022),'year'] <- '2017'
gro_kil_df[c(1023:1352),'year'] <- '2018'
gro_kil_df[c(1353:1686),'year'] <- '2019'
gro_kil_df[c(1687:2034),'year'] <- '2020'
gro_kil_df[c(2035:2380),'year'] <- '2021'
gro_kil_df[c(2381:2618),'year'] <- '2022'


#a = gro_kil_df[c(1:1022),]

#convert datetime to character
gro_kil_df$mo <- as.character(gro_kil_df$date)


png(filename="2015_2022.png",width=1200, height=800)
color <- "#d20000"
ggplot(gro_kil_df, aes(x=mo, y=Count)) + 
  geom_chicklet(radius = grid::unit(.7, "mm"),color=color, fill=color, width=1)+
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()) +
  scale_y_reverse() + 
  geom_col(aes(y = Count / 100),color=color, fill=color, width=1) +
  facet_wrap(year~., scales = "free", ncol=2) 
dev.off()




#----------------FACET BARCHART: EVRY 2 YEARS

#Each 2years
df_2 <- gro_kil_df[c(1:681),]

#convert datetime to character
df_2$mo <- as.character(df_2$date)

png(filename="2015_2026.png",width=1200, height=800)
color <- "#d20000"
ggplot(df_2, aes(x=mo, y=Count)) + 
  geom_chicklet(radius = grid::unit(.7, "mm"),color=color, fill=color, width=1)+
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()) +
  scale_y_reverse() + 
  geom_col(aes(y = Count / 100),color=color, fill=color, width=1) +
  facet_wrap(year~., scales = "free", ncol=1) 
dev.off()


-----------------------------------------------------------------------------------------------------

  
#------------------HISTOGRAMS AND DENSITY CHARTS
  
summary(df)
  
#subsetting age and gender
age_gender_na <- df[,c(6,7)]
summary(age_gender_na)

#taking only non-missingg values
age_gender <- age_gender_na[!is.na(age_gender_na$age),]

summary(age_gender_na[!is.na(age_gender_na$age),])

#check
summary(age_gender)
  

#----------------HISTOGRAM OF AGE OF BOTH GENDER
install.packages("hrbrthemes")
library(hrbrthemes)

s <- ggplot(data=age_gender, aes(x=x))
s + geom_density(aes(x=age, y = ..density..), fill="#69b3a2",
                   alpha=0.5, position="identity", bins=30)

#CALCULATE MEDIAN OF AGE
men_med <- median(age_gender[age_gender$gender=='M','age'])
women_med <- median(age_gender[age_gender$gender=='F','age'])

#----------------DENSITY OF AGE W.R.T GENDER

install.packages("GGally")  
library(GGally)
ggpairs(df[,c(6,7,8, 11, 12, 13,14)], aes(colour = body_camera, alpha = 0.4))



n <- ggplot(data=age_gender, aes(x=x)) +
geom_density(aes(x=age, y = ..density.., color=gender), fill="white", size = 2,
                 alpha=0.4, position="identity") +
  
  geom_vline(xintercept = men_med, size=1, linetype="dashed", color="#00BFC4") +
  geom_vline(xintercept = women_med, size=1, linetype="dashed", color="#F8766D") +
  
  theme_minimal() +
  ylab("Density") +
  xlab("Age") +
  ggtitle("Age distribution w.r.t gender") +
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.45,face =2),
        axis.title.x = element_text(color='black',size=20,hjust=.38,face =2),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(1,1),
        plot.title = element_text(color='black', 
                                  size = 30, 
                                  family = 'times', 
                                  hjust=0.5, vjust=2.0))

#SHOW GRAPH
n

#DOWNLOAD GRAPH
png(filename="age.png",width=1500, height=800)
n
dev.off()





------------------------------------------------------------------------------------------------------------
#----------------------------------RACE VS BODYCAM
  
install.packages("GGally")  
library(GGally)
ggpairs(df[,c(6,7,8, 11, 12, 13,14)], aes(colour = body_camera, alpha = 0.4))
  
  
# Subset Race and Body Cam  
race_na <- df[,c(8,14)]

#remove empty rows  
df_race <- race_na[!is.na(race_na$race),]
summary(df_race)

#Group killings by date
gro_race <- df_race %>% 
  group_by(race,body_camera) %>%
  summarise(race_count = n())

  
ggplot(gro_race, aes(x=race,y=log(race_count),fill=factor(body_camera)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="body_camera",
                      labels=c("ON", "OFF"))+
  xlab("Beverage")+ylab("Mean Percentage")


ggplot(gro_race, aes(x=race,y=race_count,fill=body_camera))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(labels=c("ON", "OFF"))+
  xlab("Beverage")+ylab("Mean Percentage")+
  scale_fill_manual(values=c("#2e9551",
                             "#bfbfbf"))




#---------------  BAR CHART OF BODY CAM VS RACE

png(filename="race.png",width=1500, height=800)
ggplot(gro_race, aes(x=race,y=race_count,fill=factor(body_camera)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c(color,"#bfbfbf"), name="body_camera",labels=c("OFF", "ON")) +
  theme_minimal() +
  ylab("Number of deaths") +
  ggtitle("Correlation of number of deaths w.r.t status of body camera") +
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18, angle=90, hjust=1.1),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0))+
  scale_x_discrete(labels = c("American Native or \n Aslakan Native",
                              'Black or \n African American',
                              'Hispanic or \n Latino',
                              'Not Hispanic or \n Latino',
                              'Unknown Race',
                              'White'))
dev.off()



---------------------------------------------------------------------------------------------

###------------------------CORRELATION OF RACE W.R.T OTHER VARIABLES (BOXPLOT)
  
  
cpal = c("#F8766D", "#00B0F6", "#00BFC4", "#D89000", "#39B600", "#9590FF",
            "#E76BF3", "#FF62BC")  
  
  
  # Subset Race and Body Cam  
flee_na <- df[,c(8,11,12,13,14)]
summary(flee_na)

#remove empty rows  
df_flee <- flee_na[!is.na(flee_na$race),]
summary(df_flee)

#Group killings by date
gro_flee <- df_flee %>% 
  group_by(race,body_camera,flee,signs_of_mental_illness,threat_level) %>%
  summarise(count = n())


summary(gro_flee)


#----BOXPLOT OF RACE VS BODY CAM <<<<<<<<
p <- ggplot(gro_flee, aes(x=race, y=log(count), fill=body_camera)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#F8766D", "#00B0F6"), name="body_camera",labels=c("OFF", "ON")) +
  theme_minimal() +
  ylab("Log of number of people") +
  ggtitle("Correlation of status of body camera w.r.t race") +
  
theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18, angle=90, hjust=1.1),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(1,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0)) +
  
  scale_x_discrete(labels = c("American Native or \n Aslakan Native",
                              'Black or \n African American',
                              'Hispanic or \n Latino',
                              'Not Hispanic or \n Latino',
                              'Unknown Race',
                              'White'))

p


#Download graph
png(filename="box_race.png",width=1500, height=800)
p
dev.off()



#----BOXPLOT OF RACE VS FLEE <<<<<<<<

q <- ggplot(gro_flee, aes(x=race, y=log(count), fill=flee)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#D89000","#39B600", "#F8766D","#00B0F6"), name="body_camera") +
  theme_minimal() +
  ylab("Log number of people fleeing") +
  ggtitle("Correlation of number of people fleeing w.r.t race") +
  
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18, angle=90, hjust=1.1),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(2,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0)) +
  
  scale_x_discrete(labels = c("American Native or \n Aslakan Native",
                              'Black or \n African American',
                              'Hispanic or \n Latino',
                              'Not Hispanic or \n Latino',
                              'Unknown Race',
                              'White'))

q


# DOWNLOAD GRAPH
png(filename="race_flee.png",width=1500, height=800)
q
dev.off()







#----BOXPLOT OF RACE VS THREAT LEVEL <<<<<<<<

r <- ggplot(gro_flee, aes(x=race, y=log(count), fill=threat_level)) + 
  geom_boxplot()+
  scale_fill_manual(values=c( "#F8766D","#00B0F6", "#39B600"), name="body_camera") +
  theme_minimal() +
  ylab("Log number of people") +
  ggtitle("Correlation of threat level w.r.t race") +
  
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18, angle=90, hjust=1.1),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(2,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0)) +
  
  scale_x_discrete(labels = c("American Native or \n Aslakan Native",
                              'Black or \n African American',
                              'Hispanic or \n Latino',
                              'Not Hispanic or \n Latino',
                              'Unknown Race',
                              'White'))
r


#DOWNLOAD GRAPH
png(filename="race_threat.png",width=1500, height=800)
r
dev.off()




#----BOXPLOT OF RACE VS SIGNS OF MENTAL ILLNESS  <<<<<<<<

s <- ggplot(gro_flee, aes(x=race, y=log(count), fill=signs_of_mental_illness)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#F8766D", "#00B0F6"), name="body_camera",labels=c("OFF", "ON")) +
  theme_minimal() +
  ylab("Log number of people") +
  ggtitle("Correlation of mental illness w.r.t race") +
  
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18, angle=90, hjust=1.1),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(2,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0)) +
  
  scale_x_discrete(labels = c("American Native or \n Aslakan Native",
                              'Black or \n African American',
                              'Hispanic or \n Latino',
                              'Not Hispanic or \n Latino',
                              'Unknown Race',
                              'White'))
# VIEW GRAPH
s

# DOWNLOAD GRAPH
png(filename="race_mental.png",width=1500, height=800)
s
dev.off()


---------------------------------------------------------------------------------------------------
install.packages("GGally")  
library(GGally)

ggpairs(age_gender_race[,c(1,2)], aes(colour = gender, alpha = 0.1))
ggpairs(age_gender_race, columns = 1:3, ggplot2::aes(colour=gender)) 



#subsetting age, race and gender
age_gender_race_na <- df[,c(6,7, 8)]
summary(age_gender_race_na)


#remove empty rows  
age_gender_race_na[!is.na(age_gender_race_na$race) & !is.na(age_gender_race_na$age),]
summary(age_gender_race_na[!is.na(age_gender_race_na$race) & !is.na(age_gender_race_na$age),])
age_gender_race <- age_gender_race_na[!is.na(age_gender_race_na$race) & !is.na(age_gender_race_na$age),]
summary(age_gender_race)


ggplot(age_gender_race, aes(x=age, color=gender)) + 
  geom_density()


ggplot(age_gender_race, aes(x=age, color=gender)) + 
  geom_histogram()

ggplot(age_gender_race, aes(x=age, color=gender, fill=gender)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.005) 



# BARBLOT OF DISTRIBUTION OF GENDER
t <- ggplot(age_gender_race, aes(x=gender, color=gender)) + 
  geom_bar(fill="white", size = 2)+
  theme_minimal() +
  ylab("Frequency") +
  ggtitle("Distribution of number of deaths based on gender") +
  
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18, angle=0, hjust=1.1),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "none",
        #legend.justification = c(2,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0))


# VIEW GRAPH
t

# DOWNLOAD GRAPH
png(filename="people.png",width=1500, height=800)
t
dev.off()






# Subset Race and Body Cam  
race_na <- df[,c(7,8)]
summary(race_na)


#remove empty rows  
df_race <- race_na[!is.na(race_na$race),]
summary(df_race)


ggplot(df_race, aes(x=race, color=race)) + 
  geom_bar(fill="#e5e5e5")




df_race[df_race$race =='B',]
df_race[df_race$race =='W',]
df_race[df_race$race =='H',]


subset_race <- df_race %>% 
  filter(race %in% c("B", "W", "H")) 


gro_race <- subset_race %>% 
  group_by(race) %>% 
  summarise(count=n())

gro_race$ratio <- NA


gro_race[gro_race$race =='B','ratio'] <- as.numeric(gro_race[gro_race$race =='B','count'])/41.6 #41.6 million of black people in US
gro_race[gro_race$race =='W','ratio'] <- as.numeric(gro_race[gro_race$race =='W','count'])/231.9
gro_race[gro_race$race =='H','ratio'] <- as.numeric(gro_race[gro_race$race =='H','count'])/62.57



u <- ggplot(data=gro_race, aes(x=race, y=count)) +
  geom_bar(stat="identity", fill=c("#191a1e","#f9cb40", "#dddddd")) +
  theme_minimal() +
  ylab("Number of people killed") +
  ggtitle("Number of people killed w.r.t race") +
  
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(2,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0)) +
  
  scale_x_discrete(labels = c('Black or African American',
                              'Hispanic or Latino',
                              'White'))

#SHOW GRAPH
u

#DOWNLOAD GRAPH
png(filename="BHW.png",width=1500, height=800)
u
dev.off()




v <- ggplot(data=gro_race, aes(x=race, y=ratio)) +
  geom_bar(stat="identity", fill=c("#191a1e","#f9cb40", "#dddddd")) +
  theme_minimal() +
  ylab("Number of people killed per million") +
  ggtitle("Number of people killed per million w.r.t race") +
  
  theme(axis.text.y = element_text(color='black',size=18),
        axis.text.x = element_text(color='black',size=18),
        axis.title.y = element_text(color='black',size=20,vjust=2.5,hjust=.55,face =2),
        axis.title.x = element_blank(),
        
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "top",
        #legend.justification = c(2,1),
        plot.title = element_text(color='black', 
                                  size = 30,
                                  hjust=0.5, vjust=2.0)) +
  
  scale_x_discrete(labels = c('Black or African American',
                              'Hispanic or Latino',
                              'White'))


#SHOW GRAPH
v

#DOWNLOAD GRAPH
png(filename="BHW_million.png",width=1500, height=800)
v
dev.off()












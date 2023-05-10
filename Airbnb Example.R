#Airbnb NYC EDA 
#Loading data
getwd()
setwd("C:/Users/James/Documents/R Studio/Practice")
getwd()

df = read.csv("AB_NYC_2019.csv",stringsAsFactors = TRUE)


#Load libraries
library(dplyr)
library(ggplot2)

#Initial look
is.data.frame(df)
head(df)

#Dimensions
nrow(df)
ncol(df)
dim(df)


#Checking out columns
colnames(df)


#Summarize
summary(df)
str(df)


#NA values?
sum(is.na(df)) #10052 NAs
colSums(is.na(df)) #All coming from reviews per month 
count(df[df$reviews_per_month == 0,]) 


#Data is arranged with each row is a host's unique place 
#Hosts can have multiple rows to represent each of their destinations


#Room types? Private, Entire home/apt and Shared room
unique(df$room_type)


#Count of each room type in NY
df %>% group_by(room_type) %>%
  summarize(total_count = n(), groups = 'drop') %>% 
  arrange(desc(total_count)) #total_count assigns name to col.

#Count of listings in each neighborhood
df %>% group_by(neighbourhood_group) %>% 
  summarize(total_count = n(), groups = 'drop') %>% 
  arrange(desc(total_count))
  

listings = df %>% 
  group_by(neighbourhood_group) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = reorder(neighbourhood_group,(-count)), y = count)) + 
           geom_bar(stat='identity')

listings



#Function for countplots
plot = function(group) {
  group = enquo(group) 
  df %>% 
    group_by({{group}}) %>% 
    summarize(count = n()) %>% 
    ggplot(aes(x = reorder({{group}},(-count)), y = count)) + 
    geom_bar(stat='identity',fill='Black',color='Red' ) + 
    theme_classic() +
    xlab('Grouping') +
    ylab('Count') + 
    theme(
      text = element_text(face='bold'),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10)
    )
}


plot(room_type)


#Function for count and averages by grouping
stats = function(data,group, avg){  
  group = enquo(group); avg = enquo(avg)       
  data %>% group_by({{group}}) %>%           
    summarize(total_count = n(),
              mean = mean({{avg}}, na.rm = TRUE), groups = 'drop')
}



#Average price of each room type
df %>% group_by(room_type) %>%
  summarize(total_count = n(),
            avg_price = mean(price), groups = 'drop')


#Or use new function
stats(df,room_type,price)
stats(df,neighbourhood_group,price)
stats(df,neighbourhood_group,number_of_reviews)


#Average host has 7.14 listings
mean(df$calculated_host_listings_count)

ggplot(data=subset(df,df$price<2500),aes(price)) +
  geom_histogram(binwidth = 100)

#Count of each neighborhood (popularity)
ggplot(df,aes(x=neighbourhood_group)) +
  geom_bar()

#Frequency of each location
freq_location <- data.frame(cbind(Frequency = table(df$neighbourhood_group), 
                                  Percent = prop.table(table(df$neighbourhood_group)) * 100))
freq_location <- freq_location[order(freq_location$Frequency),]
freq_location

#Group dataset by the neighborhood group
neighborhoods = df %>% group_by(neighbourhood_group) %>%  
  mutate(mean = mean(price)) %>%
  mutate(media = median(price))

#Create plot
n = ggplot(neighborhoods,aes(x=price)) +
  geom_histogram(binwidth = 10,fill='White',color = 'Black') + 
  ggtitle('Pricing by Neighborhood') +
  xlim(c(0,500)) +
  facet_grid(neighbourhood_group~., scales = 'free') +
  geom_vline(aes(xintercept = mean, group = neighbourhood_group, size= I(2)),
             color='red', linetype = 'dashed') +
  xlab('Price') + 
  ylab('Count of Listings')

#Adding in the details
n + theme(
  plot.title = element_text(size = 25, face='bold',family = 'mono'),
  axis.title.x = element_text(size=20,face='bold'),
  axis.title.y = element_text(size=20,face='bold'),
  axis.text.x = element_text(size=10,face='bold'),
  axis.text.y = element_text(size=10,face='bold'),
  strip.text.y = element_text(size=20,face='bold')
)


#Box Plot
head(df)
colnames(df)
box = ggplot(df,aes(x=room_type,y=price)) + 
  ggtitle('Room Type Pricing') +
  xlab('Neighborhood Group') +
  ylab('Price')

box = box + geom_boxplot() + ylim(c(0,350)) + 
  facet_grid(.~neighbourhood_group) 

box = box + theme(
  plot.title = element_text(size=25,face='bold',family='mono',hjust = .5),
  axis.title = element_text(size=15,face='bold'),
  strip.text = element_text(size=20,face='bold')
)

box

#How are minimum nights and price related?
summary(df$minimum_nights)
#Max minimum nights is 1250?

#Create additional dataset with av
s = subset(df,minimum_nights < 366)

ggplot(s,)

ggplot(subset(df,minimum_nights <= 3 & price < 500)) +
  geom_violin(aes(x=neighbourhood_group,y=price))

subset(df,minimum_nights > 400)

df %>% df$availability_365  %>% subset(availability_365 < 366)

#How are price and availability related?
#Use tilde (~) for defining relationship between two variables
x = df %>% select(availability_365) %>% subset(availability_365 < 366)
y = df$price 
relation = lm(y~unlist(x))
print(relation)

df %>% subset(price==0)

#Linear Regression Plot
ggplot(df %>% subset(availability_365<366 | price<1000),aes(x=availability_365,y=price)) +
  geom_point() + geom_smooth(method='lm') +



#Summary
summary(relation)

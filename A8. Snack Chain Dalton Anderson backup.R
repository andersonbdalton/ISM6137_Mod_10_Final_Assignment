#Dalton Anderson


#import data
library(readxl)
#import store data
df_stores <- read_excel("/Users/Dalton/Documents/USF/Spring-2022/ISM6137/Week 10 (Apr 19) Special Topics & Project Presentations/SnackChain.xlsx",
                        sheet = "stores")
#import product data
df_products <- read_excel("/Users/Dalton/Documents/USF/Spring-2022/ISM6137/Week 10 (Apr 19) Special Topics & Project Presentations/SnackChain.xlsx",
                        sheet = "products")
df_products <- subset(df_products, CATEGORY != "ORAL HYGIENE PRODUCTS")
#import transaction data
df_transactions <- read_excel("/Users/Dalton/Documents/USF/Spring-2022/ISM6137/Week 10 (Apr 19) Special Topics & Project Presentations/SnackChain.xlsx",
                        sheet = "transactions")
library(dplyr)
#join product dataset onto transaction data 
df_master <- merge(df_products,df_transactions,by="UPC")

#rename store_num to store_id for join
df_master$STORE_ID = df_master$STORE_NUM

#join store dataset onto master data 
df_master <- merge(df_master,df_stores,by="STORE_ID")

#make column names lower case
colnames(df_master)=tolower(make.names(colnames(df_master)))

#remove old df to free up space
rm("df_products", "df_transactions","df_stores")

#check nas
df_master %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
#packing size has 366061 nas -I am going to drop this column
#base_price has 185, not enough worry about
#price has 23, not enough to worry about



#drop columns
df <- df_master %>%
  select(-parking,
         -store_num)

#i have changed my mind dropping all nas
df <- na.omit(df) 
#remove all zeros as well
df <- subset(df, spend != 0)
df <- subset(df, units != 0)
df <- subset(df, hhs != 0)
summary(df)

#dataset for correlation plot
df_cor <- df %>%
  select(units,
         visits,
         hhs,
         spend,
         price,
         base_price,
         msa,
         size,
         avg_weekly_baskets)

#feature engineering 
#turn feature into a yes no
tempdf = df %>%
  mutate(
    feature_char = case_when(
      feature == 1 ~ "Yes",
      feature == 0 ~ "No"
    )
  )
df=tempdf

#turn display into a yes no
tempdf = df %>%
  mutate(
    display_char = case_when(
      display == 1 ~ "Yes",
      display == 0 ~ "No"
    )
  )
df=tempdf

#turn tpr_only into a yes no
tempdf = df %>%
  mutate(
    tpr_only_char = case_when(
      tpr_only == 1 ~ "Yes",
      tpr_only == 0 ~ "No"
    )
  )
df=tempdf

#factor the columns
df$product = df$description
as.factor(df$feature_char)
as.factor(df$display_char)
as.factor(df$tpr_only_char)
as.factor(df$product)
as.factor(df$manufacturer)
as.factor(df$category)
as.factor(df$segment)

#descriptive statistics
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)

#chart.Correlation(df_cor[,c("units","visits","hhs","spend","price","msa","size","avg_weekly_baskets")])

corrplot(cor(df_cor),method = 'number')

#units, visits, hhs, and spend are all highly correlated
#drop cor dataset
rm(df_cor)

table(df$spend)
head(df)

#i need to round spend,price,base_price, ave_weekly_baskets to whole numbers

df$spend = round(df$spend,0)
df$price = round(df$price,0)
df$base_price = round(df$base_price,0)
df$avg_weekly_baskets = round(df$avg_weekly_baskets,0)

unique(df$sub_category)
unique(df$category)
unique(df$segment)
(unique(df$upc))
unique(df$msa)
unique(df$product)

#Spend by Category
p <- ggplot(df, aes(spend, category))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "gold2")

#Spend by Manufacturer
p <- ggplot(df, aes(spend, manufacturer))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "darkblue")
#there are some manufacturers that do better than others
#General Mills, Kellogg
#some underperforming manufacture are MKSL, Shultz, Frito Lay

#Spend by Product Size
p <- ggplot(df, aes(spend, product_size))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "black")
#sizes 12.25 OZ,12.5 OZ,12 OZ, 11 OZ seems to do better until the we go to the next perfered size of 18 OZ
#then the next large size is 28.3
#best small sizes 12.25 OZ,12.5 OZ,12 OZ, 11 OZ
#best mid size 8 OZ
#best large 28.3

#Spend by Feature
p <- ggplot(df, aes(spend, feature_char))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "purple")
#looks like feature does have a positive effect

#Spend by Display
p <- ggplot(df, aes(spend, display_char))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "yellow2")
#looks like display does have a positive effect

#Spend by tpr_only
p <- ggplot(df, aes(spend, tpr_only_char))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "red3")
#looks like tpr_only has an negative effect on spend

#Spend by State
p <- ggplot(df, aes(spend, state))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "pink")
#looks like state does not have effect on spend

#Spend by City
p <- ggplot(df, aes(spend, city))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "brown")
#looks like city does have small positive effect on spend

#Spend by Segment
p <- ggplot(df, aes(spend, segment))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "pink2")
#looks like within segment upscale has a meaningful positive effect positive on spend

#Spend by product
p <- ggplot(df, aes(spend, description))
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "purple")
#looks like within products there are standouts within the group
#looks to me like need based products if the product is a snack there is a drop in spend

#spend by end week
tempdf=df %>%
  group_by(week_end_date) %>%
  summarise(Freq = sum(spend))

ggplot(data=tempdf, aes(x=week_end_date, y=Freq, group=1)) +
  geom_line(color="red")+
  geom_point()

#spend by week_end_date
tempdf=df %>%
  group_by(week_end_date) %>%
  summarise(Freq = sum(spend))

ggplot(data=tempdf, aes(x=week_end_date, y=spend, group=1)) +
  geom_line(color="red")+
  geom_point()

#spend
ggplot(df, aes(x=spend)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#not a normal

#spend
df$spend_log = log(df$spend + 1)
ggplot(df, aes(x=spend_log)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#fixed

#units
ggplot(df, aes(x=units)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#not normal

#units
dftest$units_log = log(df$units + 1)
ggplot(df, aes(x=units_log)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
# but the zeros mess up the distribution
#do remove the zeros from sample?
#yes i do


#hhs
ggplot(df, aes(x=hhs)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#not normal

#hhs
df$hhs_log = log(df$hhs + 1)
ggplot(df, aes(x=hhs_log)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#fixed but the zeros mess up the distribution
#do remove the zeros and put 1?


df$units_log = log(df$msa)
ggplot(df, aes(x=msa)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#I don't know what to do with this

df$units_log = log(df$units)
ggplot(df, aes(x=units_log)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
#fixed but the zeros mess up the distribution
#do remove the zeros and put 1?


#models
library(lme4)

#What are the effects of product display, featured, 
#TRP (temporary price reduction) on spend
#(spend) DV, unit sales DV, and number of household purchasers DV? (3 points)
df$spend_log = log(df$spend + 1.1)
df$units_log = log(df$units + 1.1)
df$hhs_log = log(df$hhs + 1.1)
#forget I needed to 1 becuase the log of 1 is 0 :)
question1a <- lmer(spend_log ~ display + feature + tpr_only  +  (1 | store_id) + (1 | state), data = df)
summary(question1a)     
car::vif(question1a)

question1b <- lmer(units_log ~ display + feature + tpr_only  +  (1 | store_id) + (1 | state), data = df)
summary(question1b) 
car::vif(question1b)

question1c <- lmer(hhs_log ~ display + feature + tpr_only  +  (1 | store_id) + (1 | state), data = df)
summary(question1c) 
car::vif(question1c)

#questionable. Above 1 below 1.5 for all IV

#compare models
library(stargazer)
stargazer(question1a, question1b, question1c, type="text",
          title="A8. Snack Chain Model Output")

#How do the effects of display, feature, and TPR on SPEND vary by 
#product categories (cold cereals, frozen pizza, bag snacks) 
#and store segments (mainstream, upscale, value)? (3 points)

question2 <- lmer(spend_log ~ display*category + feature*category + tpr_only*category + display*segment 
                  + feature*segment + tpr_only*segment  +  (1 | category) + (1 | segment), data = df)
summary(question2) 
car::vif(question2)

stargazer(question2, type="text",
          title="A8. Snack Chain Model Output")

re_question2_log <- lmer(feature ~ spend + units_log + hhs_log +
                           (1 | product), data=df,REML = FALSE)
summary(re_question2_log)

#What are the five most price elastic and five least price elastic products? 
#Price elasticity is the change in sales for unit change in product price? (3 points)

#As the retailer, which products would you lower the price to maximize 
#(a) product sales and 
#(b) unit sales, and why? (1 points)

question4a <- lmer(spend_log ~ price*product +  (1 | product), data = df)
summary(question4a) 

question4b <- lmer(units_log ~ price*product +  (1 | product), data = df)

summary(question4b) 

stargazer(question4a,question4b, type="text",
          title="A8. Snack Chain Model Output")

ranef(re_question2_log)

library(stargazer)

library(lme4)








#random effect model
lm(spend ~ 1, data=df)
colSums(is.na(df))
re1 <- lmer(spend ~  hhs_log + hhs_log*units_log + size + week_end_date + 
              (1 | product), data=df,REML = FALSE)
summary(re1)
cor(re1)

ranef(re1)

re2 <- lmer(spend ~  hhs_log + hhs_log*units_log + size + week_end_date + feature + display 
            + tpr_only + (1 | product), data=df,REML = FALSE)
summary(re2)

ranef(re2)

re3 <- lmer(spend ~  hhs_log + hhs_log*units_log + size + week_end_date + feature + display 
            + tpr_only + category + segment+ 
              (1 | product), data=df,REML = FALSE)
summary(re3)

ranef(re3)


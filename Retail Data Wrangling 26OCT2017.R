library(tidyverse)
library(magrittr)
library(lubridate)
library(ggthemes)

setwd("C:/Users/IBM_ADMIN/Box Sync/Jordan Lewis/Data Science Bootcamp/D2/D2-02-DSX Unsupervised Learning")

dfP <- read_delim("RetailUseCaseProductData.txt","|")
dfC <- read_delim("RetailUseCaseCustomerData.txt","|")
dfT <- read_delim("RetailUseCaseTransactionData.txt","|")
dfR <- read_delim("RetailUseCaseProductDataReview.txt","|")

### Join the data sources at the transaction level ###
dfPT <- sample_n(dfT,10000) %>%
  left_join(dfP,by="PRODUCT CODE") %>% 
  select(-6) %>% # remove "x4" field"
  mutate(MONTH = month(dfPT$PurchaseDate),`UNIT LIST PRICE`=as.numeric(gsub('\\$','',gsub(' ','',dfPT$`UNIT LIST PRICE`)))) %>% # Create Month field, remove spaces and dollar sign from prices
  mutate_at(.vars = vars(`PRODUCT CODE`, `ID`), .funs = as.factor) %>% # Change Product Code/ID to categorical
  select(ID,MONTH,everything()) %>%
  arrange(ID,MONTH)

dfPT_ID <- group_by(dfPT,ID,`PRODUCT CATEGORY`) %>%
  summarize(TotalFood = sum(`UNIT LIST PRICE`)) %>%
  summarize

dfPT_ID <- group_by(dfPT,ID,`PRODUCT CATEGORY`) %>%
  mutate(MaxCat = which.max(sum(`UNIT LIST PRICE`)))

dfC$ID %<>% as.factor
dfPTC <- left_join(dfC,dfPT,by=c("ID","MONTH")) #Join Customer info onto transactions

#################################
dfPT_IDM <- group_by(dfPT,ID,MONTH) %>%
  summarize(TransactionCount=n(),TotalPurchases=sum(`UNIT LIST PRICE`))

dfPT_IDMC <- group_by(dfPT,ID,MONTH,`PRODUCT CATEGORY`) %>%
  summarize(TransactionCount=n(),TotalPurchases=sum(`UNIT LIST PRICE`)) %>%
  spread()

dfPT_IDMP <- group_by(dfPT,MONTH,`PRODUCT CODE`) %>%
  summarize(TransactionCount=n(),TotalPurchases=sum(`UNIT LIST PRICE`))

# number of transactions

######################
hist(dfPTC$INCOME)


par(mfrow=c(2,2))
dfPTC$AGECat <-cut(dfPTC$AGE, c(25,35,40,45,50,5,60,100))
Rev_Age <- group_by(dfPTC,MONTH,AGECat) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(MONTH)
qplot(data=Rev_Age,x=MONTH,y=Revenue,color=AGECat) + geom_line(size=1) + theme_light()
ggsave("Revenue Over Time by Age Group.pdf")

dfPTC$INCOMECat <-cut(dfPTC$INCOME, breaks=c(1000,1250,1500,1750,2000,2250,3000),dig.lab=5)
Rev_Inc <- group_by(dfPTC,MONTH,INCOMECat) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(MONTH)
qplot(data=Rev_Inc ,x=MONTH,y=Revenue,color=INCOMECat) + geom_line(size=1) + theme_light()
ggsave("Revenue Over Time by Income Bracket.pdf")

Rev_MD <- group_by(dfPTC,MONTH,`MOBILE DEVICE`) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(MONTH)
qplot(data=Rev_MD ,x=MONTH,y=Revenue,color=`MOBILE DEVICE`) + geom_line(size=1) + theme_light()
ggsave("Revenue Over Time by Mobile Device.pdf")

Rev_Ed <- group_by(dfPTC,MONTH,EDUCATION) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(MONTH)
qplot(data=Rev_Ed,x=MONTH,y=Revenue,color=EDUCATION) + geom_line(size=1) + theme_light()
ggsave("Revenue Over Time by Education Level.pdf")

Rev_Inc <- group_by(dfPTC,MONTH,INCOMECat) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(MONTH)
qplot(data=Rev_Inc ,x=MONTH,y=Revenue,color=INCOMECat) + geom_line(size=1) + theme_light()
ggsave("Revenue Over Time by Income Bracket.pdf")


ggplot(data=Rev_Ed,aes(x=PurchaseDate,y=Revenue,group=EDUCATION))+geom_line(size=1) + theme_light()
ggsave("Revenue Over Time by Product Category.pdf")

Rev_P <- group_by(dfPTC,`PRODUCT CODE`,MONTH) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(desc(Revenue))

Rev_P %<>% left_join(dfP,by="PRODUCT CODE")

ggplot(data=Rev_P,aes(x=PurchaseDate,y=Revenue,group=`PRODUCT CODE`))+geom_line(aes(group=`PRODUCT CODE`),size=1) + theme_light()

ggplot(data=Rev_P,aes(x=MONTH,y=Revenue,group=`PRODUCT CODE`))+geom_line(aes(group=`PRODUCT CODE`),size=1) + theme_light()
ggsave("Revenue Over Time by Product Category.pdf")

Rev_PC <- group_by(dfPT,`PRODUCT CATEGORY`,PurchaseDate) %>%
  summarize(Revenue = sum(`UNIT LIST PRICE`)) %>%
  arrange(desc(Revenue))

#qplot(data=dfPT_1,x=PurchaseDate,y=Revenue,geom="line",color=`PRODUCT CATEGORY`,size=5) + theme_light()

dfPTC_Month <- group_by(dfPTC,ID) %>%
  summarize(Total_Purchases = sum(`UNIT LIST PRICE`)) %>% 
  arrange(desc(Total_Purchases))


#################################

#Sum of purchases by customer ID
dfPTC2 <- group_by(dfPTC,ID) %>%
  summarize(Total_Purchases = sum(`UNIT LIST PRICE`)) %>% 
  arrange(desc(Total_Purchases))

dfPTC3 <- left_join(dfPTC2,dfCM1,by="ID")


### Customer Data ###

dfC$ID %<>% as.factor
### 
dfL <- list(dfP,dfC,dfT)
dfLSm <- lapply(dfList,head,n=100)
library(dplyr)
data <- read.csv("printads.csv")
data <- data.frame(data)
str(data)


#Loreal's variables

a <- data%>%select(brand, brand_size)%>%filter(brand == "LOreal")%>%summarise(mean = mean(brand_size), sd = sd(brand_size), MAX = max(brand_size), Min = min(brand_size), nf = quantile(brand_size, probs = 0.95))
c <- data%>%select(brand, pic_size)%>%filter(brand == "LOreal")%>%summarise(mean = mean(pic_size), sd = sd(pic_size) MAX = max(pic_size), Min = min(pic_size), nf = quantile(pic_size, probs = 0.95))
e <- data%>%select(brand, brand_fix)%>%filter(brand == "LOreal")%>%summarise(mean = mean(brand_fix), sd = sd(brand_fix), MAX = max(brand_fix), Min = min(brand_fix), nf = quantile(brand_fix, probs = 0.95))
g <- data%>%select(brand, pic_fix)%>%filter(brand == "LOreal")%>%summarise(mean = mean(pic_fix), sd = sd(pic_fix), MAX = max(pic_fix), Min = min(pic_fix), nf = quantile(pic_fix, probs = 0.95))

b <- data%>%select(brand, brand_size)%>%filter(brand != "LOreal")%>%summarise(mean = mean(brand_size), sd = sd(brand_size), MAX = max(brand_size), Min = min(brand_size), nf = quantile(brand_size, probs = 0.95))
d <- data%>%select(brand, pic_size)%>%filter(brand != "LOreal")%>%summarise(mean = mean(pic_size), sd = sd(pic_size) MAX = max(pic_size), Min = min(pic_size), nf = quantile(pic_size, probs = 0.95))
f <- data%>%select(brand, brand_fix)%>%filter(brand != "LOreal")%>%summarise(mean = mean(brand_fix), sd = sd(brand_fix), MAX = max(brand_fix), Min = min(brand_fix), nf = quantile(brand_fix, probs = 0.95))
h <- data%>%select(brand, pic_fix)%>%filter(brand != "LOreal")%>%summarise(mean = mean(pic_fix), sd = sd(pic_fix), MAX = max(pic_fix), Min = min(pic_fix), nf = quantile(pic_fix, probs = 0.95))

j <-rbind(a,b,c,d,e,f,g,h)
l <- c("L'Oreal","All")
m <- c("Brand_size","Brand_size","Pic_size","Pic_size","Brand_fix","Brand_fix","Pic_fix","Pic_fix")
n <- cbind(l,m)
k <- cbind(n,j)
k

#Regression
# Poisson Regression
# BRAND_FIX - Fixation Counts of the Brand Element
model1 <- glm(brand_fix ~ brand_size+page_pos+page_num , poisson(link="log"),data = data)
summary(model1)
# PIC_FIX - Fixation Counts of the Pic Element
model2 <- glm(pic_fix ~ pic_size+page_pos+page_num , poisson(link = "log"))
summary(model2)
# RECALL_ACCU - Binary Logit Model
model3 <- glm(recall_accu ~ brand_fix+pic_fix+page_pos+page_num ,binomial(link = "logit"))
summary(model3)



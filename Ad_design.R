library(dplyr)
data <- read.csv("printads.csv")
data <- data.frame(data)
str(data)


#X's variables

a <- data%>%select(brand, brand_size)%>%filter(brand == "X")%>%summarise(mean = mean(brand_size), sd = sd(brand_size), MAX = max(brand_size), Min = min(brand_size), nf = quantile(brand_size, probs = 0.95))

c <- data%>%select(brand, pic_size)%>%filter(brand == "X")%>%summarise(mean = mean(pic_size), sd = sd(pic_size), MAX = max(pic_size), Min = min(pic_size), nf = quantile(pic_size, probs = 0.95))
e <- data%>%select(brand, brand_fix)%>%filter(brand == "X")%>%summarise(mean = mean(brand_fix), sd = sd(brand_fix), MAX = max(brand_fix), Min = min(brand_fix), nf = quantile(brand_fix, probs = 0.95))
g <- data%>%select(brand, pic_fix)%>%filter(brand == "X")%>%summarise(mean = mean(pic_fix), sd = sd(pic_fix), MAX = max(pic_fix), Min = min(pic_fix), nf = quantile(pic_fix, probs = 0.95))

b <- data%>%select(brand, brand_size)%>%filter(brand != "X")%>%summarise(mean = mean(brand_size), sd = sd(brand_size), MAX = max(brand_size), Min = min(brand_size), nf = quantile(brand_size, probs = 0.95))
d <- data%>%select(brand, pic_size)%>%filter(brand != "X")%>%summarise(mean = mean(pic_size), sd = sd(pic_size), MAX = max(pic_size), Min = min(pic_size), nf = quantile(pic_size, probs = 0.95))
f <- data%>%select(brand, brand_fix)%>%filter(brand != "X")%>%summarise(mean = mean(brand_fix), sd = sd(brand_fix), MAX = max(brand_fix), Min = min(brand_fix), nf = quantile(brand_fix, probs = 0.95))
h <- data%>%select(brand, pic_fix)%>%filter(brand != "X")%>%summarise(mean = mean(pic_fix), sd = sd(pic_fix), MAX = max(pic_fix), Min = min(pic_fix), nf = quantile(pic_fix, probs = 0.95))

j <-rbind(a,b,c,d,e,f,g,h)
l <- c("X","All")
m <- c("Brand_size","Brand_size","Pic_size","Pic_size","Brand_fix","Brand_fix","Pic_fix","Pic_fix")
n <- cbind(l,m)
k <- cbind(n,j)
k


#Regression
# Poisson Regression
# BRAND_FIX - Fixation Counts of the Brand Element
data$quad <- data$page_num*data$page_num
model1 <- glm(brand_fix ~ brand_size+ pic_size+ page_pos+ page_num+ quad , poisson(link="log"),data = data)
summary(model1)
# PIC_FIX - Fixation Counts of the Pic Element
model2 <- glm(pic_fix ~ brand_size+ pic_size+ page_pos+ page_num+ quad, poisson(link = "log"),data = data)
summary(model2)

#Adding predicted values of Brand and Picture fixation basis the above models
data$bf <- predict(model1, data)
data$pf <- predict(model2, data)

# RECALL_ACCU - Binary Logit Model
model3 <- glm(recall_accu ~ bf+pf page_pos+ page_num + quad ,binomial(link = "logit"),data = data)
summary(model3)
data$RA <- predict(model3, data)

#X compared to other similar brands
p <-  data%>%select(brand, RECALL_TIME, page_pos, page_num, bf, pf, RA)%>%filter(brand == "X" | brand == "a" | brand == "bb" | brand == "u" | brand == "gg")%>%group_by(brand)%>%summarise(page_pos = mean(page_pos), page_num = mean(page_num) ,pic_fix = mean(pf), brand_fix = mean(bf), recall_time = mean(RECALL_TIME), prob = mean(RA))
p

data%>%select(recall_accu, brand)%>%filter(brand == "X")%>%summarise(mean = mean(recall_accu))



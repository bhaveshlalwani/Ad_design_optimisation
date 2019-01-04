library(dplyr)
data <- read.csv("printads.csv")
data <- data.frame(data)
str(data)

#convert factor variables to factors
names <- c("page_pos")
data[,names] <- lapply(data[,names] , factor)
str(data)


#Loreal's variables

a <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand == "LOreal")%>%summarise(mean = mean(brand_fix), MAX = max(brand_fix), Min = min(brand_fix), nf = quantile(brand_fix, probs = 0.95))
b <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand != "LOreal")%>%summarise(mean = mean(brand_fix), MAX = max(brand_fix), Min = min(brand_fix), nf = quantile(brand_fix, probs = 0.95))
c <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand == "LOreal")%>%summarise(mean = mean(pic_fix), MAX = max(pic_fix), Min = min(pic_fix), nf = quantile(pic_fix, probs = 0.95))
d <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand != "LOreal")%>%summarise(mean = mean(pic_fix), MAX = max(pic_fix), Min = min(pic_fix), nf = quantile(pic_fix, probs = 0.95))
e <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand == "LOreal")%>%summarise(mean = mean(recall_accu), MAX = max(recall_accu), Min = min(recall_accu), nf = quantile(recall_accu, probs = 0.95))
f <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand != "LOreal")%>%summarise(mean = mean(recall_accu), MAX = max(recall_accu), Min = min(recall_accu), nf = quantile(recall_accu, probs = 0.95))
g <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand == "LOreal")%>%summarise(mean = mean(RECALL_TIME), MAX = max(RECALL_TIME), Min = min(RECALL_TIME), nf = quantile(RECALL_TIME, probs = 0.95))
h <- data%>%select(brand, brand_fix, pic_fix, recall_accu, RECALL_TIME)%>%filter(brand != "LOreal")%>%summarise(mean = mean(RECALL_TIME), MAX = max(RECALL_TIME), Min = min(RECALL_TIME), nf = quantile(RECALL_TIME, probs = 0.95))
i <- c("L_brand_fix","ALL_brand_fix","L_pic_fix","L_pic_fix","L_Recall_accu","ALL_Recall_accu","L_Recall_time","All_Recall_time")
j <-rbind(a,b,c,d,e,f,g,h)
l <- c("L'Oreal","All")
m <- c("Brand_fix","Brand_fix","Pic_fix","Pic_fix","recall_accu","recall_accu","recall_time","recall_time")
n <- cbind(l,m)
k <- cbind(n,j)
k

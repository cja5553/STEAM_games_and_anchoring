# set-up

library(data.table)
library(ggplot2)
data<-read.csv("reviewer_analysis.csv")
data<-subset(data,select=-c(X))
data[is.na(data)] <- 0
data$similarity_score<-as.numeric(data$similarity_score)
data$order<-as.numeric(data$order)
data$type<-factor(data$type,levels=c("unobservable","observable"))
data$recommendation_id<-as.factor(data$recommendation_id)
str(data)
head(data)

# figure 2b)

ggplot(data, aes(y=similarity_score, x=order, group=type))+geom_smooth(method="gam",show.legend=TRUE,aes(linetype=type), colour="blue")+labs(linetype="Display Type")+ggtitle("similarity scores across all reviews based on its order and display type")+xlab("order of review")+ylab("similarity score")+scale_x_continuous(breaks=seq(1, 10, 1))
#4E84C4
ggplot(data, aes(y=similarity_score, x=order, group=type))+geom_smooth(method="gam",show.legend=TRUE,aes(linetype=type), colour="blue")+labs(linetype="Display Type")+scale_linetype_discrete(labels=c("Recent reviews (Side-bar display)","Default \'summary\' reviews (Main-bar display)"))+ggtitle("similarity scores across all review based on display type and order")+xlab("order of review")+ylab("similarity score")+scale_x_continuous(breaks=seq(1, 10, 1))
ggplot(data, aes(y=similarity_score, x=order, group=type))+geom_smooth(method="gam",show.legend=TRUE,aes(linetype=type), colour="#4E84C4")+labs(linetype="Display Type")+scale_linetype_discrete(labels=c("Side-bar display with recent reviews","Main-bar display with default \'summary\' reviews"))+ggtitle("similarity scores across all reviews based on display type and order")+xlab("order of review")+ylab("similarity score")+scale_x_continuous(breaks=seq(1, 10, 1))

# figure 2a)
data1<-data[data$type=="observable",]
data1$type<-"Default 'summary' reviews (main-bar)"
data2<-data[data$type=="unobservable",]
data2$type<-"Recent reviews (side-bar)"
data<-rbind(data1,data2)
ggplot(data,aes(x=similarity_score,y=type,fill=type))+geom_boxplot(width=0.4,position = position_dodge())+theme_minimal()+ggtitle("similarity scores across all reviews")+xlab("similarity score")+ylab("Display type")+theme(legend.position="none",text = element_text(size=10),axis.text = element_text(size = 10))+ xlim(-0.5,1)+scale_fill_brewer()
# histogram
hist(data$similarity_score,xlab="Similarity Score",main="Distribution of similiarity scores",col="darkmagenta")
library(ggplot2)
library(stopwords)
library(wordcloud)
library(reshape2)
library(plyr)
library(tidyverse)
library(dplyr)
library(gridExtra)

options(max.print = 99999)

z.prop = function(x1,x2,n1,n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1+x2) / (n1+n2)
  denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
  z.prop.ris = numerator / denominator
  return(z.prop.ris)
}

#load data and combine leave and shelter categories
data<-read.csv("/Users/makenzie.krocak/Desktop/PhD Stuff/CRCM PhD Stuff/2019_WCAS_advanced_notice/github/WX18_adv_notice_github.csv")
data$shelter_num<-ifelse((data$long_torhit_conshelter==1 | data$long_torhit_shelter==1 | data$long_torhit_nonshelter==1),1,0)
data$leave_num<-ifelse((data$long_torhit_leave==1 | data$long_torhit_leave_shelter==1 | data$long_torhit_leave_avoid==1),1,0)

#find stop words and calculate the number of responses in each time category
stop_words <- stopwords::stopwords(language = "en", source = "snowball")
numresp_4=nrow(filter(data, long_torhit_time == 4, is.na(long_torhit_other)==TRUE))
numresp_8=nrow(filter(data, long_torhit_time == 8, is.na(long_torhit_other)==TRUE))

#unnest tokens, get rid of punctuation, stop words, and calculate number and proportion
time_4_words <- data_frame(text = data$long_torhit_resp) %>% 
  filter(data$long_torhit_time==4, is.na(data$long_torhit_other)==TRUE)%>%
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+"))%>%
  unnest() %>% 
  dplyr::count(tokens) %>% 
  filter(tokens != "") %>% 
  filter(!tokens %in% stop_words) %>% 
  mutate(prop = n / numresp_4) %>% 
  mutate(xx=reorder(tokens, prop))%>%
  arrange(desc(n))%>%
  slice(1:30)

time_8_words <- data_frame(text = data$long_torhit_resp) %>% 
  filter(data$long_torhit_time==8, is.na(data$long_torhit_other)==TRUE)%>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  dplyr::count(tokens) %>% 
  filter(tokens != "") %>% 
  filter(!tokens %in% stop_words) %>% 
  mutate(prop = n / numresp_8) %>% 
  mutate(xx=reorder(tokens, prop))%>%
  arrange(desc(n))%>%
  filter(tokens %in% time_4_words$tokens) 

#view the top 30 words in each category
time_4_words%>%print(n=30)
time_8_words%>%print(n=30)

#find the difference in proportion of the top 30 words
target<-time_4_words$tokens
time_8_words_sorted<-time_8_words[match(target, time_8_words$tokens),]
torhit_graphics<-time_4_words%>%
  mutate(torhit_8_props=time_8_words_sorted$prop)%>%
  mutate(time_diff=torhit_8_props-prop)

#test the differences for significance 
#at the p<0.1 level: shelter, take, get, weather, house, find, drive, away, family, keep
wordtotest<-'house'
word_sig<- z.prop(filter(time_4_words,tokens==wordtotest)$n,
                  filter(time_8_words,tokens==wordtotest)$n, 
                  numresp_4,numresp_8)
1.645*pnorm(-abs(word_sig))

#note which word differences are significant in the figure
torhit_graphics$tokens <- car::recode(torhit_graphics$tokens, "'get' = 'get*'; 'take' = 'take*'; 'weather'='weather*';'house'='house*';
                                      'shelter' = 'shelter*';'find'='find*';'drive'='drive*';'away'='away*';'family'='family*';'keep'='keep*'")
torhit_graphics<-torhit_graphics%>%
  mutate(diff_order=reorder(tokens,time_diff))

#plot the top 30 words in the 4hr timeframe
words_4<-ggplot(data=time_4_words,aes(x=xx,y=prop*100,fill=seq(1,30,1)))+
  geom_bar(stat='identity')+
  coord_flip(xlim = c(1,30), ylim = c(0,25))+
  scale_fill_gradient(low="blue",high="cadetblue1")+
  theme(legend.position="none",axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=14),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  labs(x="Word",y='Percentage of Responses')+
  annotate("text", x=29, y=23, label= "a",size=8) + 
  geom_text(aes(x = xx, y = prop*100 + 1.3, label = n))+
  theme(plot.title = element_text(size=16,hjust = 0.5))

#plot the proportions of the 4hr top 30 words in the 8hr timeframe
words_8<-ggplot(data=time_8_words,aes(x=xx,y=prop*100,fill=seq(1,30,1)))+
  geom_bar(stat='identity')+
  coord_flip(xlim = c(1,30), ylim = c(0,25))+
  scale_fill_gradient(low="darkorange",high="gold")+
  theme(legend.position="none",axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=14),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  labs(x="Word",y='Percentage of Responses')+
  annotate("text",x=29,y=23,label="b",size =8)+
  geom_text(aes(x = xx, y = prop*100 + 1.3, label = n))+
  theme(plot.title = element_text(size=16,hjust = 0.5))

fig1<-grid.arrange(grobs = list(words_4,words_8), layout_matrix = rbind(c(1,2)))
fig1

#now plot the difference in word usage
fig2<-ggplot(data=torhit_graphics,aes(x=diff_order,y=time_diff*100,fill=time_diff))+
  geom_bar(stat='identity')+
  coord_flip(xlim = c(1,30), ylim = c(-3,3))+
  scale_fill_gradient(low='blue',high='darkorange')+
  theme(legend.position="none",axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=14),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  labs(x="Word",y='Percentage Change (8hrs - 4hrs)')+
  theme(plot.title = element_text(size=16,hjust = 0.5))
fig2

#get dataframe of categories and calculate sums in each category
cat4_resp<-data_frame(sum=colSums(filter(data, data$long_torhit_time==4 & is.na(data$long_torhit_other)==TRUE)[c(3,4,5,12,14,15)],na.rm=TRUE))%>%
  mutate(prop=sum/numresp_4)%>%
  mutate(names=c('Do nothing','Monitor the\n Situation','Prepare for\n the event','Unsure of\n what to do','Take shelter','Leave the area'))%>%
  mutate(xx=reorder(names, prop))

cat8_resp<-data_frame(sum=colSums(filter(data, data$long_torhit_time==8 & is.na(data$long_torhit_other)==TRUE)[c(3,4,5,12,14,15)],na.rm=TRUE))%>%
  mutate(prop=sum/numresp_8)%>%
  mutate(names=c('Do nothing','Monitor the\n Situation','Prepare for\n the event','Unsure of\n what to do','Take shelter','Leave the area'))%>%
  mutate(xx=reorder(names, prop))

#find the difference in proportion of those categories
target_cat<-cat4_resp$names
cat_8_sorted<-cat8_resp[match(target_cat, cat8_resp$names),]
cat_graphics<-cat4_resp%>%
  mutate(cat_8_props=cat_8_sorted$prop)%>%
  mutate(cat_diff=cat_8_props-prop)

#test the differences for significance 
#at the p<0.1 level: nothing, nonshelter
cattotest<-'Leave the area'
cat_sig<- z.prop(filter(cat4_resp,names==cattotest)$sum,
                  filter(cat8_resp,names==cattotest)$sum, 
                  numresp_4,numresp_8)
1.645*pnorm(-abs(cat_sig))

#note which category differences are significant in the figure
cat_graphics$names<-ifelse(cat_graphics$names=='Do nothing','Do nothing*',cat_graphics$names)
cat_graphics$names<-ifelse(cat_graphics$names=='Take shelter','Take shelter*',cat_graphics$names)

cat_graphics<-cat_graphics%>%
  mutate(xx=reorder(names, cat_diff))

#plot 4hr category percentages
cat_4<-ggplot(data=cat4_resp,aes(x=xx,y=prop*100,fill=prop))+
  geom_bar(stat='identity')+
  coord_flip(xlim = c(1,6), ylim = c(0,42))+
  scale_fill_gradient(low="cadetblue1",high="blue")+
  theme(legend.position="none",axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=16),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  labs(x=" ",y='Percentage of Responses')+
  annotate("text",x=6.2,y=40,label="a",size =10)+
  geom_text(aes(x = xx, y = prop*100 + 2.5, label = sum, size=1))+
  theme(plot.title = element_text(size=16,hjust = 0.5))

#plot 8hr category percentages
cat_8<-ggplot(data=cat8_resp,aes(x=xx,y=prop*100,fill=prop))+
  geom_bar(stat='identity')+
  coord_flip(xlim = c(1,6), ylim = c(0,42))+
  scale_fill_gradient(low="gold",high="darkorange")+
  theme(legend.position="none",axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=16),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  labs(x=" ",y='Percentage of Responses')+
  annotate("text",x=6.2,y=40,label="b",size =10)+
  geom_text(aes(x = xx, y = prop*100 + 2.5, label = sum, size=100))+
  theme(plot.title = element_text(size=16,hjust = 0.5))

fig3<-grid.arrange(grobs = list(cat_4,cat_8), layout_matrix = rbind(c(1,2)))
fig3

#plot category differences
fig4<-ggplot(data=cat_graphics,aes(x=xx,y=cat_diff*100,fill=cat_diff))+
  geom_bar(stat='identity')+
  coord_flip(xlim = c(1,6),ylim=c(-5,5))+
  scale_fill_gradient(low='blue',high='darkorange')+
  theme(legend.position="none",axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=16),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  labs(x=" ",y='Percentage Change (8hrs - 4hrs)')+
  theme(plot.title = element_text(size=16,hjust = 0.5))
fig4
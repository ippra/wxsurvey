library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(tseries)
library(strucchange)

#load data-----------------------------------------------------------------------------------------------------------------------------------------
surv18<-read_csv("https://www.dropbox.com/s/cothn30qx68eeke/WX18_data_wtd.csv?dl=1")%>%
  mutate(rand_morn=gsub(":","",rand_morn))%>%
  mutate(rand_morn=car::recode(rand_morn,"'010000'='100 AM';'020000'='200 AM';'030000'='300 AM';'040000'='400 AM';
                              '050000'='500 AM';'060000'='600 AM';'070000'='700 AM';'080000'='800 AM';'090000'='900 AM'"))
surv19<-read_csv("https://www.dropbox.com/s/8s5ilm8xe602f01/WX19_data_wtd.csv?dl=1")%>%
  mutate(rand_morn=gsub(":","",rand_morn))%>%
  mutate(rand_morn=car::recode(rand_morn,"'010000'='100 AM';'020000'='200 AM';'030000'='300 AM';'040000'='400 AM';
                              '050000'='500 AM';'060000'='600 AM';'070000'='700 AM';'080000'='800 AM';'090000'='900 AM'"))

surv20<-read_csv("https://www.dropbox.com/s/6xcbb54zhrqup7w/WX20_data_wtd.csv?dl=1")%>%
  mutate(rand_morn=gsub(":","",rand_morn))%>%
  mutate(rand_morn=car::recode(rand_morn,"'010000'='100 AM';'020000'='200 AM';'030000'='300 AM';'040000'='400 AM';
                              '050000'='500 AM';'060000'='600 AM';'070000'='700 AM';'080000'='800 AM';'090000'='900 AM'"))
data<-rbind(surv18,surv19,surv20)

#Fig 1: barplot of rec_conf responses---------------------------------------------------------------------------------------------------------------------------------------
df1_morn<-data.frame(rec_conf=data$rec_morn,rec_time=data$rand_morn,type='1 AM to 9 AM')
df1_morn<-df1_morn%>%drop_na()
df1_aft<-data.frame(rec_conf=data$rec_aft,rec_time=data$rand_aft,type='10 AM to 5 PM')
df1_aft<-df1_aft%>%drop_na()
df1_eve<-data.frame(rec_conf=data$rec_eve,rec_time=data$rand_eve,type='6 PM to midnight')
df1_eve<-df1_eve%>%drop_na()
bar_conf<-rbind(df1_morn,df1_aft,df1_eve)%>% 
  mutate(type=factor(type,levels=c('1 AM to 9 AM','10 AM to 5 PM','6 PM to midnight')))%>% 
  group_by(type, rec_conf) %>% 
  drop_na() %>%
  dplyr::summarize(n = n())%>% 
  mutate(p = n / sum(n))%>% 
  ggplot(aes(x = factor(rec_conf),fill=type)) +
  geom_bar(aes(y = p),stat='identity',color='black',position='dodge') +
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label = scales::percent(p,accuracy = 0.1),y= p),
            stat = "identity", position = position_dodge(width = 0.95),vjust=-0.6,size=5) +
  ylab("Percentage of Respondents") +
  xlab(" ") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"),
        panel.grid.major = element_line(color='#FFFFFF'),
        panel.grid.minor = element_line(color='#FFFFFF'),
        plot.title = element_text(size=22,color='#4d4d4d'),
        legend.text=element_text(size=20,color='#4d4d4d'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = c(0.1,0.8),legend.title = element_blank())+
  scale_x_discrete(breaks=c(1,2,3,4,5),labels=c("Not at all", "Not very", "Somewhat","Very","Extremely"))
bar_conf

#Fig 2: lineplot of proportion answering very/extremely confident-----------------------------------------------------------------------------------------------------------
#reception
df2_morn<-data.frame(rec_conf=data$rec_morn,rec_time=data$rand_morn)
df2_aft<-data.frame(rec_conf=data$rec_aft,rec_time=data$rand_aft)
df2_eve<-data.frame(rec_conf=data$rec_eve,rec_time=data$rand_eve)
df2<-rbind(df2_morn,df2_aft,df2_eve)%>%
  mutate(f.rec_time=factor(rec_time,levels=c( '800 AM', '900 AM',
                                              '10:00 AM', '11:00 AM', '12:00 PM (noon)', 
                                              '1:00 PM', '2:00 PM', '3:00 PM', '4:00 PM', 
                                              '5:00 PM', '6:00 PM', '7:00 PM', '8:00 PM', 
                                              '9:00 PM', '10:00 PM', '11:00 PM', '12:00 AM (midnight)',
                                              '100 AM', '200 AM', '300 AM', '400 AM',
                                              '500 AM', '600 AM','700 AM')))%>%
  mutate(rec_conf=ifelse(rec_conf>3,1,0))%>%
  group_by(f.rec_time) %>%
  drop_na() %>% 
  dplyr::summarise(n = n(), 
            p = mean(rec_conf),
            se = sqrt(p * (1 - p) / n),
            lower = p - 1.96 * se,
            upper = p + 1.96 * se) %>% 
  mutate(type='Reception')
df2

#response
df3_morn<-data.frame(rec_conf=data$resp_morn,rec_time=data$rand_morn)
df3_aft<-data.frame(rec_conf=data$resp_aft,rec_time=data$rand_aft)
df3_eve<-data.frame(rec_conf=data$resp_eve,rec_time=data$rand_eve)
df3<-rbind(df3_morn,df3_aft,df3_eve)%>%
  mutate(f.rec_time=factor(rec_time,levels=c( '800 AM', '900 AM',
                                              '10:00 AM', '11:00 AM', '12:00 PM (noon)', '1:00 PM', '2:00 PM', '3:00 PM', '4:00 PM', '5:00 PM',
                                              '6:00 PM', '7:00 PM', '8:00 PM', '9:00 PM', '10:00 PM', '11:00 PM', '12:00 AM (midnight)',
                                              '100 AM', '200 AM', '300 AM', '400 AM', '500 AM', '600 AM','700 AM')))%>%
  mutate(rec_conf=ifelse(rec_conf>3,1,0))%>%
  group_by(f.rec_time) %>%
  drop_na() %>% 
  dplyr::summarise(n = n(), 
            p = mean(rec_conf),
            se = sqrt(p * (1 - p) / n),
            lower = p - 1.96 * se,
            upper = p + 1.96 * se) %>% 
  mutate(type='Response')
df3

#Fig. 2: plot timeline
labels3=c('8:00 AM', ' ',
          '10:00 AM', ' ', '12:00 PM', ' ', '2:00 PM', ' ', '4:00 PM', ' ',
          '6:00 PM', ' ', '8:00 PM', ' ', '10:00 PM', ' ', '12:00 AM',
          ' ', '2:00 AM', ' ', '4:00 AM', ' ', '6:00 AM',' ')
plot_timeframe<-bind_rows(df2, df3)%>%  
  ggplot(., aes(x = f.rec_time, y = p, group=type)) +
  annotate("rect", xmin = 17, xmax = 21, ymin = 0.28, ymax = 0.73, alpha = .3)+
  geom_point(aes(color=type),size=3) +
  geom_line(aes(color=type),size=2) +
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=type),alpha=0.4)+
  labs(y='Percentage of Respondents',x='Time of Day (LST)',color=' ')+
  guides(color=guide_legend("type"), fill = FALSE)+
  theme_bw()+
  ylim(0.28,0.73)+
  scale_x_discrete(labels=labels3)+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme(axis.title=element_text(face = "bold",size=20,color='#4d4d4d'),axis.text.x=element_text(size=20,color='#4d4d4d',angle=45, hjust = 1),
        axis.text.y=element_text(size=20,color='#4d4d4d'),
        panel.grid.major = element_line(color='#FFFFFF'),
        panel.grid.minor = element_line(color='#FFFFFF'),
        panel.border=element_rect(color='#4d4d4d'),
        plot.title = element_text(size=22,color='#4d4d4d'),
        legend.text=element_text(size=20,color='#4d4d4d'),
        legend.position = c(0.1,0.1),legend.title = element_blank())
plot_timeframe

# Table 1: Demographics of sample
demo_data<-rbind(surv18,surv19,surv20)%>%
  mutate(race=car::recode(race,"1='White';2='Black or African American';3='Some other race';4='Asain';5:7='Some other race'"))

age_r<-car::recode(demo_data$age,"18:24=1;25:34=2;35:44=3;45:54=4;55:64=5;65:120=6")
round(prop.table(table(age_r))*100,2)
#gend
round(prop.table(table(demo_data$gend))*100,2)
#hisp
round(prop.table(table(demo_data$hisp))*100,2)
#race
round(prop.table(table(demo_data$race))*100,2)
#region
round(prop.table(table(demo_data$nws_region))*100,2)

#Tables  3 - 5: regression models-----------------------------------------------------------------------------------------------------------------------------------------
surv_data<-rbind(surv18,surv19,surv20)

#load tornado climo data
tor_18<-read_csv("https://www.dropbox.com/s/bx3f8t21j4nca01/tor_info_18.csv?dl=1", col_names = c('p_id', 'tor_num', 'tor_prop'))
tor_19<-read_csv("https://www.dropbox.com/s/iuyt9aiesn6srdc/tor_info_19.csv?dl=1", col_names = c('p_id', 'tor_num', 'tor_prop'))
tor_20<-read_csv("https://www.dropbox.com/s/vvju7rcq8g3caxl/tor_info_20.csv?dl=1", col_names = c('p_id', 'tor_num', 'tor_prop'))
tor_climo_data<-rbind(tor_18,tor_19,tor_20)

#join survey data and climo data
reg_data<- plyr::join(surv_data, tor_climo_data, by = "p_id")
names(reg_data)

#create and recode regression variables
reg_data<-reg_data%>%
  filter(rand_morn=='100 AM ' | rand_morn=='200 AM' | rand_morn=='300 AM' | rand_morn=='400 AM' | rand_eve=="12:00 AM (midnight)")%>%
  mutate(race=car::recode(race,"1='White';2='Black or African American';3:7='Some other race'"),
         race=factor(race, levels=c('White','Black or African American',"Some other race")),
         hisp=car::recode(hisp,"1='Hispanic';0='Not Hispanic'"),
         gend=car::recode(gend,"0='Female';1='Male'"),
         income=car::recode(income,"1='Less than $50,000';2='$50,000-$100,000';3='$100,000-$150,000';4='More than $150,000'"),
         income=factor(income,levels=c('Less than $50,000', '$50,000-$100,000', '$100,000-$150,000', 'More than $150,000')),
         edu=car::recode(edu,"1:4='Less than a college degree';5:8='College degree or higher'"),
         mobile=ifelse(home==4,"Mobile Home","Not a Mobile Home"),
         warn_hist=car::recode(warn_hist,"0='No';1='Yes'"),
         source_totals=rowSums(cbind(wx_info1,wx_info2,wx_info3,wx_info4,wx_info5,wx_info6,wx_info7,wx_info8),na.rm = T),
         tor_prop=tor_prop*100,
         DV1=ifelse(rand_morn=='100 AM ' | rand_morn=='200 AM' | rand_morn=='300 AM' | rand_morn=='400 AM',rec_morn,rec_eve),
         DV2=rec_aft-DV1)

#difference of proportion test of those who are "not at all confident"------------------------------------------------------------------------------------------------
df4 <- rbind(df1_morn,df1_aft,df1_eve) %>%
  mutate(r.rec_conf=car::recode(rec_conf,"1:2=1;3:5=2"))%>%
  group_by(type, r.rec_conf) %>% 
  dplyr::summarize(n = n()) %>% 
  drop_na() %>% 
  mutate(
    p = n / sum(n), 
    se = sqrt(p * (1 - p) / n),
    lower = p - 1.96 * se,
    upper = p + 1.96 * se) 

day <- df4 %>% filter(type == "6 PM to midnight", r.rec_conf == 1)
night <- df4 %>% filter(type == "1 AM to 9 AM", r.rec_conf == 1)

point_estimate <- night$p - day$p
point_estimate

standard_error <- sqrt((night$se ^ 2) + (day$se ^ 2))
standard_error

confidence_interval <- c(point_estimate - 1.96 * standard_error, 
                         point_estimate + 1.96 * standard_error)
confidence_interval

z_stat <- (point_estimate - 0) / standard_error
z_stat

p_value <- 1 - pnorm(z_stat)
p_value

#Table 3: correlation table
cor_data <- reg_data %>% 
  mutate(no_college = ifelse(edu == 'Less than a college degree', 1, 0),
         college = ifelse(edu == 'College degree or higher', 0, 1),
         male = ifelse(gend == 'Male', 1, 0),
         female = ifelse(gend == 'Female', 1, 0),
         white = ifelse(race == 'White', 1, 0), 
         black = ifelse(race == 'Black or African American', 1, 0),
         oth_race = ifelse(race == 'Some other race', 1, 0),
         hisp_yes = ifelse(hisp == 'Hispanic', 1, 0), 
         hisp_no = ifelse(hisp == 'Not Hispanic', 1, 0),
         inc_50k = ifelse(income == 'Less than $50,000', 1, 0),
         inc_50_100k = ifelse(income == '$50,000-$100,000', 1, 0),
         inc_100_150k = ifelse(income == '$100,000-$150,000', 1, 0),
         inc_150k = ifelse(income == 'More than $150,000', 1, 0),
         mobile_yes = ifelse(mobile == 'Mobile Home', 1, 0),
         mobile_no = ifelse(mobile == 'Not a Mobile Home', 1, 0),
         warn_hist_yes = ifelse(warn_hist == 'Yes', 1, 0),
         warn_hist_no = ifelse(warn_hist == 'No', 1, 0))

cor_data <- cor_data %>% 
  dplyr::select(DV1, DV2, age, college, male, white, black, oth_race, hisp_yes, 
                inc_50k, inc_50_100k, inc_100_150k, inc_150k, mobile_yes, warn_hist_yes, 
                source_totals, follow, tor_prop)

stargazer::stargazer(cor_data, digits = 2, type = "text")

correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = type)
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(!is.na(R) & R < 0) > 0) {
    Rformatted = ifelse(!is.na(R) & R > 0, paste0(" ", Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
    Rformatted = paste0(Rformatted, stars)
  }
  
  # make all character strings equally long
  max_length = max(nchar(Rformatted))
  Rformatted = vapply(Rformatted, function(x) {
    current_length = nchar(x)
    difference = max_length - current_length
    return(paste0(x, paste(rep(" ", difference), collapse = ''), sep = ''))
  }, FUN.VALUE = character(1))
  
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(Rnew) <- colnames(x)
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

correlation_matrix(cor_data, digits = 2)

#Table 4: regression model WITHOUT a daytime confidence score control
library(lm.beta)

table(reg_data$source_totals)

model1<-lm(DV1~age+edu+gend+race+hisp+income+mobile+warn_hist+source_totals+follow+tor_prop,data=reg_data)
model1tidy<-broom::tidy(model1, digits = 2)
model1std<-lm.beta(model1)
tibble(
  term = model1tidy$term, 
  coef = model1tidy$estimate,
  se = model1tidy$std.error,
  std_coef = model1std$standardized.coefficients,
  p = model1tidy$p.value) %>% 
  mutate_if(is.numeric, round, 3)
stargazer::stargazer(model1, type = "text")

#Table 5: regression model WITH a daytime confidence score control
model2<-lm(DV2~age+edu+gend+race+hisp+mobile+income+mobile+warn_hist+source_totals+follow+tor_prop,data=reg_data)
model2tidy<-broom::tidy(model2, digits = 2)
model2std<-lm.beta(model2)
tibble(
  term = model2tidy$term, 
  coef = model2tidy$estimate,
  se = model2tidy$std.error,
  std_coef = model2std$standardized.coefficients,
  p = model2tidy$p.value) %>% 
  mutate_if(is.numeric, round, 3)
stargazer::stargazer(model2, type = "text", digits = 2)
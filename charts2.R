### read packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ISLR)
library(stringr)
library(dlookr)
library(DMwR)
library(gridExtra)
library(cowplot)

####### read data #######
setwd('/Users/chao/Desktop/POP daily/datasets/')
post_2019 = read.table('post_2019.csv',sep=',',header=TRUE,fill=TRUE)
post = read.table('post_full.csv',sep=',',header=TRUE,fill=TRUE)

####### Pre Processing ####### 
post$Click.rate <- (post$Link.clicks / post$Impressions) * 100 
post$Reaction.rate <- (post$Reactions / post$Total.reach) * 100

### select XXXX / non-paid / Reaction.rate < 1.2 / Total.reach > q(0.99)
pop_post <- post %>% 
  filter(Profile.Name == 'XXXX' ) %>% 
  filter(Paid.reach==0)  %>%  # drop post with ads 
  filter(Reaction.rate<10) %>% 
  filter(Total.reach < quantile(post$Total.reach,probs = 0.98,na.rm = TRUE))

### check na's  
for (col in colnames(pop_post)){
  print(col)
  print(sum(is.na(pop_post[,col])))
  print('--------------------')
}

### change value in column
unique(pop_post$babies)

pop_post <- pop_post %>% 
  mutate(babies = if_else(babies=='','normal',babies)) %>% 
  mutate(shared_method = if_else(shared_method == '','normal',shared_method)) %>% 
  mutate(shared_method = if_else(shared_method == '轉發分享','sub-share',shared_method)) %>% 
  mutate(shared_method = if_else(shared_method == '重發分享','re-share',shared_method))


### factorization 
is_names = colnames(pop_post %>% select(starts_with('is')))
count_names = colnames(pop_post %>% select(starts_with('count')))
factors_names = c('category','Type','shared_method','babies')

# transform to factor
for (name in is_names){
  pop_post[,name] = as.factor(pop_post[,name])}

for (name in count_names){
  if (name != 'count_rows'){
    pop_post[,name] = as.factor(pop_post[,name])}  
}

# factorized Type and category
for (name in factors_names){
  pop_post[,name] = as.factor(pop_post[,name])}

### create link post / video post
link_pop_post <- pop_post %>% 
  filter(Type=='Link')

video_pop_post <- pop_post %>% 
  filter(Type=='Video')



###### EDA ######
lv_pop_post <- pop_post %>% 
  filter(Type=='Link' | Type=='Video')

str(lv_pop_post)
lv_pop_post %>% 
  ggplot(aes(y=Total.reach,x=Type,fill=Type))+
  geom_boxplot()+
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))

g_pop_post <- lv_pop_post %>% 
  group_by(Type) %>% 
  summarise(average_reach=mean(Total.reach), average_reaction_rate = mean(Reaction.rate),average_reactions = mean(Reactions),
            sd_reach=sd(Total.reach), sd_reaction_rate = sd(Reaction.rate), sd_reactions=sd(Reactions))

g_pop_post

### Types

p1 <- lv_pop_post %>% 
  ggplot(aes(y=Total.reach,x=Type,fill=Type)) + 
  geom_boxplot()+
  ggtitle('Average Total.reach') +
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  

p2 <- lv_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=Type,fill=Type))+
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  ggtitle('Average Reaction.rate') +
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + 
  ylim(0,5)


p3 <- lv_pop_post %>% 
  ggplot(aes(y=Reactions,x=Type,fill=Type))+
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  ggtitle('Average Reactions')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') + 
  ylim(0,2000)

lg = get_legend(p1)
p1 <- p1 + theme(legend.position='none')

grid.arrange(p1,p2,p3,lg,nrow=1)




### distribution plan

p4 <- lv_pop_post %>% 
  ggplot(aes(Total.reach)) + 
  geom_histogram(fill="#7e67ff", color="#e9ecef", alpha=0.9)+
  ggtitle('Average Total.reach') +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
  
p5 <- lv_pop_post %>% 
  ggplot(aes(Reaction.rate)) + 
  geom_histogram(fill="#7e67ff", color="#e9ecef", alpha=0.9)+
  ggtitle('Average Reaction.rate') + 
  xlim(0,5) +
  theme(plot.title = element_text(hjust = 0.5))

p6 <- lv_pop_post %>% 
  ggplot(aes(Reactions)) + 
  geom_histogram(fill="#7e67ff", color="#e9ecef", alpha=0.9) + 
  ggtitle('Average Reactions',) + 
  xlim(0,5000) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p4,p5,p6,nrow=1)

### correlation between reaction and reach
lv_pop_post %>% 
  ggplot(aes(x=Total.reach,y=Reactions)) + 
  geom_point()

lv_pop_post %>% 
  ggplot(aes(x=Total.reach,y=Reaction.rate)) + 
  geom_point()

lm_reactions_rate <- lm(Reactions~Reaction.rate,data=lv_pop_post)
anova(lm_reactions_rate)
summary(lm_reactions_rate)

lm_reactions_reach <- lm(Reactions~Total.reach,data=lv_pop_post)
anova(lm_reactions_reach)  
summary(lm_reactions_reach)

lm_reactions_all <- lm(Reactions~Total.reach+Reaction.rate,data=lv_pop_post)
anova(lm_reactions_all)
summary(lm_reactions_all)

#------------------------------------------------------#
###### lm for count_rows ######
#------------------------------------------------------#
### 將6行以上視為同一個類別 ###

table(pop_post$count_rows)
pop_post_mutrows <- pop_post %>% 
  mutate(count_rows = if_else(count_rows>=5 & count_rows<=10,as.integer(5),count_rows)) %>% 
  filter(count_rows <= 10) %>% 
  mutate(rows3 = if_else(count_rows>3,1,0)) %>% 
  mutate_at(c('count_rows'),list(factor)) 

link_pop_post_mutrows <- pop_post_mutrows %>% 
  filter(Type=='Link')

video_pop_post_mutrows <- pop_post_mutrows %>% 
  filter(Type=='Video')

### link - reach  ### 

lm_link_reach <- lm(Total.reach~count_rows,data=link_pop_post_mutrows)
anova(lm_link_reach)
summary(lm_link_reach)

link_pop_post_mutrows %>% 
  ggplot(aes(y=Total.reach,x=count_rows,fill=count_rows))+
  geom_boxplot() + 
  ylim(0,100000)
  
### link - rate  ###   

lm_link_rate <- lm(Reaction.rate~count_rows,data=link_pop_post_mutrows)
anova(lm_link_rate)
summary(lm_link_rate)

link_pop_post_mutrows %>% 
  ggplot(aes(y=Reaction.rate,x=count_rows,fill=count_rows))+
  geom_boxplot() + 
  ylim(0,2)


### link plots
p1 <- link_pop_post_mutrows %>% 
  ggplot(aes(y=Total.reach,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,100000) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 


p2 <- link_pop_post_mutrows %>% 
  ggplot(aes(y=Reaction.rate,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,2) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

p3 <- link_pop_post_mutrows %>% 
  ggplot(aes(y=Reactions,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,1000) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')

grid.arrange(p1,p2,p3,nrow=1)



### video - reach ###

lm_video_reach <- lm(Total.reach~count_rows,data=video_pop_post_mutrows)
anova(lm_video_reach)
summary(lm_video_reach)

video_pop_post_mutrows %>% 
  ggplot(aes(y=Total.reach,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,120000)

### video - rate ###

lm_video_rate <- lm(Reactions~count_rows,data=video_pop_post_mutrows)
anova(lm_video_rate)
summary(lm_video_rate)

video_pop_post_mutrows %>% 
  group_by(count_rows) %>% 
  summarise(mean_reactions = mean(Reactions),mean_reaction_rate=mean(Reaction.rate))

### vidoe plots
p1 <- video_pop_post_mutrows %>% 
  ggplot(aes(y=Total.reach,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,200000) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 


p2 <- video_pop_post_mutrows %>% 
  ggplot(aes(y=Reaction.rate,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,2) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

p3 <- video_pop_post_mutrows %>% 
  ggplot(aes(y=Reactions,x=count_rows,fill=count_rows))+
  geom_boxplot()+
  ylim(0,1500) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,nrow=1)


### 行數是否大於三 ### 
lm_link_rows3 <- lm(Total.reach~rows3,data = link_pop_post_mutrows)
anova(lm_link_rows3)
summary(lm_link_rows3)

link_pop_post_mutrows %>% 
  ggplot(aes(y=Total.reach,x=factor(rows3)))+
  geom_boxplot()+
  ylim(0,100000)


lm_interactions_hashtag = lm(Reactions~is_hashtag,data=pop_post_exluded)
anova(lm_interactions_hashtag)
summary(lm_interactions_hashtag)

#------------------------------------------------------#
# end
#------------------------------------------------------#



### lm for category ### 
lm_cat = lm(Reactions~category,data=pop_post)
anova(lm_cat)
summary(lm_cat)
summary(pop_post)

### lm for all ###
pop_post$count_rows[pop_post$count_rows >=8] = '8 or more'  # >8 as one level
pop_post$count_rows = as.factor(pop_post$count_rows)
threshold = 5000
pop_post_exluded = subset(pop_post,Reactions<threshold)
lm_default = lm(Total.Interactions ~ Type +  create_month +
                  category * (is_at+is_comments+is_hashtag+is_poster+is_share+is_tag+
                                count_at+count_comments+count_hashtag+count_poster+count_share+count_tag+count_rows
                  )
                  ,data = pop_post_exluded)
anova(lm_default)
summary(lm_default)


hist(pop_post[,'count_rows'])

levels(category);  
levels(Type);
levels(count_rows)

# simple boxplot

boxplot(Total.Interactions~category,ylab='Total Interactions',xlab='category')
boxplot(Total.Interactions~Type)

levels(category);  levels(Type)


###### similarity ######
post_2019 = read.table('post_2019.csv',sep=',',header=TRUE,fill=TRUE)
post_2019$Reaction_rate <-  post_2019$Reactions / post_2019$Total.reach


post_2019_sub <- post_2019 %>% 
  na.omit(cols=c('tm_similarity','Reaction_rate')) %>% 
  subset(Type='Link') %>% 
  subset(Paid.reach = 0) 

lm_sim = lm(~tm_similarity,data=post_2019_sub)
anova(lm_sim)
summary(lm_sim)

post_2019_sub %>% 
  ggplot(aes(y=Reaction_rate,x=tm_similarity))+
  geom_point()



###### blank location ######

### pre-processing 
pop_post <- pop_post %>%
  mutate(blank_location = if_else(is.na(blank_location),0,blank_location))
# get blank occurances > 300
tab = table(pop_post$blank_location)
sub_pop_post <- pop_post[pop_post$blank_location %in% names(tab)[tab>300],]
sub_pop_post$blank_location <- as.factor(sub_pop_post$blank_location)

sub_pop_post$count_blank <- length(sub_pop_post$blank_location)

sub_pop_post_link <- sub_pop_post %>% 
  filter(Type == 'Link')

### Type = Link y =reaction_rate
lm_count_blank_reaction <- lm(Reaction.rate~count_blank,data=sub_pop_post_link)
anova(lm_count_blank_reaction)
summary(lm_count_blank_reaction)

### Type = Link, y= reaction_rate
lm_link_reaction <- lm(Reaction.rate~blank_location,data=sub_pop_post_link)
anova(lm_link_reaction)
summary(lm_link_reaction)

### Type = Link, y= click_rate
lm_link_click <- lm(click_rate~blank_location,data=sub_pop_post_link)
anova(lm_link_click)
summary(lm_link_click)

### Type = Video, y= reaction_rate
sub_pop_post_video <- sub_pop_post %>% 
  filter(Type == 'Video')

lm_video_reaction <- lm(Reaction.rate~blank_location,data=sub_pop_post_video)
anova(lm_video_reaction)
summary(lm_video_reaction)

### Type = Video, y= click_rate

lm_video_reaction <- lm(click_rate~blank_location,data=sub_pop_post_video)
anova(lm_video_reaction)
summary(lm_video_reaction)

### plots
sub_pop_post %>% 
  ggplot(aes(y=Reactions,x=blank_location)) +
  geom_boxplot() +
  ylim(0,1000)


###### link location ######
colnames(pop_post)

# get link occurances > 500
tab = table(pop_post$hashtag_location)

# names(tab)[tab>500]
sub_pop_post <- pop_post[pop_post$hashtag_location %in% names(tab)[tab>100],]
sub_pop_post$hashtag_location[str_detect(sub_pop_post$hashtag_location,'0|1|2|3',negate = TRUE)] <- "not in three"
sub_pop_post$hashtag_location[str_detect(sub_pop_post$hashtag_location,'1|2|3')] <- "yes in three"
sub_pop_post$hashtag_location <- as.factor(sub_pop_post$hashtag_location)

sub_pop_post <- sub_pop_post %>% 
  filter(Type == 'Link') %>% 
  filter(Paid.reach != 0)

sub_pop_post$hashtag_location <- sub_pop_post %>% 
  mutate(str_detect(hashtag_location,"0|1|2|3",negate = TRUE),'not in 3') %>% 
  mutate(str_detect(hashtag_location,"1|2|3"),'yes in 3')

table(sub_pop_post$hashtag_location)

### lm_hashtag y = reaction_rate
lm_hash_loc <- lm(Reaction.rate~hashtag_location,data=sub_pop_post)
anova(lm_hash_loc)
summary(lm_hash_loc)

### lm_hashtag y = click_rate
lm_hash_click <- lm(click_rate~hashtag_location,data=sub_pop_post)
anova(lm_hash_click)
summary(lm_hash_click)


### plots 

sub_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=hashtag_location))+
  geom_boxplot() + 
  ylim(0,0.025)
  
sub_pop_post %>% 
  ggplot(aes(y=click_rate,x=hashtag_location))+
  geom_boxplot() + 
  ylim(0,0.025)


###### is_factors #######

### see distribution
for (col in colnames(link_pop_post)) {
  if (grepl('is',col,fixed=TRUE)) {
    print(paste0('Link:',col))
    print(table(link_pop_post[,col]))
    print('------------------------')
  }
}

for (col in colnames(video_pop_post)) {
  if (grepl('is',col,fixed=TRUE)) {
    print(paste0('video: ',col))
    print(table(video_pop_post[,col]))
    print('------------------------')
  }
}

###### Comments ######

### link - Total.reach ###
lm_link_reach <- lm(Total.reach~is_comments,data=link_pop_post)
anova(lm_link_reach)
summary(lm_link_reach)

### video - total.reach ###
lm_video_reach <- lm(Total.reach~is_comments,data=link_pop_post)
anova(lm_video_reach)
summary(lm_video_reach)


link_pop_post %>% 
  group_by(is_comments) %>% 
  summarise(mean_reactions = mean(Reaction.rate), mean(Total.reach), mean(Reactions))


### link plots ### 

p1 <- link_pop_post %>% 
  ggplot(aes(x=is_comments,y=Total.reach,fill=is_comments)) +
  geom_boxplot() + 
  ylim(0,100000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  theme(plot.title = element_text(hjust=0.5))+ 
  ggtitle('Total.reach')


p2 <- link_pop_post %>% 
  ggplot(aes(x=is_comments,y=Reaction.rate,fill=is_comments)) +
  geom_boxplot() + 
  ylim(0,2)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+ 
  ggtitle('Reaction.rate')


p3 <- link_pop_post %>% 
  ggplot(aes(x=is_comments,y=Reactions,fill=is_comments)) +
  geom_boxplot() + 
  ylim(0,1000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+ 
  ggtitle('Reactions')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')

grid.arrange(p1,p2,p3,lg,nrow=1)

### vidoe plots ### 

p1 <- video_pop_post %>% 
  ggplot(aes(x=is_comments,y=Total.reach,fill=is_comments)) +
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  theme(plot.title = element_text(hjust=0.5))+ 
  ggtitle('Total.reach') + 
  scale_y_continuous(labels = function(x) format(x,scientific = FALSE),limits = c(0,220000))

p2 <- video_pop_post %>% 
  ggplot(aes(x=is_comments,y=Reaction.rate,fill=is_comments)) +
  geom_boxplot() + 
  ylim(0,2)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+ 
  ggtitle('Reaction.rate')

p3 <- video_pop_post %>% 
  ggplot(aes(x=is_comments,y=Reactions,fill=is_comments)) +
  geom_boxplot() + 
  ylim(0,1000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+ 
  ggtitle('Reactions')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,lg,nrow=1)



######Share ######

# link - reaction_rate

lm_share_link <- lm(Reaction.rate~is_share,data=link_pop_post)
anova(lm_share_link)
summary(lm_share_link)

  
p1 <- link_pop_post %>% 
  ggplot(aes(x=is_share,y=Reaction.rate,fill=is_share)) +
  geom_boxplot() + 
  ylim(0,1.5)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))

p2 <- link_pop_post %>% 
  ggplot(aes(x=is_share,y=Total.reach,fill=is_share)) +
  geom_boxplot() + 
  ylim(0,40000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust=0.5),legend.position='none')

p3 <- link_pop_post %>% 
  ggplot(aes(x=is_share,y=Reactions,fill=is_share)) +
  geom_boxplot() + 
  ylim(0,1000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust=0.5),legend.position='none')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,lg,nrow=1)


### 影片
p1 <- video_pop_post %>% 
  ggplot(aes(x=is_share,y=Reaction.rate,fill=is_share)) +
  geom_boxplot() + 
  ylim(0,1.5)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))

p2 <- video_pop_post %>% 
  ggplot(aes(x=is_share,y=Total.reach,fill=is_share)) +
  geom_boxplot() + 
  ylim(0,40000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust=0.5),legend.position='none')

p3 <- video_pop_post %>% 
  ggplot(aes(x=is_share,y=Reactions,fill=is_share)) +
  geom_boxplot() + 
  ylim(0,1000)+
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust=0.5),legend.position='none')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,lg,nrow=1)


###### hashtag ######

### link - Total.reactions ###

lm_hashtag_link <- lm(Reaction.rate~is_hashtag,data=link_pop_post)
anova(lm_hashtag_link)
summary(lm_hashtag_link)

### plots link ###
p1 <- link_pop_post %>% 
  ggplot(aes(y=Total.reach,x=is_hashtag,fill=is_hashtag)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))+
  ylim(0,100000) + 
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Total.reach')


p2 <- link_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=is_hashtag,fill=is_hashtag)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))+
  ylim(0,2) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+
  ggtitle('Reaction.rate')
  
p3 <- link_pop_post %>% 
  ggplot(aes(y=Reactions,x=is_hashtag,fill=is_hashtag)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))+
  ylim(0,1000) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+
  ggtitle('Reactions')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')

grid.arrange(p1,p2,p3,lg,nrow=1)


### plots video ###

p1 <- video_pop_post %>% 
  ggplot(aes(y=Total.reach,x=is_hashtag,fill=is_hashtag)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))+
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Total.reach') + 
  scale_y_continuous(labels = function(x) format(x,scientific = FALSE))

p2 <- video_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=is_hashtag,fill=is_hashtag)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))+
  ylim(0,2) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+
  ggtitle('Reaction.rate')

p3 <- video_pop_post %>% 
  ggplot(aes(y=Reactions,x=is_hashtag,fill=is_hashtag)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))+
  ylim(0,1000) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+
  ggtitle('Reactions') 
  

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,lg,nrow=1)


###### hashtag babies ######

gis_babies <- pop_post %>% 
  group_by(is_hashtag_babies) %>% 
  summarise(count=n())

gis_babies %>% 
  ggplot(aes(y=count,x=is_hashtag_babies,fill=is_hashtag_babies))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values=c("#7e67ff", "#f291d6"))

gis_babies

### link - reach ###
lm_link_reach <- lm(Total.reach~is_hashtag_babies,data=link_pop_post)
anova(lm_link_reach)
summary(lm_link_reach)

### link - rate ###
lm_link_rate <- lm(Reaction.rate~is_hashtag_babies,data=link_pop_post)
anova(lm_link_rate)
summary(lm_link_rate)

### link - reactions ###
lm_link_reactions <- lm(Reactions~is_hashtag_babies,data=link_pop_post)
anova(lm_link_reactions)
summary(lm_link_reactions)


### plots
p1 <- link_pop_post %>% 
  ggplot(aes(y=Total.reach,x=is_hashtag_babies,fill=is_hashtag_babies))+
  geom_boxplot()+
  ylim(0,100000) +
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Total.reach')

p2 <- link_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=is_hashtag_babies,fill=is_hashtag_babies))+
  geom_boxplot()+
  ylim(0,2) +
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) +
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none') +
  ggtitle('Reaction.rate')

p3 <- link_pop_post %>% 
  ggplot(aes(y=Reactions,x=is_hashtag_babies,fill=is_hashtag_babies))+
  geom_boxplot()+
  ylim(0,1000) +
  scale_fill_manual(values=c("#7e67ff", "#f291d6")) + 
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none')+
  ggtitle('Reactions')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,lg,nrow=1)


###### 子版成效 ######

### count boxplot 

gbabies <- pop_post %>% 
  group_by(babies) %>% 
  summarise(count=n(), average_Total.reach=mean(Total.reach))

gbabies

gbabies[-1,] %>% 
  ggplot(aes(y=count,x=babies,fill=babies))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))
  
### link - reach ###
lm_link_reach <- lm(Total.reach~babies,data=pop_post)
anova(lm_link_reach)
summary(lm_link_reach)
### link - rate ###
lm_link_rate <- lm(Reaction.rate~babies,data=link_pop_post)
anova(lm_link_rate)
summary(lm_link_rate)
### link - reactions ###
lm_link_reaction <- lm(Reactions~babies,data=link_pop_post)
anova(lm_link_reaction)
summary(lm_link_reaction)

### plot link ### 

link_pop_post %>% 
  ggplot(aes(y=Total.reach,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ylim(0,100000)

link_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ylim(0,2)

link_pop_post %>% 
  ggplot(aes(y=Reactions,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ylim(0,1000)

### best preformed babies plot ###

table(link_pop_post$babies)
levels(link_pop_post$babies)

best_babies <- c('normal','PopCutie','PopKimchi','PopTour','PopYummy')
best_babies_link <- link_pop_post %>% 
  filter(babies %in% best_babies)

p1 <- best_babies_link %>% 
  ggplot(aes(y=Total.reach,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 45)) + 
  scale_y_continuous(labels = function(x) format(x,scientific = FALSE),limits = c(0,100000))+
  ggtitle('Total.reach') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

p2 <- best_babies_link %>% 
  ggplot(aes(y=Reaction.rate,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 45),) + 
  ylim(0,2) +
  ggtitle('Reaction.rate') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

link_pop_post %>% 
  group_by(babies) %>% 
  summarise(mean_reaction_rate = mean(Reaction.rate), mean_reactions = mean(Reactions))


p3 <- best_babies_link %>% 
  ggplot(aes(y=Reactions,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 45),legend.position='none') + 
  ylim(0,1000) + 
  ggtitle('Reactions') + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#ba51e9","#d42a97",'#42218E','#b39ed9')) 

lg <- get_legend(p1)  
p1 <- p1 + theme(legend.position='none')

grid.arrange(p1,p2,p3,lg,nrow=1)

 ### video - reach ###
lm_video_reach <- lm(Total.reach~babies,data=video_pop_post)
anova(lm_video_reach)
summary(lm_video_reach)

video_pop_post %>% 
  ggplot(aes(y=Total.reach,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ylim(0,140000)

### video - rate ###
lm_video_rate <- lm(Reaction.rate~babies,data=video_pop_post)
anova(lm_video_rate)
summary(lm_video_rate)

video_pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ylim(0,2)

### video - reactions ###

lm_video_reaactions <- lm(Reactions~babies,data=video_pop_post)
anova(lm_video_reaactions)
summary(lm_video_reaactions)

video_pop_post %>% 
  ggplot(aes(y=Reactions,x=factor(babies),fill=factor(babies)))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ylim(0,1000)


###### 分享方式 ######
table(pop_post$shared_method)

gshared <- pop_post %>% 
  group_by(shared_method) %>% 
  summarise(count=n(), mean_reactions=mean(Reactions),
            mean_reaction_rate = mean(Reaction.rate),
            mean_reach = mean(Total.reach))

gshared

gshared %>% 
  ggplot(aes(y=count,x=shared_method,fill=shared_method))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#d8b1d4"))
  
### all reach ###
lm_reach <- lm(Total.reach~shared_method,data=pop_post)
anova(lm_reach)
summary(lm_reach)

### all rate ###

lm_rate <- lm(Reaction.rate~shared_method,data=pop_post)
anova(lm_rate)
summary(lm_rate)

### all reactions ###

lm_reactions <- lm(Reactions~shared_method,data=pop_post)
anova(lm_reactions)
summary(lm_reactions)


### plots 
p1 <- pop_post %>% 
  ggplot(aes(y=Total.reach,x=shared_method,fill=shared_method))+
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#d8b1d4")) + 
  ylim(0,120000) + 
  theme(plot.title= element_text(hjust = 0.5)) +
  ggtitle('Total.reach')


p2 <- pop_post %>% 
  ggplot(aes(y=Reaction.rate,x=shared_method,fill=shared_method))+
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#d8b1d4")) + 
  ylim(0,2) + 
  theme(plot.title= element_text(hjust=0.5), legend.position='none') + 
  ggtitle('Reaction.rate')

p3 <- pop_post %>% 
  ggplot(aes(y=Reactions,x=shared_method,fill=shared_method))+
  geom_boxplot() + 
  scale_fill_manual(values=c("#7e67ff", "#f291d6","#d8b1d4")) + 
  ylim(0,1000) + 
  theme(plot.title = element_text(hjust=0.5),legend.position='none')+
  ggtitle('Reactions')

lg <- get_legend(p1)
p1 <- p1 + theme(legend.position='none')
grid.arrange(p1,p2,p3,lg,nrow=1)  
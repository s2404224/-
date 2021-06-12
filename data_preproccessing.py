#!/usr/bin/env python
# coding: utf-8

# # Project Planning - Fb post NLP processing 
# 
# ### Description
# 1. 分析當臉書貼文標籤、社群互動、特定文字時和互動率、觸及率之關係
# - 貼文標籤 => @  Emoji 連結
# - 特定文字 => 節慶字眼 XX編 折扣/優惠 新手必學
# - 社群互動 => tag 請朋友留言 Hashtag 
# 
# 2. 分析臉書貼文素材種類和互動率、觸及率之關係
# - 圖片 連結 影片 
# 3. 分析文章類型和互動率、觸及率之關係
# 4. 分析臉書貼文架構和互動率、觸及率之關係
# 
# 
# ### Data Processing
# 
# **EDA**
# - counts of profile
# - length of messages
# - Indicators : Total Reaction & Organic Reach by profile
# 
# **Data Preparation** <br>
# - datatime
# - check length
# 
# **Create dataframe with target features**
# - binary or frequency ?
# 

# In[69]:


import pandas as pd
import numpy as np
import os
import nltk
import matplotlib.pyplot as plt
import math
import re
# %matplotlib inline 

# set path
path = '/Users/chao/Desktop/POP daily/datasets'
os.chdir(path)


# post_2019 = pd.read_csv('fb_post.csv')
# art = pd.read_csv('article.csv')
# post = pd.read_excel('facebook_post_info_2020_20201202更新.xlsx')
# fb_ad1 = pd.read_excel('fb_ad1.xlsx')
# fb_ad2 = pd.read_excel('fb_ad2.xlsx')
# sales = pd.read_excel('業務販售及成效資料.xlsx')
post_full = pd.read_csv('post_full.csv')


# # Data Preparations

# ### columns
# 1. category 
# 2. interaction _level
# 3. shared method
# 4. blank_location
# 5. hashtag in location

# In[8]:


### match the category 
def get_category(link):
    for cat in categories:
        if cat in link:
            return cat
    return None

categories = list(post_full['category'].value_counts().index)
post['category'] = post['Outgoing Link'].apply(lambda x : get_category(str(x)))    

### transfer to datetime
post['Create Time'] = pd.to_datetime(post['Create Time'])
post = post.sort_values('Create Time')


### cluster the interactions
def cluster_post(target,name):
    if target < 200:
        return f'low {name}'
    elif target < 500:
        return f'median {name}'
    elif target < 1000:
        return f'high {name}'
    else:
        return 'hit'
cluster_levels = ['Low Reactions','Medium Reactions','High Reactions','Hit']
post['reaction_level'] = post['Reactions'].apply(lambda x : cluster_post(x,'Reactions'))


### 轉發文章的方式 shared_method
def shared_link(link):
    split_link = link.split('facebook')
    if len(split_link) != 3:
        return np.nan
    else:
        if split_link[2][1:9] != 'page':
            return '轉發分享'
        else:
            return np.nan
        

### 判定分享方法
page = post[post['Profile Name'] == 'page ABC']
page['shared_method'] = page['Outgoing Link'].apply(lambda x :shared_link(str(x)))
post = pd.concat([post,page[['shared_method']]],axis=1)
post.loc[post['is_link'] == 1,'shared_method'] = '重發分享'
print(post['shared_method'].value_counts())


## 檢查是否有空格
mess = post['Message'].iloc[3]

def is_blank(mess):
    i = 0
    blank_location = ''
    for strs in mess.split('\n'):
        if len(strs) < 2:
            blank_location += str(i)
        i += 1
    if len(blank_location) == 0:
        return np.nan
    return blank_location

post['blank_location'] = post['Message'].apply(lambda x : is_blank(str(x)))

# hashtag出現是否在前三行

def is_location(targets,mess):
    i = 0
    target_location = ''
    for strs in mess.split('\n'):
        for target in targets:
            if target in strs:
                target_location += str(i)
        i += 1
    if target_location == '':
        return np.nan
    return target_location

post['hashtag_location'] = post_full['Message'].apply(lambda mess : is_location(['#','＃'],str(mess)))

# ### groupby month
# post_by_time = post_full.groupby(['category',pd.Grouper(key='Create Time',freq='M')]).mean()
# post_by_time.xs('entertainment').pct_change()

# for cat in categories:
#     post_by_time.xs('category')
#     group_time['Reactions'].pct_change()


# ### 2. Create frequencies

# In[6]:


# check if target element if exists in string 
def is_target(strs,target):
    if target in strs:
        return 1
    return 0

# caculate frequency of element exists in string
def count_target(strs,target):
    return strs.count(target)  

from emoji import UNICODE_EMOJI
# emojis
target_emojis = '😀😃😄😁😆😅😂🤣☺️😊😇🙂🙃😉😌😍🥰😘😗😙😚😋😛😝😜🤪🤨🧐🤓😎🤩🥳😏😒😞😔😟😕🙁☹️😣😖😫😩🥺😢😭😤😠😡🤬🤯😳🥵🥶😱😨😰😥😓🤗🤔🤭🤫🤥😶😐😑😬🙄😯😦😧😮😲🥱😴🤤😪😵🤐🥴🤢🤮🤧😷🤒🤕🤑🤠❤️🧡💛💚💙💜🖤🤍🤎💔❣️💕💞💓💗💖💘💝💟😺😸😹😻😼😽🙀😿😾'

def is_emoji(strs):
    for s in strs:
        if s in target_emojis:
            return 1
    return 0

def count_emoji(strs):
    count = 0 
    for s in strs:
        if s in UNICODE_EMOJI:
            count += 1
    return count

# def is_emoji(strs):
#     for s in strs:
#         if s in UNICODE_EMOJI and s not in trash_emojis:
#             return 1
#     return 0


# ### Create columns with target included

# In[55]:


# post['Message'] = post['Message'].astype('str')
elements = ['#','tag','留言','分享','@','編','\n']
elements_name = ['hashtag','tag','comments','share','at','poster','rows']

# boolean 1 0 
for i in range(len(elements)):
    if elements_name[i] != 'rows':
        post['is_'+elements_name[i]] = post['Message'].apply(lambda x: is_target(x,elements[i]))
# count
for i in range(len(elements)):
    post['count_'+elements_name[i]] = post['Message'].apply(lambda x: count_target(x,elements[i]))
    if elements_name[i] == 'rows':
        post['count_'+elements_name[i]] = post['Message'].apply(lambda x: count_target(x,elements[i]) + 1)

#是否tag子版        
post['is_hashtag_babies'] = post['Message'].apply(lambda x : 1 if ('Pop' in x) and ('ABC' in x) else 0)

#子版標籤
babies = post['Profile Name'].unique()
tmp = [re.match(r"[a-zA-z]+",baby) for baby in babies]
babies = [tmp[i].group() for i in range(len(tmp))][1:]
babies

def get_babies(mess):
    for baby in babies:
        if baby in mess:
            return baby
    return np.nan

post['babies'] = post['Message'].apply(get_babies)

# specials 
post['is_link'] = post['Message'].apply(lambda x : is_target(x,'bit'))
post['is_emoji'] = post['Message'].apply(lambda x : is_emoji(x))  # is_emoji
post['count_emoji'] = post['Message'].apply(lambda x : count_emoji(x))  # is_emoji
post['len'] = post['Message'].apply(len)


# In[9]:


def split_outgoinglink(link):    
    word = ''
    if 'posts' in link or 'photos' in link or 'videos' in link:
           return None
    elif 'forum' not in link:
        for i in link.split('%')[4:6]:
            word = word + i.replace('2F','') + '.'
        return word[0:len(word)-1]
    else:
        for j in link.split('%')[4:7]:
            word = word + j.replace('2F','') + '.' 
        return word[0:len(word)-1]
    
def jaccard_similarity(l1,l2):
    intersection = len(set(l1).intersection(set(l2)))
    union = len(l1) + len(l2) - intersection
    return intersection / union


# ### Caption Message Similiarity

# In[59]:


from gensim import corpora, models
import jieba.posseg as jp, jieba 
import jieba.analyse 
from ckiptagger import data_utils, construct_dictionary, WS, POS, NER #中研院語料庫

##### Default Settings
# set path
path = '/Users/chao/Desktop/POP daily/NLP_title'
os.chdir(path)

# load dictionary 
ws = WS("./data")
pos = POS("./data")
ner = NER("./data")
pos_dict = pd.read_csv('pos_dict.csv')

# stopwords
with open(r'/Users/chao/Desktop/page/NLP_title/停用詞-繁體中文.txt',encoding='utf-8') as f:
    stopWords = [line.strip() for line in f.readlines()]
    
post_2019['article_id'] = post_2019['outgoinglink'].apply(lambda x : split_outgoinglink(str(x)))
post_art = pd.merge(post_2019,art[['article_id','article_title','article_text']],on='article_id',how='left')
post_art = post_art.dropna(subset=['article_title'])
post_art = post_art[post_art['Profile Name'] == 'page ABC']


# In[60]:


# 切單字和詞性

mess = post_art['Message'].astype('str')
title = post_art['article_title'].astype('str')

w_mess = ws(mess)
w_title = ws(title)

p_mess = pos(w_mess)
p_title = pos(w_title)


# In[ ]:


# set wanted flags
flags = ['Na','VC','A','VE','Nb','VA','Nf','FW','Nc','VH'] # 選擇需要的詞性以利排除不必要的詞彙
unwanted = ['＃','～','Pop','pop','ABC','＼','／']

# set wanted words
from emoji import UNICODE_EMOJI
def select_word(w,p):
    if p in flags and w not in stopWords and '\n' not in w and w not in UNICODE_EMOJI :
        for unwant in unwanted:
            if unwant in w :
                return False
        return True
        
    
# output word list 
def final_word(w_word,p_word):
    final_lst = []
    for i in range(len(w_word)):
        temp_lst = []
        for w,p in zip(w_word[i],p_word[i]):
            if select_word(w,p):
                temp_lst.append(w)
        final_lst.append(temp_lst)
    return final_lst

def jaccard_similarity(l1,l2):
    intercept = len(set(l1).intersection(set(l2)))
    union = len(l1) + len(l2) - intercept
    return intercept/union


f_mess = final_word(w_mess,p_mess)
f_title = final_word(w_title,p_title)


# In[ ]:


similarity_lst = []

for i in range(len(f_mess)):
    similarity_lst.append(jaccard_similarity(f_mess[i],f_title[i]))

plt.hist(similarity_lst,bins=30)
plt.title('title & message similarity distribution')
plt.show()


# In[ ]:


post_art['tm_similarity'] = similarity_lst
post_2019 = pd.merge(post_2019,post_art[['Post Id','article_title','tm_similarity']],how='left',on='Post Id')
# post_2019.to_csv('post_2019.csv',index=False)
# post_2019.drop('outgoinglink',axis=1).to_excel('post_2019.xlsx',index=False)


# ### 輸出檔案

# In[64]:


path = '/Users/chao/Desktop/POP daily/datasets'
os.chdir(path)

# post_full
post.to_csv('post_full.csv',index=False)

# ### 將excel url轉成string
# post['Outgoing Link'] = post['Outgoing Link'].astype('str')
# post['Link'] = post['Link'].astype('str')

# # import xlsxwriter
# # workbook = pd.ExcelWriter('/Users/chao/Desktop/POP daily/datasets/aaaaaaa.xlsx',engine='xlsxwriter',options={'strings_to_urls': False})
# # post.to_excel('post_full.xlsx',index=False)
# # writer = pd.ExcelWriter('post_full.xlsx', engine='xlsxwriter',options={'strings_to_urls': False})
# # post.to_excel(writer)
# # writer.close()


# with pd.ExcelWriter('/Users/chao/Desktop/POP daily/datasets/post_full.xlsx',engine='xlsxwriter',options={'strings_to_urls': False}) as writer:
#     post.to_excel(writer, index=False)


# # EDA

# In[4]:


plt.rcParams['font.sans-serif'] = ['Arial Unicode MS'] #顯示中文


# In[6]:


profile_counts = post['Profile Name'].value_counts()
fig, ax = plt.subplots()
plt.pie(profile_counts,labels=profile_counts.index,autopct ='%.2f')
plt.title('Counts of Profile',pad=10,fontdict={'size':15})
plt.show()


# In[15]:


# Average of Indicators by Profile

post_groups = post.groupby('Profile Name')
post_Imeans = post_groups['Total Interactions'].mean()
post_Rmeans = post_groups['Organic reach'].mean()

fig, ax = plt.subplots()

ax2 = ax.twinx()

lns1 = ax.plot(post_Imeans.index,post_Imeans.values,label='Total Interactions',color='red')
lns2 = ax2.plot(post_Rmeans.index,post_Rmeans.values,label='Organic Reach',color='orange')

# add labels for each line
lns = lns1 + lns2
labs = [l.get_label() for l in lns]

ax.legend(lns,labs,loc=0)
ax.set_xticklabels(labels=post_Imeans.index,rotation=90)
plt.show()


# In[29]:


# distribution of lengths in messages
post['len'] = post['Message'].apply(lambda x : len(str(x)))
profile_mean_lens = post.groupby('Profile Name')['len'].mean()

fig,ax = plt.subplots(1,2,figsize=(12,6))

fig.tight_layout(pad=3)

# histrogram
ax[0].hist(post['len'],bins=50)
ax[0].set_xlim(0,200)
ax[0].set_title('Distribution of Message Lengths',pad=10,fontdict={'size':15})

# mean
ax[1].bar(profile_mean_lens.index,height=profile_mean_lens.values)
ax[1].set_xticklabels(labels=profile_mean_lens.index,rotation=90)
ax[1].set_title('Length of Messages by Profile',pad=10,fontdict={'size':15})
fig.tight_layout()
plt.show()


# In[84]:


f,ax = plt.subplots()
x1 = np.array(post[post['Type'] == 'Video']['Total Interactions'])
x2 = np.array(post[post['Type'] == 'Link']['Total Interactions'])
bins = np.linspace(0, 10000, 30)

ax.hist([x1,x2],bins=bins,color=['b','r'],label=['Video','Link'],)
ax.set(xlim=[0,10000],
      title='Total Interactions by Type')
ax.legend(loc=1)
# plt.xlim(0,10000)


# In[86]:


month_group_interaction = post.groupby('create_month')['Total Interactions']
count_month_group = month_group_interaction.count()
mean_month_group = month_group_interaction.mean()

# line plot x = time, y = mean, label = category
fig, ax = plt.subplots(1,2,figsize=(16,6))

ax[0].plot(count_month_group.index,count_month_group,label='Count of Post')
ax[0].plot(mean_month_group.index,mean_month_group, label='Average Total Interactions')
ax[0].set_title('Trend of post release and average of interactions',pad=10)
ax[0].legend(loc=2)

# 
page_post = post[post['Profile Name'] == 'page ABC'].dropna(subset = ['category'])
bar_category = page_post['category'].value_counts().sort_values(ascending=True)
ax[1].barh(bar_category.index,width= bar_category.values,)
ax[1].set_title('page category distribution',pad=10)
plt.show()


# # Previous Codes

# In[10]:


# def split_outgoinglink(link):    
#     word = ''
#     if 'posts' in link or 'photos' in link or 'videos' in link:
#            return None
#     elif 'forum' not in link:
#         for i in link.split('%')[4:6]:
#             word = word + i.replace('2F','') + '.'
#         return word[0:len(word)-1]
#     else:
#         for j in link.split('%')[4:7]:
#             word = word + j.replace('2F','') + '.' 
#         return word[0:len(word)-1]

        
# def match_article_id(article_id):
#     if len(art[art['article_id'] == article_id]) == 0:
#         return None
#     else:
#         return art[art['article_id'] == article_id]['category'].values[0]
# post['category'] = post['split_outgoinglink'].apply(lambda x : x.split('.')[0] if x != None else None)


# post['category'] = post['split_outgoinglink'].apply(lambda x : match_article_id(x) if x != None else None)

# # output to csv and xlsx
# post.drop(['valid_outgoinglink','outgoinglink'],axis=1).to_csv('fb_post.csv')
# post.drop('valid_outgoinglink',axis=1).to_excel('fb_post.xlsx')


######### growth 
# df.groupby(pd.Grouper(key='column_value', freq="M")).mean().plot()

# groups = post.groupby(['category','create_month'])
# interaction_by_month = groups.mean()
# category = page_post['category'].unique()

# # get categories which post > 200
# page_post_count = page_post.groupby('category').count()['Total Interactions']
# category_over200 = page_post_count[page_post_count>200].index

# # get growth rate for each category 
# cat_growth_list = []

# for cat in category_over200:
#     df_cat = interaction_by_month.xs(cat)
#     total_growth = df_cat.loc[9,'Total Interactions'] / df_cat.loc[1,'Total Interactions']
#     monthly_growth = math.pow(total_growth,1/9)
#     cat_growth_list.append([cat,monthly_growth])

# cat_growth_dict = dict(cat_growth_list)
# page_post['monthy_growth_interactions'] = page_post['category'].apply(lambda x :cat_growth_dict[x]-1 if x in cat_growth_dict.keys() else None)    

# # post = pd.merge(post,pd.DataFrame(page_post['monthy_growth_interactions']),how='left',left_index=True,right_index=True)
# # post.drop('outgoinglink',axis=1).to_excel('fb_post.xlsx',index=False)

# post.columns


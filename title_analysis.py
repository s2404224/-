#!/usr/bin/env python
# coding: utf-8

# # 熱門詞組研究

# In[233]:


import pandas as pd
import numpy as np
import os
import nltk
import matplotlib.pyplot as plt
import seaborn as sns
import math
import re
from functools import reduce

# %matplotlib inline 

# set path
path = '/Users/chao/Desktop/page/datasets'
os.chdir(path)


# ### Preprocessing

# In[234]:


df = pd.read_csv('post_art_all.csv')
yummy_dictionary = pd.read_excel('Yummy_dictionary.xlsx')
df['rate'] = df['Reactions'] / df['Organic reach']


def reaction_level(x):
    if x < 200:
        return 'low'
    elif x < 500:
        return 'medium'
    elif x < 1000:
        return 'high'
    else :
        return 'hit'
    
df['reaction_level'] = df['Reactions'].apply(reaction_level)
df = df.rename(columns={'Reactions':'reactions','Organic reach':'reach','reaction_rate':'rate'})

ABC = df.loc[df['Profile Name'] == 'ABC page']
yummy = df.loc[df['Profile Name'] == 'ABC page2']


# In[235]:


from pylab import mpl
mpl.rcParams['font.sans-serif'] = ['Microsoft YaHei']  
mpl.rcParams['axes.unicode_minus'] = False
plt.rcParams['font.sans-serif'] = ['Arial Unicode MS']


# In[236]:


def clipboard_to_df():
    df_word = pd.read_clipboard(header=None) 
    word_list = df_word[0].values
    pop_count_list, all_count_list = [],[]

    for word in word_list:
        pop_count = post_art['article_title'].apply(lambda x :True if str(word) in x else False).sum()
        all_count = post_all['article_title'].apply(lambda x :True if str(word) in x else False).sum()
        pop_count_list.append(pop_count)
        all_count_list.append(all_count)
        print(f'ABC主板 : {word} ',pop_count)
        print(f'所有版面      : {word}', all_count)
        print('----------------------------------------------------------')
    
    df_word['pop_count'] = pop_count_list
    df_word['all_count'] = all_count_list
    df_word.to_clipboard(sep=',', index=False)


# ## 熱門詞組研究

# In[237]:


class word_info():
    def __init__(self,df,objectives,word_lst):
        self.df = df
        self.objectives = objectives
        self.word_lst = word_lst
        self.df = self.df[self.df['reactions']>0]

    def get_dataframe(self):
        df_word = pd.DataFrame()
        
        for words in self.word_lst:
            # get objectives   ##### not automized yet!!!
            objectives_arr = self.df.apply(lambda x : [x[self.objectives[0]],x[self.objectives[1]],x[self.objectives[2]],x[self.objectives[3]]] if any(word in str(x.article_title) for word in words.split(',')) else np.nan,axis=1).dropna().values
            reactions_arr = np.array([i[0] for i in objectives_arr])
            reach_arr = np.array([i[1] for i in objectives_arr])
            rate_arr = np.array([i[2] for i in objectives_arr])
            level_arr = np.array([i[3] for i in objectives_arr])
            tmp_dict = {'reactions':reactions_arr,'reach':reach_arr,'rate':rate_arr,'reaction_level':level_arr}
            tmp = pd.DataFrame(data=tmp_dict,columns=['reactions','reach','rate','reaction_level'])
            # add word
            tmp['word'] = words
            # add count
            df_word = pd.concat([df_word,tmp])
        return df_word

    def get_summary(self,df_word,aggfunc,thresh=10):
        self.df_word = df_word
        
        dfs = []
        count = self.df_word.pivot_table(index='word',values=self.objectives[0],aggfunc='count').reset_index().rename(columns={self.objectives[0]:'count'})
        self.df_word = pd.merge(self.df_word,count,on='word',how='inner')
        for i in range(len(self.objectives)):
            if self.objectives[i] == 'reaction_level':                
                df_levels = self.df_word.pivot_table(index='word',columns='reaction_level',aggfunc= 'size').fillna(0) 
                levels = df_levels.columns
                df_levels['count'] = df_levels.apply(sum,axis=1)
                # add rate 
                for l in levels :
                    df_levels[l+'_rate'] = df_levels[l] / df_levels['count']
                df_levels = df_levels.drop(columns=['count']).reset_index()
                dfs.append(df_levels)
            else : 
                tmp_mean = self.df_word.pivot_table(index='word',values=self.objectives[i],aggfunc=aggfunc).reset_index()
                dfs.append(tmp_mean)
        
        dfs.append(count)
        summary = reduce(lambda left,right : pd.merge(left,right,on='word'), dfs)
        summary = summary.loc[summary['count'] >= thresh]
        summary = summary.sort_values(by='count',ascending=False)
        return summary
    
    def plot_settings(self, summary, word_lst, numbers=30, order = 'count', obj = 'count', palette='flare_r', count_thresh = 0, thresh=0, ascending=False, reverse=False):
        self.summary = summary
        self.palette = palette
        self.thresh = thresh
        self.word_lst = word_lst
        self.numbers = numbers
        
         # set threshold
        self.summary = self.summary.loc[summary[obj] > thresh] 
        self.summary = self.summary.loc[summary['count'] > count_thresh]
        # sort values
        self.word_lst = self.summary.sort_values(by=[order],ascending=ascending)['word'].values
        if len(self.word_lst) > numbers:
            self.word_lst = self.word_lst[:numbers]
        if reverse == True:
            self.word_lst = self.word_lst[::-1]
            
    
    def countplot(self,figsize=(10,8)):
        cmap = sns.cubehelix_palette(self.numbers,reverse=True)
        data = self.df_word.loc[self.df_word['word'].isin(self.word_lst)]        
        fig,ax = plt.subplots(figsize = figsize)
        ax = sns.barplot(x='word',y='count',data=data, order=self.word_lst,palette=cmap)
        ax.set_title('count by words', pad=10, fontdict={'fontsize':20})

        xlocs, xlabels = plt.xticks()
#         ylocs, ylabels = plt.yticks()                
        ax.set_xticklabels(xlabels, size = 15)        
#         ax.set_yticklabels(ylocs, size = 15)        

        ax.set_xlabel('字詞',fontdict={'fontsize':15})
        ax.set_ylabel('發文數',fontdict={'fontsize':15}) 
        
        plt.show()
    
    def boxplot(self, objective, ylim, figsize=(10,8), sort = True):
        sns.cubehelix_palette(self.numbers,as_cmap=True)
        fig,ax = plt.subplots(figsize = figsize)
        data = self.df_word.loc[self.df_word['word'].isin(self.word_lst)]        
        if sort == True:
            ax = sns.boxplot(x='word',y=objective,data=data,order=self.word_lst,palette=self.palette)
        else :
            ax = sns.boxplot(x='word',y=objective,data=data,palette = self.palette)
        ax.set_title(f'{objective} by words',pad=10,fontdict={'fontsize':20})

        xlocs, xlabels = plt.xticks()
#         ylocs, ylabels = plt.yticks()                
        ax.set_xticklabels(xlabels, size = 15)        
#         ax.set_yticklabels(ylocs, size = 15)        

        ax.set_ylim(0,ylim)
        ax.set_xlabel('字詞',fontdict={'fontsize':15})
        ax.set_ylabel(f'{objective}',fontdict={'fontsize':15}) 
        plt.show()

        
    def barplot(self, objective, ylim, figsize=(10,8), sort=True):   
        sns.cubehelix_palette(self.numbers,as_cmap=True)        
        fig, ax = plt.subplots(figsize=figsize)
        if sort == True :
            ax = sns.barplot(x='word',y=objective,data=self.summary,order=self.word_lst,palette=self.palette)
        else :
            ax = sns.barplot(x='word',y=objective,data=self.summary)            
        ax.set_title(f'{objective} by words',pad=10,fontdict={'fontsize':20})

        xlocs, xlabels = plt.xticks()
#         ylocs, ylabels = plt.yticks()                
        ax.set_xticklabels(xlabels, size = 15)        
#         ax.set_yticklabels(ylocs, size = 15)        
        ax.set_ylabel(f'{objective}',fontdict={'fontsize':15}) 
        ax.set_xlabel('字詞',fontdict={'fontsize':15})
        ax.set_ylim(0,ylim)


# # Yummy

# In[74]:


# all categories

### 熱門字詞 
impress_lst = ['熱門字詞','必系列','時間性','動詞','藏系列']
tmp = pd.DataFrame()
for col in impress_lst:
    tmp = pd.concat([tmp,yummy_dictionary[col]])
impress = tmp.unstack().dropna().values

### 餐廳種類 
food = yummy_dictionary['餐廳種類'].dropna().values

# ### 形容詞
adj_lst = ['形容詞','動詞','時間性','好系列','價格','用途']
tmp = pd.DataFrame()
for col in adj_lst:
    tmp = pd.concat([tmp,yummy_dictionary[col]])
adj = tmp.unstack().dropna().values


# ### 醒目標題 - good

# In[239]:


# set variables
word_lst = impress
objectives = ['reactions','reach','rate','reaction_level']

obj_dict = dict(zip(objectives[:3],[5500,400000,0.03]))

# get data 
info = word_info(yummy,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'mean')

### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=10, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 10 
    numbers = 10 
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=False)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 10, numbers=10, order='hit_rate', ascending=False)
info.barplot(objective ='hit_rate',ylim=1,)


# ### 醒目標題 - bad

# In[ ]:


# set variables
word_lst = impress
objectives = ['reactions','reach','rate','reaction_level']

obj_dict = dict(zip(objectives[:3],[5500,400000,0.03]))

# get data 
info = word_info(yummy,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'median')

### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=15, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 10 
    numbers = 15 
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=True,reverse=True)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 10, numbers=10, order='hit_rate', ascending=False)
info.barplot(objective ='hit_rate',ylim=1,)


# ### 餐廳種類 - good

# In[219]:


# set variables
word_lst = food
objectives = ['reactions','reach','rate','reaction_level']

obj_dict = dict(zip(objectives[:3],[5500,400000,0.03]))

# get data 
info = word_info(yummy,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'mean')

### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=10, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 10 
    numbers = 10 
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=False)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 10, numbers=10, order='hit_rate', ascending=False)
info.barplot(objective ='hit_rate',ylim=1,)


# In[32]:


df_summary.sort_values(by=['hit_rate'],ascending=False)[['word','hit_rate']]


# ### 餐廳種類 - bad

# In[ ]:


# set variables
word_lst = food
objectives = ['reactions','reach','rate','reaction_level']

obj_dict = dict(zip(objectives[:3],[5500,400000,0.03]))

# get data 
info = word_info(yummy,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'median')

### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=7, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 10 
    numbers = 6 
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=True,reverse=True)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 10, numbers=10, order='hit_rate', ascending=False)
info.barplot(objective ='hit_rate',ylim=1,)


# ### 形容詞 - good 

# In[202]:


# set variables
word_lst = adj
objectives = ['reactions','reach','rate','reaction_level']

obj_dict = dict(zip(objectives[:3],[4500,400000,0.03]))

# get data 
info = word_info(yummy,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'median')
df_summary.sort_values(by=['reactions'],ascending=False)

### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=6, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 20 
    numbers = 10
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=False)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 30, numbers=10,order='hit_rate')
info.barplot(objective ='hit_rate',ylim=1,)


# ### 形容詞 - bad 

# In[ ]:


# set variables
word_lst = adj
objectives = ['reactions','reach','rate','reaction_level']

obj_dict = dict(zip(objectives[:3],[4500,400000,0.03]))

# get data 
info = word_info(yummy,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'median')

### count plot
info.plot_settings(df_summary, word_lst, order='count')
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 20 
    numbers = 
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers=numbers,ascending=True, reverse=True)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 30, numbers=10,order='hit_rate')
info.barplot(objective ='hit_rate',ylim=1,)


# # ABC

# In[225]:


# all categories

### 熱門字詞 
impress_lst = ['熱門字詞','必系列','時間性','動詞','藏系列']
tmp = pd.DataFrame()
for col in impress_lst:
    tmp = pd.concat([tmp,yummy_dictionary[col]])
impress = tmp.unstack().dropna().values


# In[232]:


df_summary.sort_values(by=['reactions'],ascending=False)[['word','reactions']]


# In[240]:


# set variables
word_lst = impress
objectives = ['reactions','reach','rate','reaction_level']
obj_dict = dict(zip(objectives[:3],[3500,400000,0.03]))

# get data 
info = word_info(ABC,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'mean')


### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=15, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 20 
    numbers = 10
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=False)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 30, numbers=10,order='hit_rate')
info.barplot(objective ='hit_rate',ylim=1,)


# In[213]:


# set variables
word_lst = impress
objectives = ['reactions','reach','rate','reaction_level']
obj_dict = dict(zip(objectives[:3],[1500,400000,0.03]))

# get data 
info = word_info(ABC,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'median')


### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=6, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 20 
    numbers = 10
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=False)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 30, numbers=10,order='hit_rate')
info.barplot(objective ='hit_rate',ylim=1,)


# In[12]:


# set variables
word_lst = impress
objectives = ['reactions','reach','rate','reaction_level']
obj_dict = dict(zip(objectives[:3],[1000,400000,0.03]))

# get data 
info = word_info(ABC,objectives,word_lst)
df_word = info.get_dataframe() 
df_summary = info.get_summary(df_word,'mean')

### count plot
info.plot_settings(df_summary, word_lst, order='count',numbers=20, ascending=False,reverse=False)
info.countplot()

### box plot 
for obj in objectives[:3]:
    count_thresh = 10 
    numbers = 15 
    info.plot_settings(df_summary, word_lst, order=obj, count_thresh = count_thresh, numbers = numbers,ascending=True,reverse=True)
    info.boxplot(objective = obj,ylim = obj_dict[obj],sort=True)
    
### bar plot 
info.plot_settings(df_summary, word_lst, count_thresh = 10, numbers=10, order='hit_rate', ascending=False)
info.barplot(objective ='hit_rate',ylim=1,)


# In[ ]:





#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np 
import pandas as pd 

from sklearn import preprocessing
from sklearn.model_selection import cross_validate
from sklearn.model_selection import train_test_split
from sklearn import linear_model
from sklearn import metrics


# In[2]:


import matplotlib.pyplot as plt
import seaborn as seabornInstance 


# In[4]:


prices=pd.read_csv("/Users/Sarah/Desktop/CMC/Academics/Spring2020/MATH389/Midterm/nyse/prices.csv")


# In[5]:


prices.head()


# In[15]:


df = pd.read_csv("/Users/Sarah/Desktop/CMC/Academics/Spring2020/MATH389/Midterm/nyse/prices.csv") #loading the csv file
df=df[df.symbol=='RHT']#choosing stock symbol


# In[16]:


df


# In[17]:


plt.title('low VS high')
plt.scatter(df.date, df.low)
plt.plot(df.date, df.high, color='red')
plt.show()


# In[18]:


plt.title('open VS close')
plt.scatter(df.date, df.open)
plt.plot(df.date, df.close, color='red')
plt.show()


# In[19]:


plt.figure(figsize=(10,5))
plt.tight_layout()
seabornInstance.distplot(df['close'])


# In[37]:


def prepare_data(df,forecast,out,test_size):
    label = df[forecast].shift(-out);
    X = np.array(df[[forecast]]); 
    X = preprocessing.scale(X) 
    X_lately = X[-out:] 
    X = X[:-out] 
    label.dropna(inplace=True); 
    y = np.array(label)  
    X_train, X_test, Y_train, Y_test = train_test_split(X, y, test_size=test_size)  

    response = [X_train, X_test, Y_train, Y_test, X_lately];
    return response;


# In[44]:


forecast = 'close'
out = 5 
test_size = 0.2; 
X_train, X_test, Y_train, Y_test , X_lately =prepare_data(df,forecast,out,test_size); 

learner = linear_model.LinearRegression(); 

learner.fit(X_train,Y_train); 
score=learner.score(X_test,Y_test);
y_hat = learner.predict(X_test)
score1=learner.score(X_test,Y_test)

forecasty= learner.predict(X_lately); 

response={};
response['test_score']=score; 
response['forecast_set']=forecasty;

print(response);


# In[45]:


fig, ax = plt.subplots(figsize=(15, 10))
ax.plot(range(len(Y_test)), Y_test, label='y_test')
ax.plot(range(len(y_hat)), y_hat, label='y_pred')


# In[46]:


print('Mean Absolute Error:', metrics.mean_absolute_error(Y_test, y_hat))  
print('Mean Squared Error:', metrics.mean_squared_error(Y_test, y_hat))  
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(Y_test, y_hat)))


# In[ ]:





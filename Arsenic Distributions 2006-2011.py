import numpy as np
import pandas as pd
import sklearn
import matplotlib.pyplot as plt

#importing the data
arsenic_df = pd.read_csv('arsenic_df.txt', sep = '\t')

#Corr plot of the data
import seaborn as sns

df = arsenic_df.corr()

plt.figure(figsize=(8, 8))
sns.heatmap(df, annot=True, cmap='coolwarm', fmt=".2f", annot_kws={"size": 9})
plt.title('Arsenic Correlation Plot')
plt.show()

arsenic_df.describe()

arsenic_df.columns

#Get years alone as ints in new column

tlist = []
for i in arsenic_df['Sample Collection Date']:
  year = i[:4]
  tlist.append(year)

arsenic_df['year'] = pd.DataFrame(tlist)

#separate data by year

dat_06 = arsenic_df[arsenic_df['year']== '2006']
dat_07 = arsenic_df[arsenic_df['year']== '2007']
dat_08 = arsenic_df[arsenic_df['year']== '2008']
dat_09 = arsenic_df[arsenic_df['year']== '2009']
dat_10 = arsenic_df[arsenic_df['year']== '2010']
dat_11 = arsenic_df[arsenic_df['year']== '2011']

df_yr = [dat_06, dat_07, dat_08,
         dat_09, dat_10, dat_11]

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import precision_recall_fscore_support
from sklearn.metrics import classification_report
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
import statsmodels.api as sm

def logit_reg_it(data):
  X = data[['Region', 'State Code',  'Source Water Type']] #'State Code', 'Adjusted Total Population Served', 'Division', 'Region',
  y = data['Detect']

  labelencoder = LabelEncoder()
  X['Region'] = labelencoder.fit_transform(X['Region'])
  X['State Code'] = labelencoder.fit_transform(X['State Code'])
  X['Source Water Type'] = labelencoder.fit_transform(X['Source Water Type'])

  X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

  model = LogisticRegression()
  model.fit(X_train, y_train)

  coefficients = np.exp(model.coef_)
  intercepts = model.intercept_

  y_pred = model.predict(X_test)
  X_test = sm.add_constant(X_test)
  y_pred_class = (y_pred > 0.5).astype(int)

  accuracy = accuracy_score(y_test, y_pred_class)
  precision = precision_score(y_test, y_pred_class)
  recall = recall_score(y_test, y_pred_class)
  fscore = f1_score(y_test, y_pred_class)

  return coefficients, accuracy, intercepts, precision, recall, fscore

def countpos_neg (data):
  pos = 0
  neg = 0
  l = 0
  for l in (data['Detect']):
    if l == 1:
      pos += 1
    else:
      neg += 1
  merg = [pos, neg, pos+neg]
  return merg

#iterate thru the model and showcase the years
blank_l = []
cell_df = []
for j in range(0, 6):
  cell_df = df_yr[j]
  countpos_neg(cell_df)
  temp_dump = logit_reg_it(cell_df)
  blank_l.append(temp_dump)

#plotting prep
x = []
x2 = []
x3 = []
cell = []
coefs = []
acc = []
pres = []
reca = []
fone = []
for j in range(0, 6):
  cell = blank_l[j]
  coefs = cell[0]
  acc.append(cell[1])
  pres.append(cell[3])
  reca.append(cell[4])
  fone.append(cell[5])
  for k in range(0,3):
    if k == 0:
      x.append(coefs[0][k])
    elif k == 1:
      x2.append(coefs[0][k])
    else:
      x3.append(coefs[0][k])


years = list(range(2006, 2012))

def convert_to_perclog (x):
  new_list = []
  for i in range(0,len(x)):
    y = x[i] * 100 - 100
    new_list.append(round(y, 2))

  return new_list

def convert_to_perc (x):
  new_list = []
  for i in range(0,len(x)):
    y = x[i] * 100
    new_list.append(round(y, 2))

  return new_list

fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8))

# Plot percentages
ax1.set_title('Variable Likelihood of Arsenic Detection Imputation')
ax1.plot(years, convert_to_perclog(x), label='Region')
ax1.plot(years, convert_to_perclog(x2), label='State')
ax1.plot(years, convert_to_perclog(x3), label='Source Water Type')
ax1.set_ylabel('Odds Ratio % Detection')
ax1.legend()

# Plotting accuracy
ax2.plot(years, convert_to_perc(acc), color='red', label='Accuracy', marker='o', linestyle='--')
ax2.plot(years, convert_to_perc(pres), color='blue', label='Precision', marker='o', linestyle='--')
ax2.plot(years, convert_to_perc(reca), color='orange', label='Recall', marker='o', linestyle='--')
ax2.plot(years, convert_to_perc(fone), color='green', label='F1-Score', marker='o', linestyle='--')

ax2.set_ylabel('Percent %')
ax2.set_xlabel('Years')
ax2.legend()

plt.tight_layout()
plt.show()

merged_df = pd.DataFrame([years, acc, pres, reca, fone, x,x2,x3]).T
merged_df.columns = ['Year', 'Accuracy', 'Precision',  'Recall', 'F1-Score', 'Region', 'State', 'Source Water Type']

print(merged_df.iloc[:,:5])

#counts of 1,0 in datasets
dect_tab = []
for p in df_yr:
  print(countpos_neg(p))
  dect_tab += [countpos_neg(p)]

#percent 1 in datasets
for i in range(0,6):
  print(dect_tab[i][0] / dect_tab[i][2])

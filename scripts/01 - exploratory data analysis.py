import pandas as pd

df = pd.read_csv('data/train.csv')
print(df.head())
print(df.shape)
frequency_table = df['Transported'].value_counts()
print(frequency_table)
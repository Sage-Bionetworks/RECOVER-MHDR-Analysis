#%%
import numpy as np
import os
import pandas as pd
import matplotlib.pyplot as plt
from data_utils import *
from constant import *
from sklearn.cluster import KMeans


# Specify the folder containing the .npy files
folder_path = './miss_day_time'

# Get a list of .npy files in the folder
file_names = os.listdir(folder_path)

# %%
# # Load the arrays and combine them into a matrix
df_list = [pd.read_csv(os.path.join(folder_path, file), index_col=[0]) for file in file_names]
for i in range(len(df_list)):
    print(file_names[i])
    df_list[i]['interval_start'] = pd.to_datetime(df_list[i]['interval_start'])
    df_list[i] = clean_rows(df_list[i], START_TIME, END_TIME)

# %%
array_list = []
# combined_matrix = np.stack(arrays, axis=0)
for df in df_list:
    df['interval_start'] = pd.to_datetime(df['interval_start'])
    df = add_rows(df, START_TIME, END_TIME).copy()
    array_list.append(df['missflg'].values)
    # print(len(df))
# print(combined_matrix)

# Combine all the arrays
combined_matrix = np.stack(array_list, axis=0)

# %%
df = pd.DataFrame(combined_matrix, index=file_names, columns=range(combined_matrix.shape[1]))
# %%

kmeans = KMeans(n_clusters=4)
kmeans.fit(combined_matrix)
clusters = kmeans.labels_

df['rk'] = clusters
# %%
df = df.sort_values('rk')
# %%
df.drop(columns='rk', inplace=True)

# %%
time_range_15min = pd.date_range(start=pd.Timestamp('2023-09-01'), end=pd.Timestamp('2024-02-24 23:49:00'), freq='15min')
# %%
# plt.imshow(df2.to_numpy(), aspect=100, cmap='Blues')
plt.pcolormesh(df.to_numpy().astype("int"), cmap='Blues_r')
plt.ylabel('Person ID')
plt.xticks(np.arange(0, 16692, 600), time_range_15min[np.arange(0, 16692, 600)], rotation=90)
plt.xlabel('Time Range')
# %%
mt = combined_matrix.copy()
mt = mt.reshape(mt.shape[0], mt.shape[1] // 96, 96).astype(int)
mt = mt.sum(axis=2)
df = pd.DataFrame(mt, index=file_names, columns=range(mt.shape[1]))

# %%
kmeans = KMeans(n_clusters=5)
kmeans.fit(combined_matrix)
clusters = kmeans.labels_

df['rk'] = clusters
df = df.sort_values('rk')
df.drop(columns='rk', inplace=True)
time_chunk_1day = pd.date_range(start=pd.Timestamp('2023-09-01'), end=pd.Timestamp('2024-02-24 23:49:00'), freq='24h')

# %%
plt.pcolormesh(df.to_numpy() / 96, cmap='Blues_r')
plt.ylabel('Person ID')
plt.xticks(np.arange(0, df.shape[1], 15), time_chunk_1day[np.arange(0, df.shape[1], 15)], rotation=90)
plt.colorbar(label='Average missingness across every day within 6 months')
plt.xlabel('Time Range')
# %%

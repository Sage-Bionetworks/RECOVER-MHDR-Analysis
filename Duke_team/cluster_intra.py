#%%
import numpy as np
import os
import pandas as pd
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
from constant import *


# Specify the folder containing the .npy files
folder_path = './missratio_h'

# Get a list of .npy files in the folder
file_names = [f for f in os.listdir(folder_path) if f.endswith('.npy')]

# Load the arrays and combine them into a matrix
arrays = [np.load(os.path.join(folder_path, file)) for file in file_names]
combined_matrix = np.stack(arrays, axis=0)

print(combined_matrix)

# %%
kmeans = KMeans(n_clusters=6)
kmeans.fit(combined_matrix)
clusters = kmeans.labels_
df = pd.DataFrame(combined_matrix, index=file_names, columns=range(combined_matrix.shape[1]))
df['rk'] = clusters
# %%
df = df.sort_values('rk')
df.drop(columns='rk', inplace=True)
# %%
after_cluster = combined_matrix[clusters]
plt.figure(figsize=(10, 6))
plt.pcolormesh(df.to_numpy().astype('float'), shading='auto', cmap='Blues_r')
plt.xticks(np.arange(0, df.shape[1], 15), time_chunk_15min[np.arange(0, df.shape[1], 15)], rotation=90)
# plt.pcolormesh(df.columns, df.index, df.to_numpy().astype('float'), shading='auto', cmap='Blues_r')
plt.colorbar(label='Average missingness across one day')
plt.ylabel('Person ID')
plt.xlabel('Time in a day')
plt.show()

# %% Smooth across one hour
mt = combined_matrix.copy()
mt = mt.reshape(mt.shape[0], mt.shape[1] // 4, 4).astype(float)
mt = mt.mean(axis=2)
df = pd.DataFrame(mt, index=file_names, columns=range(mt.shape[1]))

# %%
kmeans = KMeans(n_clusters=7)
kmeans.fit(combined_matrix)
clusters = kmeans.labels_

df['rk'] = clusters
df = df.sort_values('rk')
df.drop(columns='rk', inplace=True)
time_chunk_1hour = pd.date_range(start=pd.Timestamp('00:00'), end=pd.Timestamp('23:49'), freq='1h').time

# %%
plt.pcolormesh(df.to_numpy(), cmap='Blues_r')
plt.ylabel('Person ID')
plt.xticks(np.arange(0, df.shape[1], 1), time_chunk_1hour[np.arange(0, df.shape[1], 1)], rotation=90)
plt.colorbar(label='Average missingness across every day within 6 months')
plt.xlabel('Time Range')


# %%
# Smooth across one hour
cluster_random_state = 42
mt = combined_matrix.copy()
mt = mt.reshape(mt.shape[0], mt.shape[1] // 4, 4).astype(float)
mt = mt.mean(axis=2)
df = pd.DataFrame(mt, index=file_names, columns=range(mt.shape[1]))

# Determine the optimal number of clusters using the elbow method
wcss = []
K = range(1, 15)
for k in K:
    kmeans = KMeans(n_clusters=k, random_state=cluster_random_state)
    kmeans.fit(df)
    wcss.append(kmeans.inertia_)

# Plot the WCSS to find the elbow
plt.figure(figsize=(10, 6))
plt.plot(K, wcss, 'bo-')
plt.xlabel('Number of Clusters')
plt.ylabel('Within-Cluster Sum of Squares (WCSS)')
plt.title('Elbow Method for Optimal Number of Clusters')
plt.show()

# Assuming the elbow point is determined to be 7 for demonstration
optimal_k = 6

# Fit KMeans with the optimal number of clusters
kmeans = KMeans(n_clusters=optimal_k, random_state=cluster_random_state)
kmeans.fit(df)
clusters = kmeans.labels_

# Add cluster labels to the dataframe and sort by cluster
df['rk'] = clusters
df = df.sort_values('rk')
df.drop(columns='rk', inplace=True)
time_chunk_1hour = pd.date_range(start=pd.Timestamp('00:00'), end=pd.Timestamp('23:49'), freq='1h').time

# Plot the clustered heatmap
plt.figure(figsize=(12, 8))
plt.pcolormesh(df.to_numpy(), cmap='Blues_r')
plt.ylabel('Person ID')
plt.xticks(np.arange(0, df.shape[1], 1), [t.strftime('%H:%M') for t in time_chunk_1hour[np.arange(0, df.shape[1], 1)]], rotation=90)
plt.colorbar(label='Average missingness across every day')
plt.xlabel('Hour of Day')
plt.tight_layout()
plt.savefig('figures/clu_hour_average_all.jpg', dpi=600)
plt.show()


# %%
import numpy as np
xx = np.load("ID.npy", allow_pickle=True)
# %%

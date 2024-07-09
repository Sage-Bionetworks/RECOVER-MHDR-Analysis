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

# %%

#%%
import numpy as np
import os

# Specify the folder containing the .npy files
folder_path = './missratio_h'

# Get a list of .npy files in the folder
file_names = [f for f in os.listdir(folder_path) if f.endswith('.npy')]

# Load the arrays and combine them into a matrix
arrays = [np.load(os.path.join(folder_path, file)) for file in file_names]
combined_matrix = np.stack(arrays, axis=0)

print(combined_matrix)

# %%
import pandas as pd
df = pd.DataFrame(combined_matrix, index=file_names, columns=range(96))
# %%
from sklearn.cluster import KMeans

kmeans = KMeans(n_clusters=4)
kmeans.fit(combined_matrix)
clusters = kmeans.labels_

df['rk'] = clusters
# %%
df = df.sort_values('rk')
# %%
df.drop(columns='rk', inplace=True)
# %%
import matplotlib.pyplot as plt
after_cluster = combined_matrix[clusters]
plt.figure(figsize=(10, 6))
plt.pcolormesh(df.columns.values, df.index, df.to_numpy(), shading='auto', cmap='Blues_r')
plt.colorbar(label='Miss Ratio')

plt.show()

# %%
from missing_pattern_plot import MissingPatternPlot
# %%
plot_data = MissingPatternPlot.initialize(df, None, 'study_period', clusters)
plot_data.plot('cluster', direction=False, y_label='PERSON_ID', y_ticks=True)
# %%
plt.imshow(df.to_numpy(), aspect='auto', cmap='viridis')
plt.colorbar(label='Miss Ratio')

# %%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Generate the date range from 0:00 to 24:00 with 15-minute intervals
time_chunks = pd.date_range(start='00:00', end='23:59', freq='15min').strftime('%H:%M')

# Sample data for demonstration (for example purposes, generating a random matrix)
data = np.random.rand(len(time_chunks), 10)  # 10 subjects and 96 time slots (15 minutes each in 24 hours)

# Plot using imshow
plt.figure(figsize=(12, 6))
plt.imshow(data, aspect='auto', cmap='viridis')

# Set x-ticks
plt.xticks(ticks=np.arange(len(time_chunks)), labels=time_chunks, rotation=90, step=4)

# Set y-label and x-label
plt.ylabel('Subjects')
plt.xlabel('Time of Day')

# Display the plot
plt.colorbar(label='Random Value')
plt.tight_layout()
plt.show()

# %%

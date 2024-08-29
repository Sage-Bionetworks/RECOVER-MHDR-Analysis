
# %%

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
# %%
from pathlib import Path

# Specify the directory
pivot_d = Path('./missratio_week')
array_d = Path('./missratio_h')

pivot_files = [file.resolve() for file in pivot_d.iterdir() if file.is_file() and file.suffix == '.csv']
missratio_h_files = [file.resolve() for file in array_d.iterdir() if file.is_file() and file.suffix == '.npy']
# Load the data
# %%
id = 26
miss_ratio_h = np.load(missratio_h_files[id], allow_pickle=True)
pivot_table = pd.read_csv(pivot_files[id], index_col=0)
subject_id = pivot_files[0].stem

step = 4
# %% Smooth the pivot table
pivot_table['group'] = np.repeat(np.arange(0, len(pivot_table) // step), step)
pivot_table_gb = pivot_table.groupby('group').mean()

# %%

time_series = pivot_table.index.values
reduced_time_series = time_series[::step]

# Plot the pivot table using pcolormesh
plt.figure(figsize=(10, 6))
plt.pcolormesh(pivot_table_gb.T.to_numpy(), shading='auto', cmap='Blues_r')
plt.colorbar(label='Missingness Ratio')

# Set the ticks for x and y axis
plt.yticks(ticks=np.arange(0, len(pivot_table_gb.columns)), labels=pivot_table_gb.columns)
# plt.yticks(ticks=np.arange(0, len(pivot_table.columns)), labels=pivot_table.columns)

plt.title('Missingness by 1 hour Chunks and Weekday ')
plt.xlabel('1-Hour Chunks')
plt.ylabel('Weekday')
plt.show()

# Your array

# Duplicate the array to create a 2D array
array_2d = np.tile(miss_ratio_h, (2, 1))
reduced_time_series = time_series[::step]

# Create the plot
plt.figure(figsize=(10, 2))
plt.pcolormesh(array_2d, shading='auto', cmap='Blues_r')
plt.colorbar(label='Missingness')
plt.title('Missingness during a day ' + str(id))
plt.xlabel('Time Intervals')
plt.xticks(ticks=np.arange(0, len(time_series), step), labels=reduced_time_series, rotation=90)
plt.yticks([])
plt.show()
# %%

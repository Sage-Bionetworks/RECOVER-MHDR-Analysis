#%%
import numpy as np
import os
import pandas as pd

# Specify the folder containing the .npy files
folder_path = './miss_day_time'

# Get a list of .npy files in the folder
file_names = os.listdir(folder_path)

# %%
def add_rows(df, start_date, end_date):
    # Create new date range
    new_dates = pd.date_range(start=start_date, end=end_date, freq='15min')

    # Generate new DataFrame
    new_data = {
        'interval_start': new_dates,
        'missflg': [True] * len(new_dates),
        '15_min_chunk': new_dates.strftime('%H:%M:%S'),
        'date': new_dates.date
    }
    new_df = pd.DataFrame(new_data)
    new_data['interval_start'] = pd.to_datetime(new_data['interval_start'])
    df.set_index('interval_start', inplace=True)
    new_df.set_index('interval_start', inplace=True)
    new_df.drop(df.index, inplace=True)
    # Combine old and new DataFrames
    combined_df = pd.concat([df, new_df])
    # Sort by 'interval_start'
    combined_df = combined_df.sort_values(by='interval_start').reset_index(drop=True)
    combined_df.drop(combined_df.tail(1).index,inplace=True)
    print(len(combined_df))
    return combined_df
# %%
# # Load the arrays and combine them into a matrix
df_list = [pd.read_csv(os.path.join(folder_path, file), index_col=[0]) for file in file_names]
array_list = []
# combined_matrix = np.stack(arrays, axis=0)
for df in df_list:
    df['interval_start'] = pd.to_datetime(df['interval_start'])
    df = add_rows(df, '2023-08-01', '2023-12-30').copy()
    array_list.append(df['missflg'].values)
    # print(len(df))
# print(combined_matrix)

# Combine all the arrays
combined_matrix = np.stack(array_list, axis=0)

# %%
df = pd.DataFrame(combined_matrix, index=file_names, columns=range(14496))
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
plt.pcolormesh(df.columns.values, df.index, df.to_numpy()+0.0, shading='auto', cmap='Blues_r')
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

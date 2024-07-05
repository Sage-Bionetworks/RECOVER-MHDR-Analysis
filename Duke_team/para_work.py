# %%
import pandas as pd
import numpy as np

# Create the initial DataFrame
data = {
    'interval_start': pd.date_range(start='2023-10-04', end='2023-10-11 23:45:00', freq='15min'),
    'missflg': [True] * 768,  # The count of entries between the given dates
}
data['15_min_chunk'] = data['interval_start'].strftime('%H:%M:%S')
data['date'] = data['interval_start'].date

df = pd.DataFrame(data)

# Function to generate rows from 2023-08-01 to 2023-12-30 with 15 minute intervals
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

    # Combine old and new DataFrames
    combined_df = pd.concat([df, new_df]).drop_duplicates().reset_index(drop=True)
    # Sort by 'interval_start'
    combined_df = combined_df.sort_values(by='interval_start').reset_index(drop=True)
    combined_df.drop(combined_df.tail(1).index,inplace=True)
    return combined_df

# Add rows to DataFrame
df_updated = add_rows(df, '2023-08-01', '2023-12-30')

print(df_updated)

# %%

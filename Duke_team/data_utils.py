import matplotlib.pyplot as plt
import pyarrow.parquet as pq
import numpy as np
import pandas as pd
from tqdm import tqdm
import matplotlib.dates as mdates


def hist_dist(hist_val, x_label="", y_label="", bins=1, x_range=None, y_range=None, title="", st=0):
    plt.xlabel(x_label, fontsize=10)
    plt.ylabel(y_label, fontsize=10)
    n, bins, p = plt.hist(hist_val, range=(hist_val.min()+st, hist_val.max()), bins = int(hist_val.max()-st-hist_val.min()))

    plt.xticks(fontsize=10)  # Increase font size for x ticks
    plt.yticks(fontsize=10)  # Increase font size for y ticks
    plt.grid(True, linestyle='--', alpha=0.5)  # Add grid lines
    max_val = max(hist_val.dropna())
    plt.text(max_val, 0, f'Max: {max_val}', verticalalignment='bottom', horizontalalignment='right', fontsize=10)  # Add text annotation for maximum value
    plt.subplots_adjust(hspace=0.6)

    plt.title(title, fontsize=10)  # Add title with increased font size
    if x_range:
        plt.xlim(x_range)
    if y_range:
        plt.ylim(y_range)

    peak_bin_index = np.argmax(n)
    peak_bin_value = bins[peak_bin_index]
    
    return peak_bin_value, hist_val.mean()

def get_dataset_df(dataset_paths, i, fs, piece_num=0):
    dataset = pq.ParquetDataset(dataset_paths[i], filesystem=fs)
    # print("get_data:"+str(dataset_paths[i]))
    # parquet_file = pq.ParquetFile("s3://" + dataset_paths[i])

    # for batch in parquet_file.iter_batches():
    #     print("RecordBatch")
    #     batch_df = batch.to_pandas()
    #     print("batch_df:", batch_df)
    
    if piece_num == 0:
        df = dataset.read().to_pandas()
    else:
        mm = dataset.read([0])
        # tables = []
        # for i, piece in enumerate(dataset.pieces[:piece_num]):
        #     parquet_file = pq.ParquetFile(piece.path, filesystem=fs)

        #     table = piece.to_table()
        #     tables.append(table)
    
        # # Combine the tables into a single DataFrame
        # combined_table = pq.concat_tables(tables)
        # df = combined_table.to_pandas()
    print(df.columns)
    return df

def update_df_object_numeric(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = df[column_name].astype('float')
    return df

def update_min_to_hour(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = df[column_name].astype('float') / 60
    return df

def update_df_object_str(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = df[column_name].astype('str')
    return df

def update_df_object_date(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = pd.to_datetime(df[column_name])
    return df

def add_weekday_num(df, col_name):
    df['weekday'] = pd.to_datetime(df[col_name]).dt.weekday
    return df

def update_df_object_bool(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = df[column_name].astype('bool')
    return df

def meta_df_df(df, col_name, xlabel, ylabel):
    plt.figure(figsize=(12, 6))
    print(df[col_name].value_counts())
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.hist(df[col_name].value_counts(), bins = 200)
    plt.show()

def check_duplicate(df, refer_name, col_name):
    flag = 0
    for id in df[refer_name].unique():
        if df.loc[df[refer_name]==id][col_name].value_counts().max() > 1:
            print(id)
            flag = 1
    if flag == 0:
        print("No duplicate date")

def show_none_df(df):
    missing_percentage = (df.isnull().mean() * 100).round(2)
    # Display the missing percentage of each column
    for name,val in zip(missing_percentage.index, missing_percentage):
        print(name, val)
    # print("Missing Percentage of Each Column:")
    # print(np.sort(missing_percentage))

    return missing_percentage

def get_mean_df(df, col_name):
    data = {'ID': df['ParticipantIdentifier'].unique(), 'avg': [-0.1] * len(df['ParticipantIdentifier'].unique())}
    avg_wearing_df = pd.DataFrame(data)
    for id in df['ParticipantIdentifier'].unique():
        value = df[df['ParticipantIdentifier']==id][col_name].astype("float").mean()
        avg_wearing_df.loc[avg_wearing_df['ID']==id, 'avg'] = value

    return avg_wearing_df
    # xx = df[df['ParticipantIdentifier']=="RA11003-00024"]['HeartRateIntradayMinuteCount'].astype("float")

def cal_ad_here(df, hr_col_name, col_name):
    df[col_name] = df[hr_col_name] / 1440
    return df

def date_avg_ad(df, col_1, col_2, xlabel, ylabel):
    stat_df_date_list = df[col_1].unique()
    date_val_dict = {col_1 :stat_df_date_list, col_2: np.nan}
    stat_df = pd.DataFrame(date_val_dict)
    df = df[df['Date']>'2022-01-01']
    stat_df = stat_df[stat_df[col_1]>'2022-01-01']
    for date in stat_df_date_list:
        avg = df[df[col_1]==date][col_2].mean()
        stat_df.loc[stat_df[col_1]==date, col_2] = avg
    stat_df.loc[stat_df[col_2].isnull(), col_2] = -0.1
    return stat_df

def plot_bar(df, x_col, y_col, labels, rotation=45):
    plt.bar(df[x_col], df[y_col])
    plt.xlabel(labels[0])
    plt.ylabel(labels[1])
    plt.xticks(rotation=rotation)

def merge_df_devices(df, df_devices):
    df.insert(1, 'Device', None)
    device_merge = df_devices.iloc[:, [0,2]]
    print("begin")
    for id in tqdm(device_merge.iloc[:,0].values):
        device_name = device_merge[device_merge['ParticipantIdentifier']==id]['Device'].value_counts().index[0]
        df.loc[df['ParticipantIdentifier']==id,'Device'] = device_name
    return df

def get_sleep_date(df, col_name_1, col_name_2, new_col):
    df.loc[:,new_col] = None
    for index, row in df.iterrows():
        row[new_col] = pd.to_datetime(row[col_name_2])
    return df

def filter_fitbit_daily(df, df_enrolled, df_devices):
    # Read in dataset_enrolledparticipants and save it as enrolledparticipants
    enrolledparticipants = df_enrolled
    
    # Read in dataset_fitbitdailydata and save it as fitbit_daily
    fitbit_daily = df
    
    # Read in dataset_fitbitdevices and save it as fitbit_devices
    fitbit_devices = df_devices
    
    # Subset fitbit daily data based on the definition of the start/end date
    ID_daily = fitbit_daily['ParticipantIdentifier'].unique()
    fitbit_daily['Date2'] = pd.to_datetime(fitbit_daily['Date'])
    new2_fitbit_daily = pd.DataFrame()
    
    for num, ID in enumerate(ID_daily):
        enrollment_date = pd.to_datetime(enrolledparticipants.loc[enrolledparticipants['ParticipantIdentifier'] == ID, 'EnrollmentDate'].values[0])
        tmp_daily = fitbit_daily.loc[(fitbit_daily['ParticipantIdentifier'] == ID) & (fitbit_daily['Date2'] >= enrollment_date)].sort_values('Date2')
        row_index = np.where(tmp_daily['HeartRateIntradayMinuteCount'] != 0)[0]
        
        if len(row_index) > 0:
            data2 = tmp_daily.iloc[row_index[0]:row_index[-1] + 1]
        else:
            data2 = pd.DataFrame()
            
        if num > 10:
            break
        new2_fitbit_daily = pd.concat([new2_fitbit_daily, data2])

    print(new2_fitbit_daily)
    
    # Label participants who only used Sense 2 and/or Charge 5 during the study
    ID_devices = fitbit_devices['ParticipantIdentifier'].unique()
    num_of_devices = []  # get the number of devices for each participant
    enrolledparticipants['onlyS2C5'] = 0  # dummy variable: 1 = only used Sense 2 and/or Charge 5
    
    for num, ID in enumerate(ID_devices):
        device_info = fitbit_devices.loc[fitbit_devices['ParticipantIdentifier'] == ID, 'Device'].unique()
        num_of_devices.append(len(device_info))
        
        if len(device_info) == np.sum(np.isin(device_info, ['Sense 2', 'Charge 5'])):
            enrolledparticipants.loc[enrolledparticipants['ParticipantIdentifier'] == ID, 'onlyS2C5'] = 1

        if num > 10:
            break
    
    ID_onlyS2C5 = enrolledparticipants.loc[enrolledparticipants['onlyS2C5'] == 1, 'ParticipantIdentifier']

    print("new2_fitbit_daily number is " + str(len(new2_fitbit_daily['ParticipantIdentifier'].unique())))
    print("onlyS2C5 number is " + str(len(ID_onlyS2C5)))

    return new2_fitbit_daily, ID_onlyS2C5

def filter_(fitbit_daily, enrolledparticipants):
    # Subset fitbit daily data based on the definition of the start/end date
    ID_daily = fitbit_daily['ParticipantIdentifier'].unique()
    fitbit_daily['Date2'] = pd.to_datetime(fitbit_daily['Date'])
    new2_fitbit_daily = pd.DataFrame()

    for ID in ID_daily:
        enrollment_date = pd.to_datetime(enrolledparticipants.loc[enrolledparticipants['ParticipantIdentifier'] == ID, 'EnrollmentDate'].values[0])
        tmp_daily = fitbit_daily.loc[(fitbit_daily['ParticipantIdentifier'] == ID) & (fitbit_daily['Date2'] >= enrollment_date)].sort_values('Date2')
        row_index = np.where(tmp_daily['HeartRateIntradayMinuteCount'] != 0)[0]
        
    if len(row_index) > 0:
        data2 = tmp_daily.iloc[row_index[0]:row_index[-1] + 1]
    else:
        data2 = pd.DataFrame()
        
    new2_fitbit_daily = pd.concat([new2_fitbit_daily, data2])

    return new2_fitbit_daily

def merge_df_enrolled(df, df_enrolled, col_id, col_refer, new_col_name):
    if new_col_name not in df.columns:
        df.insert(1, new_col_name, None)
    for id in tqdm(df_enrolled[col_id].unique()):
        # print(id)
        enrolled_date = df_enrolled[df_enrolled[col_id]==id][col_refer]
        # print(enrolled_date)
        df.loc[df[col_id]==id, new_col_name] = pd.to_datetime(enrolled_date.values[0]).date()
    return df

def filter_hr_df(df):
    df_not_zero = df[df['HeartRateIntradayCount'].astype('float')>0]
    df_after_erl = df_not_zero[df_not_zero['Date'] >= df_not_zero['EnrollmentDate_fr_erl']]
    return df_after_erl
    
def adjust_enroll_time_form(df_enrolled):
    for i in range(len(df_enrolled)):
        df_enrolled.loc[i, 'EnrollmentDate'] = df_enrolled['EnrollmentDate'][i][:-1]

    return df_enrolled

def show_stat_df_sleep(df_sleep, col_names):
    for col in col_names:
       print(df_sleep[col].astype(float).describe())

def cal_date_hour_mx(count_df):
    # Extract date and hour from interval_start
    count_df['date'] = count_df['interval_start'].dt.date
    count_df['hour'] = count_df['interval_start'].dt.hour

    # Create a pivot table
    # TODO: Do not take the average
    pivot_table = count_df.pivot_table(values='total', index='date', columns='hour', fill_value=0)
    return pivot_table

def cal_subject_missflg(intra_hr):
    intervals = pd.date_range(intra_hr['DateTime'].min(), intra_hr['DateTime'].max(), freq='15min')
    count_df = pd.DataFrame({
        'interval_start': intervals,
        'missflg': True
    })

    count_df = count_df.sort_values(by='interval_start').reset_index(drop=True)
    intra_hr = intra_hr.sort_values(by='DateTime').reset_index(drop=True)

    for j, i in tqdm(enumerate(range(len(count_df)))):
        print(count_df.iloc[i])
        print("test")

        start_time = count_df.loc[j, 'interval_start']
        if i < len(count_df) - 1:
            end_time = count_df.loc[j + 1, 'interval_start']
            if intra_hr[(intra_hr['DateTime'] >= start_time) & (intra_hr['DateTime'] < end_time)].shape[0] > 0:
                count_df.iloc[j, 'missflg'] = False
        else:
            if intra_hr[intra_hr['DateTime'] >= start_time].shape[0] > 0:
                count_df.iloc[i, 'missflg'] = False

    # Assign the counts to the 'total' column in df
    return count_df

def cal_subject_counts(intra_hr):
    intervals = pd.date_range(intra_hr['DateTime'].min(), intra_hr['DateTime'].max(), freq='30min')
    count_df = pd.DataFrame({
        'interval_start': intervals,
        'total': -1
    })

    count_df = count_df.sort_values(by='interval_start').reset_index(drop=True)
    intra_hr = intra_hr.sort_values(by='DateTime').reset_index(drop=True)

    counts = []

    for i in tqdm(range(len(count_df))):
        start_time = count_df.loc[i, 'interval_start']
        if i < len(count_df) - 1:
            end_time = count_df.loc[i + 1, 'interval_start']
            count = intra_hr[(intra_hr['DateTime'] >= start_time) & (intra_hr['DateTime'] < end_time)].shape[0]
        else:
            count = intra_hr[intra_hr['DateTime'] >= start_time].shape[0]
        counts.append(count)

    # Assign the counts to the 'total' column in df
    count_df['total'] = counts
    return count_df

def plot_date_hour_mx(pivot_table):

    # Plotting
    plt.figure(figsize=(12, 8))
    plt.pcolormesh(pivot_table.columns, pivot_table.index, pivot_table.values, cmap='Blues', shading='auto')

    # Set the labels
    plt.xlabel('Hour of Day')
    plt.ylabel('Date')
    plt.title('Pseudocolor Plot of Total by Date and Hour')

    # Format the date on y-axis

    plt.colorbar(label='Number of entries within a hour')

    plt.xticks(np.arange(0, 24, 1))  # Assuming hours are from 0 to 23
    plt.yticks(rotation=45)
    plt.grid(False)
    plt.tight_layout()

    plt.show()   
        

def flag_missing_data(df, column_to_query, study_period=(), interval_size=15):
    """
    Flags missing data for a specific column within a given study period.
    Parameters:
    - df: DataFrame with 'person_id', 'date', and other columns.** date or datetime should be flexible
    - column_to_query: either string or list, the name of the column(s) to check for missing data.
    - study_period: Tuple or list, containing the start and end dates of the study period ('YYYY-MM-DD', 'YYYY-MM-DD').
    - interval_size: Integer, how big of a gap there is between data points in output table in minutes (aggregated
    raw data points into 'bins' of size interval_size)

    Returns:
    - DataFrame with an additional column indicating missing data for the queried column (called 'Missing_Flag')
    """

    # Check to make sure column_to_query is a column in df
    if column_to_query not in df.columns:
        raise Exception("Column to query is not a column in the provided DataFrame")

    # Preprocessing of dataset
    df['datetime'] = pd.to_datetime(df['datetime']) # Convert to datetime objects

    # Determine study_period
    if len(study_period) == 2:
        start_date = pd.to_datetime(study_period[0])
        end_date = pd.to_datetime(study_period[1])

    # If no study period provided, use entire dataset
    else:
        start_date = df['datetime'].min()
        end_date = df['datetime'].max()

    start_date = round_to_nearest_interval(start_date, interval_size)
    end_date = round_to_nearest_interval(end_date, interval_size)

    # Filter to only have study period
    df = df[(df['datetime'] >= start_date) & (df['datetime'] <= end_date)]

    # Group by person_id, resample based on interval_size provided
    df.set_index('datetime', inplace=True)

    # Resample data based on interval_size, take mean heart_rate of all entries in that time interval
    resampled = df.groupby('person_id', as_index=False).resample(f'{interval_size}min').mean().reset_index()
    #resampled['datetime'] = resampled['datetime'].apply(lambda dt: round_to_nearest_interval(dt, interval_size))

    # Make sure there is a row in the DataFrame for every person for every time point
    all_people = df['person_id'].unique()
    all_intervals = pd.date_range(start=start_date, end=end_date, freq=f"{interval_size}min")
    all_df = pd.DataFrame([(person, interval) for person in all_people for interval in all_intervals], columns=['person_id', 'datetime'])

    merged_df = pd.merge(all_df, resampled, on=['person_id', 'datetime'], how='left')

    # Create Missing_Flag column
    merged_df['Missing_Flag'] = merged_df[column_to_query].isna()
    
    return merged_df[['person_id', 'datetime', column_to_query, 'Missing_Flag']]

def round_to_nearest_interval(dt, interval_size):
    """
    Rounds a datetime object to the nearest interval (always rounds down).
    """
    rounded_minute = (dt.minute // interval_size) * interval_size
    return dt.replace(minute=rounded_minute, second=0, microsecond=0)
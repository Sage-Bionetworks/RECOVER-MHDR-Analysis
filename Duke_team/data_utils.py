import matplotlib.pyplot as plt
import pyarrow.parquet as pq
import numpy as np
import pandas as pd
from tqdm import tqdm

def hist_dist(hist_val, x_label, y_label, bins, st=0):
    plt.figure(figsize=(12, 6))
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.hist(hist_val, range=(hist_val.min()+st, hist_val.max()), bins = bins)
    plt.show()

def get_dataset_df(dataset_paths, i, fs):
    dataset = pq.ParquetDataset(dataset_paths[i], filesystem=fs)
    print("get_data:"+str(dataset_paths[i]))
    df = dataset.read().to_pandas()
    print(df.columns)
    return df

def update_df_object_numeric(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = df[column_name].astype('float')
    return df

def update_df_object_str(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = df[column_name].astype('str')
    return df

def update_df_object_date(df, column_name_list):
    for column_name in column_name_list:
        df[column_name] = pd.to_datetime(df[column_name])
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
    
    for ID in ID_daily:
        enrollment_date = pd.to_datetime(enrolledparticipants.loc[enrolledparticipants['ParticipantIdentifier'] == ID, 'EnrollmentDate'].values[0])
        tmp_daily = fitbit_daily.loc[(fitbit_daily['ParticipantIdentifier'] == ID) & (fitbit_daily['Date2'] >= enrollment_date)].sort_values('Date2')
        row_index = np.where(tmp_daily['HeartRateIntradayMinuteCount'] != 0)[0]
        
        if len(row_index) > 0:
            data2 = tmp_daily.iloc[row_index[0]:row_index[-1] + 1]
        else:
            data2 = pd.DataFrame()
        
        new2_fitbit_daily = pd.concat([new2_fitbit_daily, data2])
    
    # Label participants who only used Sense 2 and/or Charge 5 during the study
    ID_devices = fitbit_devices['ParticipantIdentifier'].unique()
    num_of_devices = []  # get the number of devices for each participant
    enrolledparticipants['onlyS2C5'] = 0  # dummy variable: 1 = only used Sense 2 and/or Charge 5
    
    for ID in ID_devices:
        device_info = fitbit_devices.loc[fitbit_devices['ParticipantIdentifier'] == ID, 'Device'].unique()
        num_of_devices.append(len(device_info))
        
        if len(device_info) == np.sum(np.isin(device_info, ['Sense 2', 'Charge 5'])):
            enrolledparticipants.loc[enrolledparticipants['ParticipantIdentifier'] == ID, 'onlyS2C5'] = 1
    
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

    
    
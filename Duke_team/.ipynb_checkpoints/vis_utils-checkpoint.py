import pandas as pd
import numpy as np
from datetime import datetime

def show_stat(df):
    print("The row number:" + str(df.shape[0]))
    print("The number of participant is " + str(len(df['ParticipantIdentifier'].unique())))

def sleep_hour_stat(df_sleep):
    df_sleep['StartDate'].apply(lambda x: datetime.strptime(x, "%Y-%m-%dT%H:%M:%S").hour)
    plt.figure(figsize=(12, 6))
    plt.xlabel("Sleep hour")
    plt.ylabel("Number of observations")
    plt.hist(yy.values, bins=200)
    plt.show()


import pandas as pd
import numpy as np

def show_stat(df):
    print("The row number:" + str(df.shape[0]))
    print("The number of participant is " + str(len(df['ParticipantIdentifier'].unique())))


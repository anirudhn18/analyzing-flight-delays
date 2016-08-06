
import pandas as pd
import numpy as np
import os

#processing weather data

dataloc = '../data/raw/weather/'

msp_weatherfile = os.listdir("{}{}".format(dataloc,"MSP/"))

msp_test = pd.read_csv('{}MSP/MonthlyHistory-aug.html'.format(dataloc))
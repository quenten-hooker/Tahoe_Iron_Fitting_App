from typing import List
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import scipy as scipy
import downrange.downrange as downrange

environment = {
        'env_temp_F':  75, #[deg F]
        'env_rh-percent' : 50, #[% relative humidity]
        'env_pre-psi': 14.7, #[psi]
        'gravity' : 9.8066352, #[m/s^2], value from Petrisor's code
        'wind' : [0,0,0] #X,Y,Z - positive = downwind, __, __
        }

ball_properties={
        'ball_size-inch': 1.6822,
        'ball_mass-g':45.538,
        'ball_moi-ozins2': 0.0011609
        }

demo = downrange.Downrange(
                            environment=environment,
                            ball_properties=ball_properties,
                            #aerosurfacepath=aerosurfacepath
                            )
                            
                            
                            
def defaultdownrange(df: pd.DataFrame,LaunchColumnnames: List[str] = ['Ballspeed','LaunchAngle','Backspin','SideAngle','SideSpin'])->pd.DataFrame:
    """Run the downrange code on each row of the dataframe to make a downrange prediction.

    Args:
        df (pd.DataFrame): dataframe with launch data
        LaunchColumnnames (List[str], optional): list of columns with launch conditions. Defaults to ['Ballspeed','LaunchAngle','Backspin','SideAngle','SideSpin'].

    Returns:
        pd.DataFrame: _description_
    """

    d=downrange.Downrange()
    predictionshots=df.apply(d.downrangefromdf,axis=1,lstdefault=LaunchColumnnames)
    print(predictionshots)

    return predictionshots
  
  
# def testAero(aero_data):
#   
#   #py_aero_predict = r.aero_data
#   py_aero_predict = aero_data
#   test = []
#      
#   # for k, row in py_aero_predict.iterrows():
#   #    results = demo.predictdownrange(row)
#   #    test.append(results)      
#   #    
#   #    trajectory, results = zip(*test)
#   #  
#   #    df = pd.DataFrame(results)
#   #    df2 = pd.DataFrame(trajectory)
#   
#   return py_aero_predict


def aerotest(aero_data): 
    
    data = defaultdownrange(aero_data) 
    #data = data["carrydist", "carrydisp"]
    print(data)
    
    # #py_aero_predict = r.aero_data
    # py_aero_predict = aero_data
    # 
    # test = []
    # for k, row in py_aero_predict.iterrows():
    #   results = demo.predictdownrange(row)
    #   test.append(results)
    # 
    # trajectory, results = zip(*test)
    # 
    # data = pd.DataFrame(results)
    # print(data)
    # data2 = pd.DataFrame(trajectory)

    return data
  
  
def testMethod(): #get number of bins passed by R Shiny server
  string = "I came from a Python Function" 

  return string

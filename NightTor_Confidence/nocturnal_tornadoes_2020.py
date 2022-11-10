#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import matplotlib.pyplot as plt
from pylab import rcParams
import scipy.spatial
get_ipython().run_line_magic('matplotlib', 'inline')
rcParams['figure.figsize'] = (10,8)
import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import cartopy.feature as cfeature
import pandas as pd
from matplotlib.pyplot import figure
from timezonefinder import TimezoneFinder
tf = TimezoneFinder()
import collections
import math
import csv

#Read in Data, available on the GitHub
climo=np.load('/Users/annawanless/Dropbox (Univ. of Oklahoma)/IPPRA/GITrepository/Kenzie/Tor_night/hourlytorarr_two_new.npy')
print(np.shape(climo))

eastern_list=['America/New_York','America/Toronto','America/Nipigon','America/Nassau','America/Havana','America/Detroit',
             'America/Halifax','America/Santo_Domingo','America/Moncton','America/Indiana/Indianapolis','America/St_Johns',
             'America/Port-au-Prince','America/Grand_Turk','America/Iqaluit','Atlantic/Bermuda','America/Glace_Bay',
             'America/Indiana/Vincennes','America/Blanc-Sablon','America/Thunder_Bay','America/Kentucky/Louisville']
eastern_set=set(eastern_list)

central_list=['America/Chicago','America/Monterrey','America/Winnipeg','America/Rainy_River',
             'America/Mexico_City','America/Matamoros','America/Merida','America/Cancun','America/Menominee',
             'America/North_Dakota/New_Salem','America/Atikokan','America/North_Dakota/Center']
central_set=set(central_list)

mountain_list=['America/Denver','America/Mazatlan','America/Phoenix','America/Edmonton','America/Chihuahua',
              'America/Boise','America/Regina','America/Swift_Current','America/Ojinaga']
mountain_set=set(mountain_list)

pacific_list=['America/Los_Angeles','America/Hermosillo','America/Vancouver','America/Tijuana','America/Creston']
pacific_set=set(pacific_list)


# In[ ]:





# In[ ]:





# In[4]:


#get timezone data
fig = plt.figure()
ax_lcc = fig.add_axes([0, 0, 1, 1], projection=ccrs.LambertConformal(central_longitude=-100,central_latitude=39.8, standard_parallels=(39.8,39.8)))
xmin, ymin = ax_lcc.projection.transform_point(-121.04, 20.5,ccrs.Geodetic())
xmax, ymax = ax_lcc.projection.transform_point(-59.80, 48.30,ccrs.Geodetic())
dx = dy = 80*1000.

xaxis = np.arange(xmin, xmax+1, dx).tolist()
yaxis = np.arange(ymin, ymax+1, dy).tolist()
XX, YY = np.meshgrid(xaxis, yaxis)
print(np.shape(XX))

proj = ccrs.LambertConformal(central_longitude=-100,central_latitude=39.8,standard_parallels=(39.8,39.8))
proj_cart = ccrs.PlateCarree() 
f, ax = plt.subplots(subplot_kw=dict(projection=proj))

grid_tz=np.ones((45,66))
grid_tz[:]=np.nan

grid_lats=np.ones((45,66))
grid_lats[:]=np.nan

grid_lons=np.ones((45,66))
grid_lons[:]=np.nan


for i in range(45):
    for j in range(66):
        # convert from Axes coordinates to display coordinates
        p_a_disp = ax.transAxes.transform((XX[i,j],YY[i,j]))

        # convert from display coordinates to data coordinates
        p_a_data = ax.transData.inverted().transform(p_a_disp)

        # convert from data to cartesian coordinates
        p_a_cart = proj_cart.transform_point(*p_a_data, src_crs=proj)
        
        time_name=tf.timezone_at(lng=p_a_cart[0], lat=p_a_cart[1])
        grid_lats[i,j]=p_a_cart[1]
        grid_lons[i,j]=p_a_cart[0]

        if time_name in eastern_set:
            grid_tz[i,j]=1
        elif time_name in central_set:
            grid_tz[i,j]=2
        elif time_name in mountain_set:
            grid_tz[i,j]=3
        elif time_name in pacific_set:
            grid_tz[i,j]=4
        if math.isnan(grid_tz[i,j])==True and time_name!=None:
            print(time_name)
    
print('done')


# In[ ]:





# In[5]:


#proportion of tornadoes that occure between 12a and 4a
night_tor=np.ones((45,66))
night_tor[:]=np.nan
for m in range(45):
    for n in range(66):
        night_tot1=0
        for i in range(366):
            marker=i*24
            nighttor=0
            if grid_tz[m,n]==4:
                #pacific
                day=climo[marker+1:marker+25,m,n]
                nighttor=sum(day[18:23])
            if grid_tz[m,n]==3:
                #mountain
                day=climo[marker:marker+24,m,n]
                nighttor=sum(day[18:23])
            if grid_tz[m,n]==2:
                #central
                day=climo[marker:marker+24,m,n]
                nighttor=sum(day[17:22])
            if grid_tz[m,n]==1:
                #eastern
                if i==0:
                    day=climo[marker:marker+23,m,n] 
                    nighttor=sum(day[16:21])
                else:
                    day=climo[marker-1:marker+23,m,n] 
                    nighttor=sum(day[16:21])
            
            night_tot1=night_tot1+nighttor
            #last line of i loop
        if sum(climo[:,m,n])==0 or night_tot1==0:
            num_append=float('nan')
        else:
            num_append=float(night_tot1)/float(sum(climo[:,m,n]))
        night_tor[m,n]=num_append
        #last lineo of n loop
    #last line of m loop

    
#get rid of Canada/Mexico data
array_map=night_tor
array_map[0:4,:]=float('nan')
array_map[4,0:30]=float('nan')
array_map[5,0:29]=float('nan')
array_map[6,0:28]=float('nan')
array_map[7,0:27]=float('nan')
array_map[8,0:23]=float('nan')
array_map[9,0:22]=float('nan')
array_map[10,0:21]=float('nan')
array_map[11,0:20]=float('nan')
array_map[12,0:15]=float('nan')
array_map[13,0:13]=float('nan')
array_map[14,0:11]=float('nan')
array_map[15,0:9]=float('nan')
array_map[41:45,:]=float('nan')
array_map[34:45,49:52]=float('nan')
array_map[36:45,53]=float('nan')
array_map[37,54:56]=float('nan')
array_map[37:45,43:57]=float('nan')
array_map[38:45,40:43]=float('nan')
array_map[37:45,35:39]=float('nan')
array_map[38,22:33]=float('nan')
array_map[:,62:65]=float('nan')
array_map[40,0:45]=float('nan')
array_map[39,15:45]=float('nan')
array_map[35,45:49]=float('nan')
array_map[39:41,61]=float('nan')
array_map[40:41,60]=float('nan')

#carribean/eastern side of map
array_map[0:15,55:66]=float('nan')
array_map[0:6,50:66]=float('nan')
array_map[:,62:66]=float('nan')

#lake michigan weirdness
array_map[36:,44:49]=float('nan')
    
night_prop_fixed=array_map

print(np.nanmax(night_tor),np.nanmin(night_tor))
print(np.nanmax(array_map),np.nanmin(array_map))


# In[ ]:





# In[41]:


#figure 3 left: number of tornadoes between 12 and 4 am

fig = plt.figure()
ax_lcc = fig.add_axes([0, 0, 1, 1], projection=ccrs.LambertConformal(central_longitude=-100,central_latitude=39.8,
                                                                    standard_parallels=(39.8,39.8)))

ax_lcc.set_extent([-121.04, -73.1, 20.5, 51.3], ccrs.Geodetic())
ax_lcc.coastlines(resolution='10m')
ax_lcc.add_feature(cfeature.STATES.with_scale('10m'))
xmin, ymin = ax_lcc.projection.transform_point(-121.04, 20.5,ccrs.Geodetic())
xmax, ymax = ax_lcc.projection.transform_point(-59.80, 48.30,ccrs.Geodetic())
dx = dy = 80*1000.

xaxis = np.arange(xmin, xmax+1, dx).tolist()
yaxis = np.arange(ymin, ymax+1, dy).tolist()
XX, YY = np.meshgrid(xaxis, yaxis)

col=plt.cm.YlGnBu
v=np.arange(0,0.20,0.02)

zzz=array_map
for x,y,c in zip(XX,YY,zzz):
    plt.scatter(x,y,s=110,c=c,cmap=col,marker='s',vmin=0,vmax=0.18)
cbar=plt.colorbar(ticks=v,fraction=0.066, pad=0.04,orientation='horizontal')
cbar.ax.tick_params(labelsize=16)


plt.show()


# In[ ]:





# In[6]:


#mean number of tornadoes that occur between 12a and 4a

night_tor=np.ones((45,66))
night_tor[:]=np.nan
for m in range(45):
    for n in range(66):
        night_tot1=0
        for i in range(366):
            marker=i*24
            nighttor=0
            if grid_tz[m,n]==4:
                #pacific
                day=climo[marker+1:marker+25,m,n]
                nighttor=sum(day[18:23])
            if grid_tz[m,n]==3:
                #mountain
                day=climo[marker:marker+24,m,n]
                nighttor=sum(day[18:23])
            if grid_tz[m,n]==2:
                #central
                day=climo[marker:marker+24,m,n]
                nighttor=sum(day[17:22])
            if grid_tz[m,n]==1:
                #eastern
                if i==0:
                    day=climo[marker:marker+23,m,n] 
                    nighttor=sum(day[16:21])
                else:
                    day=climo[marker-1:marker+23,m,n] 
                    nighttor=sum(day[16:21])
            
            night_tot1=night_tot1+nighttor
            #last line of i loop
        if sum(climo[:,m,n])==0 or night_tot1==0:
            num_append=float('nan')
        else:
            num_append=float(night_tot1)
        night_tor[m,n]=num_append
        #last lineo of n loop
    #last line of m loop

    
#get rid of Canada/Mexico data
array_map=night_tor
array_map[0:4,:]=float('nan')
array_map[4,0:30]=float('nan')
array_map[5,0:29]=float('nan')
array_map[6,0:28]=float('nan')
array_map[7,0:27]=float('nan')
array_map[8,0:23]=float('nan')
array_map[9,0:22]=float('nan')
array_map[10,0:21]=float('nan')
array_map[11,0:20]=float('nan')
array_map[12,0:15]=float('nan')
array_map[13,0:13]=float('nan')
array_map[14,0:11]=float('nan')
array_map[15,0:9]=float('nan')
array_map[41:45,:]=float('nan')
array_map[34:45,49:52]=float('nan')
array_map[36:45,53]=float('nan')
array_map[37,54:56]=float('nan')
array_map[37:45,43:57]=float('nan')
array_map[38:45,40:43]=float('nan')
array_map[37:45,35:39]=float('nan')
array_map[38,22:33]=float('nan')
array_map[:,62:65]=float('nan')
array_map[40,0:45]=float('nan')
array_map[39,15:45]=float('nan')
array_map[35,45:49]=float('nan')
array_map[39:41,61]=float('nan')
array_map[40:41,60]=float('nan')

#carribean/eastern side of map
array_map[0:15,55:66]=float('nan')
array_map[0:6,50:66]=float('nan')
array_map[:,62:66]=float('nan')

#lake michigan weirdness
array_map[36:,44:49]=float('nan')

night_tor_fixed=array_map
    
print(np.nanmax(night_tor),np.nanmin(night_tor))
print(np.nanmax(array_map),np.nanmin(array_map))


# In[ ]:





# In[45]:


#figure 3 right, proportion of nocturnal tornadoes

fig = plt.figure()
ax_lcc = fig.add_axes([0, 0, 1, 1], projection=ccrs.LambertConformal(central_longitude=-100,central_latitude=39.8,
                                                                    standard_parallels=(39.8,39.8)))

ax_lcc.set_extent([-121.04, -73.1, 20.5, 51.3], ccrs.Geodetic())
ax_lcc.coastlines(resolution='10m')
ax_lcc.add_feature(cfeature.STATES.with_scale('10m'))
xmin, ymin = ax_lcc.projection.transform_point(-121.04, 20.5,ccrs.Geodetic())
xmax, ymax = ax_lcc.projection.transform_point(-59.80, 48.30,ccrs.Geodetic())
dx = dy = 80*1000.

xaxis = np.arange(xmin, xmax+1, dx).tolist()
yaxis = np.arange(ymin, ymax+1, dy).tolist()
XX, YY = np.meshgrid(xaxis, yaxis)

col=plt.cm.YlGnBu
v=np.arange(0,0.20,0.02)

zzz=array_map
for x,y,c in zip(XX,YY,zzz):
    plt.scatter(x,y,s=110,c=c,cmap=col,marker='s',vmin=0,vmax=0.15)
cbar=plt.colorbar(ticks=v,fraction=0.066, pad=0.04,orientation='horizontal')
cbar.ax.tick_params(labelsize=16)



plt.show()


# In[ ]:





# In[ ]:





# In[ ]:





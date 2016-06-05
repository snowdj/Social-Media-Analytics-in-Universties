import numpy as np
import pandas as pd
import matplotlib
import matplotlib.path as path
import matplotlib.pyplot as plt
import matplotlib.patches as patches



# Data to be represented
# ----------
my_university_name = ["The University of Melbourne"]

timesdata = pd.read_csv('input/timesData.csv')

properties = ['teaching', 'international', 'research',
              'citations', 'income']
datarow = timesdata[timesdata.university_name.isin(my_university_name)].tail(1).reset_index(drop=True)
values = datarow[properties].astype(float).as_matrix().flatten()

title_text = '-'.join([my_university_name[0]])

# ----------

# Choose some nice colors
matplotlib.rc('axes', facecolor = 'white')

# Make figure background the same colors as axes
fig = plt.figure(figsize=(10,8), facecolor='white')
fig.suptitle(title_text, fontsize=14, fontweight='bold')

# Use a polar axes
axes = plt.subplot(111, polar=True)

# Set ticks to the number of properties (in radians)
t = np.arange(0,2*np.pi,2*np.pi/len(properties))
plt.xticks(t, [])

# Set yticks from 0 to 100
plt.yticks(np.linspace(0,100,11))

# Draw polygon representing values
points = [(x,y) for x,y in zip(t,values)]
points.append(points[0])
points = np.array(points)
codes = [path.Path.MOVETO,] + \
        [path.Path.LINETO,]*(len(values) -1) + \
        [ path.Path.CLOSEPOLY ]
_path = path.Path(points, codes)
_patch = patches.PathPatch(_path, fill=True, color='blue', linewidth=0, alpha=.1)
axes.add_patch(_patch)
_patch = patches.PathPatch(_path, fill=False, linewidth = 2)
axes.add_patch(_patch)

# Draw circles at value points
plt.scatter(points[:,0],points[:,1], linewidth=2,
            s=50, color='white', edgecolor='black', zorder=10)

# Set axes limits
plt.ylim(0,100)

# Draw ytick labels to make sure they fit properly
for i in range(len(properties)):
    angle_rad = i/float(len(properties))*2*np.pi
    angle_deg = i/float(len(properties))*360
    ha = "right"
    if angle_rad < np.pi/2 or angle_rad > 3*np.pi/2: ha = "left"
    plt.text(angle_rad, 100.75, properties[i], size=14,
             horizontalalignment=ha, verticalalignment="center")

    # A variant on label orientation
    #    plt.text(angle_rad, 11, properties[i], size=14,
    #             rotation=angle_deg-90,
    #             horizontalalignment='center', verticalalignment="center")

# Done
plt.savefig('uq_radar-chart.png', facecolor='white')
plt.show()
  
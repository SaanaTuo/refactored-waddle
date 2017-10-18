import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib as mpl

mpl.rcParams['legend.fontsize']=10

fig=plt.figure()
ax=fig.gca(projection='3d')

file='run/trajectory_for_python2.xyz'
x=np.loadtxt(file,usecols=(1,))
y=np.loadtxt(file,usecols=(2,))
z=np.loadtxt(file,usecols=(3,))

#ax.set_xlim3d(-0.0095,0.0095) #these produced a bit hard to read plots with bigger masses
#ax.set_ylim3d(0.0,0.076)
#ax.set_zlim3d(-0.0095,0.0095)

ax.plot(x,y,z,marker='o',color='r', label='trajectory of a particle')
ax.legend()

plt.savefig("run/trajectory.jpg")
plt.show()

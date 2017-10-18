import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib as mpl

mpl.rcParams['legend.fontsize']=10

fig=plt.figure()
ax=fig.gca(projection='3d')

file='run/trajectory_for_python1.xyz'
x=np.loadtxt(file,usecols=(1,))
y=np.loadtxt(file,usecols=(2,))
z=np.loadtxt(file,usecols=(3,))

ax.plot(x,y,z,marker='o',color='r', label='trajectory of a particle')
ax.legend()

plt.savefig("run/trajectory0.jpg")
plt.show()

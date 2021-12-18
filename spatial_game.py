#!/usr/bin/env python
# coding: utf-8

# In[9]:


import numpy as np
import pandas as pd


# In[4]:


# this code gives us the spatial cooperation gif.

# for as many steps as we take, we make that many n x n grids.
# we play with the rules of the game outlined in nowak and may's "spatial games"

# we have results for each timestep to make the visualization.


# In[2]:


def paf(x,y):
        if x == 0. and y == 0.:
            return 1.
        if x == 0. and y == 1.:
            return 0.
        if x == 1. and y == 1.:
            return 0.
        if x == 1. and y == 0.:
            return 1.9


# In[3]:


def do_timestep_2D(u0):
    
    # this code works until we reach the boundaries. 
    # the boundaries stay cooperators. we could keep 
    # going with cooperators on the edges, but it's 
    # not reeeeeeeally true to the experiment.
    
    (Nx,Ny) = np.shape(u0)
    
    u = np.zeros((Nx,Ny))
    
    pf = np.zeros((Nx,Ny))
    
    uj = [-1,-1, 1,1, 0,0,-1,1]
    ui = [-1, 1,-1,1,-1,1, 0,0]
    
    for i in range(1,Nx-1):
        for j in range(1,Ny-1):
            
            pay = paf(u0[i,j],u0[i,j])
            
            for n,m in zip(ui,uj):
                pay += paf(u0[i,j],u0[i+n,j+m])
            
            pf[i,j] = pay
    
    
    for i in range(1,Nx-1):
        for j in range(1,Ny-1):
            dummy = np.zeros((Nx,Ny))
            
            dummy[i,j] = pf[i,j]
            
            for n,m in zip(ui,uj):
                dummy[i+n,j+m] = pf[i+n,j+m]
                    
            u[i,j] = u0[np.unravel_index(np.argmax(dummy,axis=None), dummy.shape)]
    
    return u


# In[5]:


def diffus_solve_2D(u0, Nsteps):
    
    (Nx, Ny) = np.shape(u0)
    
    u = np.zeros((Nsteps, Nx, Ny))
    
    u[0,:,:] = u0[:,:]
    
    for n in range(Nsteps - 1):
        u[n+1,:,:] = do_timestep_2D(u[n,:,:])
        
    for n in range(1,Nsteps):
        for i in range(Nx):
            for j in range(Ny):
                if u[n-1,i,j] == 0. and u[n,i,j] == 1.:
                    u[n-1,i,j] = 2.
                if u[n-1,i,j] == 1. and u[n,i,j] == 0.:
                    u[n-1,i,j] = 3.
                    
    u = u[0:-1,:,:]
    
    return u


# In[7]:


Nx,Ny = 99,99
Nsteps = 48
u0 = np.zeros((Nx,Ny))

u0[48,48] = 1

u = diffus_solve_2D(u0, Nsteps)


# In[11]:


u = u.reshape(47*99,99)
pd.DataFrame(u).to_csv("~/Projects/darwinism/evoo2.csv")


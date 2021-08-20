### Social Network Analysis: network graphs       

Three networks of more than 20 nodes available in the ``networkdata`` package are used to explore different kinds of networks. The purpose of this simple proeject is to get some practice in describing cohesion, communities, and a hierarchical organization in networks:       
- by calculating basic network properties
- by performing visualizations of networks and degree distributions
- by performing community detection algorithms
- by checking for the hierarchical organization of roles in the network
- using available network data, but generating my networks.             
              
To use network graphs in igraph format available in the R package ``networkdata``. Firstly, install this package by typing in the console: 
``install.packages(‘networkdata’)``      

To inspect the networks available, following commands can be used:          
``data(package = ‘networkdata’)``         
``show_networks()``
                 
Three networks that I chosed are:  **Brunson Corporate Leadership network**, **Radoslaw Email network**, and **Residence Hall Friendship network**. Brunson Corporate Leadership network is undirected and has 44 nodes, while Radoslaw Email and Residence Hall Friendship network are directed with 167 and 217 nodes respectively. Thus, they have some variations in terms of the size and direction for further analysis (see Report.pdf).

from libpysal import weights
import matplotlib.pyplot as plt
import networkx as nx
import geopandas
import numpy as np

# read in example data from geojson. GeoJSON is a file format
# for encoding geographic data based on JSON. It is useful for
# presenting geographic data on the web, and is increasingly
# used as a file format for geographic data.
filepath = "latmap.shp"
polygons = geopandas.read_file(filepath)



# extract the centroids for connecting the regions, which is
# the average of the coordinates that define the polygon's boundary
polygons = polygons.set_crs(epsg=4326)
centroids = np.column_stack((polygons.centroid.x, polygons.centroid.y))

# construct the "Queen" adjacency graph. In geographical applications,
# the "Queen" adjacency graph considers two polygons as connected if
# they share a single point on their boundary. This is an analogue to
# the "Moore" neighborhood nine surrounding cells in a regular grid.
queen = weights.Queen.from_dataframe(polygons)

# Then, we can convert the graph to networkx object using the
# .to_networkx() method.
graph = queen.to_networkx()
# Remove islands so the graph is connected.

print(graph)

# To plot with networkx, we need to merge the nodes back to
# their positions in order to plot in networkx
positions = dict(zip(graph.nodes, centroids))


# plot with a nice basemap
ax = polygons.plot(linewidth=1, edgecolor="grey", facecolor="lightblue")
nx.draw(graph, positions, ax=ax, node_size=5, node_color="r")
plt.show()

# An alternative method to construct graphs from polygons may use
# pygeos. This package is a high-performance interface to the GEOS C
# library, used in computing geographical relationships. These let us
# describe the relationships between "point sets," like polygons whether
# or not a line "crosses" a polygon, or whether two polygons "touch."
# These relationships, called "predicates", are extensive, and are documented
# by the pygeos package.

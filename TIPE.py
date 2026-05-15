from scipy.spatial import Voronoi, voronoi_plot_2d
import numpy as np
import csv
import matplotlib.pyplot as plt
from collections import defaultdict
from geopy.distance import geodesic

coord=[]

with open ('BONNE VERSION.csv', newline = '', encoding='utf-8') as csvfile :
 reader = csv.DictReader(csvfile)
 for row in reader :
    lat = row['Latitude'].strip()
    lon = row['Longitude'].strip()
    if lat and lon:  # ignorer les lignes vides
        coord.append([float(lat), float(lon)])

#Créer le graphe
points = np.array(coord)
vor = Voronoi(points)

#Afficher le graphe
fig, ax = plt.subplots()
voronoi_plot_2d(vor, ax=ax, show_vertices=False, line_colors='blue')
ax.plot(points[:, 0], points[:, 1], 'ro')
plt.show()

#Récupérer les voisins de chaque point (dictionnaire)
voisins = defaultdict(set) #dictionnaire spécial, crée automatiquement les clés si elles ne sont pas déjà présentes
for s1, s2 in vor.ridge_points:
    voisins[s1].add(s2)
    voisins[s2].add(s1)

#Calcul des distances avec les voisins (geodesic : tient compte de la courbure de la Terre)

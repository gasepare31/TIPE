from scipy.spatial import Voronoi, voronoi_plot_2d
import numpy as np
import csv
import matplotlib.pyplot as plt
from collections import defaultdict
from geopy.distance import geodesic

coord=[]

with open ('Monuments.csv', newline = '', encoding='utf-8') as csvfile :
 reader = csv.DictReader(csvfile)
 for row in reader :
    lat = row['Latitude'].strip()
    lon = row['Longitude'].strip()
    if lat and lon:  # ignorer les lignes vides
        coord.append([float(lat), float(lon)])

#Créer le diagramme
points = np.array(coord)
vor = Voronoi(points)

#Afficher le diagramme
fig, ax = plt.subplots()
voronoi_plot_2d(vor, ax=ax, show_vertices=False, line_colors='blue')
ax.plot(points[:, 0], points[:, 1], 'ro')
#plt.show()

#Récupérer les voisins de chaque point (dictionnaire)
voisins = defaultdict(set) #dictionnaire spécial, crée automatiquement les clés si elles ne sont pas déjà présentes
for s1, s2 in vor.ridge_points:
    voisins[s1].add(s2)
    voisins[s2].add(s1)

#Calcul des distances avec les voisins (geodesic : tient compte de la courbure de la Terre)
vois_dist=[]
for i in range(len(points)):
    monument=[]
    for j in voisins[i]:
        coord_i = (points[i][0], points[i][1])
        coord_j = (points[j][0], points[j][1])   
        distance = geodesic(coord_i, coord_j).km
        monument.append((int(j), round(distance,4)))
    vois_dist.append(monument)
    
#Ecrire les informations dans le fichier csv (crée un nouveau fichier)

#on relit le fichier pour récupérer toutes les lignes
with open('Monuments.csv', newline='', encoding='utf-8') as csvfile:
    reader = csv.DictReader(csvfile)
    lignes = [row for row in reader if row['Latitude'].strip() and row['Longitude'].strip()]

#on ajoute la colonne pour chaque ligne
for i in range(len(lignes)):
    ligne = lignes[i]
    ligne['Voisins'] = str(vois_dist[i])

# Écrire le nouveau fichier
with open('Monuments_avec_voisins.csv', 'w', newline='', encoding='utf-8') as csvfile:
    fieldnames = reader.fieldnames + ['Voisins']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(lignes)

print("Fichier écrit avec succès !")


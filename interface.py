import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("Monuments_avec_voisins.csv")

plt.scatter(df['Longitude'], df['Latitude'], c='red', marker='o', alpha=0.6, edgecolors='black')

plt.title('Répartition spatiale des Monuments')
plt.xlabel('Longitude')
plt.ylabel('Latitude')
plt.show()
#plt.grid(True, linestyle='--', alpha=0.5) 

#plt.savefig('carte_monuments.png')
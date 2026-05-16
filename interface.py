import pandas as pd
import matplotlib.pyplot as plt
import ast

df = pd.read_csv("Monuments_avec_voisins.csv")

print (df)

def parse_voisins (valeur) : 
    try : 
        return ast.literal_eval(valeur) #on converti la liste de caractères en une vraie liste
    except : 
        []

df["Voisins_list"] = df["Voisins"].apply(parse_voisins) #on enregistre cette liste dans une nouvelle colonne

for idx, row in df.iterrows() :
    long1 = row['Longitude']
    lat1= row ['Latitude']

    for voisin in row['Voisins_list']:
        voisin_id = voisin[0] #on récupère l'indices des voisins auquel il est relié
        long2 = df.loc[voisin_id, 'Longitude'] #on récupère les coordonnées de ce voisin
        lat2 = df.loc [voisin_id, 'Latitude']

        plt.plot([long1, long2], [lat1, lat2], color='blue', alpha=0.3, linewidth=0.8)


plt.scatter(df['Longitude'], df['Latitude'], c='red', marker='o', alpha=0.6, edgecolors='black')

plt.title('Répartition spatiale des Monuments')
plt.xlabel('Longitude')
plt.ylabel('Latitude')
plt.show()

#plt.grid(True, linestyle='--', alpha=0.5) 

#plt.savefig('carte_monuments.png')
import tkinter as tk
import pandas as pd
import ast
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# ──────────────────────────────────────────────────────────────────────────────
# 1. Préparation des données
# ──────────────────────────────────────────────────────────────────────────────
df = pd.read_csv("Monuments_avec_voisins.csv")

def parse_voisins(val):
    try:
        return ast.literal_eval(val)
    except:
        return []

df['Voisins_list'] = df['Voisins'].apply(parse_voisins)

# ──────────────────────────────────────────────────────────────────────────────
# 2. Fonction de mise à jour du graphique
# ──────────────────────────────────────────────────────────────────────────────
def update_graph(show_edges):
    ax.clear() # Efface le graphique précédent
    
    # Tracer les arêtes si demandé
    if show_edges:
        for idx, row in df.iterrows():
            lon1, lat1 = row['Longitude'], row['Latitude']
            for voisin_tuple in row['Voisins_list']:
                voisin_id = voisin_tuple[0]
                if voisin_id in df.index:
                    lon2 = df.loc[voisin_id, 'Longitude']
                    lat2 = df.loc[voisin_id, 'Latitude']
                    ax.plot([lon1, lon2], [lat1, lat2], color='blue', alpha=0.3, linewidth=0.8, zorder=1)
    
    # Tracer les noeuds (points)
    ax.scatter(df['Longitude'], df['Latitude'], c='red', marker='o', alpha=0.8, edgecolors='black', zorder=2)
    
    # Mise en forme
    ax.set_title('Graphe des Monuments')
    ax.set_xlabel('Longitude')
    ax.set_ylabel('Latitude')
    ax.grid(True, linestyle='--', alpha=0.5)
    
    # Rafraîchir l'affichage Tkinter
    canvas.draw()

# ──────────────────────────────────────────────────────────────────────────────
# 3. Création de l'interface graphique (Tkinter)
# ──────────────────────────────────────────────────────────────────────────────
root = tk.Tk()
root.title("Visualisation TIPE - Graphe")
root.geometry("1000x700")

# Création de deux zones (Frames) : Gauche pour les boutons, Droite pour le graphe
frame_left = tk.Frame(root, width=200, bg="#f0f0f0", padx=10, pady=20)
frame_left.pack(side=tk.LEFT, fill=tk.Y)

frame_right = tk.Frame(root)
frame_right.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

# ──────────────────────────────────────────────────────────────────────────────
# 4. Intégration de Matplotlib dans la zone droite
# ──────────────────────────────────────────────────────────────────────────────
fig = Figure(figsize=(8, 6))
ax = fig.add_subplot(111)

# FigureCanvasTkAgg fait le pont entre Matplotlib et Tkinter
canvas = FigureCanvasTkAgg(fig, master=frame_right)
canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

# ──────────────────────────────────────────────────────────────────────────────
# 5. Ajout des boutons dans la zone gauche
# ──────────────────────────────────────────────────────────────────────────────
btn_points = tk.Button(
    frame_left, 
    text="1. Afficher Points Seuls", 
    command=lambda: update_graph(show_edges=False),
    width=25, height=2
)
btn_points.pack(pady=10)

btn_complet = tk.Button(
    frame_left, 
    text="2. Afficher Points + Voisins", 
    command=lambda: update_graph(show_edges=True),
    width=25, height=2
)
btn_complet.pack(pady=10)

# Affichage initial (complet par défaut)
update_graph(show_edges=True)

# Lancement de la boucle principale de l'interface
root.mainloop()
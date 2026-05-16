import tkinter as tk
from tkinter import ttk, messagebox
import pandas as pd
import ast
import subprocess
import os
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

# Extraction de la liste des monuments
liste_monuments = df.iloc[:, 0].dropna().astype(str).tolist()

# ──────────────────────────────────────────────────────────────────────────────
# 2. Fonction de mise à jour du graphique
# ──────────────────────────────────────────────────────────────────────────────
def update_graph(show_edges, path_indices=None):
    ax.clear() 
    
    # Tracer les arêtes
    if show_edges:
        for idx, row in df.iterrows():
            lon1, lat1 = row['Longitude'], row['Latitude']
            for voisin_tuple in row['Voisins_list']:
                voisin_id = voisin_tuple[0]
                if voisin_id in df.index:
                    lon2 = df.loc[voisin_id, 'Longitude']
                    lat2 = df.loc[voisin_id, 'Latitude']
                    ax.plot([lon1, lon2], [lat1, lat2], color='blue', alpha=0.3, linewidth=0.8, zorder=1)
    
    # Tracer les noeuds (tous les monuments)
    ax.scatter(df['Longitude'], df['Latitude'], c='red', marker='o', alpha=0.8, edgecolors='black', zorder=2)
    
    # Tracer le chemin optimal si calculé par Dijkstra
    if path_indices:
        # Filtrer pour s'assurer que les indices existent dans le DataFrame
        valid_indices = [i for i in path_indices if i in df.index]
        path_lon = [df.loc[i, 'Longitude'] for i in valid_indices]
        path_lat = [df.loc[i, 'Latitude'] for i in valid_indices]
        ax.plot(path_lon, path_lat, color='lime', linewidth=3, marker='o', markersize=6, zorder=3, label="Chemin")
        ax.legend()
    
    ax.set_title('Graphe des Monuments')
    ax.set_xlabel('Longitude')
    ax.set_ylabel('Latitude')
    ax.grid(True, linestyle='--', alpha=0.5)
    
    canvas.draw()

# ──────────────────────────────────────────────────────────────────────────────
# 3. Fonction d'exécution de Dijkstra.ml
# ──────────────────────────────────────────────────────────────────────────────
def executer_dijkstra():
    depart = combo_depart.get()
    arrivee = combo_arrivee.get()
    
    if not depart or not arrivee:
        messagebox.showwarning("Attention", "Veuillez sélectionner un point de départ et un point d'arrivée.")
        return

    try:
        # Appel du programme OCaml en passant les valeurs au terminal via stdin
        process = subprocess.Popen(
            ["ocaml", "Dijkstra.ml"], 
            stdin=subprocess.PIPE, 
            stdout=subprocess.PIPE, 
            text=True
        )
        process.communicate(input=f"{depart}\n{arrivee}\n")
        
        # Vérification et lecture du fichier de sortie
        if not os.path.exists("resultats_dijkstra.txt"):
            messagebox.showerror("Erreur", "Le fichier resultats_dijkstra.txt n'a pas été créé par Dijkstra.ml.")
            return

        with open("resultats_dijkstra.txt", "r", encoding="utf-8") as f:
            resultat = f.read().strip()
            
        if not resultat or resultat == "NULL":
            messagebox.showinfo("Résultat", "Aucun chemin trouvé entre ces deux monuments.")
            lbl_distance.config(text="Distance : Inatteignable")
            update_graph(show_edges=True)
        else:
            # Traitement de la chaine de format attendu "(id id id ), distance"
            # Ajuste ce parsing si le format de sortie de ton OCaml est différent
            partie_chemin, partie_distance = resultat.split("),")
            partie_chemin = partie_chemin.replace("(", "").replace(")", "").strip()
            
            # Extraction des indices et de la distance
            path_indices = [int(x) for x in partie_chemin.split() if x.isdigit()]
            distance = float(partie_distance.strip())
            
            lbl_distance.config(text=f"Distance : {distance:.2f} km")
            update_graph(show_edges=True, path_indices=path_indices)
            
    except Exception as e:
        messagebox.showerror("Erreur d'exécution", f"Erreur lors de l'appel à Dijkstra : {e}")

# ──────────────────────────────────────────────────────────────────────────────
# 4. Création de l'interface graphique (Tkinter)
# ──────────────────────────────────────────────────────────────────────────────
root = tk.Tk()
root.title("Visualisation TIPE - Graphe")
root.geometry("1100x700")

frame_left = tk.Frame(root, width=250, bg="#f0f0f0", padx=10, pady=20)
frame_left.pack(side=tk.LEFT, fill=tk.Y)

frame_right = tk.Frame(root)
frame_right.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

fig = Figure(figsize=(8, 6))
ax = fig.add_subplot(111)

canvas = FigureCanvasTkAgg(fig, master=frame_right)
canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

# ──────────────────────────────────────────────────────────────────────────────
# 5. Contrôles (Menus et Boutons)
# ──────────────────────────────────────────────────────────────────────────────
tk.Label(frame_left, text="Point de départ :", bg="#f0f0f0", font=("Arial", 10, "bold")).pack(pady=(10, 0))
combo_depart = ttk.Combobox(frame_left, values=liste_monuments, state="readonly", width=25)
combo_depart.pack(pady=5)

tk.Label(frame_left, text="Point d'arrivée :", bg="#f0f0f0", font=("Arial", 10, "bold")).pack(pady=(15, 0))
combo_arrivee = ttk.Combobox(frame_left, values=liste_monuments, state="readonly", width=25)
combo_arrivee.pack(pady=5)

# Nouveau bouton pour calculer le chemin (qui appelle OCaml)
btn_calculer = tk.Button(
    frame_left, 
    text="Calculer le chemin", 
    command=executer_dijkstra,
    width=22, height=2, bg="#4CAF50", fg="black", font=("Arial", 10, "bold")
)
btn_calculer.pack(pady=15)

# Label pour afficher la distance finale
lbl_distance = tk.Label(frame_left, text="Distance : --", bg="#f0f0f0", font=("Arial", 11))
lbl_distance.pack(pady=5)

ttk.Separator(frame_left, orient='horizontal').pack(fill='x', pady=15)

btn_points = tk.Button(
    frame_left, 
    text="1. Afficher Points Seuls", 
    command=lambda: update_graph(show_edges=False, path_indices=None),
    width=22, height=1
)
btn_points.pack(pady=5)

btn_complet = tk.Button(
    frame_left, 
    text="2. Afficher Points + Voisins", 
    command=lambda: update_graph(show_edges=True, path_indices=None),
    width=22, height=1
)
btn_complet.pack(pady=5)

update_graph(show_edges=True)

root.mainloop()
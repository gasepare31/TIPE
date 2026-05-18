import tkinter as tk
from tkinter import ttk, messagebox
import pandas as pd
import ast
import subprocess
import shutil
import os
import sys
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# ──────────────────────────────────────────────────────────────────────────────
# 1. Préparation des données
# ──────────────────────────────────────────────────────────────────────────────
print("Exécution de TIPE.py...")
subprocess.run([sys.executable, "TIPE.py"], check=True)

df = pd.read_csv("Monuments_avec_voisins.csv")

def parse_voisins(val):
    try:
        return ast.literal_eval(val)
    except Exception:
        return []

df['Voisins_list'] = df['Voisins'].apply(parse_voisins)

# Extraction de la liste des monuments (première colonne = noms)
liste_monuments = df.iloc[:, 0].dropna().astype(str).tolist()

# ──────────────────────────────────────────────────────────────────────────────
# 2. Fonction de mise à jour du graphique
# ──────────────────────────────────────────────────────────────────────────────
def update_graph(show_edges=True, path_indices=None):
    """Met à jour le graphe matplotlib.
    
    - show_edges   : affiche les arêtes de voisinage (Voronoï)
    - path_indices : liste d'indices du chemin Dijkstra à mettre en évidence
    """
    ax.clear()

    # Tracer les arêtes de voisinage
    if show_edges:
        drawn = set()   # évite de tracer chaque arête deux fois
        for idx, row in df.iterrows():
            lon1, lat1 = row['Longitude'], row['Latitude']
            for voisin_tuple in row['Voisins_list']:
                voisin_id = voisin_tuple[0]
                edge = (min(idx, voisin_id), max(idx, voisin_id))
                if voisin_id in df.index and edge not in drawn:
                    drawn.add(edge)
                    lon2 = df.loc[voisin_id, 'Longitude']
                    lat2 = df.loc[voisin_id, 'Latitude']
                    ax.plot([lon1, lon2], [lat1, lat2],
                            color='blue', alpha=0.3, linewidth=0.8, zorder=1)

    # Tracer tous les nœuds
    ax.scatter(df['Longitude'], df['Latitude'],
               c='red', marker='o', alpha=0.8, edgecolors='black', s=20, zorder=2)

    # Tracer le chemin Dijkstra si disponible
    if path_indices:
        valid_indices = [i for i in path_indices if i in df.index]
        if valid_indices:
            path_lon = [df.loc[i, 'Longitude'] for i in valid_indices]
            path_lat = [df.loc[i, 'Latitude'] for i in valid_indices]
            ax.plot(path_lon, path_lat,
                    color='lime', linewidth=3, marker='o',
                    markersize=8, zorder=3, label="Chemin optimal")

            # Annoter le départ et l'arrivée
            nom_col = df.columns[0]
            ax.annotate(df.loc[valid_indices[0],  nom_col],
                        (path_lon[0],  path_lat[0]),
                        fontsize=7, color='darkgreen',
                        xytext=(4, 4), textcoords='offset points')
            ax.annotate(df.loc[valid_indices[-1], nom_col],
                        (path_lon[-1], path_lat[-1]),
                        fontsize=7, color='darkgreen',
                        xytext=(4, 4), textcoords='offset points')
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
    depart  = combo_depart.get()
    arrivee = combo_arrivee.get()

    if not depart or not arrivee:
        messagebox.showwarning(
            "Attention",
            "Veuillez sélectionner un point de départ et un point d'arrivée.")
        return

    # Désactiver le bouton pendant le calcul pour éviter les double-clics
    btn_calculer.config(state=tk.DISABLED, text="Calcul en cours…")
    root.update_idletasks()

    try:
        # --- Appel OCaml via le shell système ---
        # shell=True hérite du PATH complet du terminal (opam, brew, etc.)
        # ce qui évite les "FileNotFoundError" même quand ocaml n'est pas dans
        # le PATH minimal de subprocess.
        #
        # On essaie deux commandes :
        #   1. ocamlfind  → nécessaire si Dijkstra.ml utilise #require "csv"
        #   2. ocaml seul → si csv est déjà chargé via .ocamlinit ou opam switch

        # ── Étape 1 : compilation ──────────────────────────────────────────────
        # On compile Dijkstra.ml en un exécutable natif avec ocamlfind+ocamlopt.
        # C'est la seule façon fiable de lier des bibliothèques externes (csv).
        # L'exécutable produit s'appelle "dijkstra_exec" (ou "dijkstra_exec.exe").
        ocamlfind_path = shutil.which("ocamlfind")
        if not ocamlfind_path:
            messagebox.showerror(
                "OCaml introuvable",
                "La commande 'ocamlfind' est introuvable.\n\n"
                "Installez-la via :\n"
                "  opam install ocamlfind\n"
                "puis relancez le programme depuis votre terminal opam."
            )
            return

        exec_name = "dijkstra_exec"
        compile_cmd = (
            f'"{ocamlfind_path}" ocamlopt '
            f'-package csv -linkpkg Dijkstra.ml -o {exec_name}'
        )

        comp = subprocess.run(
            compile_cmd, shell=True,
            capture_output=True, text=True, timeout=60
        )
        if comp.returncode != 0:
            messagebox.showerror(
                "Erreur de compilation OCaml",
                f"La compilation de Dijkstra.ml a échoué :\n\n{comp.stderr}"
            )
            return

        # ── Étape 2 : exécution ────────────────────────────────────────────────
        run_cmd = f"./{exec_name}"
        process = subprocess.Popen(
            run_cmd, shell=True,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        stdout, stderr = process.communicate(
            input=f"{depart}\n{arrivee}\n", timeout=60
        )
        if process.returncode != 0:
            messagebox.showerror(
                "Erreur d'exécution OCaml",
                f"L'exécutable Dijkstra a échoué :\n\n{stderr}"
            )
            return

        # --- Lecture du fichier de sortie ---
        if not os.path.exists("resultats_dijkstra.txt"):
            messagebox.showerror(
                "Erreur",
                "Le fichier resultats_dijkstra.txt n'a pas été créé par Dijkstra.ml.")
            return

        with open("resultats_dijkstra.txt", "r", encoding="utf-8") as f:
            resultat = f.read().strip()

        # --- Cas : aucun chemin ---
        if not resultat or resultat == "NULL":
            messagebox.showinfo("Résultat",
                                "Aucun chemin trouvé entre ces deux monuments.")
            lbl_distance.config(text="Distance : Inatteignable")
            lbl_chemin.config(text="Chemin : —")
            update_graph(show_edges=True)
            return

        # --- Parsing du résultat : format "(id id … ), distance" ---
        # maxsplit=1 évite un crash si la chaîne contenait plusieurs "),"
        parties = resultat.split("),", 1)
        if len(parties) != 2:
            raise ValueError(f"Format de résultat inattendu : {resultat!r}")

        partie_chemin, partie_distance = parties
        partie_chemin = partie_chemin.replace("(", "").strip()

        # Extraction robuste des indices (int, pas isdigit pour éviter les faux négatifs)
        path_indices = []
        for token in partie_chemin.split():
            try:
                path_indices.append(int(token))
            except ValueError:
                pass  # token non numérique, ignoré

        distance = float(partie_distance.strip())

        # --- Mise à jour de l'interface ---
        nom_col = df.columns[0]
        noms_chemin = [df.loc[i, nom_col] for i in path_indices if i in df.index]
        lbl_distance.config(text=f"Distance : {distance:.2f} km")
        lbl_chemin.config(text="  →  ".join(noms_chemin))

        update_graph(show_edges=True, path_indices=path_indices)

    except subprocess.TimeoutExpired:
        process.kill()
        messagebox.showerror("Timeout", "Le calcul Dijkstra a dépassé le temps imparti (60 s).")
    except Exception as e:
        messagebox.showerror("Erreur d'exécution",
                             f"Erreur lors de l'appel à Dijkstra :\n{e}")
    finally:
        # Réactiver le bouton quoi qu'il arrive
        btn_calculer.config(state=tk.NORMAL, text="Calculer le chemin")

# ──────────────────────────────────────────────────────────────────────────────
# 4. Création de l'interface graphique (Tkinter)
# ──────────────────────────────────────────────────────────────────────────────
root = tk.Tk()
root.title("Visualisation TIPE - Graphe")
root.geometry("1200x720")

frame_left  = tk.Frame(root, width=270, bg="#f0f0f0", padx=10, pady=20)
frame_left.pack(side=tk.LEFT, fill=tk.Y)
frame_left.pack_propagate(False)   # empêche le frame de rétrécir

frame_right = tk.Frame(root)
frame_right.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

fig    = Figure(figsize=(9, 6))
ax     = fig.add_subplot(111)
canvas = FigureCanvasTkAgg(fig, master=frame_right)
canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

# ──────────────────────────────────────────────────────────────────────────────
# 5. Contrôles (Menus et Boutons)
# ──────────────────────────────────────────────────────────────────────────────
tk.Label(frame_left, text="Point de départ :", bg="#f0f0f0",
         font=("Arial", 10, "bold")).pack(pady=(10, 0))
combo_depart = ttk.Combobox(frame_left, values=liste_monuments,
                            state="readonly", width=27)
combo_depart.pack(pady=5)

tk.Label(frame_left, text="Point d'arrivée :", bg="#f0f0f0",
         font=("Arial", 10, "bold")).pack(pady=(15, 0))
combo_arrivee = ttk.Combobox(frame_left, values=liste_monuments,
                             state="readonly", width=27)
combo_arrivee.pack(pady=5)

btn_calculer = tk.Button(
    frame_left,
    text="Calculer le chemin",
    command=executer_dijkstra,
    width=24, height=2, bg="#4CAF50", fg="black", font=("Arial", 10, "bold")
)
btn_calculer.pack(pady=15)

lbl_distance = tk.Label(frame_left, text="Distance : --",
                        bg="#f0f0f0", font=("Arial", 11))
lbl_distance.pack(pady=(5, 0))

# NOUVEAU : label pour afficher la séquence des monuments du chemin
lbl_chemin = tk.Label(frame_left, text="Chemin : —",
                      bg="#f0f0f0", font=("Arial", 8),
                      wraplength=240, justify=tk.LEFT)
lbl_chemin.pack(pady=(2, 10))

ttk.Separator(frame_left, orient='horizontal').pack(fill='x', pady=15)

btn_points = tk.Button(
    frame_left,
    text="1. Afficher Points Seuls",
    command=lambda: update_graph(show_edges=False, path_indices=None),
    width=24, height=1
)
btn_points.pack(pady=5)

btn_complet = tk.Button(
    frame_left,
    text="2. Afficher Points + Voisins",
    command=lambda: update_graph(show_edges=True, path_indices=None),
    width=24, height=1
)
btn_complet.pack(pady=5)

# Affichage initial
update_graph(show_edges=True)

root.mainloop()
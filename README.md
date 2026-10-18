# TIPE — Recherche de chemin entre monuments

Projet de TIPE consacré à la modélisation d'un réseau de monuments et à la recherche de chemins optimaux.

Le projet combine :
- un prétraitement Python des coordonnées et des voisinages par diagramme de Voronoï ;
- un graphe pondéré dont les arêtes représentent les distances géographiques ;
- une implémentation OCaml de l'algorithme de Dijkstra ;
- une interface Tkinter/Matplotlib pour visualiser le chemin calculé.

## Organisation

- TIPE.py : calcule les voisins et produit Monuments_avec_voisins.csv.
- Dijkstra.ml : calcule un plus court chemin sur le graphe.
- interface.py : lance le prétraitement, appelle Dijkstra et affiche le résultat.
- algo-*.ml et test.ml : expérimentations et variantes.
- Monuments*.csv : données d'entrée et données enrichies.
- Figures et captures : illustrations et résultats visuels.
- resultats_dijkstra.txt : résultat produit par l'exécution.

La structure détaillée et les pistes de rangement sont décrites dans [docs/structure.md](docs/structure.md).

## Prérequis

- Python 3
- OCaml et ocamlfind
- numpy, scipy, matplotlib, pandas et geopy
- le paquet OCaml csv

## Utilisation

Depuis la racine du dépôt :

    python3 TIPE.py
    python3 interface.py

L'interface compile ensuite Dijkstra.ml avec ocamlfind. Les fichiers CSV et le fichier de résultat sont lus ou générés dans le répertoire courant.

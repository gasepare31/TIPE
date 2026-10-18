# Organisation du dépôt

Le dépôt contient actuellement des sources, données, illustrations et artefacts de compilation à la racine. Pour les prochaines évolutions, l'organisation cible recommandée est :

    .
    ├── README.md
    ├── .gitignore
    ├── docs/                 notes et rapport
    ├── src/                  sources OCaml et Python
    ├── data/                 CSV d'entrée et sorties documentées
    ├── assets/               figures et captures utiles
    └── build/                exécutables et fichiers compilés locaux

Les fichiers existants restent à leur emplacement pour préserver les chemins utilisés par l'interface et l'historique. Un déplacement pourra être envisagé séparément après vérification des chemins.

## Nettoyage progressif conseillé

1. Distinguer les données d'entrée des sorties générées.
2. Déplacer les figures dans assets/ après vérification des références.
3. Déplacer les sources dans src/ après adaptation des chemins relatifs.
4. Régénérer les fichiers compilés localement plutôt que les versionner.
## Projet Lemming

# Projet

Projet universitaire de l'UE PAF (Programmation Avancée Fonctionnelle) réalisé en binôme.\
Le jeu développé s'appuie sur le jeu Lemmings sortie en 1991.\
Le but du jeu est de faire atteindre le point d'arrivé à un ou plusieurs personnages appelés Lemmings. Ces personnages peuvent leur voir attribuer un rôle. Par exemple, un Lemming Flotteur va tomber plus doucement et ne subir aucun dégât de chute, un Lemming Creuseur peut creuser sous ses pieds, un Lemming Grimpeur peut grimper sur le mur.\
Plusieurs niveaux sont incorporés de base de dans le jeu, mais il est possible d'importer son propre niveau sous forme de fichier en respectant une certaine structure.

## Compilation

Ce projet requiert la bibliothèque sdl2 (Simple Media Libary, v2.0).

Sous Linux ou MacOS, il suffit d'installer la dépendance associée
(par exemple `libsdl2-dev` sous Debian/Ubuntu).

**Remarque**: SDL2 ne semble pas encore compatible avec la puce M1 des nouveaux MAC.

Sous Windows, c'est un peu plus complexe (comme d'habitude).  Le plus simple est de passer par *msys2* dont une version est installée par *stack*.  Normalement, la commande suivante devrait suffire :

```
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2
```

Dans tous les cas, on utilisera :

```
stack build
```

Pour construire le projet.

et :

```
stack run
```

Pour le lancer...


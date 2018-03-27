# Projet 2 : Fouine (Sébastien Michelland)

Originellement en binôme avec Sébastien Baumert, et puis finalement non.

J'ai traité les parties débutant, intermédiaire et avancé, puis implémenté des
types sommes et du pattern matching (je n'ai pas traité le sucre syntaxique
pour les listes).

## Les parties subtiles

Le parsing des applications de fonction et le problème du moins unaire ("f -3")
est résolu en n'autorisant que les "expressions simples" en paramètre des
fonctions. Moralement, ça marche parce que l'application a une priorité très
élevée.

Pour les fonction récursives, j'aurais voulu les ajouter à leurs propres
clôtures, mais je pense que mon choix d'un Map (au lieu d'une liste
d'association) pour l'environnemment l'empêche (voyez src/eval.ml, eval() dans
le cat Let, pour les détails).

J'ai traité assez bourrinement les références avec une mémoire sous forme de
table de hachage envoyant des adresses sur des valeurs ; il n'y a ni
réutilisation des adresses, ni garbage collector.

Il existe une subtilité de parsing pour les couples, si l'on veut pouvoir
omettre les parenthèses : la règle qui construit la liste des opérandes de ","
doit être « avare ». J'ai volontairement baissé sa priorité en-dessous de celle
de COMMA en reprenant une astuce d'OCaml.

J'ai implémenté les types somme sous la forme d'un map constructeur -> type ;
en l'état, on ne peut pas définir de constructeurs constants (chaque occurrence
doit faire apparaître un argument). On peut les déconstruire par pattern
matching.

Sur le pattern matching, il y a une autre fourberie de %prec que je devrais
pouvoir éviter avec un peu plus de temps. Il supporte toutes les formes de
binding de let-vameur et permet donc de déconstruire aussi bien les types somme
que les tuples.

---

NB : File highlight, erreurs (tester ceux de tests/except à la main)
Relativement approximatif parce qu'ils ne surligne que des expressions
complètes (mais pas inutile, normalement)

La fameuse erreur de make clean qui plante sur le PC de la salle europe : bref.

Les ADTs DOIVENT avoir des paramètres (pas de constructeurs d'arité 0)

Ça manque un peu de fichiers de test...

Le pattern matching sait faire tous les bindings de let-value et marche donc
en particulier avec les tuples...

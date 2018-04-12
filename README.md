# Projet 2 : Fouine (Sébastien Michelland)

Originellement en binôme avec Sébastien Baumert, et puis finalement non.

Pour ce rendu intermédiaire, je n'ai pas beaucoup abatttu le sujet, mais passé
du temps à nettoyer et affiner le code qui existait. Concrètement, j'ai
redistribué les fonctions en modules et écrits les fichiers d'interface. Il y a
plus de modules que nécessaire, mais OCaml panique dès la première apparition
d'une dépendance circulaire, donc pas le choix.

J'ai ajusté mon `menhir.sh` pour passer le parser à m4 avant de le compiler. Ça
a solidement allégé le texte. Aucune raison que ça gwacke à la compilation,
mais par prudence, je le mentionne.

Comme ça ne coûtait pas cher, j'ai rajouté un symbole de début à la grammaire
et implémenté un shell interactif tout simple. Vous pouvez le tester en lançant
`./fouine` sans argument. D'ailleurs, il marche très bien avec `ledit`.

J'ai tout de même implémenté les exceptions. Le style par continuations me
paraîssait plus élégant, donc j'ai modifié eval() pour l'utiliser et implémenté
les exceptions avec. Je n'ai pas de type d'exceptions pour l'instant, donc on
peut jeter tout et n'importe quoi, entier, fonction, liste... (et c'est un peu
de la triche, mais il me suffit de définir un ADT "type exn = E" pour remplir
le contrat du rendu).

Enfin, il y a d'énormes warnings de pattern matching non exhaustif à la
compilation. C'est parce que j'ai fait une implémentation d'Algorithme W sur le
lambda-calcul entre les deux rendus, et que je ne l'ai pas encore étendu sur
l'ensemble des expressions de Fouine pour l'instant (ni intégré au reste).

## Organisation des fichiers

	src/		Les sources
	tests/		Tests unitaires
	doc/		Un peu de doc/notes
	menhir.sh	Un wrapper pour menhir avec options
	test.sh		Un script de tests automatisés
	CHANGELOG.md	Log des ajouts au programme

Le dossier `doc/` contient essentiellement une vue d'ensemble de l'API de
l'interpréteur, plus agréable à utiliser que le paquet de fichiers mli.

## Compilation et utilisation

Pour compiler l'interpréteur, lancez `make`. J'utilise Menhir pour le parser,
ce qui est à la fois plus fun et inévitable (Daniel Hirschkoff y tenait car
Menhir a été écrit par mon futur encadrant de stage).

L'usage typique est avec un fichier, ou l'entrée standard :

	# ./fouine <file>
	# ./fouine -stdin

Les options sont détaillées sur `--help` ; grosso modo, `-stdin` lit sur
l'entrée standard, `-ast` affiche un AST similaire à `-debug`, et `-parse`
arrête l'exécution après le passage du parser.

Je sais que `make clean` plante sur les machines de l'ENS ; c'est une
bizarrerie assez répandue semble-t-il, que je n'explique pas et qui ne se
produit pas chez moi.

## Tests automatisés

Pour lancer tous les tests, utilisez `make test-all`. Il y en a 5 types :

- Les tests de parser (*parsing*) qui testent à la fois l'acceptation et le
  rejet en comparant avec OCaml.
- Les tests généraux (*compare*) qui comparent la sortie de Fouine avec celle
  d'OCaml sur le même fichier.
- Les tests Fouine-only (*zero*) parce que les types somme que j'ai implémentés
  ne typeraient pas dans Ocaml. Ils doivent afficher 0.
- Les tests d'erreurs (*exceptions*) qui vérifient que Fouine n'est pas une
  passoire.
- Les réplications (*replicate*) qui utilisent les mêmes fichiers que les
  généraux, excepté que l'on fait tourner OCaml sur `fouine -debug` au lieu du
  fichier source. (C'est pour vérifier que `debug` produit une sortie fidèle.)

J'ai une soixantaine de fichiers de tests, voulus unitaires, répartis par
thématique. Chaque dossier de `tests/` est affilié à l'un des types ci-dessus ;
le fichier `tests/prelude.ml` sert pour tester avec OCaml.

## Extensions ou choses fun

Le parser garde la trace de chaque expression dans le code source et l'affiche
en cas d'erreur (si la source est un fichier). Un bon exemple est le suivant
(les autres sont moins convaincants parce que l'implémentation est imparfaite).

	# ./fouine tests/except/zerodiv.ml

Le pattern matching sait faire les mêmes bindings que let et peut donc le
remplacer entièrement excepté pour les fonctions récursives.

Le shell interactif fonctionne très bien avec `ledit`.

## Bugs ou problèmes connus

- On ne peut pas capturer les built-ins (prInt, raise) dans des clotûres.
- Il y a un `%prec` probablement évitable pour le pattern matching. Les
  autres... OCaml n'a pas pu les éviter visiblement, donc je m'en contente.
- Le jeu de tests est trop faible puisque j'arrive à le passer.

## Parties subtiles

L'implémentation des exceptions en style par continuations nécessite que eval
soit elle-même par continuations.

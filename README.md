# Projet 2 : Fouine (Sébastien Michelland)

Originellement en binôme avec Sébastien Baumert, et puis finalement non.

Pour le rendu 3, je propose les exceptions et les deux transformations
demandées. Je n'ai pas implémenté le call/cc (qui nécessaite d'utiliser -E) et
les tableaux au profit d'un shell et un poil de typage.

Les exceptions sont implémentées en style par continuations, qui est donc aussi
celui de eval(). On peut jeter n'importe quel objet fouine, et "E" n'est qu'un
constructeur d'ADT utilisé pour ce cas spécifique.

Les transformations sont implémentées dans un module Transform. Malgré toutes
les tentatives pour alléger, ça reste dense. La gestion des builtins a été un
peu difficile, mais c'est plus un "détail d'implémentation". J'ai triché pour
le let-rec en continuations, j'en parle plus bas.

On peut lancer un shell interactif en invoquant `./fouine` sans argument, et il
est tout aussi bien avec `ledit`. Le module Typing contient une implémentation
d'Algorithme W sur le lambda-calcul et n'est pas encore intégré au le reste du
programme.

On peut trouver, dans le fichier `notes/todo`, une description d'un algorithme
pour tester l'exhaustivité des match et les cas inutiles. Je doute d'avoir le
temps de l'implémenter...

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
Menhir a été écrit par mon futur encadrant de stage). L'usage type est:

	# ./fouine		# Shell interactif
	# ./fouine <file>	# Exécution de script
	# ./fouine -stdin	# Lecture sur stdin

Les options sont détaillées sur `--help` ; grosso modo, `-stdin` lit sur
l'entrée standard, `-ast` affiche un AST similaire à `-debug`, et `-parse`
arrête l'exécution après le passage du parser. En plus des options demandées
par le sujet.

Je sais que `make clean` plante sur les machines de l'ENS ; c'est une
bizarrerie assez répandue semble-t-il, que je n'explique pas et qui ne se
produit pas chez moi.

## Tests automatisés

Pour lancer tous les tests, utilisez `make test-all`. Il y en a 6 types :

- Parsing
- Comparaison avec OCaml
- Les fichiers incompatibles avec OCaml doivent renvoyer 0
- Vérification de quelques exceptions
- Fidélité de -debug
- Comparaison des transformations avec le programme original

J'ai environ soixante-dix fichiers de tests, voulus unitaires, répartis par
thématique dans les sous-dossiers de `tests/`. Le fichier `tests/prelude.ml`
sert pour tester avec OCaml.

Ça fait quasiment 300 tests avec toutes les réplications, mais y'a beaucoup de
redondance...

## Point critique : le let-rec par continuations

Mon implémentation du let-rec rend la conception d'une transformation terminale
assez impossible. Pour créer une clôture récursive, on est obligés d'écrire :

	[| let rec f = e in g |] = fun k kE ->
		... let rec f = ... [| e |] ... in ...

Et comme il y a un `in`, ce n'est par nature pas terminal. La transformation
que je rends est comme ça et peut faire exploser la pile sur les récursions
dès 10 niveaux de profondeur quand on utilise -RE et -ER.

En pratique je pense que c'est un problème de langage et d'interprétation
plutôt qu'un problème de fond. On pourrait introduire une sorte de combinateur
de point fixe pour se substituer à let rec, qui rende les clôtures récursives :

	Φ : string -> closure -> closure

(il faut indiquer le nom sous lequel la récursion se produit). On aurait alors
l'implémentation suivante :

	[| let rec f = e in g |] = fun k kE ->
		[| e |] (fun f0 -> let f = Φ f0 in [| g |] k kE) kE

Il faudrait aussi que l'interpréteur ne gueule pas (trop) en voyant que `f` est
manquante dans l'environnement au moment de créer la clôture durant
l'évaluation de `[| e |]`. Pour en avoir brièvement parlé avec Victor Bonne, je
crois qu'il a cette "liberté", et ça lui permet de faire une implémentation
plus standard.

J'ai dû chercher un bon moment et je n'ai pas eu le temps de l'implémenter (ni
de me résoudre à rendre laxiste la partie de l'interpréteur qui crée les
clôtures).

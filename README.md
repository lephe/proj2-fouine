# Projet 2 : Fouine (Sébastien Michelland)

Un peu trop de modules : bon ben "circular build" partout...

---

Originellement en binôme avec Sébastien Baumert, et puis finalement non.

J'ai traité les parties débutant, intermédiaire et avancé, puis implémenté des
types sommes et du pattern matching; enfin j'ai collé du sucre syntaxique pour
les listes par-dessus les types algébriques.

J'ai tenté de coller au plus près à l'énoncé ; s'il y a une différence avec ce
qui est attendu, c'est involontaire.

Pour l'organisation des fichiers:

	src/		Les sources
	tests/		Tests unitaires
	doc/		Un peu de doc/notes
	menhir.sh	Un wrapper pour menhir avec options
	CHANGELOG.md	Log des ajouts au programme

Le dossier `doc/` contient essentiellement une vue d'ensemble de l'API de
l'interpréteur.

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
- Les réplications (*bootstrap*) qui utilisent les mêmes fichiers que les
  généraux, excepté que l'on fait tourner OCaml sur `fouine -debug` au lieu du
  fichier source. (C'est pour vérifier que `debug` produit une sortie fidèle.)

J'ai une soixantaine de fichiers de tests, voulus unitaires, répartis par
thématique. Chaque dossier de `tests/` est affilié à l'un des types ci-dessus ;
le fichier `tests/prelude.ml` sert pour tester avec OCaml.

## Extensions ou choses fun

Le parser garde le trace de la position de chaque expression dans le code
source. En cas d'erreur, si la source est un fichier, l'interpréteur surligne
la partie mise en cause. Un exemple où ça marche bien est le suivant :

	# ./fouine tests/except/zerodiv.ml

Les autres cas des `tests/except` marchent moins bien parce que seules les
expressions complètes sont gardées en mémoire, et pas (par exemple) le pattern
dans "let pattern = expr in expr".

Le pattern matching que j'ai implémenté utilise les mêmes patterns que let et
n'est donc pas limité aux constructeurs. En théorie il peut entièrement
remplacer le let excepté pour les fonctions récursives.

Mon implémentation des listes est un peu survolée (le temps pressait) ; grosso
modo `[]` et `::` ne sont que du sucre syntaxique pour les constructeurs
`Empty` et `Cons` qui sont définis par défaut par Fouine pour chaque programme,
et qui constituent le type `list`.

## Bugs ou problèmes connus

- `Constructor (1, 2)` c'est un constructeur avec un couple, et pas un
  constructeur à deux arguments comme on peut en voir en OCaml en tapant
  `let a = (1, 2) in Constructor a`, qui plante (merci Nicolas Chappe).
- Il y a un `%prec` probablement évitable pour le pattern matching. Les
  autres... OCaml n'a pas pu les éviter visiblement, donc je m'en contente.
- Ça manque de tests sur les ADTs et les listes.
- De toute façon, le jeu de tests est trop faible puisque j'arrive à le passer.

## Parties subtiles

Le parsing des applications de fonction et le problème du moins unaire ("f -3")
est résolu en n'autorisant que les "expressions simples" en paramètre des
fonctions. Moralement, ça marche parce que l'application a une priorité très
élevée, donc tout ce qui n'est pas "élémentaire" ne peut pas être un argument.

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
doit faire apparaître un argument). Le match permet de les déconstruire, comme
tous les bindings de let d'ailleurs. Au fond tout les let-valeur pourraient
être remplacés par des matchs.

Théoriquement le nom du type dans le map peut servir à vérifier que les match
sont homogènes et exhaustifs, mais je ne l'ai pas fait parce qu'il faut faire
une recherche récursive pour laquelle je n'ai pas eu le temps. Dans la même
idée, il reste cette autre fourberie de %prec que je devrais pouvoir éviter
avec un peu plus de travail.

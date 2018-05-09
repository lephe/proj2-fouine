# Projet 2 : Fouine (Sébastien Michelland)

Je n'ai pas implémenté d'option de l'exécutable pour comparer le résultat
renvoyé par la machine à pile au résultat interprété ; le script de test le
fait déjà de façon automatique.

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

Pour lancer tous les tests, utilisez `make test-all`. Il y en a 7 types :

- Parsing
- Comparaison avec OCaml
- Les fichiers incompatibles avec OCaml doivent renvoyer 0
- Vérification de quelques exceptions
- Fidélité de -debug
- Comparaison des transformations avec le programme original
- Comparaison de la machine à pile avec le programme original

J'ai soixante-quinze fichiers de tests, voulus unitaires, répartis par
thématique dans les sous-dossiers de `tests/`. Le fichier `tests/prelude.ml`
sert pour tester avec OCaml.

## Corrections depuis le rendu 3



* `let rec f = fun x -> if x=0 then 1 else f 0 in prInt (f 5)` refusé
   Mon parser prenait ça pour un let-valeur et le classifiait abusivement en
   non-récursif. J'ai relégué le refus du let-valeur récursif à l'évaluateur
   pour autoriser cette construction.

* `let a = 3; () let b = 5;; prInt b` refusé
  La règle du let est [let pattern = seq_expr in seq_expr], mais j'avais oublié
  de changer le [= expr] en [= seq_expr] quand j'ai introduit seq_expr.

* L’interprète ne renvoie pas le bon résultat lorsque l’on lève une exception
  “à l’intérieur d’une exception”.
  L'erreur était significative. Mon implémentation originale des built-ins
  était naïve (value -> value) et je passais le résultat à la continuation
  courante sans la leur montrer. Évidemment, pour `raise` ce n'est pas
  acceptable, mais mes tests avaient le mauvais goût de "marcher" quand même.
  La résolution du bug a aussi expliqué pourquoi l'un des tests (try/nested.ml)
  était lent sous -ER et -RE.

* `try (r := !r+1; raise (E 0)) with | E y -> 12` refusé
  Un autre oubli: j'avais laissé [(expr)] dans le parser au lieu d'autoriser
  [(seq_expr)], donc il rejetait (r := r + 1; raise (E 0)).

## Langage adopté pour la machine SECD

J'ai adopté un langage type assembleur, par envie de faire de l'impératif je
crois. Pour se rapprocher du réalisme, j'ai évité de placer du code dans les
instructions de clôture et je l'ai mis dans le listing à la place. Il a donc
fallu agencer le code de façon à ce que la cohabitation des fonctions et de
leurs sous-fonctions au même niveau se passe bien.

Je n'ai pas implémenté de parser pour le langage SECD ; ce qui suit est les
conventions utilisées par -stackcode. Chaque instruction est accompagnée d'une
description (rapide) de son effet.

  push <value>
    Empile une donnée sur la pile. La valeur peut être arbitrairement
    compliquée, y compris être une liste de 1000 éléments.
  tuple n
    Dépile les n derniers éléments de la pile et forme un tuple en inversant
    l'ordre des valeurs. Empile le résultat.
  ctor "Name"
    Dépile une valeur v et empile Name v.
  ref
    Dépile une valeur v et empile une nouvelle référence initialisée à v.

  let "pattern"
    Dépile une valeur et filtre par le pattern. En cas d'échec, une exception
    est levée. Sinon, empile sur l'environnement autant de bindings qu'il y a
    de variables libres dans le pattern.
  match "pattern"
    En cas de succès, se comporte comme "let" et empile false sur la pile. En
    cas d'échec, aucun binding n'est fait, la valeur filtrée est renvoyée sur
    la pile, et true est empilé.
  endlet "pattern"
    Dépile autant d'éléments de l'environnement qu'il y a de variables libres
    dans le pattern.
  access "name"
    Empile la valeur associée à "name" dans l'environnement.

  close "pattern" <addr>
    Empile une clôture dont l'argument est "pattern" et l'adresse du code
    <addr> sur la pile.
    NB. En pratique <addr> est stocké relativement à la valeur de PC.
  close ("name") "pattern" <addr>
    Comme close, mais la clôture est rendue récursive sous le nom "name".
    NB. En pratique <addr> est stocké relativement à la valeur de PC.
  apply
    Dépile une fonction et un argument, binde l'argument et appelle la
    fonction. Il peut s'agir d'un built-in.

  jump <addr>
    Saute à l'adresse <addr>.
    NB. En pratique <addr> est stocké relativement à la valeur de PC.
  jumpif <addr>
    Dépile un booléen, et saute à l'adresse <addr> si le booléen est "true".
    C'est pour utiliser cette instruction (comme je n'ai pas implémenté les
    opérations logiques) que "match" a cette convention bizarre.
    NB. En pratique <addr> est stocké relativement à la valeur de PC.
  ret
    Quitte la fonction et remonte au stack frame précédent.

  setjmp <addr>
    Sauvegarde l'état de la machine en indiquant que le code à exécuter en cas
    de "longjmp" est à l'addresse <addr>. L'état sauevgardé est empilé sur une
    pile à part.
    NB. En pratique <addr> est stocké relativement à la valeur de PC.
  longjmp
    Dépile le dernier état sauvegardé et saute à l'endroit indiqué en
    restaurant l'état de la machine. La dernière valeur de la pile est
    préservée au titre d'argument du saut.

  bang
    Dépile une référence, la déréférence, et empile le résultat.
  assign
    Dépile une référence, une valeur, et met à jour la référence.

  plus, minus, add, sub, mul, div
    Arithmétique.
  cmp.eq, cmp.ne, cmp.gt, cmp.ge, cmp.lt, cmp.le
    Comparaisons.

## Points intéressants du langage SECD

J'autorise la machine à se casser la figure si le programme d'entrée ne se
termine pas sans exception, notamment si :
- Une valeur passe à travers tous les cas d'un match
- Une exception n'est pas rattrapée

J'ai implémenté les if/else avec des sauts au lieu de procéder comme le
λ-calcul (apparemment la raison historique de la machine SECD), qui m'aurait
obligé à calculer les deux branches avant de choisir. Cela aurait posé des
problèmes de terminaison pour les fonctions récursives.

J'ai implémenté les exceptions à la sauce impérative avec du setjmp et du
longjmp. J'ai sauvegardé les états sur une pile indépendante mais si j'avais
disposé d'un nom réservé (genre "JMPBUF") j'aurais également pu les mettre sur
l'environnement.

Les long jumps étant déjà très puissants, je n'ai plus eu besoin d'utiliser un
built-in pour raise. J'ai donc pris le parti de définir la fonction raise
en assembleur/SECD au début de chaque programme. Le seul built-in restant est
du coup prInt (entrées/sorties tout ça tout ça).

Le plus intéressant reste ce que j'appelle le "linker" dans le code mais qui
consiste juste à attribuer des adresses aux fonctions et à gérer les clôtures.
Je voulais que le code soit "plat" donc éviter de mettre du code dans les
instructions de clôture.

Le "linker" sert à agencer tout cela en mémoire en s'assurant que les
sous-fonctions soient compilées avant d'être référencées et à leur attribuer
des adresses (temporaires dans un premier temps) quitte à éditer les
instructions de clôture après coup.

Le seul résultat vraiment "utile" est que le code généré est position-
independent. L'affichage de -stackcode affiche les adresses absolutes pour la
lisibilité, mais tout est en relatif.

Le fichier `src/machine.ml` contient une description complète du système.

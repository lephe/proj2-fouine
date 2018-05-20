# Projet 2 : Fouine (Sébastien Michelland)

Pour ce rendu 4, je propose une machine SECD/assembly et une inférence de type
polymorphe. Ça m'a pris un moment donc le rendu est un peu rugueux : pas mal de
choses par-ci par-là auraient pu être améliorées avec un peu plus de temps.

En particulier l'inférence de type n'est pas assez avancée pour être mise en
production : il faudrait plus de travail pour parvenir à quelque chose de
fonctionnel, puis robuste. Voyez plus bas.

Je n'ai pas implémenté d'option de l'exécutable pour comparer le résultat
renvoyé par la machine à pile au résultat interprété ; le script de test le
fait déjà de façon automatique.

Je tiens à préciser tout de suite que j'autorise la machine à exploser si
exécuter le code Fouine d'origine résulte en une exception au toplevel.

Pour l'inférence de types, j'ai suivi une présentation d'Algorithme W à base
d'Union-Find qui vient de l'article Wikipédia anglais pour éviter le formalisme
à base de substitutions de Damas et Milner. C'est plus confortable.

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

## Langage choisi pour la machine SECD

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
    En cas de succès, se comporte comme "let" puis empile false sur la pile. En
    cas d'échec, aucun binding n'est fait, la valeur filtrée est remise sur la
    pile, puis true est empilé.
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
    Comme "close", mais la clôture est rendue récursive sous le nom "name".
    NB. En pratique <addr> est stocké relativement à la valeur de PC.
  apply
    Dépile une fonction et un argument, binde l'argument et appelle la
    fonction. La fonction peut être soit une clôture, soit un built-in.

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
disposé d'un nom réservé (genre "JMP_BUF") j'aurais également pu les mettre sur
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
independent. L'affichage de -stackcode affiche les adresses absolues pour la
lisibilité, mais tout est en relatif.

Le fichier `src/machine.ml` contient une description complète du système.

## Inférence de types incomplète

Je cite, en vrac, les différentes choses qui font que le système n'est pas
assez mûr. J'ai traité le polymorphisme au rendu 3 pour le dégager du rendu 4,
mais malheureusement les plannings étaient trop serrés quand même.

- Des bugs sur les tests (essayez `make test-typing`)
- L'inférence pour la récurrence n'est pas assez rodée (faite rapidement)
- Les ADTs sont trop pauvres (entre autres il n'y a pas d'ADTs paramétrés)
- Les listes built-in ne marchent donc que pour les entiers
- `raise` peut jeter n'importe quoi, pas forcément des `exn`
- ...

Par conséquent, l'inférence de type est activée par défaut en interactif, mais
désactivée par défaut le reste du temps. Utilisez `-typing` et `-no-typing`
pour contrôler ce paramètre.

## Cas intéressants de typage

Il y a quand même des trucs intéressants à sortir du polymorphisme. J'ai réussi
à comprendre en profondeur la généralisation des variables de type et à trouver
(expérimentalement) des règles pour décider quand généraliser. J'arrive à des
choses assez réalistes :

  [1]: let first x y = x;;
  val first : ∀ a b. (a -> (b -> a)) = <closure>
  [2]: let g = first 1;;
  val g : (d -> int) = <closure>
  [3]: g true;;
  - : int = 1
  [4]: g;;
  - : (bool -> int) = <closure>
  [5]: fun y -> first 1 y;;
  - : ∀ i. (i -> int) = <closure>

Bon, c'est classique, mais c'est satisfaisant.

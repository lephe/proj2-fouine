# Projet 2 : Fouine

Ce changelog recense en détail les différentes modifications faites à chaque
commit. Le fichier se lit de haut en bas, et le numéro de commit est indiqué
après chaque série de modifications (ajouté a posteriori une fois le commit
envoyé).

## Rendu 2

* Créé un système de compilation manuel à base d'un Makefile et de Menhir...
  modifié a posteriori pour `ocamlbuild` à cause de l'ordre des dépendances du
  linker. Pour passer des options à Menhir (sans utiliser `myocamlbuild.ml`)
  je triche en configurant bin/menhir.sh à la place de Menhir et en ajoutant
  les options à la volée.

* Créé le module *Expr* avec quelques expressions de base. Une variante du type
  permet d'annoter l'arbre des expressions avec des étiquettes. Je pense que ça
  s'avèrera utile, par exemple pour mettre des numéros de ligne/colonne sur les
  expressions après parsing.

* Créé un module *Util* avec des fonctions pour gérer les ligne/colonnes et les
  afficher de façon sympathique dans le terminal. On y mettra probablement
  d'autres choses par la suite.

* Commencé le lexer/parser, qui reconnaît pour l'instant les expressions
  arithmétiques. Implémenter dans la grammaire la contrainte de ne pouvoir
  utiliser des expressions booléennes que dans les if/else m'attristant un peu,
  je vais tenter d'analyser a posteriori plutôt.

*[master d819a05]*
	First commit: build system, annotated expressions, source file ranges,
	begin lexing/parsing.

---

* Avancé significativement le parser, et ajouté quelques bouts pour typer les
  expressions ; cela permettrait d'appliquer la politique des expressions
  "booléennes" de fouine sans twister la grammaire outre mesure.

* Fait quelques expériences sur le type `expr` suite à une suggestion
  d'utiliser un GADT. Un proof-of-concept de typeur automatique en est sorti,
  mais ne s'étendait pas aux fonctions. Placé dans le dossier `notes/`.

* Ajouté le support dans le parser de tout ce qui est nécessaire aux rendus
  débutant et intermédiaire, plus la détection de certains opérateurs définis
  par l'utilisateur (`1--2` étant différent de `1- -2`), et des commentaires
  récursifs dans le style Caml.

* Créé un évaluateur dans le module *Eval*. Tout se passe ici ou presque.

* Créé un script de test `test.sh` géré par `make test` pour automatiser deux
  types de tests: le passage du parser et la sortie du programme. Dans les deux
  cas, le script compare le comportement de l'interpréteur avec celui d'Ocaml.
  Une batterie de tests se construit au fur et à mesure...

* Changé le type des annotations de `expr` en un record plus flexible et
  syntaxiquement plus parlant. Implémenté une fonction qui affiche une source
  reconstituée, `expr_source`.

* Ajouté à main.ml de quoi traiter proprement les options de ligne de commande,
  plus `-ast` (affichage de l'AST) et `-parse` (ne pas interpréter).

* Créé un module *Exceptions* pour définir les exceptions du langage fouine.
  La fonction principale pourra afficher des messages d'erreur appropriés en
  s'appuyant sur les méta-données des exceptions.

* Centralisé la définition des types dans *Types* pour mieux pouvoir séparer
  les fonctions ensuite (sinon les fonctions associées à des types mutuellement
  récursifs doivent être dans le même fichier).

* Ajouté à l'interpréteur le support de `let` via le module *Pattern*. Ajouté
  la construction des clôtures et les appels de fonctions (non récursives).

*[master 659955c]*
	Substantial parsing, automated tests, record-based expressions, let
	bindings and function calls.

---

* Implémenté les fonctions récursives, malgré l'impossibilité de les ajouter à
  leur clôture. Un booléen dans le constructeur Closure s'en charge. Ajouté des
  tests pour ces fonctions.

* Ajouté au parser de quoi traiter les aspects impératifs du langage: `!`, `;`,
  `:=`, `ref`. Le type unit était déjà présent donc pas de souci. `ref` est un
  mot clé, pas une fonction, mais pourrait être un builtin (prInt aussi).

* Imlémenté un module *Memory* pour la gestion de la mémoire impérative, en
  utilisant innocement une table de hachage sur des entiers. Implémenté `ref`,
  `!` et `:=`. `;` est traduit comme un `let _ = .. in ..` pour la simplicité.

* Ajouté d'autres tests de fonctions récursives, des tests pour les
  fonctionnalités impératives, et des tests pour vérifier que les erreurs sont
  bien détectées.

* Réimplementé `range_highlight` pour afficher la source faultive en cas
  d'erreur (seulement si le script vient d'un fichier). On pourrait améliorer
  en stockant toute la source dans un objet Bytes avant de lexer, mais on
  perdrait l'aspect online.

* Implémenté le parsing des couples avec un trick de priorité, et leur gestion
  en général. Ajouté des tests sur le sujet.

* Ajouté la déclaration des types algébriques et leur utilisation dans les
  patterns de let. Pas de match pour l'instant.

*[master 1a3c270]*
	Recursive functions, imperative features, more tests, better error
	reports, tuples, basic ADTs.

*[master d1637d6]*
	Making the program build under Ocaml 4.02.3.

---

* Implémenté le pattern matching avec match; tous les constructeurs doivent
  posséder un argument (il n'y a pas de constants), pour des raisons de parsing
  (et de temps... surtout).

* Configuré une option `-bootstrap` au script de test, qui réexécute tous les
  tests de comparison Fouine/OCaml en passant à OCaml la sortie produite par
  Fouine avec l'option `-debug` au lieu du script d'origine. Placé sous la
  cible `make test-all`.

* Peaufiné les messages d'erreur dans *Main* ; le surlignage des erreurs est un
  peu grossier, mais ça fera l'affaire.

*[master 9166661]*
	Pattern matching, bootstrap tests, more error reports.

---

* Ajouté un message d'aide sous `./fouine --help`. C'est plus accueillant.

* Ajouté sur une impulsion du sucre syntaxique pour les listes, en les ramenant
  à un ADT `list = Empty | Const`. Implémenté de quoi les déconstruire joliment
  dans les match.

*[master a3f322f]*
	Rendu 2 is drawing *very* near.

## Rendu 3

* Séparé `let` en plusieurs parties. Syntaxiquement, on distingue let-variable
  et let-fonction. Une fois le parser passé, on ne distique pus que let-valeur
  (qui inclut les fonctions non récursives) et let-fonction-récursive.

* Fait du parser un fichier m4 et travaillé à la lisibilité. Commencé à
  préfixer les constructeurs pour éviter d'inventer des synonymes à tous bouts
  de champ (pattern.Identifier, expr.Name -> pattern.P_Name, expr.E_Name).

* Implémenté une variante de l'Algorithme W de Damas et Milner dans
  `src/typing.ml`. Il supporte pour l'instant les opérateurs du λ-calcul et
  imite OCaml pour la généralisation des variables de type (afin d'éviter les
  paradoxes connus comme la référence polymorphe).

  La version originale utilise des substitutions, mais ici j'exploite un
  Union-Find pour grouper les variables de types qui représentent des monotypes
  identiques. L'algorithme est sensiblement linéaire, mais je ne fais pas
  encore les "occurs check", donc les types récursifs arbitraires sont
  autorisés.

* Commencé un shell interactif à l'aide des modules *Repl* et *Toplevel*. Un
  programme n'est désormais plus une expression mais une suite de déclarations.
  Le shell interactif affiche le résultat de leur exécution (c'est plus naturel
  pour les définitions de types). Le shell démarre en mode interactif si aucun
  nom de fichier n'est indiqué et que `-stdin` n'est pas utilisé.

  Le shell utilise ";;", ce qui est essentiel pour résoudre les end-of-stream
  conflicts relevés par Menhir. Sans ";;", il faudrait lire un token de plus
  pour décider que la commande est finie, ce qui est impossible sur stdin.
  D'ailleurs, le shell fonctionne bien avec ledit.

*[master 0c57472]*
	Cleaner parser, Hindley-Milner type inference stub, simple shell.

---

* Réorganisé un certain désordre dans les sources ; finalement rédigé les
  fichiers d'interfaces (.mli) des modules. Avec OCaml qui a une peur panique
  des dépendances circulaires, c'était pas gagné d'avance.

* Documenté la nouvelle API dans doc/api. C'est une vue d'ensemble plus
  pratique que les fichiers d'interface. Dans les grandes lignes :
  - *Eval* disparaît au profit de *Value* et *Expr*
  - *Util* est renommé en *Range*
  - *Toplevel* est renommé en *Interpreter*
  - *Errors* est créé pour contenir errors_try que je ne pouvais pas mettre
    dans *Exceptions* à cause de dépendances circulaires.

*[master 18f8987]*
	Reorganized all of the project's sources. Hopefully the last time I do
	this.

---

* Supprimé l'artifice de récursion V_Rec.

* Ajouté la gestion des erreurs dans le shell interactif ; les exceptions sont
  maintenant rattrapées et reportées. Un travail reste à faire sur les noms des
  sources et le surlignage des zones d'erreur.

* Mis en place un affichage spécial pour les listes, "a :: b :: []". Il
  faudrait utiliser la vraie syntaxe "[a; b]" mais ça ne peut être consistent
  que si l'arbre des valeurs enforce un usage typable des constructeurs de
  listes (pour éviter "1 :: 2" par exemple), et pour l'instant c'est pas prêt.

* Implémenté des exceptions du genre try/catch. Comme il n'y a pas (encore) de
  typage et que j'ai principalement repiqué du match, on peut pour l'instant
  jeter n'importe quoi comme, même 2 ou [ (); fun x -> "a" ], ce qui est je
  trouve assez fun.

*[master 4ba1e23]*
	Cleaned impurities; added continuation-style exception handling.

---

* Implémenté un système de built-ins via V_Builtin. Ce sont des fonctions
  écrites en OCaml et chargées par défaut par le module *Interpreter* au début
  de l'exécution.

  Réécrit prInt et raise (plus subtil !) en built-ins. Tout cela pour préparer
  alloc, read et write que je ne veux ajouter à l'environnement que si on a
  effectué la transformation qui élimine les exceptions.

* Étendu les capacités de récursion en autorisant `let rec` à recevoir toute
  expression qui s'évalue en une clôture et pas seulement les expressions qui
  sont syntaxiquement des fonctions.

* Implémenté les transformations : impérative et par continuations. Il y a des
  builtins spécifiques qui s'activent sur -R : "read", "write" et "alloc". Tout
  cela se passe dans le module *Transform*; les builtins sont dans
  *Interpreter*.

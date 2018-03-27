# Projet 2 : Fouine

Ce changelog recense en détail les différentes modifications faites à chaque
commit. Le fichier se lit de haut en bas, et le numéro de commit est indiqué
après chaque série de modifications (ajouté a posteriori une fois le commit
envoyé).

---

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


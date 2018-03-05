# Projet 2 : Fouine

Ce changelog recense en détail les différentes modifications faites à chaque
commit. Le fichier se lit de haut en bas, et le numéro de commit est indiqué
après chaque série de modifications (ajouté a posteriori une fois le commit
envoyé).

---

* Créé un système de compilation manuel à base d'un Makefile et de Menhir. Le
  sujet n'impose pas d'utiliser `ocamlbuild`, c'est donc l'occasion de tester.
  Le seul inconvénient actuel est qu'il faut entrer manuellement l'ordre de
  linkage. Si ça gêne vraiment, je reviendrai sur `ocamlbuild`.

* Créé le module *Expr* avec quelques expressions de base. Ce type possède un
  paramètre qui représente des étiquettes pour annoter l'arbre. Je pense que ça
  s'avèrera utile, par exemple pour mettre des numéros de ligne/colonne sur les
  expressions après parsing.

* Créé un module *Util* avec des fonctions pour gérer les ligne/colonnes et les
  afficher de façon sympathique dans le terminal. On y mettra probablement
  d'autres choses par la suite.

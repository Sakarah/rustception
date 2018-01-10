# Rustception
Un compilateur d'un fragment de Rust écrit en Rust

## Spécifications
Les spécifications du fragment de Rust sont aux liens suivants:
* [Sujet](https://www.lri.fr/~filliatr/ens/compil/projet/sujet-v2.pdf)
* [Typage des ressources] (https://www.lri.fr/~filliatr/ens/compil/projet/bc-v1.pdf)

## Utilisation
Compilez avec `cargo build --release`. Vous obtenez un exécutable `prustc` avec
les options suivantes:
* `--parse-only` : exécute uniquement le parsing
* `--type-only` : parse et type
* `--no-asm` : parse, type et effectue le borrow-checking
Il faut ensuite renseigner un fichier d'entrée. Si aucune option n'a été 
renseignée, alors `prust` écrit un fichier avec l'extension `.s`.
Pour assembler, taper la commande suivante:
`as ($filename).s -o ($filename).o && ld ($filename.o) -o ($excutable)`

## Description du compilateur

### Symbol
Ce module permet de gérer le sytockage des chaînes de caractères (identifiant 
de variable, de structures, de fonctions) en leur associant un entier unique.
Cela permet d'éviter de recopier des chaînes de caractères au long des 
différentes passes du compilateur, et de comparer rapidement deux chaînes.
Debug et Display ont été implémenté pour Symbol : en afficher un affichera la 
chaîne correspondante.

### Lexer
Ce module effectue le Lexing d'un fichier, transformant l'entrée en tokens. Pour
cela, un automate minimaliste a été écrit à la main. Les deux fonctions 
utilitaires sont les suivantes : 
* `pub fn from_channel(channel: R, filename: String) -> Lexer<R> : construit un
lexer depuis un canal (fichier en l'occurrence, mais cela pourrait aussi être 
l'entrée standard) possédant le trait Read.
* La fonction `next` du trait Iterator, qui renvoie un token et sa localisation 
dans le fichier.

### Parser
Le parser a été généré grâce au module externe lalrpop documenté au lien 
suivant: [nikomatsakis/larlpop](https://github.com/nikomatsakis/lalrpop). Il
renvoie le programme sous forme d'arbre de syntaxe suivant le module `ast`.

### Typeur
#### Conventions utilisées
Les types `int`, `bool`, `void` sont représentés sur 64 bits (8 octets) sur la 
pile. Un booléen vaut 0 si `false` et 1 si `true`. Une variable `void` vaut 
toujours 0.

Un tableau est représenté par 128 bits (16 octets) sur la pile, à savoir un  
pointeur vers le tas, et la taille totale du tableau (la taille des éléments du
tableau étant déterminée statiquement, il n'est pas nécessaire de la retenir). 

Une structure est representée sur la pile, sans garantie sur la position des 
attributs à l'intérieur. 

À la fin du calcul d'une expression le résultat est renseigné :
* dans `%rax` si c'est un type `int`, `bool` ou `void`;
* dans `%rax` et `%rdx` dans le cas d'un `vector`;
* à la position indiquée par `%rbx` dans le cas d'une `struct` (on respecte 
le fait que `%rbx` est "callee saved".


#### Allocation des ressources
#### Ecriture des instructions



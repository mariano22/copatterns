# Copatterns: Programming Infinite Structures by Observation

This is small, experimental implementation of the language described on http://www.cse.chalmers.se/~abela/popl13.pdf (Andreas Abel, Brigitte Pientka David Thibodeau, Anton Setzer).

**We extended mentioned work proposing a polynomial algorithm for inductive and coinductive types (pattern and copattern matching).**

Further information about syntaxis, modules could be found in "Copatterns.pdf" (in spanish).

# Installing
- Download hte interpreter
```sh
git clone https://github.com/mariano22/copatterns.git
```
- Open the repository folder
```sh
cd copatterns
```
- Install it and dependencies using cabal
```sh
cabal install
```

Cabal will install it on `./cabal` so you must add:
```sh
export PATH="$HOME/.cabal/bin:$PATH"
```

And then executes it with:
```sh
copatterns
```

Once opened you will see the interpreter console:
```sh
CP>
```

# Usage
The available commands are:

| Command | Description |
| ------ | ------ |
| `:load <file>`  | Loads a program from file. |
| `:reload`  | Reloads the last file loaded. |
| `:help`   | Shows the help. |
| `:quit`  | Exits the interpreter. |
| `:clear`  | Clears all the loaded definitions. |
| `:show <symbol>`  | Prints a definition of a given symbol  |
| `:show`  | Prints all symbol definitions. |
| `<exp> `  | Evaluates an expression. |
| `:type <term>`  | Tries to infer the type of a term. |
| `:bruteval <exp>`  | Evaluates an expression as much as possible (until no redex available). |

Remember that the expressions of our language must always include a type: an expression is `<term> : <Type>` by the defined syntax. This is because in our system we can not infer types of certain terms (i.e. records fields names shared by two or more coinductive types).

When you load a file, the previous file loaded is discarted.

# Markdown extension
We have created a markdown extension so gedit (or other editors) markdown the code of the language on our .lcp files (.lcp is our used copattern file extension). You can copy on gedit markdown folder with:
```sh
sudo cp lcp.lang /usr/share/gtksourceview-3.0/language-specs/
```

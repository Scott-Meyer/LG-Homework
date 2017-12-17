# LG Project 4

## Notes
1.    I ran into a LOT of problems with coding this. Mainly because this grammar was made with globals and arrays in mind, and I uses non-mutable and non-globals. There is a LOT of copying of massive structs with one thing changed, passing around massive state objects. It got bad, then I had to implement a global to manually do the search tree because I was having some problems and that duality just made it worse.
2.  Because of the manual coded tree, I left FROM/TO/Who are empty. However I coded it to print graphs so you can look above/below to see what they would be.

# Enviroment Preperation
1. First download and install [racket](https://racket-lang.org/ "Racket Lang")
2. Download entire "project 2" directory
3. open **project3.rkt** with DrRacket gui
4. click run in the top right of DrRacket gui
5. Enter functions into the REPL (interperter) at the bottom of the window. Find function examples further down in this readme

# Running Code
## Single Zone
This one is kinda simple
just do
```racket
(Grs1 0)
```
Basically Grammer of Reduced Searches, step 1, with i=0.

you can see in the code that I bascially copied the grammar as closly as possible.

output should look something like this:
![Example](https://i.imgur.com/6DqaVvR.png)
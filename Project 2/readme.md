# Readme

# IMPORTANT
I havn't done non-chess peices yet, so there are no built in non-chess peices and you can't do custom ones.

Also, I havn't created a re-distributable exe with a menu/options. 

You have to run the code manually. Sorry, there are instructions below.

TODO: Optimize anything

# Enviroment Preperation
1. First download and install [racket](https://racket-lang.org/ "Racket Lang")
2. Download entire "project 2" directory
3. open project2.rkt with DrRacket gui
4. click run in the top right of DrRacket gui
5. Enter functions into the REPL (interperter) at the bottom of the window. Find function examples further down in this readme

# Running code

## Graphs
You have to run the "graph" function

**(graph [moves] [piece] [start] [end] [dementions] [[blocked]])**

    [moves] = 1, 2, 3, ...
    [piece] = Queen, Rook, Knight, King, Pawn, Bishop (no others or custom yet)
    [start] = '(x y)
    [end]   = '(x y)
    [dementions] = '(width height)
    [[blocked]] = This field is an optional list of blocked spaces '((x y) (x y))

```scheme
(graph 7 King '(1 5) '(8 5) '(8 8))

(graph 8 King '(1 5) '(8 5) '(8 8) '((2 4)(2 5)(2 6)))
```
![(graph 7 King '(1 5) '(8 5) '(8 8))](https://i.imgur.com/PNmZTW4.png)    ![(graph 8 King '(1 5) '(8 5) '(8 8) '((2 4)(2 5)(2 6)))](https://i.imgur.com/5H21vhZ.png)


```scheme
(graph 3 Queen '(1 1) '(1 8) '(8 8))
```
![Queen](https://i.imgur.com/8TGcFVA.png)

## Trajectories
You have to run either the horizon or admissible functions


**(admissible [k] [piece] [start] [end] [dementions] [[blocked]])**
    
    [k] = 1, 2, 3, ...
    [piece] = Queen, Rook, Knight, King, Pawn, Bishop (no others or custom yet)
    [start] = '(x y)
    [end]   = '(x y)
    [dementions] = '(width height)
    [[blocked]] = This field is an optional list of blocked spaces '((x y) (x y))

```scheme
>(admissible 1 Queen '(1 1) '(1 8) '(8 8))
'(((1 1) (1 8)))

>(admissible 1 Queen '(1 1) '(1 8) '(8 8) '((1 4)))
'(((1 1) (8 1) (1 8)) 
  ((1 1) (8 8) (1 8)))

>(admissible 2 Queen '(1 1) '(1 8) '(8 8))
'(((1 1) (8 8) (1 8))
  ((1 1) (1 7) (1 8))
  ((1 1) (1 6) (1 8))
  ((1 1) (1 5) (1 8)) 
  ((1 1) (1 4) (1 8)) 
  ((1 1) (1 3) (1 8)) 
  ((1 1) (1 2) (1 8)) 
  ((1 1) (8 1) (1 8)))
```


**(horizon [k] [piece] [start] [end] [dementions] [[blocked]])**
    
    [k] = 1, 2, 3, ...
    [piece] = Queen, Rook, Knight, King, Pawn, Bishop (no others or custom yet)
    [start] = '(x y)
    [end]   = '(x y)
    [dementions] = '(width height)
    [[blocked]] = This field is an optional list of blocked spaces '((x y) (x y))

```scheme
>(horizon 2 Queen '(1 1) '(1 8) '(8 8))
'((((1 1) (1 8))) 
  (((1 1) (8 8) (1 8)) 
   ((1 1) (1 7) (1 8)) 
   ((1 1) (1 6) (1 8)) 
   ((1 1) (1 5) (1 8)) 
   ((1 1) (1 4) (1 8)) 
   ((1 1) (1 3) (1 8)) 
   ((1 1) (1 2) (1 8)) 
   ((1 1) (8 1) (1 8))))
```

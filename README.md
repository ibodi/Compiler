
## Compiler of simple functional programming language 

The only file in this project that was written by me is `BohdanShcherbakCompiler.hs`. All the other files are written by employees of Institute of Computer Science of University of Wroclaw. `BohdanShcherbakCompiler.hs` is the solution to the task defined in jako `Tasks/prac456_extra.pdf` (in Polish). The file `BohdanShcherbakCompiler.hs` contains a compiler of a simple programming language which is described in files `Tasks/prac4.pdf`, `Tasks/prac5.pdf` and `Tasks/prac6.pdf`. Examples of programs written in this language are in `Examples` catalog.

The author of the file BohdanShcherbak.hs is Konrad Werbliński. 

A program written in this language consists of list of functions and a line:
```
input [list of variables] in [expression that may contain those variables and functions]
```
Example program:
```
fun plus(xy : int * int) : int = fst xy + snd xy 
input x y in plus(x, y)
```
In order to compile and run an example program `Examples/example.pp6` you have to install [haskell](https://www.haskell.org/platform/) and run the following commands in the terminal in the root of the repo:
```
$ make
$ ./Comp6 ./Examples/example.pp6
$ ./emu6809 ./Examples/example.b09
```
Then you would have to enter values of every input variable in the list of variables of the input instruction of the program. Then you will see the result of evaluation of the expression of the input insruction. For example:
```
x = 5
y = 6
11
```

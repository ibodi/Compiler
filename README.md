
Jestem dumny z tego, że napisałem plik BohdanShcherbakCompiler.hs jako rozwiązanie zadania, treść którego można znaleźć w pliku Tasks/prac456_extra.pdf. Plik BohdanShcherbakCompiler.hs jest kompilatorem prostego języka programowania, składnia którego jest opisana w plikach Tasks/prac4.pdf, Tasks/prac5.pdf oraz Tasks/prac6.pdf. Przykłady programów napisanych w tym języku znajdują się w katalogu Examples.

Autorem pliku BohdanShcherbak.hs jest mój kolega Konrad Werbliński.

Program w tym języku programowania składa się z listy funkcji i wiersza 

input [lista zmiennych] in [wyrażenie, które może zawierać te zmienne i funkcje]

Przykładowy program:

fun plus(xy : int * int) : int = fst xy + snd xy 
input x y in plus(x, y)

Żeby skompilować i uruchomić przykładowy program Examples/example.pp6 trzeba w korzeniu repozytorium wpisać następujące instrukcje w terminalu:

$ make
$ ./Comp6 ./Examples/example.pp6
$ ./emu6809 ./Examples/example.b09

Potem trzeba wprowadzić znaczenia dla każdej zmiennej w liście zmiennych instrukcji input tego programu, po czym będzie wypisany wynik wyrażenia instrukcji input. Na przykład:

x = 5
y = 6
11

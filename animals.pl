/*This is an implementation of the Animal Game in Prolog*/

/*Important Info:*/
/* - When playing the game for the first time use 'init.' since it will initialize
      all the data
   - If you are back in the terminal after finishing a game, but you
      want to play more, just type 'play.' since the initializations
      are already done
   - When typing an input, please do it surrounded by single quotes
   - Sadly I was not able to perform the a/an checking, so I'm trusting the 
     user's grammar when typing the correct article for the given animal*/

animallist:- asserta(animals(['Does it bark? ', ['Does it swim? ', 'horse', 'dolphin'], 'dog'])).

/*To initialize the animal list*/
/*Should only be used one time at the beginning of the game*/
init:- write('Welcome to the Animal Game!!'), nl, nl, animallist, play.

play:- write('Do you want to load a file? '), read_atom(Answ), verifyRead(Answ).
/*After initialization is done, start navigating the animals*/
go:- write('** Answer everything surrounded by single quotes ('') **'), nl,nl, animals(A), navigate(A).

/*Ask question*/
ask(Node):- nth(1,Node,X), write(X).

/*Receive a node, ask question in first element ang get answer*/
navigate(Node):-ask(Node),
		read_atom(Answ), 
		verify(Answ, Node).

/*Check if the input is y or n, return answer in A*/
/*If answer is invalid, ask question again calling navigate()*/
verify(Answ, Node):- ((Answ='y'; Answ='yes'; Answ='Yes'; Answ='Y';Answ='n'; Answ='no'; Answ='No'; Answ='N') 
		-> checkYN(Answ, Node); 
		navigate(Node)).

checkYN(Answ,Node):- ((Answ='y'; Answ='yes'; Answ='Y'; Answ='Yes') -> 
		nth(3,Node,NewNode), moveLR(Node, NewNode) ; 
		nth(2,Node,NewNode), moveLR(Node, NewNode) ).

/*Check if element is a list*/
is_list(List) :- var(List), !, fail.
	is_list([_|Tail]) :- is_list(Tail).
	is_list([]).

/*navigate through the animals*/
/*If actual node is a list, navigate through it again*/
/*If it is an animal, check if it was guessed*/
moveLR(Parent, Node):- (is_list(Node) -> 
		navigate(Node); 
		isAnimal(Parent,Node)).

/*Check if animal was guessed*/
isAnimal(Parent, Node):- write('Is it '), write(Node), write('? '),
		read_atom(Answ),
		verifyAni(Answ, Node, Parent).

/*To verify entered answer is valid*/
verifyAni(Answ, Node, Parent):- ((Answ='y'; Answ='yes'; Answ='Y';Answ='Yes';Answ='n'; Answ='no'; Answ='No'; Answ='N') 
		-> checkYNAni(Answ, Node); 
		isAnimal(Parent, Node)).

/*If answer is yes, the animal was guessed, ask to play again!*/
/*If answer is no, then the complicated part begins...*/
checkYNAni(Answ,Node):- ((Answ='y'; Answ='yes'; Answ='Y';Answ='Yes') -> 
		write('I guessed your animal!!'), nl, nl, playAgain; 
		notGuessed(Node) ).

playAgain:- write('Do you want to play again? '),
		read_atom(Answ),nl,
		verifyPA(Answ).


/*To verify user input is valid*/
/*I wanted to reuse this code somehow, since it is reapeated a lot*/
/*But I couldn't find a way to do it, so here it is again slightly modified*/
verifyPA(Answ):- ((Answ='y'; Answ='Y'; Answ='yes'; Answ='Yes';Answ='n'; Answ='no'; 
		Answ='No'; Answ='No') 
		-> checkYNPA(Answ); 
		playAgain).

/*Play again or terminate game*/
checkYNPA(Answ):- ((Answ='y'; Answ='Y'; Answ='Yes';Answ='yes') -> 
		go ; 
		save).

/*If animal was not guessed, ask for new animal and question*/
notGuessed(Actual):- askAnimal(NewAnimal), convertLower(NewAnimal, NewAnimal2),
			askDistinguish(Actual, NewAnimal2, Q), checkForQuestMark(Q,Q2),
			convertLower(Q2,Q3), capitalize(Q3,QCap), forA(NewAnimal2, Actual, QCap).

askAnimal(Animal):- write('What animal were you thinking of? '),
		read_atom(Animal).

askDistinguish(Actual, New, Q):-write('Which question would distinguish '),
				write(Actual), write(' from '), write(New), write('?'),
				nl, read_atom(Q).

/*Check for existance of question mark */
checkForQuestMark(OldQ, NewQ):- (exists(OldQ, '?') -> 
			atom_concat(OldQ, ' ', NewQ); 
			atom_concat(OldQ, '? ', NewQ)).

/*Since strings are lists, look for a ? in a list*/
find_element(X,[X|_], 1).
find_element(X,[_|T], C) :- find_element(X,T,TEMPC), C is TEMPC +1.

exists(Atom, Ch):- name(Atom, L), name(Ch, [Z]), find_element(Z, L, _Count).

/*To capitalize the first char of the quesiton typed by the user*/
capitalize(Q, QCap):-atom_codes(Q,Ascii), nth(1,Ascii,First), (First > 90 ->
			Lower is First-32, replace(Ascii,0,Lower,X), atom_codes(QCap,X);
			atom_concat(Q, '', QCap)).
/*Replace an element of a list at a given index*/
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

/*To invoke the lower case method*/
convertLower(Q, NewQ):- atom_codes(Q,Ascii), lower(Ascii,X,New), atom_codes(NewQ,New).

/*Lower case all the letters in the string*/
lower([],X,Y):- nth(1,X,F), replace(X,0,100,N), replace(N,0,F,Y).
lower([H|T],X,Y):- ((H <91 , H>64) ->
			N is H+32, append2(X,[N],A),lower(T,A,Y);
			append2(X, [H], A), lower(T,A,Y)).

append2([],X,X).
append2([A|B],Y,[A|W]):-append2(B,Y,W).

forA(Animal, Actual, Q):-write('For '), write(Animal), write(' the answer would be? '),
			read_atom(AnswR), verifyForA(AnswR, Animal, Actual, Q).

/*Again this dreaded piece of code...*/
verifyForA(Answ, Animal, Node, Quest):- ((Answ='y'; Answ='yes';Answ='n'; Answ='no') 
		-> checkForA(Answ, Animal, Node, Quest); 
		forA(Animal, Node, Quest)).

/*Depending if answer is yes/no, subsitute animal name for a list containing the new info*/
checkForA(Answ, Animal, Node, Q):- ((Answ='y'; Answ='yes'; Answ='Y'; Answ='Yes') -> 
		animals(A), retract(animals(A)), substitute(Node,[Q,Node,Animal],A,New), asserta(animals(New)), nl, playAgain; 
		animals(A), retract(animals(A)), substitute(Node,[Q,Animal,Node],A,New), asserta(animals(New)), nl, playAgain ).

/*To substitute an animal name for a list!*/
substitute(_, _, [], []).
substitute(X, Y, [X|T1], [Y|T2]) :- substitute(X, Y, T1, T2), !.
substitute(X, Y, [H|T1], [H|T2]) :- \+ is_list(H), substitute(X, Y, T1, T2), !.
substitute(X, Y, [H1|T1], [H2|T2]) :- substitute(X, Y, H1, H2), substitute(X, Y, T1, T2).

/*Check if user wants to read a file*/
verifyRead(Answ):- ((Answ='y'; Answ='Y'; Answ='yes'; Answ='Yes';Answ='n'; Answ='no'; 
		Answ='No'; Answ='No') 
		-> checkYNRead(Answ); 
		play).

/*Play again or terminate game*/
checkYNRead(Answ):- ((Answ='y'; Answ='yes'; Answ='Y'; Answ='Yes') -> 
		write('Type the name of the file: '), read_atom(File), readFile(File) ; 
		write('Using default animals...'),nl,go ).

readFile(File):- open(File, read, Stream),
        read(Stream, Inp),
        close(Stream),
        retract(animals(A)),
        asserta(Inp), nl, go.

save:-write('Do you want to save your game? '), read_atom(Answ), verifySave(Answ).

/*Check if user wants to save a file*/
verifySave(Answ):- ((Answ='y'; Answ='Y'; Answ='yes'; Answ='Yes';Answ='n'; Answ='no'; 
		Answ='No'; Answ='No') 
		-> checkYNSave(Answ); 
		save).

/*Play again or terminate game*/
checkYNSave(Answ):- ((Answ='y'; Answ='yes'; Answ='Y'; Answ='Yes') -> 
		write('Type the name of the file: '), read_atom(File), saveFile(File) ; 
		 write('Okay, no worries, thanks for playing!!') ).

saveFile(File):- write('Saving the following animals...'),nl,open(File, write, Stream),
        add_stream_mirror(top_level_output, Stream),
        listing(animals), close(Stream), nl, write('Game saved, thanks for playing!!').





/*read_atom(X), atom_codes(X,A), iterate(A,N).*/



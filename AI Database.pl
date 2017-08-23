%FORMAT OF CALLING EXECUTE
%execute([the,color,of,the,car,is,dark,blue],X).
%execute([the,color,of,the,car,is,blue],X).


%execute([what, is, the, color, of, the, car], X).
%execute([what, is, the, color, of, the, van], X).


%Knowledge base 
:- dynamic fact/3, fact/4.

execute(TheList, Returned):- phrase(sentence(S), TheList), process(S, Returned).


/*
CFG

s -> np, np, ap
q -> qStart, np, np

np -> det, n, prep
np -> det, n
ap -> verb, adjective
vp -> det, verb
adjective -> what
det -> the
prep -> of
verb -> is
n -> car

*/

% Converts the important words in a fact (TWO WORDED DESCRIPTIONS) I.E. fact(color, car, dark, blue)
statement(Statement) --> np(Description), np(N), ap(A, A2), 
    { Statement =.. ['fact',Description, N, A, A2]}.	

% Converts the important words in a fact (ONE WORDED DESCRIPTIONS) I.E. fact(color, car, blue)
statement(Statement) --> np(Description), np(N), ap(A), 
    { Statement =.. ['fact',Description, N, A]}.	
	
% Converts the important words in a fact to be queried. I.E. fact(color, car)
query(Query) -->  qStart, np(A), np(N),
    { Query =.. ['fact',A, N]}.

%DCG   
np(Noun) --> det, [Noun], prep.
np(Noun) --> det, [Noun].

ap(Adj) --> verb, [Adj].
ap(Adj, Adj2) --> verb, [Adj], [Adj2].

qStart --> adjective, verb.

vp --> det, verb.
 

adjective --> [what].

det --> [the].

prep --> [of].

verb -->[is].


%% Combine grammar rules into one sentence

sentence(statement(S)) --> statement(S).
sentence(query(Q)) --> query(Q).

%STARTS THE ANALYZING OF THE STATEMENTS OR QUERIES
process(statement(S), Z) :- checkStatement(S, Z).
process(query(Q), Z):- checkQuery(Q, Z).


%THIS CHECKQUERY IS USED TO DETECT AND RETURN FACTS WHERE THE DESCRIPTION IS ONE WORDED
checkQuery(Query, Z):-
    Query =.. [Fun, Description, Object],
    D = Description,
    N = Object,
    New =.. ['fact', D, N, X],
    New,
    Z= X.
		
%THIS CHECKQUERY IS USED TO DETECT AND RETURN FACTS WHERE THE DESCRIPTION IS TWO WORDED   
%AND IF IT DOESN'T EXIST, IT RETURNS DON'T KNOW 
checkQuery(Query2, Z):-
    Query2 =.. [Fun, Description, Object],
    D = Description,
    N = Object,
    New =.. ['fact', D, N, X, X2],
    New,
    ResponseList = [X, X2],
    atomic_list_concat(ResponseList, ' ', Atom), atom_string(Atom, Z);
    Z = 'I dont know'.		%If it isn't itll print "I dont know"
    
checkStatement(Fact, Z) :-		% This checks whether the database already has the fact or not.
    call(Fact), !,
    Z= 'I know';
    checkStatement2(Fact, Z).
%THIS CHECKS IF THE STATEMENT WITH ONE WORDED DESCRIPTIONS EXIST IN THE DATABASE, IF IT DOES WE RETURN THE FACT
%OTHERWISE IT FAILS AND MOVES ON TO THE NEXT CHECKSTATEMENT2. THE FACT IS TAKEN IN AND THE OUTPUT IS THE RESPONSE
checkStatement2(Fact2, Z):- 
    functor(Fact2,F,A),
    A == 3,
    Fact2 =.. [Fun, Description, Object, Adjective],
    D = Description,
    N = Object,
    New =.. ['fact',D, N, X],
    New,
    Adjective \= X,
    ResponseList = ['No, its', X],
    atomic_list_concat(ResponseList, ' ', Atom), atom_string(Atom, Z).

%IF GIVEN A STATEMENT WITH ONE WORD DESCRIPTION, THEN WE CHECK IF THERE'S ALREADY A TWO WORDED DESCRIPTION
%IF IT DOES, THEN RETURN THE FACT, IF NOT ASSERT THE NEW FACT. THE FACT IS TAKEN IN AND THE OUTPUT IS THE RESPONSE
checkStatement2(Fact5, Z):- 
    functor(Fact5,F,A),
    A == 3,
    Fact5 =.. [Fun, Description, Object, Adjective],
    D = Description,
    N = Object,
    New =.. ['fact',D, N, X, X2],
    New,
    ResponseList = ['No, its', X, X2],
    atomic_list_concat(ResponseList, ' ', Atom), atom_string(Atom, Z);
    functor(Fact5,F,A),
    A == 3,
    assert(Fact5),
    Z = 'OK'.
%THIS CHECKS IF THE FIRST PART OF THE 2 WORDED DESCRIPTION UNIFIES, IF NOT, STATE THE EXISTING FACT
%%IF NOT, CONTINUE TO CHECK IF SECOND PART OF THE DESCRIPTION UNIFIES
checkStatement2(Fact3, Z):- 
    Fact3 =.. [Fun, Description, Object, Adjective, Adjective2],
    D = Description,
    N = Object,
    New =.. ['fact',D, N, X],
    New,
    Adjective \= X,
    ResponseList = ['No, its', X],
    atomic_list_concat(ResponseList, ' ', Atom), atom_string(Atom, Z);
    checkArg(Fact3, Z).
%THIS CHECKS WHETHER THE FACT EXISTS FOR 2 WORDED DESCRIPTIONS.THE FACT IS TAKEN IN AND THE OUTPUT IS THE RESPONSE
checkArg(Fact4, Z):-
    Fact4 =.. [Fun, Description, Object, Adjective, Adjective2],
    D = Description,
    N = Object,
    X = Adjective,
    New =.. ['fact',D, N, X, X2],
    New,
    X2 \= Adjective2,
    ResponseList = ['No, its', X, X2],
    atomic_list_concat(ResponseList, ' ', Atom), atom_string(Atom, Z);    
    assert(Fact4),
    Z = 'OK'.



%This is the reply for the any statements.
callKnow:- format('I know').
callOK:- format('OK').
callIts:- format('It's ').
callDK:- format('I don't know').
%%%%%%%%%%PART ONE%%%%%%%%%%
np_names(N):- %names of all national parks
    np(N,_,_).

np_names_not_yosemite(N):- %names of all national parks other than yosemite
    np(N,_,_),
    N \= yosemite.

np_activities_yosemite(A):- %list of activities at yosemite
    np(yosemite,_,A).

np_states_yosemite(S):- %list of states for yosemite
    np(yosemite,S,_).

np_states_grandcanyon(S):- %list of states for grandcanyon
    np(grandcanyon,S,_).

np_states(N,S):- %list of states for national park with name N
    np(N,S,_).

np_sorted_activities_yosemite(SA):- %sorted list of activities at yosemite
    np_activities_yosemite(A),
    sort(A,SA).

np_single_state(N):- %names of parks that are exactly within one state
    np(N,[_],_).

np_multi_state(N):- %names of parks that are within two or more states
    np(N,[_,_|_],_).

np_pair_names([N1,N2]):- %ordered pairs of names of 2 parks that are within exactly one state that is the same
    np(N1,[H],_),
    np(N2,[H],_),
    N1 @< N2.

np_2_state_2_activities(N):- %names of parks that are within exactly two states and offer exactly two activities
    np(N,[_,_],[_,_]).

np_12_states_1or(N):- %names of parks that are within exactly one or exactly two states (using or)
    np(N,[_],_);
    np(N,[_,_],_).

np_12_states_2wo(N):- %names of parks that are within exactly one or exactly two states (using two def)
    np(N,[_],_).
np_12_states_2wo(N):-
    np(N,[_,_],_).

np_camping_hiking_1or(N):- %names of parks that provide exactly camping and hiking (using or)
    np(N,_,[camping,hiking]);
    np(N,_,[hiking,camping]).

np_camping_hiking_2wo(N):- %names of parks that provide exactly camping and hiking (using two def)
    np(N,_,[camping,hiking]).

np_camping_hiking_2wo(N):- %names of parks that provide exactly camping and hiking (using two def)
    np(N,_,[hiking,camping]).

np_camping_hiking_sort(N):- %names of parks that provide exactly camping and hiking (using sort)
    np(N,_,A),
    sort(A,[camping,hiking]).

%%%%%%%%%%PART TWO%%%%%%%%%%
insert(L,E,Z):- %returns in list Z all elements of L and element E is sorted order
    sort([E|L],Z).

butlast([_],[]). % returns in list Z all elements in L except the last one
butlast([H|T],[H|Z]):-
    butlast(T,Z).
    
naaa([],[],[]). %returns in NAL all the non-atoms from L and in AL all the atoms from L
naaa([H|L],NAL,[H|AL]):- %atom
    atom(H),
    naaa(L,NAL,AL).
naaa([H|L],[H|NAL],AL):- %non-atom
 	\+atom(H),
 	naaa(L,NAL,AL).

%%%%%%%%%%PART THREE%%%%%%%%%%

%Left: all elements appear before the pivot
%Right: all elements appear after the pivot
splitlist(L,Left,Pivot,Right):- %the pivot is the first element
	L = [H|T],
	H = Pivot,
	Left = [],
	Right = T.
splitlist(L,[H|Left],Pivot,Right):-
	L = [H|T],
	H \= Pivot,
	splitlist(T,Left,Pivot,Right).

%Pivot is the element whose third element is owner
split3list(L,Owner,Left,Pivot,Right):- 
	L = [H|T], 
	H = [_,_,Owner],
	Pivot = H,
	Left = [],
	Right = T.
split3list(L,Owner,[H|Left],Pivot,Right):-
	L = [H|T],
	H = [_,_,E],
	E \= Owner,
	split3list(T,Owner,Left,Pivot,Right).	

%%%%%%%%%%PART FOUR%%%%%%%%%%

%return the permutations of L
perm([],[]).
perm(L,PermL):-
	select(X,L,R),
	perm(R,PermR),
	PermL = [X|PermR].
	
%return the permutations in which the non-atoms do not get permuted

permsub(L,PermL):-
	perm(L,PermL),
	naaa(L,NAL,_),
	naaa(PermL,NAL,_).

%%%%%%%%%%PART FIVE%%%%%%%%%%

fit1stRequest([Owner,Size], Memlist, NewMemList):- %first block is free & has just enough space
	Memlist = [H|T],
	H = [Ad,S,z], %first block is free	
	Size = S, %has just enough space
	NewMemList = [[Ad,Size,Owner]|T].
fit1stRequest([Owner,Size], Memlist, NewMemList):- %first block is free & has more than enough space
	Memlist = [H|T],
	H = [Ad,S,z], %first block is free	
	(Size < S-> %has more than enough space
	Freespace is S-Size,
	Starting is Ad+Size,
	NewT = [[Starting, Freespace, z]|T],
	NewMemList = [[Ad,Size,Owner]|NewT];
    Size > S, %not enough space
    fit1stRequest([Owner,Size],T,NewT),
    NewMemList = [H|NewT]).
fit1stRequest([Owner,Size], Memlist, [H|NewMemList]):- %recursive case
	Memlist = [H|T],
	H \= [_,_,z], %the first block is not free
	fit1stRequest([Owner,Size],T,NewMemList).


fitRelease(Owner, MemList, NewMemList):-
    split3list(MemList,Owner,Left,Pivot,Right),
	Pivot = [Ad, Size, Owner],


    %come up with the left portion
    (Left = []-> %Left is empty
     NewLeft = [],
     StartingAdr = Ad,
     CurBlockSize is Size
    ;
     last(Left,Leftlast),
     (Leftlast = [ADL,SizeL,z]-> %Leftlast is free
      StartingAdr = ADL, %starting address for the free block
      butlast(Left,Leftbutlast),
      NewLeft = Leftbutlast,
      CurBlockSize is Size + SizeL;

      %Leftlast is not free
      StartingAdr = Ad,
      NewLeft = Left,
      CurBlockSize is Size
     )
    ), %end of Left

    %the right portion
    (Right = []-> %Right is empty
     NewRight = [[StartingAdr, CurBlockSize, z]]
    ;Right = [RH|RT], %Right is not empty
     (RH = [_,RSize,z]-> %RH is free
      NewSize is CurBlockSize + RSize,
      NewRight = [[StartingAdr, NewSize, z]|RT];
        
      %RH is not free
      NewRight = [[StartingAdr, CurBlockSize,z]|Right]
     )
    ), %end of Right

	append(NewLeft, NewRight, NewMemList).

%%%%%%%%%%PART SIX%%%%%%%%%%

%allocate memory in any free block
fitanyRequest([Owner,Size],MemList,NewMemList):- %the free block has just enough space
	select([Ad,BlockSize,z], MemList,Rest),
	BlockSize = Size,
	insert(Rest, [Ad,Size,Owner],NewMemList).
fitanyRequest([Owner,Size],MemList,NewMemList):- %the free block has more than enough space
	select([Ad,BlockSize,z], MemList,Rest),
	BlockSize > Size,
	Freespace is BlockSize-Size,
	Starting is Ad+Size, %starting is the starting address of the new free block
	insert(Rest, [Ad,Size,Owner],NewMemList1),
	insert(NewMemList1,[Starting,Freespace,z],NewMemList).		

%%%%%%%%%%PART SEVEN%%%%%%%%%%%

fit1st([],MemList,MemList).
fit1st([H|T],MemList,NewMemList):-
	(H = [_,_]-> %request
	fit1stRequest(H,MemList,NewMemList1);
    atom(H), %release
    fitRelease(H,MemList,NewMemList1)),
	fit1st(T,NewMemList1,NewMemList).


fitany([],MemList,MemList). %similar to fit1st, but invoke fitanyRequest
fitany([H|T],MemList,NewMemList):-
	(H = [_,_]-> %request
	fitanyRequest(H,MemList,NewMemList1);
    atom(H), %release
    fitRelease(H,MemList,NewMemList1)),
	fitany(T,NewMemList1,NewMemList).

%%%%%%%%%%PART EIGHT%%%%%%%%%%

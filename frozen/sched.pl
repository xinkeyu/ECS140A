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

/*
fitRelease(Owner, MemList, NewMemList):- %Left, Right both nothing
	split3list(MemList,Owner,[],Pivot,[]),
	Pivot = [Ad, Size, Owner],
	New = [Ad, Size, z], %set it free
	NewMemList = [New].	
fitRelease(Owner, MemList, NewMemList):- %Left nothing, RH not free
	split3list(MemList,Owner,[],Pivot,Right),
	Right = [RH|_],
	Pivot = [Ad, Size, Owner],
	RH \= [_,_,z], %RH not free
	New = [Ad, Size, z], %set it free
	NewMemList = [New|Right].
fitRelease(Owner, MemList, NewMemList):- %Left nothing, RH free
	split3list(MemList,Owner,[],Pivot,Right),
	Right = [RH|RT],
	Pivot = [Ad, Size, Owner],
	RH = [_,RHSize,z], %RH free
	NewSize is RHSize + Size,
	New = [Ad, NewSize, z], %set it free
	NewMemList = [New|RT].
fitRelease(Owner, MemList, NewMemList):- %LeftLast not free, RH nothing
	split3list(MemList,Owner,Left,Pivot,[]),
	last(Left,LeftLast),
	Pivot = [Ad, Size, Owner],
	LeftLast \= [_,_,z], %LeftLast not free
	New = [Ad, Size, z], %set it free	
	append(Left,[New],NewMemList).
fitRelease(Owner, MemList, NewMemList):- %LeftLast free, RH nothing
	split3list(MemList,Owner,Left,Pivot,[]),
	last(Left,LeftLast),
	Pivot = [_, Size, Owner],
	LeftLast = [AdL,LSize,z], %LeftLast free
	NewSize is LSize + Size,
	New = [AdL, NewSize, z], %set it free
	butlast(Left,Leftbutlast),	
	append(Leftbutlast,[New],NewMemList).
fitRelease(Owner, MemList, NewMemList):- %both not free
	split3list(MemList,Owner,Left,Pivot,Right),
	last(Left,LeftLast),
	Right = [RH|_],
	LeftLast \= [_,_,z],
	RH \=[_,_,z],
	Pivot = [Ad, Size, Owner],
	New = [[Ad, Size, z]|Right], %set it free	
	append(Left,New,NewMemList).
fitRelease(Owner, MemList, NewMemList):- %LeftLast not free, RH free
	split3list(MemList,Owner,Left,Pivot,Right),
	last(Left,LeftLast),
	Right = [RH|RT],
	LeftLast \= [_,_,z],
	RH =[_,RHSize,z],
	Pivot = [Ad, Size, Owner],
	NewSize is RHSize + Size,
	New = [[Ad, NewSize, z]|RT], %set it free	
	append(Left,New,NewMemList).
fitRelease(Owner, MemList, NewMemList):- %LeftLast free, RH not free
	split3list(MemList,Owner,Left,Pivot,Right),
	last(Left,LeftLast),
	Right = [RH|_],
	LeftLast = [AdL,LSize,z],
	RH \=[_,_,z],
	Pivot = [_, Size, Owner],
	NewSize is LSize + Size,
	New = [[AdL, NewSize, z]|Right], %set it free
	butlast(Left,Leftbutlast),	
	append(Leftbutlast,New,NewMemList).	
fitRelease(Owner, MemList, NewMemList):- %both free
	split3list(MemList,Owner,Left,Pivot,Right),
	last(Left,LeftLast),
	Right = [RH|RT],
	LeftLast = [AdL,LSize,z],
	RH =[_,RSize,z],
	Pivot = [_, Size, Owner],
	NewSize is LSize + Size + RSize,
	New = [[AdL, NewSize, z]|RT], %set it free
	butlast(Left,Leftbutlast),	
	append(Leftbutlast,New,NewMemList).			

*/

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

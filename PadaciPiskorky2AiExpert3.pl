%
%
% Padaci piskorvky
% Padaci piskorky maji pravidlo, ze lze vhazovat piskorky pouze do sloupce, kde spadnou az uplne dolu, kam muhou.
% Vyhravva 4(nebo vice} po sobe jdoucich piskvorek.
%
%


disp() :-
	write('| _  | _  | _  | _  | _  | _  |'),nl,
 	write('| _  | _  | _  | _  | _  | _  |'),nl,
    write('| _  | _  | _  | _  | _  | _  |'),nl,
 	write('| _  | _  | _  | _  | _  | _  |'),nl,
	write('| _  | _  | _  | _  | _  | _  |'),nl,
 	write('| _  | _  | _  | _  | _  | _  |'),nl,
 	write('| 1  | 2  | 3  | 4  | 5  | 6  |'),nl.

hrat :- jak_hrat.

jak_hrat :-
  write('adaci piskorky maji pravidlo, ze lze vhazovat piskorky pouze do sloupce, kde spadnou az uplne dolu, kam muhou,'),
  nl, write('Vyhravva 4(nebo vice} po sobe jdoucich piskvorek.'),nl,
  disp(),
    write('Do jakeho sloupce chceÜ um?stit piÜkvorku?'),
    tah(["_","_","_","_","_","_"],["_","_","_","_","_","_"],
        ["_","_","_","_","_","_"],["_","_","_","_","_","_"],
        ["_","_","_","_","_","_"],["_","_","_","_","_","_"]).
tah(A,B,C,D,E,F):-vyhraAI(A,B,C,D,E,F). 
tah(A,B,C,D,E,F):-write('Do jakeho sloupce chces umistit piskvorku? Hraje X'),nl,read(N),nl,
    xtah(A,B,C,D,E,F,N,A2,B2,C2,D2,E2,F2),
    
    
    zhodnoceni(A2,B2,C2,D2,E2,F2,1,1,VystupHodX,VystupHodO,0,0,0),
    write('score X:'),write(VystupHodX),nl,write('score Y:'),write(VystupHodO),nl,
     %rozhodovani(A,B,C,D,E,F,1,Nejlepsi,-50000,1),write('nejlepsi tah je'),write(Nejlepsi),
    
   nl,ukaz(A2,B2,C2,D2,E2,F2),vyhra(A2,B2,C2,D2,E2,F2),
    tahO(A2,B2,C2,D2,E2,F2).

tahO(A,B,C,D,E,F):-vyhraAI(A,B,C,D,E,F). 
tahO(A,B,C,D,E,F):-%write('Do jakeho sloupce chces umistit piskvorku? Hraje O'),nl,read(Nejlepsi),nl,
   rozhodovani(A,B,C,D,E,F,1,Nejlepsi,-50001000,1),
    otah(A,B,C,D,E,F,Nejlepsi,A2,B2,C2,D2,E2,F2),
    ukaz(A2,B2,C2,D2,E2,F2),vyhra(A2,B2,C2,D2,E2,F2),
    write('Protivnik tahl do '),write(Nejlepsi),write('. sloupce.'),nl,
    zhodnoceni(A2,B2,C2,D2,E2,F2,1,1,VystupHodX,VystupHodO,0,0,0),
    write('score X:'),write(VystupHodX),nl,write('score Y:'),write(VystupHodO),nl,
    vyhra(A2,B2,C2,D2,E2,F2),
    tah(A2,B2,C2,D2,E2,F2).
    

xtah(A,B,C,D,E,F,N,A,B,C,D,E,F):- xY(N,6,A,B,C,D,E,F,"x"),write('Nedaval/a jsi pozor a dal jsi do plneho sloupce, hrajes znovu'),nl,
    tah(A,B,C,D,E,F).
xtah(A,B,C,D,E,F,N,A,B,C,D,E,F):- xY(N,6,A,B,C,D,E,F,"o"),write('Nedaval/a jsi pozor a dal jsi do plneho sloupce, hrajes znovu'),nl,
    tah(A,B,C,D,E,F).
xtah(A,B,C,D,E,F,1,R,B,C,D,E,F):-zmen(A,R,"x",1).
xtah(A,B,C,D,E,F,2,A,R,C,D,E,F):-zmen(B,R,"x",1).
xtah(A,B,C,D,E,F,3,A,B,R,D,E,F):-zmen(C,R,"x",1).
xtah(A,B,C,D,E,F,4,A,B,C,R,E,F):-zmen(D,R,"x",1).
xtah(A,B,C,D,E,F,5,A,B,C,D,R,F):-zmen(E,R,"x",1).
xtah(A,B,C,D,E,F,6,A,B,C,D,E,R):-zmen(F,R,"x",1).
xtah(A,B,C,D,E,F,N,_,_,_,_,_,_):-(   N>6;N<1),write('spatn2 tah'),tah(A,B,C,D,E,F).



otah(A,B,C,D,E,F,N,A,B,C,D,E,F):- xY(N,6,A,B,C,D,E,F,"x"),write('Nedaval/a jsi pozor a dal jsi do plneho sloupce, hrajes znovu'),nl,
  	tahO(A,B,C,D,E,F).
otah(A,B,C,D,E,F,N,A,B,C,D,E,F):- xY(N,6,A,B,C,D,E,F,"o"),write('Nedaval/a jsi pozor a dal jsi do plneho sloupce, hrajes znovu'),nl,
    tahO(A,B,C,D,E,F).
otah(A,B,C,D,E,F,1,R,B,C,D,E,F):-zmen(A,R,"o",1).
otah(A,B,C,D,E,F,2,A,R,C,D,E,F):-zmen(B,R,"o",1).
otah(A,B,C,D,E,F,3,A,B,R,D,E,F):-zmen(C,R,"o",1).
otah(A,B,C,D,E,F,4,A,B,C,R,E,F):-zmen(D,R,"o",1).
otah(A,B,C,D,E,F,5,A,B,C,D,R,F):-zmen(E,R,"o",1).
otah(A,B,C,D,E,F,6,A,B,C,D,E,R):-zmen(F,R,"o",1).
otah(A,B,C,D,E,F,N,_,_,_,_,_,_):-(   N>6;N<1),write('spatn2 tah'),tahO(A,B,C,D,E,F).



zmen([A|T],[A|T2],Symbol,N):- (   A="x";A="o"),N2 is N+1,N<6,zmen(T,T2,Symbol,N2).
zmen(["_"|T],[Symbol|T2],Symbol,N):- nl, N2 is N+1, N<6 ,zmen2(T,T2,N2).
zmen(["_"],[Symbol],Symbol,6).
zmen([A],[A],_,6):-(A="X";A="O").
zmen2([A|T],[A|T2],N):-N<6,N2 is N+1,zmen2(T,T2,N2).
zmen2([A],[A],6).

ukaz(A2,B2,C2,D2,E2,F2):- otoceni(A2,A1),otoceni(B2,B1),otoceni(C2,C1),otoceni(D2,D1),otoceni(E2,E1),otoceni(F2,F1),ukaz2(A1,B1,C1,D1,E1,F1,1).
ukaz2([A|Ta],[B|Tb],[C|Tc],[D|Td],[E|Te],[F|Tf],N):-N<6,write('| '),write(A),write(' | '),write(B),write(' | '),write(C),write(' | '),write(D),write(' | '),write(E),write(' | '),write(F),write(' |'),
  N2 is N+1,nl,ukaz2(Ta,Tb,Tc,Td,Te,Tf,N2).%,vyhra2([A,B,C,D,E,F]).
ukaz2([A],[B],[C],[D],[E],[F],6):-write('| '),write(A),write(' | '),write(B),write(' | '),write(C),write(' | '),write(D),write(' | '),write(E),write(' | '),write(F),write(' | '),nl,write('|'),
 nl, write('| 1  | 2  | 3  | 4  | 5  | 6  |').%,vyhra2([A,B,C,D,E,F]),nl.



vyhra(A,B,C,D,E,F):-vodorovne(A,B,C,D,E,F),write('vodorovne').
vyhra(A,B,C,D,E,F):-nahoru(A,B,C,D,E,F),write('nahoru').
vyhra(A,B,C,D,E,F):-sikmo(A,B,C,D,E,F),write('sikmo').
vyhra(_,_,_,_,_,_).

nahoru(A,B,C,D,E,F):-member(X,[A,B,C,D,E,F]),vyhra2(X).



vyhra2(X):-substring(["x","x","x","x"],X), write('!!!!>> Vyhra X <<!!!! Gratulace'),nl.
vyhra2(X):-substring(["o","o","o","o"],X), write('!!!!>> Vyhra O <<!!!! Gratulace'),nl.
%vyhra2([A,B,C,D,E,F]).

sikmo(A,B,C,D,E,F):-sikmo2(A,B,C,D,E,F,1,[1,2,3]).
sikmo(A,B,C,D,E,F):-sikmo2(A,B,C,D,E,F,-1,[4,5,6]).


sikmo2(A,B,C,D,E,F,Smer,Oblast):-member(X,Oblast),member(Y,[1,2,3]),sikmo2(A,B,C,D,E,F,X,Y,Smer,Oblast,4,Retezec),vyhra2(Retezec).
sikmo2(A,B,C,D,E,F,X,Y,Smer,Oblast,N,[Cast|Tail]):-N>0,N2 is N-1,xY(X,Y,A,B,C,D,E,F,Cast),X2 is X + Smer ,Y2 is Y + 1,
    sikmo2(A,B,C,D,E,F,X2,Y2,Smer,Oblast,N2,Tail).
sikmo2(_,_,_,_,_,_,_,_,_,_,0,Tail):-Tail is "_".%  aby to nedelalo falesnou vyhru.

vodorovne(A,B,C,D,E,F):-member(T,[1,2,3,4,5,6]),
    xY(1,T,A,B,C,D,E,F,T1),xY(2,T,A,B,C,D,E,F,T2),xY(3,T,A,B,C,D,E,F,T3),
    xY(4,T,A,B,C,D,E,F,T4),xY(5,T,A,B,C,D,E,F,T5),xY(6,T,A,B,C,D,E,F,T6),
    vyhra2([T1,T2,T3,T4,T5,T6]).


xY(X,Y,A,B,C,D,E,F,Symbol):-sloupec(X,K,A,B,C,D,E,F),nClen(K,Y,Symbol).

sloupec(1,X,X,_,_,_,_,_).
sloupec(2,X,_,X,_,_,_,_).
sloupec(3,X,_,_,X,_,_,_).
sloupec(4,X,_,_,_,X,_,_).
sloupec(5,X,_,_,_,_,X,_).
sloupec(6,X,_,_,_,_,_,X).

%Substring(+retezecA,+retezecB). Je pravda, jestli B obsahuje A .
substring(X,S) :-append(_,T,S) ,append(X,_,T) ,X \= [].
         
%NCeln(+Retezec,+NClen,-R). Vrat? prvn?ch Delka clenu v retezci otocen2ch.
nClen([R|_],1, R).
nClen([_|Xs],Delka, R):-Delka>1,Delka1 is Delka -1,nClen(Xs,Delka1, R).

%otocceni2(+S,-R). Otoci seznam S a vrati vysledek v R.
otoceni(S,R):- otoceni2(S,[],R).

%otoceni2(+S, @A, -R). Otoci seznam S a vrati vysledek v R, A je zasobnik
otoceni2([], A, A).
otoceni2([X|Xs], A, R):-otoceni2(Xs, [X|A], R).

%tadu zacocina umela inteligence:::
%-------------ohodnoceni dane situace----------------------

zhodnoceni(_,_,_,_,_,_,7,_,VystupHodX,VystupHodO,HodX,HodY,_):-VystupHodX is HodX, VystupHodO is HodY.
zhodnoceni(A,B,C,D,E,F,X,7,VystupHodX,VystupHodO,HodX,HodY,_):-X<7,X1 is X+1,zhodnoceni(A,B,C,D,E,F,X1,1,VystupHodX,VystupHodO,HodX,HodY,0).
zhodnoceni(A,B,C,D,E,F,X,Y,VystupHodX,VystupHodO,HodX,HodY,_):- Y<7, Y1 is Y +1,
xY(X,Y,A,B,C,D,E,F,Symbol),(Symbol="x";Symbol="o")
,zhodnoceni(A,B,C,D,E,F,X,Y1,VystupHodX,VystupHodO,HodX,HodY,0).


%kontrola jestli nekdo nema 2 nebezpecna policka za sebou.


zhodnoceni(A,B,C,D,E,F,X,Y,VystupHodX,VystupHodO,HodX,HodY,PredchHodX):- Y<7, Y1 is Y +1,
 xY(X,Y,A,B,C,D,E,F,Symbol),Symbol="_",
zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,0,0,-1,-1), %defaulnte hodnota 0, smer zacina na -1.
posouzeni(HodX,HodY,HodnotaX,HodnotaO,PredchHodX,HodX1,HodY1),
%kontrola hodnot jednotlivych policek.    
zhodnoceni(A,B,C,D,E,F,X,Y1,VystupHodX,VystupHodO,HodX1,HodY1,HodnotaX).

posouzeni(HodX,HodY,HodnotaX,HodnotaO,PredchHodX,HodX1,HodY1):-PredchHodX >999,HodnotaX > 999,HodX1= HodX+ 15000,HodY1 is HodY + HodnotaO.
posouzeni(HodX,HodY,HodnotaX,HodnotaO,_,HodX1,HodY1):-HodX1 is HodX + HodnotaX,HodY1 is HodY + HodnotaO.


zhodnoceni2(_,_,_,_,_,_,_,Y,HodnotaX,HodnotaO,StarX,StarO,_,2):-DolniJeLepsi is 12-Y,HodnotaX is StarX*DolniJeLepsi,HodnotaO is StarO*DolniJeLepsi.

zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,StarO,2,SmerY):-SmerY1 is SmerY +1,
    zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,StarO,-1,SmerY1).
zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,StarO,SmerX,SmerY):-
    hodnota(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,_),
 
    
    
    HodX>StarX,
    zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,HodX,StarO,SmerX,SmerY).

zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,StarO,SmerX,SmerY):-
    hodnota(A,B,C,D,E,F,X,Y,SmerX,SmerY,_,HodY),
    HodY>StarO,
    SmerX1 is SmerX + 1,
    zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,HodY,SmerX1,SmerY).

zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,StarO,SmerX,SmerY):-
    SmerX<2,SmerY<2,SmerX1 is SmerX + 1,
    zhodnoceni2(A,B,C,D,E,F,X,Y,HodnotaX,HodnotaO,StarX,StarO,SmerX1,SmerY).


hodnota(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,HodY):-(SmerX*3+X)<7,(   SmerY*3+Y)<7,(   SmerX*3+X)>0,(   SmerY*3+Y)>0,
    hodX(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX),
    hodY(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodY).
hodnota(_,_,_,_,_,_,_,_,_,_,HodX,HodY):-HodX is 0,HodY is 0. 



hodX(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX):-hodX2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,1,3).
%jestlize se uz v tom smeru nachazi "o", ohodnoti radek jako 0 - nezajimavy....
hodX2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,_,N):-N>0,X1 is X +SmerX,Y1 is Y+SmerY,xY(X1,Y1,A,B,C,D,E,F,Symbol),
Symbol="o",    
hodX2(A,B,C,D,E,F,X1,Y1,SmerX,SmerY,HodX,0,0).

%jestlize se uz v tom smeru nachazi "_", hodnota zustava....
hodX2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,StarHodX,N):-N>0,N1 is N-1,X1 is X +SmerX,Y1 is Y+SmerY,xY(X1,Y1,A,B,C,D,E,F,Symbol),
Symbol="_",    
hodX2(A,B,C,D,E,F,X1,Y1,SmerX,SmerY,HodX,StarHodX,N1).

%jestlize se uz v tom smeru nachazi "x", hodnota se nasobi 10....
hodX2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,StarHodX,N):-N>0,N1 is N-1,X1 is X +SmerX,Y1 is Y+SmerY,xY(X1,Y1,A,B,C,D,E,F,Symbol),
Symbol="x", StarHodX1 is StarHodX *10,
hodX2(A,B,C,D,E,F,X1,Y1,SmerX,SmerY,HodX,StarHodX1,N1).

hodX2(_,_,_,_,_,_,_,_,_,_,HodX,StarHodX,0):-HodX is StarHodX.


%to same akorat pro O, asi by to slo napsat do stejne funkce akorat s jinym argumentem, ale takto je to vice prehledne
hodY(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodY):-hodY2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodY,1,3).

%jestlize se uz v tom smeru nachazi "x", ohodnoti radek jako 0 - nezajimavy....
hodY2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,_,N):-N>0,X1 is X +SmerX,Y1 is Y+SmerY,xY(X1,Y1,A,B,C,D,E,F,Symbol),
Symbol="x",    
hodY2(A,B,C,D,E,F,X1,Y1,SmerX,SmerY,HodX,0,0).

%jestlize se uz v tom smeru nachazi "_", hodnota zustava....
hodY2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,StarHodX,N):-N>0,N1 is N-1,X1 is X +SmerX,Y1 is Y+SmerY,xY(X1,Y1,A,B,C,D,E,F,Symbol),
Symbol="_",    
hodY2(A,B,C,D,E,F,X1,Y1,SmerX,SmerY,HodX,StarHodX,N1).

%jestlize se uz v tom smeru nachazi "o", hodnota se nasobi 10....
hodY2(A,B,C,D,E,F,X,Y,SmerX,SmerY,HodX,StarHodX,N):-N>0,N1 is N-1,X1 is X +SmerX,Y1 is Y+SmerY,xY(X1,Y1,A,B,C,D,E,F,Symbol),
Symbol="o", StarHodX1 is StarHodX *10,
hodY2(A,B,C,D,E,F,X1,Y1,SmerX,SmerY,HodX,StarHodX1,N1).
hodY2(_,_,_,_,_,_,_,_,_,_,HodX,StarHodX,0):-HodX is StarHodX.%



%testovani zda neni vyhra
vyhraAI(A,B,C,D,E,F):-nahoruAI(A,B,C,D,E,F).
vyhraAI(A,B,C,D,E,F):-sikmoAI(A,B,C,D,E,F).   
vyhraAI(A,B,C,D,E,F):-vodorovneAI(A,B,C,D,E,F).
nahoruAI(A,B,C,D,E,F):-member(X,[A,B,C,D,E,F]),vyhra2AI(X).
vyhra2AI(X):-substring(["x","x","x","x"],X).
vyhra2AI(X):-substring(["o","o","o","o"],X).


sikmoAI(A,B,C,D,E,F):-sikmo2AI(A,B,C,D,E,F,1,[1,2,3]).
sikmoAI(A,B,C,D,E,F):-sikmo2AI(A,B,C,D,E,F,-1,[4,5,6]).                

sikmo2AI(A,B,C,D,E,F,Smer,Oblast):-member(X,Oblast),member(Y,[1,2,3]),sikmo2(A,B,C,D,E,F,X,Y,Smer,Oblast,4,Retezec),vyhra2AI(Retezec).
sikmo2AI(A,B,C,D,E,F,X,Y,Smer,Oblast,N,[Cast|Tail]):-N>0,N2 is N-1,xY(X,Y,A,B,C,D,E,F,Cast),X2 is X + Smer ,Y2 is Y + 1,
    sikmo2AI(A,B,C,D,E,F,X2,Y2,Smer,Oblast,N2,Tail).
sikmo2AI(_,_,_,_,_,_,_,_,_,_,0,Tail):-Tail is "_".%  aby to nedelalo falesnou vyhru.

vodorovneAI(A,B,C,D,E,F):-member(T,[1,2,3,4,5,6]),xY(1,T,A,B,C,D,E,F,T1),xY(2,T,A,B,C,D,E,F,T2),xY(3,T,A,B,C,D,E,F,T3),
    xY(4,T,A,B,C,D,E,F,T4),xY(5,T,A,B,C,D,E,F,T5),xY(6,T,A,B,C,D,E,F,T6),
    vyhra2AI([T1,T2,T3,T4,T5,T6]).
                                                                      
%vodorovneAI(_,_,_,_,_,_).



%-------------rozhodovani se----------------------
%rozhodovani(-Nejlepsi)



rozhodovani(_,_,_,_,_,_,7,Nejlepsi,_,NejlepsiTah):-Nejlepsi is NejlepsiTah.
    
%kontrola presahu
rozhodovani(A,B,C,D,E,F,N,Nejlepsi,ZatimNejelpsi,NejlepsiTah):-N<7,N1 is N+1,otahAIKontrola(A,B,C,D,E,F,N),%write('kontrola na presah O'),
    rozhodovani(A,B,C,D,E,F,N1,Nejlepsi,ZatimNejelpsi,NejlepsiTah).

rozhodovani(A,B,C,D,E,F,N,Nejlepsi,_,_):-N<7,otahAI(A,B,C,D,E,F,N,A2,B2,C2,D2,E2,F2,"o"),
    vyhraAI(A2,B2,C2,D2,E2,F2),Nejlepsi is N.

rozhodovani(A,B,C,D,E,F,N,Nejlepsi,ZatimNejelpsi,NejlepsiTah):- N<7,N1 is N+1, 
    otahAI(A,B,C,D,E,F,N,A2,B2,C2,D2,E2,F2,"o"),
    
    rozhodovani2(A2,B2,C2,D2,E2,F2,1,_,Score,-61001000,1),
    write('O:'),   write(N),write(': '),write(Score),
    rozhodniSe(Score,ZatimNejelpsi,NejlepsiTah,N,VystupNejScore,VystupNejTah),
    rozhodovani(A,B,C,D,E,F,N1,Nejlepsi,VystupNejScore,VystupNejTah).

rozhodovani2(_,_,_,_,_,_,7,Nejlepsi,Score,NejScore,NejlepsiTah):-Nejlepsi is NejlepsiTah,Score is -NejScore.

rozhodovani2(A,B,C,D,E,F,N,Nejlepsi,NejScore,ZatimNejelpsi,NejlepsiTah):-N<7,N1 is N+1,otahAIKontrola(A,B,C,D,E,F,N),
    rozhodovani2(A,B,C,D,E,F,N1,Nejlepsi,NejScore,ZatimNejelpsi,NejlepsiTah).%,write('kontrola na presah X').

rozhodovani2(A,B,C,D,E,F,N,Nejlepsi,NejScore,_,_):-N<7,otahAI(A,B,C,D,E,F,N,A2,B2,C2,D2,E2,F2,"x"),
    vyhraAI(A2,B2,C2,D2,E2,F2),Nejlepsi is N,NejScore is -400100.%,write('hrozi nebezpeci ze zahraje 4').


rozhodovani2(A,B,C,D,E,F,N,Nejlepsi,NejScore,ZatimNejelpsi,NejlepsiTah):- N<7,N1 is N+1, 
    otahAI(A,B,C,D,E,F,N,A2,B2,C2,D2,E2,F2,"x"),
    zhodnoceni(A2,B2,C2,D2,E2,F2,1,1,VystupHodX,VystupHodO,0,0,0),
    Score is VystupHodX - VystupHodO,
    %write('X:'),write(N),write(': Score:'),write(VystupHodX),write(', '), write(VystupHodO),
    rozhodniSe(Score,ZatimNejelpsi,NejlepsiTah,N,VystupNejScore,VystupNejTah),
    rozhodovani2(A,B,C,D,E,F,N1,Nejlepsi,NejScore,VystupNejScore,VystupNejTah).


otahAIKontrola(A,B,C,D,E,F,N):- xY(N,6,A,B,C,D,E,F,"x").
otahAIKontrola(A,B,C,D,E,F,N):- xY(N,6,A,B,C,D,E,F,"o").
otahAI(A,B,C,D,E,F,1,R,B,C,D,E,F,Symbol):-zmen(A,R,Symbol,1).
otahAI(A,B,C,D,E,F,2,A,R,C,D,E,F,Symbol):-zmen(B,R,Symbol,1).
otahAI(A,B,C,D,E,F,3,A,B,R,D,E,F,Symbol):-zmen(C,R,Symbol,1).
otahAI(A,B,C,D,E,F,4,A,B,C,R,E,F,Symbol):-zmen(D,R,Symbol,1).
otahAI(A,B,C,D,E,F,5,A,B,C,D,R,F,Symbol):-zmen(E,R,Symbol,1).
otahAI(A,B,C,D,E,F,6,A,B,C,D,E,R,Symbol):-zmen(F,R,Symbol,1).           


%toto jsem musel udelat, protoze z nejakeho duvodu to loopovalo do nekonecna kdyz jsem dal podminku score>ZatimNejelpsi
%rozhodniSe(+Score,+ZatimNejelpsi,+NejlepsiTah,+N,-VystupNejScore,-VystupNejTah)
rozhodniSe(Score,ZatimNejelpsi,_,N,VystupNejScore,VystupNejTah):-Score>ZatimNejelpsi,VystupNejScore is Score,VystupNejTah is N.
rozhodniSe(_,ZatimNejelpsi,NejlepsiTah,_,VystupNejScore,VystupNejTah):-VystupNejScore is ZatimNejelpsi, VystupNejTah is NejlepsiTah.






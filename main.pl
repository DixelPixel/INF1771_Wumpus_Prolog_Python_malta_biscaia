
:-dynamic posicao/3.
:-dynamic memory/3.
:-dynamic visitado/2.
:-dynamic certeza/2.
:-dynamic energia/1.
:-dynamic pontuacao/1.
:-dynamic ouros_coletados/1.

:-consult('mapa.pl').

delete([], _, []).
delete([Elem|Tail], Del, Result) :-
    (   \+ Elem \= Del
    ->  delete(Tail, Del, Result)
    ;   Result = [Elem|Rest],
        delete(Tail, Del, Rest)
    ).
	


:- discontiguous reset_game/0.

reset_game :- retractall(memory(_,_,_)), 
			retractall(visitado(_,_)), 
			retractall(certeza(_,_)),
			retractall(energia(_)),
			retractall(pontuacao(_)),
			retractall(posicao(_,_,_)),
			retractall(ouros_coletados(_)),
			assert(energia(100)),
			assert(pontuacao(0)),
			assert(ouros_coletados(0)),
			assert(posicao(1,1, norte)).


:-reset_game.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Controle de Status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%atualiza_qtd_ouros
atualiza_ouros(N) :-
    ouros_coletados(Qtd),
    retract(ouros_coletados(Qtd)),
    NovaQtd is Qtd + N,
    assert(ouros_coletados(NovaQtd)).

%atualiza pontuacao
atualiza_pontuacao(X):- pontuacao(P), retract(pontuacao(P)), NP is P + X, assert(pontuacao(NP)),!.

%atualiza energia
atualiza_energia(N):- energia(E), retract(energia(E)), NE is E + N, 
					(
					 (NE =<0, assert(energia(0)),posicao(X,Y,_),retract(posicao(_,_,_)), assert(posicao(X,Y,morto)),!);
					 (NE >100, assert(energia(100)),!);
					  (NE >0,assert(energia(NE)),!)
					 ).

%verifica situacao da nova posicao e atualiza energia e pontos
verifica_player :- posicao(X,Y,_), tile(X,Y,'P'), atualiza_energia(-100), atualiza_pontuacao(-1000),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'D'), random_between(-80,-50,D), atualiza_energia(D),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'d'), random_between(-50,-25,D), atualiza_energia(D),!.
verifica_player :- posicao(X,Y,Z), tile(X,Y,'T'), 
					map_size(SX,SY), random_between(1,SX,NX), random_between(1,SY,NY),
				retract(posicao(X,Y,Z)), assert(posicao(NX,NY,Z)), atualiza_obs, verifica_player,!.
verifica_player :- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comandos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%virar direita
virar_direita :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.

%virar esquerda
virar_esquerda :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.

%andar
andar :- posicao(X,Y,P), P = norte, map_size(_,MAX_Y), Y < MAX_Y, YY is Y + 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),
         ((retract(visitado(X,YY)), assert(visitado(X,YY))); assert(visitado(X,YY))),
         atualiza_pontuacao(-1),!.
		 
andar :- posicao(X,Y,P), P = sul,  Y > 1, YY is Y - 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),
         ((retract(visitado(X,YY)), assert(visitado(X,YY))); assert(visitado(X,YY))),
         atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = leste, map_size(MAX_X,_), X < MAX_X, XX is X + 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),
         ((retract(visitado(XX,Y)), assert(visitado(XX,Y))); assert(visitado(XX,Y))),
         atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = oeste,  X > 1, XX is X - 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),
         ((retract(visitado(XX,Y)), assert(visitado(XX,Y))); assert(visitado(XX,Y))),
         atualiza_pontuacao(-1),!.
		 
%pegar	
pegar :- posicao(X,Y,_), tile(X,Y,'O'), retract(tile(X,Y,'O')), assert(tile(X,Y,'')), atualiza_pontuacao(-5), atualiza_pontuacao(500),format('[PROLOG-DEBUG] pegou ouro '), atualiza_ouros(1), set_real(X,Y),!.
pegar :- posicao(X,Y,_), tile(X,Y,'U'), retract(tile(X,Y,'U')), assert(tile(X,Y,'')), atualiza_pontuacao(-5), atualiza_energia(50),set_real(X,Y),!. 
pegar :- atualiza_pontuacao(-5),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funcoes Auxiliares de navegação e observação
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		 
%Define as 4 adjacencias		 
adjacente(X, Y) :- posicao(PX, Y, _), map_size(MAX_X,_),PX < MAX_X, X is PX + 1.  
adjacente(X, Y) :- posicao(PX, Y, _), PX > 1, X is PX - 1.  
adjacente(X, Y) :- posicao(X, PY, _), map_size(_,MAX_Y),PY < MAX_Y, Y is PY + 1.  
adjacente(X, Y) :- posicao(X, PY, _), PY > 1, Y is PY - 1.  

%cria lista com a adjacencias
adjacentes(L) :- findall(Z,(adjacente(X,Y),tile(X,Y,Z)),L).

%define observacoes locais
observacao_loc(brilho,L) :- member('O',L).
observacao_loc(reflexo,L) :- member('U',L).

%define observacoes adjacentes
observacao_adj(brisa,L) :- member('P',L).
observacao_adj(palmas,L) :- member('T',L).
observacao_adj(passos,L) :- member('D',L).
observacao_adj(passos,L) :- member('d',L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tratamento de KB e observações
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%consulta e processa observações
atualiza_obs:-adj_cand_obs(LP), observacoes(LO), iter_pos_list(LP,LO), observacao_certeza, observacao_vazia.

%adjacencias candidatas p/ a observacao (aquelas não visitadas)
adj_cand_obs(L) :- findall((X,Y), (adjacente(X, Y), \+visitado(X,Y)), L).

%cria lista de observacoes
observacoes(X) :- adjacentes(L), findall(Y, observacao_adj(Y,L), X).

%itera posicoes da lista para adicionar observacoes
iter_pos_list([], _) :- !.
iter_pos_list([H|T], LO) :- H=(X,Y), 
							((corrige_observacoes_antigas(X, Y, LO),!);
							adiciona_observacoes(X, Y, LO)),
							iter_pos_list(T, LO).							 

%Corrige observacoes antigas na memoria que ficaram com apenas uma adjacencia
corrige_observacoes_antigas(X, Y, []):- \+certeza(X,Y), memory(X,Y,[]).
corrige_observacoes_antigas(X, Y, LO):-
	\+certeza(X,Y), \+ memory(X,Y,[]), memory(X, Y, LM), intersection(LO, LM, L), 
	retract(memory(X, Y, LM)), assert(memory(X, Y, L)).

%Adiciona observacoes na memoria
adiciona_observacoes(X, Y, _) :- certeza(X,Y),!.
adiciona_observacoes(X, Y, LO) :- \+certeza(X,Y), \+ memory(X,Y,_), assert(memory(X, Y, LO)).

%Quando há apenas uma observação e uma unica posição incerta, deduz que a observação está na casa incerta
%e marca como certeza
%observacao_certeza:- findall((X,Y), (adjacente(X, Y), 
%						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,ZZ),ZZ\=[])),
%						memory(X,Y,Z), Z\=[]), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).
						
observacao_certeza:- observacao_certeza('brisa'),
						observacao_certeza('palmas'),
						observacao_certeza('passos').
						
observacao_certeza(Z):- findall((X,Y), (adjacente(X, Y), 
						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,[Z]))),
						memory(X,Y,[Z])), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).						

%Quando posição não tem observações
observacao_vazia:- adj_cand_obs(LP), observacao_vazia(LP).
observacao_vazia([]) :- !.
observacao_vazia([H|T]) :- H=(X,Y), ((memory(X,Y,[]), \+certeza(X,Y),assert(certeza(X,Y)),!);true), observacao_vazia(T).

%Quando posicao é visitada, atualiza memoria de posicao com a informação real do mapa 
set_real(X,Y):- ((retract(certeza(X,Y)), assert(certeza(X,Y)),!); assert(certeza(X,Y))), set_real2(X,Y),!.
set_real2(X,Y):- tile(X,Y,'P'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brisa])),!);assert(memory(X,Y,[brisa]))),!.
set_real2(X,Y):- tile(X,Y,'O'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brilho])),!);assert(memory(X,Y,[brilho]))),!.
set_real2(X,Y):- tile(X,Y,'T'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[palmas])),!);assert(memory(X,Y,[palmas]))),!.
set_real2(X,Y):- ((tile(X,Y,'D'),!); tile(X,Y,'d')), ((retract(memory(X,Y,_)),assert(memory(X,Y,[passos])),!);assert(memory(X,Y,[passos]))),!.
set_real2(X,Y):- tile(X,Y,'U'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[reflexo])),!);assert(memory(X,Y,[reflexo]))),!.
set_real2(X,Y):- tile(X,Y,''), ((retract(memory(X,Y,_)),assert(memory(X,Y,[])),!);assert(memory(X,Y,[]))),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa real
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_player(X,Y) :- posicao(X,Y, norte), write('^'),!.
show_player(X,Y) :- posicao(X,Y, oeste), write('<'),!.
show_player(X,Y) :- posicao(X,Y, leste), write('>'),!.
show_player(X,Y) :- posicao(X,Y, sul), write('v'),!.
show_player(X,Y) :- posicao(X,Y, morto), write('+'),!.

%show_position(X,Y) :- show_player(X,Y),!.
show_position(X,Y) :- (show_player(X,Y); write(' ')), tile(X,Y,Z), ((Z='', write(' '));write(Z)),!.

show_map :- map_size(_,MAX_Y), show_map(1,MAX_Y),!.
show_map(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_position(X,Y), write(' | '), XX is X + 1, show_map(XX, Y),!.
show_map(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_map(1, YY),!.
show_map(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa conhecido
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_mem_info(X,Y) :- memory(X,Y,Z), 
		((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
		((member(brisa, Z), write('P'));write(' ')),
		((member(palmas, Z), write('T'));write(' ')),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!.

show_mem_info(X,Y) :- \+memory(X,Y,[]), 
			((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
			write('     '),!.		
		
		

show_mem_position(X,Y) :- posicao(X,Y,_), 
		((visitado(X,Y), write('.'),!); (certeza(X,Y), write('!'),!); write(' ')),
		write(' '), show_player(X,Y),
		((memory(X,Y,Z),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!);
		(write('   '),!)).

		
show_mem_position(X,Y) :- show_mem_info(X,Y),!.


show_mem :- map_size(_,MAX_Y), show_mem(1,MAX_Y),!.
show_mem(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_mem_position(X,Y), write('|'), XX is X + 1, show_mem(XX, Y),!.
show_mem(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_mem(1, YY),!.
show_mem(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%apagar esta linha - apenas para demonstracao aleatoria
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Lógica do Agente Inteligente
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Verifica se uma célula é segura
% Uma célula é segura se temos certeza sobre ela E não tem perigos (Brisa, Passos, Palmas/Flash)
% Ou se já visitamos ela (então sabemos que é segura)
eh_seguro(X, Y) :- visitado(X,Y), !.
eh_seguro(X, Y) :- certeza(X,Y), memory(X,Y, Conteudo),
                   \+ member(brisa, Conteudo),
                   \+ member(passos, Conteudo),
                   \+ member(palmas, Conteudo).

% Verifica se uma célula é um objetivo de exploração (segura e não visitada)
objetivo_explorar(X, Y) :- map_size(MX, MY),
                           between(1, MX, X), between(1, MY, Y),
                           \+ visitado(X,Y),
                           eh_seguro(X,Y).

% Distância de Manhattan
distancia_manhattan(X1, Y1, X2, Y2, D) :- D is abs(X1 - X2) + abs(Y1 - Y2).

:- discontiguous pegar/0.
:- discontiguous proximo_objetivo/3.

% Encontra a célula segura não visitada mais próxima
melhor_exploracao(TargetX, TargetY) :-
    posicao(CurX, CurY, _),
    findall(D-(X,Y), (objetivo_explorar(X,Y), distancia_manhattan(CurX, CurY, X, Y, D)), Candidates),
    keysort(Candidates, [_-(TargetX, TargetY)|_]).

% PREDICADOS DE RISCO (usados por proximo_objetivo)

% Nível 1: Célula sem nenhuma informação (melhor risco - território desconhecido)
risco_sem_informacao(X, Y) :-
    map_size(MX, MY),
    between(1, MX, X), 
    between(1, MY, Y),
    \+ visitado(X,Y),
    \+ certeza(X,Y),
    \+ memory(X,Y,_).

% Auxiliares de Percepção
tem_brisa(X, Y) :- visitado(X,Y), memory(X,Y,M), member(brisa, M).
tem_passos(X, Y) :- visitado(X,Y), memory(X,Y,M), member(passos, M).
tem_palmas(X, Y) :- visitado(X,Y), memory(X,Y,M), member(palmas, M).

% Buraco Confirmado (Certeza)
eh_buraco(X, Y) :- certeza(X,Y), memory(X,Y,M), member(brisa, M).

% Brisa Explicada: Existe um buraco confirmado adjacente
brisa_explicada(X, Y) :-
    tem_brisa(X, Y),
    adjacente(NX, NY),
    eh_buraco(NX, NY).

% Brisa Não Explicada: Não há buraco confirmado adjacente
brisa_nao_explicada(X, Y) :-
    tem_brisa(X, Y),
    \+ brisa_explicada(X, Y).

% Nível 2.1: Suspeita de Teleportador (Menor risco, apenas deslocamento)
risco_incerto_teleport(X, Y) :-
    map_size(MX, MY),
    between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), \+ certeza(X,Y),
    memory(X,Y,M), member(palmas, M),
    \+ member(brisa, M). % Removido \+ member(passos, M) para ser mais inclusivo

% Nível 2.2: Suspeita de Monstro (Dano, mas não morte instantânea)
risco_incerto_monstro(X, Y) :-
    map_size(MX, MY),
    between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), \+ certeza(X,Y),
    memory(X,Y,M), member(passos, M),
    \+ member(brisa, M).

% Nível 2.3: Suspeita de Buraco - BAIXO RISCO (Brisa Explicada)
% Se a brisa vizinha já tem um "culpado" (buraco confirmado), esta célula pode ser segura.
risco_incerto_buraco_baixo(X, Y) :-
    map_size(MX, MY),
    between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), \+ certeza(X,Y),
    memory(X,Y,M), member(brisa, M),
    % Verifica se TODAS as brisas vizinhas são explicadas
    forall((adjacente(NX,NY), tem_brisa(NX,NY)), brisa_explicada(NX,NY)).

% Nível 2.4: Suspeita de Buraco - ALTO RISCO (Brisa Não Explicada)
% Existe uma brisa vizinha que NÃO tem culpado conhecido -> Esta célula pode ser o buraco!
risco_incerto_buraco_alto(X, Y) :-
    map_size(MX, MY),
    between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), \+ certeza(X,Y),
    memory(X,Y,M), member(brisa, M),
    % Existe pelo menos uma brisa vizinha NÃO explicada
    adjacente(NX,NY), brisa_nao_explicada(NX,NY).

% Nível 2.5: Suspeita Genérica (Qualquer coisa incerta que sobrou)
% Isso garante que QUALQUER incerteza seja melhor que um monstro confirmado
risco_incerto_generico(X, Y) :-
    map_size(MX, MY),
    between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), \+ certeza(X,Y),
    memory(X,Y,M), M \= [].

% Nível 3: Riscos Confirmados
risco_monstro_confirmado(X, Y) :-
    map_size(MX, MY), between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), certeza(X,Y), memory(X,Y,M), member(passos, M).

risco_teleportador_confirmado(X, Y) :-
    map_size(MX, MY), between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), certeza(X,Y), memory(X,Y,M), member(palmas, M).

risco_buraco_confirmado(X, Y) :-
    map_size(MX, MY), between(1, MX, X), between(1, MY, Y),
    \+ visitado(X,Y), certeza(X,Y), memory(X,Y,M), member(brisa, M).


% OBJETIVOS DO AGENTE (em ordem de prioridade)

conhece_pocao(X, Y) :- memory(X, Y, M), member(reflexo, M).
conhece_ouro(X, Y) :- memory(X, Y, M), member(brilho, M).

% 0.5.1 - Já estou na base (1,1) com os ouros -> SAIR
proximo_objetivo(1, 1, sair) :-
    posicao(1, 1, _),
    ouros_coletados(N),
    N >= 3,
    write('[PROLOG-VICTORY] Missão cumprida. Saindo da caverna!'), nl,
    !.

proximo_objetivo(X, Y, pegar) :-
    posicao(X, Y, _), tile(X, Y, 'O'),
    write('[PROLOG-ACTION] Ouro encontrado! Pegando.'), nl, !.

proximo_objetivo(X, Y, pegar) :-
    posicao(X, Y, _), tile(X, Y, 'U'),
    energia(E), E =< 80,
    format('[PROLOG-ACTION] Vida crítica (~w). Bebendo poção.~n', [E]), !.

% ESCAPE
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _), (tile(CurX, CurY, 'D'); tile(CurX, CurY, 'd')),
    adjacente(X, Y),
    ((visitado(X, Y), memory(X, Y, M), \+ member(passos, M), \+ member(brisa, M));
     (\+ visitado(X, Y), eh_seguro(X, Y))),
    write('[PROLOG-ESCAPE] PERIGO! Fugindo para '), write((X,Y)), nl, !.

% VOLTAR BASE
proximo_objetivo(1, 1, ir_para) :-
    ouros_coletados(N), N >= 3, posicao(X, Y, _), (X \= 1; Y \= 1),
    write('[PROLOG-ACTION] Voltando para casa!'), nl, !.

% PRIORIDADE ALTA: Ir pegar ouro conhecido (descoberto mas não coletado)
proximo_objetivo(X, Y, ir_para) :-
    ouros_coletados(N), N < 3,
    posicao(CurX, CurY, _),
    findall(D-(OX,OY), (conhece_ouro(OX,OY), distancia_manhattan(CurX, CurY, OX, OY, D)), Ouros),
    Ouros \= [], keysort(Ouros, [_-(X, Y)|_]),
    format('[PROLOG-GOLD] Indo coletar ouro conhecido em (~w,~w)!~n', [X, Y]), !.

% PRIORIDADE 1: Exploração Segura (Unknown Safe)
proximo_objetivo(X, Y, ir_para) :- melhor_exploracao(X, Y), !.

% PRIORIDADE 2: Sobrevivência (Buscar Poção se vida baixa < 80)
proximo_objetivo(X, Y, ir_para) :-
    energia(E), E < 80,
    posicao(CurX, CurY, _),
    findall(D-(PX,PY), (conhece_pocao(PX,PY), distancia_manhattan(CurX, CurY, PX, PY, D)), Pots),
    Pots \= [], keysort(Pots, [_-(X, Y)|_]),
    format('[PROLOG-SURVIVAL] Vida baixa (~w). Buscando poção em (~w,~w).~n', [E, X, Y]), !.

% PRIORIDADE 4: Explorar Suspeita TELEPORTADOR
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_incerto_teleport(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]), 
    write('[PROLOG-RISK] Investigando suspeita de TELEPORTADOR.'), nl, !.

% PRIORIDADE 5: Explorar Suspeita MONSTRO
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_incerto_monstro(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]), 
    write('[PROLOG-RISK] Investigando suspeita de MONSTRO.'), nl, !.

% Define a posição à frente do agente
frente(FX, FY) :- posicao(X, Y, norte), map_size(_, MY), Y < MY, FY is Y + 1, FX is X.
frente(FX, FY) :- posicao(X, Y, sul), Y > 1, FY is Y - 1, FX is X.
frente(FX, FY) :- posicao(X, Y, leste), map_size(MX, _), X < MX, FX is X + 1, FY is Y.
frente(FX, FY) :- posicao(X, Y, oeste), X > 1, FX is X - 1, FY is Y.

% PRIORIDADE 5.5: Explorar Suspeita BURACO (Prioridade: Frente > Adjacente > Mais Próximo)
% 1. Tenta ir para frente se for suspeita
proximo_objetivo(FX, FY, ir_para) :-
    frente(FX, FY),
    risco_incerto_buraco_baixo(FX, FY),
    write('[PROLOG-RISK] Arriscando BURACO À FRENTE (Risco BAIXO).'), nl, !.

% 2. Tenta ir para qualquer adjacente se for suspeita (excluindo a frente, pois já falhou)
proximo_objetivo(AX, AY, ir_para) :-
    adjacente(AX, AY),
    risco_incerto_buraco_baixo(AX, AY),
    write('[PROLOG-RISK] Arriscando BURACO ADJACENTE (Risco BAIXO).'), nl, !.

% 3. Caso contrário, o mais próximo (lógica original)
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_incerto_buraco_baixo(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]),
    write('[PROLOG-RISK] Arriscando BURACO (Risco BAIXO - Brisa Explicada).'), nl, !.

% PRIORIDADE 6: Explorar Suspeita BURACO (BAIXO RISCO - Explicado)
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_incerto_buraco_baixo(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]), 
    write('[PROLOG-RISK] Arriscando BURACO (Risco BAIXO - Brisa Explicada).'), nl, !.

% PRIORIDADE 7: Explorar Suspeita BURACO (ALTO RISCO - Não Explicado)
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_incerto_buraco_alto(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]), 
    write('[PROLOG-RISK] Arriscando BURACO (Risco ALTO - Brisa NÃO Explicada).'), nl, !.

% PRIORIDADE 3: Explorar SEM INFORMAÇÃO (Unknown No Info)
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_sem_informacao(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]),
    write('[PROLOG-EXPLORE] Explorando desconhecido SEM informação.'), nl, !.

% PRIORIDADE 8: Explorar Suspeita GENÉRICA (Qualquer incerteza restante)
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_incerto_generico(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]), 
    write('[PROLOG-RISK] Arriscando INCERTEZA GENÉRICA (Melhor que monstro confirmado).'), nl, !.

% 7. Riscos Confirmados (Monstro > Teleport > Buraco)
proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_monstro_confirmado(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]),
    write('[PROLOG-RISK] Escolheu MONSTRO CONFIRMADO!'), nl, !.

proximo_objetivo(X, Y, ir_para) :-
    posicao(CurX, CurY, _),
    findall(D-(XX,YY), (risco_teleportador_confirmado(XX,YY), distancia_manhattan(CurX, CurY, XX, YY, D)), Candidates),
    Candidates \= [], keysort(Candidates, [_-(X, Y)|_]), 
    write('[PROLOG-RISK] Escolheu TELEPORTADOR CONFIRMADO!'), nl, !.

% Se não há células seguras para explorar e não pegamos tudo...
% Se estivermos em (1,1), saímos.
proximo_objetivo(1, 1, sair) :- posicao(1,1,_), !.
% Se não estivermos em (1,1), voltamos para lá.
proximo_objetivo(1, 1, ir_para) :- !.

% Interface para o Python chamar e receber a ação imediata ou o alvo
% O Python vai chamar executa_acao(Acao)? Não, mudamos para proximo_objetivo.
% Mas para manter compatibilidade mínima ou facilitar, vamos manter um wrapper se precisar.
% O Python vai chamar `proximo_objetivo(X,Y,Tipo)`.

% Reset game deve resetar ouros
reset_game :- retractall(memory(_,_,_)), 
			retractall(visitado(_,_)), 
			retractall(certeza(_,_)),
			retractall(energia(_)),
			retractall(pontuacao(_)),
			retractall(posicao(_,_,_)),
            retractall(ouros_coletados(_)),
			assert(energia(100)),
			assert(pontuacao(0)),
			assert(posicao(1,1, norte)),
            assert(visitado(1,1)),
            assert(ouros_coletados(0)).





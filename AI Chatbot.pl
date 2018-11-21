:- [names].
:- use_module(library(random)).
:- dynamic usr_name/1, usr_location/1, information/2, feedback/2, alevel/1, loc/1.

chat:-
        print_welcome,
        conversations.

conversations:-
        repeat, 
        print_prompt(you),
        readin(S),
        gen_reply(S,R),
        print_prompt(me),
        write_list(R),
        is_quit(S), 
        print_report, !,halt.

gen_reply(S, R):-
        is_quit(S), !,
        responses_db(bye, Res), 
        random_pick(Res, R).

%gen_reply(S, R):-
%        is_greeting(S), !,
%        responses_db(greeting, Res), 
%        random_pick(Res, R).

gen_reply(S, R):-
        is_thanks(S), !,
        responses_db(thanked, Res), 
        random_pick(Res, R).
gen_reply(S, R):-
        pattern_to_from(S, X, Y), !,
        find_route(X, Y, R).
gen_reply(S, R):-
        pattern_where_is(S, X), !,
        (info(D, X); next(X,_,_,_,_)),
        print_prompt(me),
        write('Where are you at the moment?\n'),
        get_location(2),
        loc(Y),
        find_route(Y, D, R), !,
        retract(loc(Y)).
gen_reply(S, R):-
        question(Tree2, S, _Rest), 
        mapping(s2name,Tree1, Tree2), !,
        sentence(Tree1, Rep,[]),
        append(Rep, ['!'], R).
gen_reply(S, R):-
        pattern_name(S, _), !,
        responses_db(my_name, D),
        random_pick(D, R).
gen_reply(S, R):-
        pattern_my_subjects(S, _), !,
        responses_db(my_subjects, D),
        random_pick(D, R).
gen_reply(S, R):-
        question(Tree2, S, _Rest), !, 
        mapping(s2how,Tree1, Tree2),
        sentence(Tree1, Rep,[]), !,
        append(Rep, ['!'], R).
gen_reply(S, R):-
        pattern_me(S, _), !,
        responses_db(me, D),
        random_pick(D, R).
gen_reply(S, R):-
        sentence(Tree1, S, _Rest), !, 
        mapping(s2why,Tree1, Tree2),
        question(Tree2, Rep,[]),
        append(Rep, ['?'], R).
gen_reply(S, R):-
        question(Tree2, S, _Rest), !, 
        mapping(s2q,Tree1, Tree2),
        sentence(Tree1, Rep,[]),
        append([yes, ','|Rep], ['!'], R).

gen_reply(S, R):-
        \+ is_question(S), 
        \+ information(_, _), !,
        get_info(4),
        responses_db(thanks, D),
        random_pick(D, R).
gen_reply(S, R):-
        \+ is_question(S), 
        \+ feedback(_, _), !,
        get_feedback(4),
        responses_db(thanks, D),
        random_pick(D, R).
gen_reply(S, R):-
        \+ is_question(S), !,
        responses_db(random_q, Res),
        random_pick(Res, R).
gen_reply(S, R):- 
        is_question(S), !,
        responses_db(random_s, Res),
        random_pick(Res, R).


is_greeting(S):-
        greeting_db(D),
        intersect(S, D, A),
        A \== [].


is_question(S):-
        member('?', S).


is_thanks(S):-
        thanks_db(D),
        intersect(S, D, A),
        A \== [].


is_quit(S):- 
        subset([bye], S).


get_location(0).
get_location(N):-
        print_prompt(you),
        readin(L),
        M is N - 1,
        get_location(L, M).
get_location(_, 0).
get_location(X, _):-
        is_valid_loc(X, L), 
        assert(loc(L)), !.
get_location(_, N):- 
        responses_db(get_location, D),
        random_pick(D, R),
        print_prompt(me),
        write_list(R),
        M is N - 1,
        get_location(M).

is_valid_loc([H|_], L):- 
        (info(L, H); next(H,_,_,_,_)), !.
is_valid_loc([_|T], L):-
        is_valid_loc(T, L).


get_feedback(0).
get_feedback(N):-
        questions_db(feedback, D),
        nth_item(D, N, R),
        print_prompt(me),
        write_list(R),
        print_prompt(you),
        readin(S),
        assert(feedback(R, S)),
        M is N - 1,
        get_feedback(M).


get_info(0).
get_info(N):-
        questions_db(info, D),
        nth_item(D, N, Q),
        print_prompt(me),
        write_list(Q),
        print_prompt(you),
        readin(R),
        assert(information(Q, R)),
        get_info(Q, R),
        M is N - 1,
        get_info(M).
get_info(QL, RL):-
        nth_item(QL, 1, Q),
        contains(Q, name), !,
        get_usr_name(Q, RL).
get_info(QL, RL):-
        nth_item(QL, 1, Q),
        contains(Q, subjects), !,
        get_alevel_info_loop(RL).
get_info(QL, RL):-
        nth_item(QL, 1, Q),
        contains(Q, from), !,
        assert(usr_location(RL)).
get_info(_, _).

get_usr_name(Q):-
        print_prompt(you),
        readin(S),
        get_usr_name(Q, S).
get_usr_name(_, RL):-
        is_valid_name(RL), !.
get_usr_name(Q, _):-
        responses_db(get_name, D), 
        random_pick(D, X), 
        print_prompt(me),
        write_list(X),
        get_usr_name(Q).

is_valid_name(NL):-
        nth_item(NL, 1, N),
        name(N),
        assert(usr_name(N)).

get_alevel_info_loop:-
        print_prompt(you),
        readin(S),
        get_alevel_info_loop(S).
get_alevel_info_loop(S):- 
        is_valid_alevel(S), !.
get_alevel_info_loop(_):- 
        responses_db(get_alevels, D),
        random_pick(D, R),
        print_prompt(me),
        write_list(R),
        get_alevel_info_loop.


is_valid_alevel(S):- 
        alevel_db(D),
        intersect(S, D, A),
        A \== [],
        assert(alevel(A)).

print_welcome:-
        responses_db(greeting, D),
        random_pick(D, W),
        print_prompt(me),
        write_list(W), 
        flush_output. 

print_prompt(me):-
        my_icon(X), write(X), write(': '), flush_output.
print_prompt(you):-
        user_icon(X), write(X), write(': '), flush_output.
my_icon('Bot ').
user_icon('User').


random_pick(Res, R):- 
        length(Res, Length),  
        Upper is Length + 1,
        random(1, Upper, Rand),
        nth_item(Res, Rand, R).


print_report:-
        write('\n--- Conversation report ---\n'),
        usr_name(X), usr_location(Y), alevel(Z), 
        write_list(['User name: ', X, '\nFrom: ', Y, '\nStudying: ', Z]),
        retract(usr_name(X)),retract(usr_location(Y)), retract(alevel(Z)), fail.
print_report:-
        nl, feedback(X, Y), write(X), write(' : '), write_list(Y), 
        retract(feedback(X, Y)), fail.
print_report:-
        nl, information(X, Y), write(X), write(' : '), write_list(Y), 
        retract(information(X, Y)), fail.
print_report.



responses_db(bye, [
        ['Bye!'], 
        ['Hope to see you again.'], 
        ['Have a nice day!']
        ]).

responses_db(greeting, [
        ['Hello!'], 
        ['Hello, nice to meet you.'], 
        ['Hi there!'],
        ['Welcome!'],
        ['Good afternoon!'],
        ['Hi.']
        ]).

responses_db(change_topic, [
        ['Do you mind if I ask you some questions?']
        ]).

responses_db(location, [
        ['Where are you at the moment?'],
        ['What room are you in?'],
        ['Where are you?']
        ]).

responses_db(get_location, [
        ['Sorry, I don\'t know where that is.'],
        ['Are you sure that\'s in Q block?'],
        ['That\'s not in Q block...'],
        ['Can you try that again?'],
        ['I don\'t know that room, where is it?']
        ]).

responses_db(get_alevels, [
        ['Haven\'t heard of that one before!'],
        ['That\'s not a real subject...'],
        ['Are you sure?'],
        ['Hold on, I need to know about your subjects!'],
        ['Don\'t you want to tell me your subject?']
        ]).

responses_db(get_name, [
        ['Is that your real name?'],
        ['That\'s not your real name...'],
        ['That can\'t be your name.'],
        ['Just tell me your name...'],
        ['I need to know your name!'],
        ['Come on, what are you called?']
        ]).
            
responses_db(my_name, [
        ['My name is Frank, nice to meet you.'],
        ['I\'m Frank!'],
        ['My name isn\'t important right now.'],
        ['Frank, at your service, how may I help?']
        ]).

responses_db(my_subjects, [
        ['I\'m studying Computer Science!'],
        ['Computer Science - it\'s great.'],
        ['Never mind about my subjects...'],
        ['Computer Science.'],
        ['Why do you want to know what I\'m studying?'],
        ['Never mind that, what do you want?']
        ]).

responses_db(thanks, [
        ['Thanks for the info!'],
        ['Thanks, that\'s helpful.'],
        ['Ok, thanks.'],
        ['Cheers for that.'],
        ['Nice one.'],
        ['Great!'],
        ['Awesome']
        ]).

responses_db(thanked, [
        ['You\'re welcome!'],
        ['Any time.'],
        ['Glad to be of service.'],
        ['No worries.'],
        ['No problem.']
        ]).

responses_db(me, [
        ['I\'m great, thanks for asking.'],
        ['Can\'t complain!'],
        ['Not too bad, yourself?'],
        ['I\'m okay, I suppose...'],
        ['Yep, I\'m fine, how are you?']
        ]).

responses_db(random_q, [
        ['Isn\'t it a nice day?'],
        ['Oh... ok.'],
        ['Do you like your university?'],
        ['Can we be friends?'],
        ['Have you talked to me before?'],
        ['...what do you mean?'],
        ['How impertinent.'],
        ['You\'re quite rude, aren\'t you?'],
        ['Don\'t be silly.'],
        ['This is silly.'],
        ['Lol'],
        [':)'],
        ['Umm.. what?'],
        ['Excuse me?']
        ]).

responses_db(random_s, [
        ['I dunno...'],
        ['Sorry, I can\'t answer that one.'],
        ['Not sure!'],
        ['Can I get a different question?'],
        ['Oh, you\'ll have to ask someone else that.'],
        ['Sorry, I\'m only a simple Frank.'],
        ['Sorry, I can\'t remember everything you said...'],
        ['Can you say that again?'],
        ['Now, there\'s a question.'],
        ['Who knows!'],
        ['No. Just no.'],
        ['Yes, I agree.']
        ]).

questions_db(feedback, [
        ['Okay. Did you find any of the talks interesting?'],
        ['Hmm. Do you think that I am a human?'],
        ['Ok, thanks. Have I been helpful?'],
        ['So, what are your thoughts to make me better?']
        ]).

questions_db(info, [
        ['Okay, so where are you from?'],
        ['Haha, fair enough. Which universities have you applied to?'],
        ['Nice to meet you. So what subjects are you taking?'],
        ['What\'s your name?']
        ]).

greeting_db([
        hello, 
        hi, 
        hey
        ]).

thanks_db([
        thanks,
        thankyou,
        thank,
        cheers
        ]).

alevel_db([math,
        mathematics,
        maths,
        c,
        programming,
        java,
        algorithms,
        engineering,
        software,
        testing,
        prolog,
        ai,
        artificial,
        intelligence,
        parallel,
        data,
        structure,
        computer,
        architecture,
        database,
        networks,
        compiler,
        cloud,
        computing,
        security,
        cyber,
        machine,
        learning,
        ml,
        assembly,
        physics,
        chemistry,
        geography,
        biology,
        history,
        psychology,
        english,
        french,
        spanish,
        german,
        italian,
        music
        ]).


sentence( s(X,Y, is, Z) ) --> belonging_phrase(X), abstract_noun(Y),  
                              [is],  special_noun(Z).

sentence(s(X, Y, Z)) --> subject_pronoun(X), indicative_verb(Y), 
                         adjective(Z).

sentence(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).

sentence(s(X, Y, Z)) --> question(X), determiner(Y), place_name(Z).

sentence(s(X, Y)) --> determiner(X), place_name(Y).

sentence(s(X, Y)) --> subject_tobe_verb(X), prepositional_phrase(Y).

sentence(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).

belonging_phrase(belong(your)) --> [your].
belonging_phrase(belong(my)) --> [my].

abstract_noun(abs_noun(name)) --> [name].

special_noun(sp_noun(justin)) --> [justin].
special_noun(sp_noun(frank)) --> [frank].


subject_phrase(sp(X)) --> subject_pronoun(X).
subject_phrase(sp(X)) --> noun_phrase(X).

object_phrase(op(X,Y)) --> noun_phrase(X), adverb(Y).
object_phrase(op(X, Y)) --> object_pronoun(X), adverb(Y).

noun_phrase(np(X, Y)) --> determiner(X), noun(Y).
noun_phrase(np(Y)) --> noun(Y).

prepositional_phrase(pp(X, Y)) --> preposition(X), place_name(Y).

preposition(prep(in)) --> [in].
preposition(prep(at)) --> [at].
preposition(prep(from)) --> [from].

place_name(pname(reception)) --> [reception].
place_name(pname(cafe)) --> [cafe].
place_name(pname(toilet)) --> [toilet].
place_name(pname(vending_machines)) --> [vending_machines].
place_name(pname(lockers)) --> [lockers].
place_name(pname(exit)) --> [exit].
place_name(pname(london)) --> [london].
place_name(pname(bristol)) --> [bristol].
place_name(pname(exeter)) --> [exeter].
place_name(pname(X)) --> [X], { next(X,_,_,_,_) }.

subject_pronoun(spn(i)) --> [i].
subject_pronoun(spn(we)) --> [we].
subject_pronoun(spn(you)) --> [you].
subject_pronoun(spn(they)) --> [they].
subject_pronoun(spn(he)) --> [he].
subject_pronoun(spn(she)) --> [she].
subject_pronoun(spn(it)) --> [it].
subject_pronoun(spn(who)) --> [who].

object_pronoun(opn(you))--> [you].
object_pronoun(opn(your))--> [your].
object_pronoun(opn(me))--> [me].
object_pronoun(opn(us))--> [us].
object_pronoun(opn(them))--> [them].
object_pronoun(opn(him))--> [him].
object_pronoun(opn(her))--> [her].
object_pronoun(opn(it))--> [it].

determiner(dtmnr([])) --> [].
determiner(dtmnr([a])) --> [a].
determiner(dtmnr([the])) --> [the].
determiner(dtmnr([my])) --> [my].
determiner(dtmnr([some])) --> [some].
determiner(dtmnr([all])) --> [all].
determiner(dtmnr([that])) --> [that].

noun(noun(uwe)) --> [uwe].
noun(noun(cs_course)) --> [cs_course].
noun(noun(robotics_course)) --> [robotics_course].
noun(noun(robotics_course)) --> [computing_course].
noun(noun(robotics_course)) --> [sd_course].
noun(noun(name)) --> [name].

adverb(ad([very, much])) --> [very, much].
adverb(ad([how])) --> [how].
adverb(ad([])) --> [].

verb(vb(like)) --> [like].
verb(vb(love)) --> [love].
verb(vb(is)) --> [is].

indicative_verb(ivb(are)) --> [are].
indicative_verb(ivb(am)) --> [am].

subject_tobe_verb(s_2b([you, are])) --> [you, are].
subject_tobe_verb(s_2b([i,am])) --> [i, am].
subject_tobe_verb(s_2b([we, are])) --> [we, are].

adjective(adj(great)) --> [great].
adjective(adj(good)) --> [good].
adjective(adj(fine)) --> [fine].

question(q(why,do,S)) --> [why, do], sentence(S).
question(q(do,S)) --> [do], sentence(S).

question(q(X, Y, Z)) --> adverb(X), indicative_verb(Y), subject_pronoun(Z).
question( q( what, is, X, Y ) ) -->  [what, is],  belonging_phrase(X),  
                                     abstract_noun(Y).   

/* version 4 add rules for changing a sentence to a question, vice versa */

mapping(s2why, 
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(why,do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2why,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(why,do,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).


mapping(s2q,
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2q,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(do,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).

mapping(s2name,
        s( belong(Y1), abs_noun(X2), is, sp_noun(Y2) ),
        q( what, is, belong(X1), abs_noun(X2) )
        ):-
        mapping_belong(X1, Y1), mapping_noun(X2, Y2).

mapping(s2how,
        s(spn(X1), ivb(Y1), adj(_)),
        q(ad(_), ivb(Y2), spn(Z2))
        ):-
        mapping_spn(X1, Z2), mapping_indicative(Y1, Y2).

mapping_belong(my,your).
mapping_belong(your,my).

mapping_noun(name, frank).
mapping_noun(frank, name).

mapping_indicative(are, am).
mapping_indicative(am, are).

mapping_ad(how, fine).
mapping_ad(fine, how).

mapping_spn(i, you).
mapping_spn(you, i).

mapping_opn(you,me).
mapping_opn(me,you).


intersect([], _, []).
intersect([H|T1], L2, [H|T3]):- 
        member(H, L2), !,
        intersect(T1, L2, T3).
intersect([_|T1], L2, L3):-
        intersect(T1, L2, L3).


write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).


subset([], _).
subset([H|T], L2):- 
        member(H, L2),
        subset(T, L2).

nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
        nth_item(T, N1, X),
        N is N1 + 1.

contains(A, B) :-
  atom(A),
  atom(B),
  name(A, AA),
  name(B, BB),
  contains(AA, BB).
contains(A, B) :-
  atom(A),
  name(A, AA),
  contains(AA, B).
contains(A, B) :-
  sublist(B, A),
  B \= [].

sublist(S, L) :-
  append(_, L2, L),
  append(S, _, L2).


next('exit2', '2q56', east, right, 5).          
next('2q56', 'exit2', west, right, 5).          
next('exit1', 'area1', west, right, 2).
next('area1', 'exit1', east, right, 2).
next('exit2', 'exit1', west, right, 1).
next('exit1', 'exit2', east, right, 1).
next('area1', 'exit3', west, front, 2).
next('exit3', 'area1', east, left, 2).
next('2q56', '2q4', east, left, 3).
next('2q4', '2q56', west, left, 3).
next('junt2', 'junt1', west, front, 5).
next('junt1', 'junt2', east, right, 5).
next('2q4', 'junt1', east, front, 1).
next('junt1', '2q4', west, right, 1).
next('junt1', '2q5', north, left, 2).
next('2q5', 'junt1', south, front, 2).
next('2q5', '2q6', north, left, 5).
next('2q6', '2q5', south, right, 5).
next('2q6', '2q31', north, right, 4).
next('2q31', '2q6', south, right, 4).           
next('2q31', '2q30', north, right, 5).
next('2q30', '2q31', south, left, 5).
next('2q30', '2q9', north, left, 2).
next('2q9', '2q30', south, left, 2).            
next('2q9', '2q29', north, right, 2).
next('2q29', '2q9', south, right, 2).           
next('2q29', 'exit5', north, left, 1).
next('exit5', '2q29', south, left, 1).          
next('exit5', 'junt3', north, front, 12).
next('junt3', 'exit5', south, right, 12).
next('junt3', '2q23', north, right, 6).
next('2q23', 'junt3', south, front, 6).
next('2q23', '2q21', north, right, 1).
next('2q21', '2q23', south, left, 1).
next('2q21', 'exit6', north, front, 1).
next('exit6', '2q21', south, left, 1).
next('junt3', 'area2', east, right,10).
next('area2', 'junt3', west, front,10).
next('area2', '2q24', east, left,2).
next('2q24', 'area2', west, left,2).            
next('2q24', '2q25', east, left,4).
next('2q25', '2q24', west, right,4).
next('junt2','2q52', south, right,3).
next('2q52', 'junt2', north, front,3).
next('2q52', '2q50', south, right, 6).
next('2q50', '2q52', north, left, 6).
next('2q50', '2q42', south, left, 4).
next('2q42', '2q50', north, left, 4).           
next('2q42', '2q43', south, left, 2).
next('2q43', '2q42', north, right, 2).
next('2q43', '2q49', south, right, 1).
next('2q49', '2q43', north, right, 1).          
next('2q49', 'area3', south, left, 1).
next('area3', '2q49', north, left, 1).
next('area3', '2q46', south, left, 3).
next('2q46', 'area3', north, right, 3).
next('2q46', '2q47', south, left, 2).
next('2q47', '2q46', north, right, 2).
next('2q47', '2q48', south, right, 2).
next('2q48', '2q47', north, right, 2).
next('2q48', 'exit4', south, front, 2).
next('exit4', '2q48', north, left, 2).          

info('2q56', reception).
info('2q6', cafe).
info('2q4', toilet).
info('2q5', toilet).
info('2q21', toilet).
info('2q23', toilet).
info('area1', 'vending machine').
info('area3', 'vending machine').
info('area2', lockers).
info('exit1', exit).
info('exit2', exit).
info('exit3', exit).
info('exit4', exit).
info('exit5', exit).
info('exit6', exit).
info('exit1', out).
info('exit2', out).
info('exit3', out).
info('exit4', out).
info('exit5', out).
info('exit6', out).


pattern_to_from([to, X, from, Y |_], Y, X):-!.
pattern_to_from([from, X, to, Y |_], X, Y):-!.
pattern_to_from([at, X, how, do, i, get, to, Y |_], Y, X):-!.
pattern_to_from([from, X, how, do, i, get, to, Y |_], X, Y):-!.
pattern_to_from([_|T], X, Y):-
        pattern_to_from(T, X, Y).


pattern_where_is([where, is, the, X |_], X):-!.
pattern_where_is([where, is, a, X |_], X):-!.
pattern_where_is([where, is, X |_], X):-!.
pattern_where_is([where, can, i, find, the, X |_], X):-!.
pattern_where_is([where, can, i, find, a, X |_], X):-!.
pattern_where_is([where, can, i, find, X |_], X):-!.
pattern_where_is([how, do, i, find, X |_], X):-!.
pattern_where_is([how, do, i, get, X |_], X):-!.
pattern_where_is([how, do, i, get, to, X |_], X):-!.
pattern_where_is([is, there, a, X |_], X):-!.
pattern_where_is([_|T], X):-
        pattern_where_is(T, X).

pattern_name([what, is, your, name, X |_], X):-!.
pattern_name(['what\'s', your, name, X |_], X):-!.
pattern_name([whats, your, name, X |_], X):-!.
pattern_name([what, are, you, called, X |_], X):-!.
pattern_name([who, are, you, X |_], X):-!.
pattern_name([_|T], X):-
        pattern_name(T, X).


pattern_my_subjects([what, are, you, studying, X |_], X):-!.
pattern_my_subjects([what, do, you, study, X |_], X):-!.
pattern_my_subjects([what, course, are, you, on, X |_], X):-!.
pattern_my_subjects([what, is, your, degree, X |_], X):-!.
pattern_my_subjects([_|T], X):-
        pattern_my_subjects(T, X).


pattern_me([how, are, you, X |_], X):-!.
pattern_me([are, you, ok, X |_], X):-!.
pattern_me([you, ok, X |_], X):-!.
pattern_me([you, okay, X |_], X):-!.
pattern_me([_|T], X):-
        pattern_me(T, X).


read_in(P):-initread(L),words(P,L,[]).


%initread([K1,K2|U]):-get(K1),get0(K2),readrest(K2,U). 
initread([K1,K2|U]):-get_code(K1),get_code(K2),readrest(K2,U).

%readrest(46,[]):-!.
readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(10,[]):-!.
%readrest(K,[K1|U]):-K=<32,!,get(K1),readrest(K1,U).
%readrest(_K1,[K2|U]):-get0(K2),readrest(K2,U).
readrest(K,[K1|U]):-K=<32,!,get_code(K1),readrest(K1,U).
readrest(_K1,[K2|U]):-get_code(K2),readrest(K2,U).

words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K+32. 
lc(K,K):-K>96,K<123.


my_filter([],[]).
my_filter(['\n'|T], R):-  !,
        my_filter(T, R).
my_filter([nb(2), X|T], [Rm|R]):- 
        name(X, CharList),
        q_followed_by_nb(CharList),!,
        name(Rm, [50|CharList]),
        my_filter(T, R).
my_filter([X|T], [X|R]):- 
        my_filter(T, R).

q_followed_by_nb([113,X|_]):-
        digit(X).

readin(S):- read_in(L), my_filter(L,S).


find_route(X, Y, R):-
        calc_route(X, Y, start, Dir, Dist, _),
        simp_route(Dir, Dist, NewDir, NewDist),
        print_route(X, Y, NewDir, NewDist, R).


calc_route(X, Y, PreDir, [Dir], [Dist], [X, Y]):- 
        next(X, Y, Dir, _, Dist),
        \+u_turn(PreDir, Dir).
calc_route(X, Y, PreDir, [Dir | SubDir], [Dist | SubDist], [X | SubPath]):-
        next(X, Z, Dir, _, Dist),
        \+u_turn(PreDir, Dir),
        calc_route(Z, Y, Dir, SubDir, SubDist, SubPath).
        

u_turn(south, north). u_turn(north, south).
u_turn(east, west). u_turn(west, east).


simp_route([L1], [L2], [L1], [L2]). 
simp_route([H1, HT1|T1], [H2, HT2|T2], [H1 | SubDir], [H2 | SubDist]):- 
        H1 \== HT1,
        simp_route([HT1|T1], [HT2|T2], SubDir, SubDist).
simp_route([H1, H1|T1], [H2, HT2|T2], SubDir, SubDist):- 
        NewDist is H2 + HT2,
        simp_route([H1|T1], [NewDist | T2], SubDir, SubDist).


print_route(Dest, [Dir], [Dist], String):- !,
        next(_, Dest, Dir, WhichSide, _),
        String = ['walk', Dir, Dist, 'metres.', 'Your destination,'
                 , Dest, ', will be on your', WhichSide, '.'].
print_route(Dest, [Dir | DirList], [Dist | DistList], Str):-    
        Str = ['walk', Dir, Dist, 'metres, then' | SubStr],
        print_route(Dest, DirList, DistList, SubStr).
print_route(Orig, Dest, DirList, DistList, Str):-
        Str = ['From', Orig | SubStr],
        print_route(Dest, DirList, DistList, SubStr).

% auto run code :) BY Ahmed Madbouly
?-chat.


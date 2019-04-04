choices([]).
choices([A]):- write(A).
choices([A|B]):- write(A), write(', '), choices(B), !.

display_choices(breads):- breads(L), write('breads = '), choices(L), nl.
display_choices(mains):- mains(L), write('mains = '), choices(L), nl.
display_choices(cheeses):- cheeses(L), write('cheeses = '), choices(L), nl.
display_choices(vegs):- vegs(L), write('vegs = '), choices(L), nl.
display_choices(sauces):- sauces(L), write('sauces = '), choices(L), nl.
display_choices(cookies):- cookies(L), write('cookies = '), choices(L), nl.
display_choices(addons):- addons(L), write('addons = '), choices(L), nl.

valid_bread(X) :- breads(B), member(X, B), !.
valid_main(X) :- mains(M), member(X, M), !.
valid_cheese(X) :- cheeses(C), member(X, C), !.
valid_veg(X) :- vegs(V), member(X, V), !.
valid_sauce(X) :- sauces(S), member(X, S), !.
valid_cookie(X) :- cookies(C), member(X, C), !.
valid_addon(X) :- addons(A), member(X, A), !.


breads([
    hearty_italian,
    italian_white,
    italian_herb,
    grain_honey,
    honey_oat,
    flat_bread,
    parmesan_oregano,
    monterey_cheddar,
    roasted_garlic_bread
]).
mains([ham, turkey, coldcut, bbqchicken, tuna, eggmayo, meatball]).
cheeses([american, monterrey, none]).
vegs([lettuce, tomato, cucumber, capsicum, onion, jalapeno, pickle, avocado]).
sauces([bbq, honeymustard, mustard, sweetonion, redwine, mayonnaise, chipottle, ranch, vinegar]).
cookies([chocchip, doublechoc, walnut, macnut, raspberry, brownie]).
addons([drink, soup, chips]).


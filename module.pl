/*
 * Helpers for writing menu choices
 */
choices([]). % empty list
choices([A]):- write(A). % end of the list
choices([A|B]):- write(A), write(', '), choices(B), !. % printing sublist

/*
 * Helpers for displaying choices for each menu
 */
display_choices(breads):- breads(L), write('breads = '), choices(L), nl.
display_choices(mains):- mains(L), write('mains = '), choices(L), nl.
display_choices(cheeses):- cheeses(L), write('cheeses = '), choices(L), nl.
display_choices(vegs):- vegs(L), write('vegs = '), choices(L), nl.
display_choices(sauces):- sauces(L), write('sauces = '), choices(L), nl.
display_choices(healthy_sauces):- healthy_sauces(L), write('sauces = '), choices(L), nl.
display_choices(cookies):- cookies(L), write('cookies = '), choices(L), nl.
display_choices(sides):- sides(L), write('sides = '), choices(L), nl.
display_choices(drinks):- drinks(L), write('drinks = '), choices(L), nl.


/*
 * Helpers for validating each menu
 */
valid_bread(X) :- breads(B), member(X, B), !.
valid_main(X) :- mains(M), member(X, M), !.
valid_cheese(X) :- cheeses(C), member(X, C), !.
valid_veg(X) :- vegs(V), member(X, V), !.
valid_sauce(X) :- sauces(S), member(X, S), !.
valid_healthy_sauce(X) :- healthy_sauces(S), member(X, S), !.
valid_cookie(X) :- cookies(C), member(X, C), !.
valid_side(X) :- sides(A), member(X, A), !.
valid_drink(X) :- drinks(D), member(X, D), !.


/*
 * Choices for each menu
 */
breads([hearty_italian, italian_white, italian_herb, grain_honey, honey_oat, flat_bread, parmesan_oregano, monterey_cheddar, roasted_garlic_bread]).
mains([ham, turkey, coldcut, bmt, roast_beef, tuna, steak_and_cheese, subway_melt, subway_club ]).
cheeses([american, monterrey, none]).
vegs([lettuce, tomato, cucumber, capsicum, onion, jalapeno, pickle, avocado]).
sauces([bbq, chili, honeymustard, mustard, sweetonion, redwine, mayonnaise, chipotle, ranch, vinegar]).
healthy_sauces([redwine, chipotle, ranch, vinegar]).
cookies([chocochip, doublechocochip, oatmeal, white_choco_chip, macnut, raspberry, brownie]).
sides([chips, yoghurts, cookies, energy_bar]).
drinks([dasani, fountain, pulpy, green_tea, coffee, tea]).
:- ['module.pl'].

% Declare dynamic predicates to store results
:- dynamic meal/1, bread/1, main/1, cheese/1, veg/1, sauce/1, cookie/1, addon/1.


validate_bread :-
    read(X),
    valid_bread(X) -> write('breads = '), write(X), nl, assert(bread(X));
    write('Invalid bread, try again'), nl,
    validate_bread. 

validate_main :-
    read(X),
    valid_main(X) -> write('main = '), write(X), nl, assert(main(X));
    write('Invalid main, try again'), nl,
    validate_main. 

validate_cheese :-
    read(X),
    valid_cheese(X) -> write('cheese = '), write(X), nl, assert(cheese(X));
    write('Invalid cheese, try again'), nl,
    validate_cheese. 

validate_veg :-
    read(X),
    (not(X == 0) -> 
        (valid_veg(X) -> write('vegs = '), write(X), nl, assert(veg(X));
        write('Invalid veg, try again'), nl),
        validate_veg;
        true
    ).

validate_sauce :-
    read(X),
    (not(X == 0) -> 
        (valid_sauce(X) -> write('sauce = '), write(X), nl, assert(sauce(X));
        write('Invalid sauce, try again'), nl),
        validate_sauce;
        true
    ).

validate_cookie :-
    read(X),
    (not(X == 0) -> 
        (valid_cookie(X) -> write('cookie = '), write(X), nl, assert(cookie(X));
        write('Invalid cookie, try again'), nl),
        validate_cookie;
        true
    ).

validate_addon :-
    read(X),
    (not(X == 0) -> 
        (valid_addon(X) -> write('addon = '), write(X), nl, assert(addon(X));
        write('Invalid addon, try again'), nl),
        validate_addon;
        true
    ).

ask_bread :-
    write('Please choose breads'),nl,
    display_choices(breads),
    validate_bread.

ask_main :-
    write('Please choose main: '),nl,
    display_choices(mains),
    validate_main.

ask_cheese :-
    write('Please choose cheese: '),nl,
    display_choices(cheeses),
    validate_cheese.

ask_veg :-
    write('Please choose veg: '),nl,
    display_choices(vegs),
    validate_veg.

ask_sauce :-
    write('Please choose sauce: '),nl,
    display_choices(sauces),
    validate_sauce.

ask_cookie :-
    write('Please choose cookie: '),nl,
    display_choices(cookies),
    validate_cookie.

ask_addon :-
    write('Please choose addon: '),nl,
    display_choices(addons),
    validate_addon.

meal_normal :-
    ask_bread, ask_main, ask_cheese, ask_veg,
    ask_sauce, ask_cookie, ask_addon.

meal_vegan :-
    ask_bread, ask_veg,
    ask_sauce, ask_addon.

meal_veggie :-
    ask_bread, ask_veg,
    ask_sauce, ask_cookie, ask_addon.

meal_value :-
    ask_bread, ask_main, ask_cheese,
    ask_veg, ask_sauce.


get_choices(Meal, Bread, Main, Cheese, Vegs, Sauces, Cookies, Addons) :-
    findall(X, meal(X), Meal),
    findall(X, bread(X), Bread),
    findall(X, main(X), Main),
    findall(X, cheese(X), Cheese),
    % meal(Meal),
    % bread(Bread),
    % main(Main),
    % cheese(Cheese),
    findall(X, veg(X), Vegs),
    findall(X, sauce(X), Sauces),
    findall(X, cookie(X), Cookies),
    findall(X, addon(X), Addons).

overview :-
    get_choices(Meal, Bread, Main, Cheese, Vegs, Sauces, Cookies, Addons), 
    write('Selected Meal: '), write(Meal), nl,
    write('Your orders:'),nl,
    write('Bread: '), write(Bread), nl,
    write('Main: '), write(Main), nl,
    write('Cheese: '), write(Cheese), nl,
    atomic_list_concat(Vegs, ',', Veg),
    write('Vegs: '), write(Veg), nl,
    atomic_list_concat(Sauces, ',', Sauce),
    write('Sauces: '), write(Sauce), nl,
    atomic_list_concat(Cookies, ',', Cookie),
    write('Cookies: '), write(Cookie), nl,
    atomic_list_concat(Addons, ',', Addon),
    write('Addons: '), write(Addon), nl.


init :-
    write('Please choose meal type [normal, vegan, veggie, value]'), nl,
    read(Meal),

    (Meal == normal ->
        write('meal = '), write(Meal), nl,
        meal_normal, assert(meal(normal));

    Meal == vegan -> 
        write('meal = '), write(Meal), nl,
        meal_vegan, assert(meal(vegan));

    Meal == veggie -> 
        write('meal = '), write(Meal), nl,
        meal_veggie, assert(meal(veggie));

        write('meal = '), write(Meal), nl,
        meal_value, assert(meal(value))
    ),
    nl,
    overview. 

% Flush all choices from memory
flush :- retract(meal(_)), fail.
flush :- retract(bread(_)), fail.
flush :- retract(main(_)), fail.
flush :- retract(cheese(_)), fail.
flush :- retract(veg(_)), fail.
flush :- retract(sauce(_)), fail.
flush :- retract(cookie(_)), fail.
flush :- retract(addon(_)), fail.
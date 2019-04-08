% Load the helpers
:- ['module.pl'].

% Declare dynamic predicates to store results
:- dynamic meal/1, bread/1, main/1, cheese/1, veg/1, sauce/1, cookie/1, side/1, drink/1.

/**
 * Interactive validation for each menu
 */
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
        (valid_veg(X) -> write('vegs = '), write(X), write(' (Enter 0 to conclude veggies)'), nl, assert(veg(X));
        write('Invalid veg, try again'), nl),
        validate_veg;
        true
    ).

validate_sauce :-
    read(X),
    (not(X == 0) -> 
        (valid_sauce(X) -> write('sauce = '), write(X), write(' (Enter 0 to conclude sauces)'), nl, assert(sauce(X));
        write('Invalid sauce, try again'), nl),
        validate_sauce;
        true
    ).

validate_healthy_sauce :-
    read(X),
    (not(X == 0) -> 
        (valid_healthy_sauce(X) -> write('sauce = '), write(X), write(' (Enter 0 to conclude sauces)'), nl, assert(sauce(X));
        write('Invalid sauce, try again'), nl),
        validate_healthy_sauce;
        true
    ).

validate_cookie :-
    read(X),
    (not(X == 0) -> 
        (valid_cookie(X) -> write('cookie = '), write(X), write(' (Enter 0 to conclude cookies)'), nl, assert(cookie(X));
        write('Invalid cookie, try again'), nl),
        validate_cookie;
        true
    ).

validate_side :-
    read(X),
    (not(X == 0) -> 
        (valid_side(X) -> write('sides = '), write(X), write(' (Enter 0 to conclude sides)'), nl, assert(side(X));
        write('Invalid sides, try again'), nl),
        validate_side;
        true
    ).

validate_drink :-
    read(X),
    (not(X == 0) -> 
        (valid_drink(X) -> write('drinks = '), write(X), write(' (Enter 0 to conclude drinks)'), nl, assert(drink(X));
        write('Invalid drinks, try again'), nl),
        validate_drink;
        true
    ).
/**
 * Interactive prompt for each menu
 */
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

ask_healthy_sauce :-
    write('Please choose healthy sauce: '),nl,
    display_choices(healthy_sauces),
    validate_healthy_sauce.

ask_cookie :-
    write('Please choose cookie: '),nl,
    display_choices(cookies),
    validate_cookie.

ask_side :-
    write('Please choose sides: '),nl,
    display_choices(sides),
    validate_side.

ask_drink :-
    write('Please choose drinks: '),nl,
    display_choices(drinks),
    validate_drink.

/**
 * Main flow given the meal type
 */
meal_normal :-
    ask_bread, ask_main, ask_cheese, ask_veg,
    ask_sauce, ask_cookie, ask_side, ask_drink.

meal_vegan :-
    ask_bread, ask_veg,
    ask_healthy_sauce, ask_side, ask_drink.

meal_veggie :-
    ask_bread, ask_veg,
    ask_healthy_sauce, ask_cookie, ask_side, ask_drink.

meal_value :-
    ask_bread, ask_main, ask_cheese,
    ask_veg, ask_sauce, ask_drink.


/**
 * Gathering all user inputs 
*/
get_choices(Meals, Breads, Mains, Cheeses, Vegs, Sauces, Cookies, Sides, Drinks) :-
    findall(X, meal(X), Meals),
    findall(X, bread(X), Breads),
    findall(X, main(X), Mains),
    findall(X, cheese(X), Cheeses),
    findall(X, veg(X), Vegs),
    findall(X, sauce(X), Sauces),
    findall(X, cookie(X), Cookies),
    findall(X, side(X), Sides),
    findall(X, drink(X), Drinks).

/**
 * Display them prettily
 */ 
overview :-
    get_choices(Meals, Breads, Mains, Cheeses, Vegs, Sauces, Cookies, Sides, Drinks), 
    atomic_list_concat(Meals, Meal),
    write('Selected Meal: '), write(Meal), nl,

    write('Your orders:'),nl,
    atomic_list_concat(Breads, Bread),
    write('Bread: '), write(Bread), nl,
    atomic_list_concat(Mains, Main),
    write('Main: '), write(Main), nl,

    atomic_list_concat(Cheeses, Cheese),
    write('Cheese: '), write(Cheese), nl,

    atomic_list_concat(Vegs, ',', Veg),
    write('Vegs: '), write(Veg), nl,
    atomic_list_concat(Sauces, ',', Sauce),
    write('Sauces: '), write(Sauce), nl,
    atomic_list_concat(Cookies, ',', Cookie),
    write('Cookies: '), write(Cookie), nl,
    atomic_list_concat(Sides, ',', Side),
    write('Sides: '), write(Side), nl,
    atomic_list_concat(Drinks, ',', Drink),
    write('Drinks: '), write(Drink), nl.


/**
 * Flush all choices from memory
 */ 
flush :- retract(meal(_)), fail.
flush :- retract(bread(_)), fail.
flush :- retract(main(_)), fail.
flush :- retract(cheese(_)), fail.
flush :- retract(veg(_)), fail.
flush :- retract(sauce(_)), fail.
flush :- retract(cookie(_)), fail.
flush :- retract(side(_)), fail.

start:-
    write('--------------------END SUBWAY--------------------'), nl,
    prompt,
    end.

prompt :-
    write('Please choose meal type (normal, vegan, veggie, value)'), nl,
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

        % else, value
        write('meal = '), write(Meal), nl,
        meal_value, assert(meal(value))
    ),
    nl,
    overview. 

end :-
    write('--------------------END SUBWAY--------------------'), nl,
    flush.
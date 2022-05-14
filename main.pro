% ramazan burak saritas
% 2020400321
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :- .
% predicate that calculates manhattan distance.
manhattan_distance([X1,Y1], [X2,Y2], Distance):-
    X is X2-X1,
    Y is Y2-Y1,
    Distance is abs(X) + abs(Y).

% 10 points
% minimum_of_list(+List, -Minimum) :- .
% predicate that returns the smallest value of a list.
minimum_of_list([Head], Head).
minimum_of_list([Head1,Head2|Tail], Minimum) :-
    % compares first two elements.
    % calls itself recursively with smaller element as head.
	(Head1<Head2, minimum_of_list([Head1|Tail], Minimum);
    minimum_of_list([Head2|Tail], Minimum)).

% additional
% key-distance pair verison of the predicate minimum_of_list.
% compares distances and returns a key-distance pair with smallest distance.
minimum_of_distances([[Key,Head]], [[Key,Head]]).
minimum_of_distances([[Key1,Head1],[Key2,Head2]|Tail], Minimum) :-
    % compares first two elements.
    % calls itself recursively with smaller element as head.
	(Head1<Head2, minimum_of_distances([[Key1,Head1]|Tail], Minimum), !;
    minimum_of_distances([[Key2,Head2]|Tail], Minimum)).

% additional
% predicate that returns the object and its properties depending on given information.
% useful to take advantage of findall function.
object_info([A,Objects,_], Key, Health, Type, Coordinates, Distance_to_agent):-
    Object = Objects.get(Key),
    Health = Object.get(hp),
    Type = Object.get(type),
    Coordinates = [Object.get(x), Object.get(y)],
    Coordinates_agent = [A.get(x), A.get(y)],
    manhattan_distance(Coordinates_agent, Coordinates, Distance_to_agent).

% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
find_nearest_type([A,O,_], ObjectType, ObjKey, Object, Distance):-
    % find all key and distance pairs of objects of given object type.
    findall([Key, Distance_to_agent], object_info([A,O,_], Key, _, ObjectType, _, Distance_to_agent), Bag),
    % find the pair with minimum distance.
    minimum_of_distances(Bag, [[ObjKey, Distance]]),
    Object = O.get(ObjKey).

% additional
% adds actions to move in x axis.
add_action_x(0, []).
add_action_x(Dist_x, ActionList):-
    % adds (distance in x axis) times actions to its local actionlist.
    % sign sensitive.
    (Dist_x<0, abs(Dist_x, X), length(ActionList,X), maplist(=(go_left),ActionList), !; 
    Dist_x>0, length(ActionList,Dist_x), maplist(=(go_right),ActionList), !).

% additional
% adds actions to move in y axis.
add_action_y(0, []).
add_action_y(Dist_y, ActionList):-
    % adds (distance in y axis) times actions to its local actionlist.
    % sign sensitive.
    (Dist_y<0, abs(Dist_y, Y), length(ActionList,Y), maplist(=(go_up),ActionList), !; 
    Dist_y>0, length(ActionList,Dist_y), maplist(=(go_down),ActionList), !).

% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
% moves the agent to the given location.
navigate_to([A,_,_], X, Y, ActionList, DepthLimit):-
    Dist_x is X - A.get(x),
    Dist_y is Y - A.get(y),
    Abs_x is abs(Dist_x),
    Abs_y is abs(Dist_y),
    Total_distance is Abs_x + Abs_y,
    Total_distance =< DepthLimit,
    % creates action list.
    add_action_x(Dist_x, ActionList_x),
    add_action_y(Dist_y, ActionList_y),
    append(ActionList_x, ActionList_y, ActionList).
    % updates location of agent.
    %%%%nb_set_dict(x, A, X),
    %%%%nb_set_dict(y, A, Y).

% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
% navigates to nearest tree and chops it.    
chop_nearest_tree([A,O,T], ActionList) :-
    find_nearest_type([A,O,T], tree, _, Object, Distance),
    Destination_x is Object.get(x),
    Destination_y is Object.get(y),
    Depth_limit is Distance,
    navigate_to([A,O,T], Destination_x, Destination_y, Actions, Depth_limit),
    Current_inventory = A.get(inventory),
    % if agent has stone_axe, adds 2 clicks to the action list.
    % otherwise, adds 4 clicks to the action list.
    findall([stone_axe,Val], get_dict(stone_axe, Current_inventory, Val), Bag_1),
    (Bag_1=[] -> length(Additional_actions, 4), maplist(=(left_click_c),Additional_actions),!;
                length(Additional_actions, 2), maplist(=(left_click_c),Additional_actions)),
    append( Actions, Additional_actions, ActionList).
    
% 10 points
% mine_nearest_stone(+State, -ActionList) :- .
% navigates to nearest stone and mines it. 
mine_nearest_stone([A,O,T], ActionList) :-
    find_nearest_type([A,O,T], stone, _, Object, Distance),
    Destination_x is Object.get(x),
    Destination_y is Object.get(y),
    Depth_limit is Distance+1,
    navigate_to([A,O,T], Destination_x, Destination_y, Actions, Depth_limit),
    Current_inventory = A.get(inventory),
    % if agent has stone_pickaxe, adds 2 clicks to the action list.
    % otherwise, adds 4 clicks to the action list.
    findall([stone_pickaxe,Val], get_dict(stone_pickaxe, Current_inventory, Val), Bag_1),
    (Bag_1=[] -> length(Additional_actions, 4), maplist(=(left_click_c),Additional_actions),!;
                length(Additional_actions, 2), maplist(=(left_click_c),Additional_actions)),
    append( Actions, Additional_actions, ActionList).

% 10 points
% gather_nearest_food(+State, -ActionList) :- .
% navigates the nearest food and gathers it.
gather_nearest_food([A,O,T], ActionList) :-
    find_nearest_type([A,O,T], food, _, Object, Distance),
    Destination_x is Object.get(x),
    Destination_y is Object.get(y),
    Depth_limit is Distance,
    navigate_to([A,O,T], Destination_x, Destination_y, Actions, Depth_limit),
    % adds 1 click to the action list.
    length(Additional_actions, 1), maplist(=(left_click_c),Additional_actions),
    append( Actions, Additional_actions, ActionList).

% additional
requires(stick, [tree]).
requires(stone_pickaxe, [tree, tree, stone]).
requires(stone_axe, [tree, tree, stone]).
requires(castle, [stone, stone, stone]).

% additional
% executes actions temporarily to generate action lists from next states.
% directly using execute_actions did not work for some reason, but this works.
execute_temp(State, ActionList, Next_state):-
    execute_actions(State, ActionList, Next_state).

% additional
% to make it easier to manage in the collect_requirements predicate.
% does the major job of collect_requirements.
% takes a list of requested items to chop or mine and generates action list.
collect_by_list(_, [], []).
collect_by_list(State, [Head|Tail], ActionList):-
    (Head = tree -> chop_nearest_tree(State, ActionList_each),!;
                mine_nearest_stone(State, ActionList_each)),
    execute_temp(State, ActionList_each, Next_State),
    collect_by_list(Next_State, Tail, ActionList_next),
    append(ActionList_each, ActionList_next, ActionList).

% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .
% collects requirements of given item type.
% uses the collect_by_list predicate defined above with the help of requires facts.
collect_requirements(State, ItemType, ActionList) :-
    requires(ItemType, Item_list),
    collect_by_list(State, Item_list, ActionList).

% additional
% useful to check if a possible area is available for a castle.
myblocking(stone).
myblocking(tree).
myblocking(bedrock).
myblocking(cobblestone).
myblocking(food).

% additional
% useful to check if a possible area is available for a castle.
tile_is_occupied(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    get_dict(type, Object, Type),
    myblocking(Type),
    X = Ox, Y = Oy.

% additional
% generates a list of all coordinates of a possible castle.
castle_area([Xmin, Ymin], Coordinates):-
    Coordinates = [[Xmin, Ymin], [X1, Ymin], [Xmax, Ymin],
                    [Xmin, Y1], [X1, Y1], [Xmax, Y1],
                    [Xmin, Ymax], [X1, Ymax], [Xmax, Ymax]],
    X1 is Xmin+1, Xmax is Xmin+2,
    Y1 is Ymin+1, Ymax is Ymin+2.

% additional
% returns the value of the largest [x_min, y_min] pair.
% which is the top left coordinate of a possible castle located on right bottom corner of the map.
coordinates_to_check(X_min, Y_min):-
    width(W), height(H),
    X_min is W-4, Y_min is H-4.

% additional
% generates [x_min, y_min] pairs for all possible 3x3 castle areas in the map recursively.
generate_min_coordinates(X,Y,X,_Ylast,[[X,Y]]).
generate_min_coordinates(X,Y,_Xlast,Y,[[X,Y]]).
generate_min_coordinates(Xfirst, Yfirst, Xlast, Ylast, MinList):-
    Xfirst<Xlast, Yfirst<Ylast,
    Xnext is Xfirst+1, generate_min_coordinates(Xnext, Yfirst, Xlast, Ylast, MinList_next1),
    Ynext is Yfirst+1, generate_min_coordinates(Xfirst, Ynext, Xlast, Ylast, MinList_next2),
    append(MinList_next1, MinList_next2, MinList_next),
    append([[Xfirst, Yfirst]], MinList_next ,MinList).

% additional
% checks if a given castle area coordinate list is available.
area_is_available([], _State).
area_is_available([[X_min,Y_min]|Tail],State):-
    % iterates over all coordinate pairs recursively.
    not(tile_is_occupied(X_min, Y_min, State)), 
    area_is_available(Tail, State).

% additional
% takes the list of all possible [x_min, y_min] pairs.
% checks if the castle area of those pairs are available.
% does the major job of find_castle_location predicate with one extra parameter.
find_available_area(State, [[X_head, Y_head]|Tail], Xmin, Ymin, Xmax, Ymax):-
    castle_area([X_head, Y_head], Coordinates),
    (area_is_available(Coordinates, State),
    Xmin is X_head, Ymin is Y_head,
    Xmax is Xmin+2, Ymax is Ymin+2),!;
    % if the area of head pair is not available, checks next one recursively.
    find_available_area(State, Tail, Xmin, Ymin, Xmax, Ymax).

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
% finds an available 3x3 area to build a castle.
find_castle_location(State, Xmin, Ymin, Xmax, Ymax):-
    coordinates_to_check(Xlast, Ylast),
    generate_min_coordinates(1,1,Xlast,Ylast,MinList),
    find_available_area(State, MinList, Xmin, Ymin, Xmax, Ymax).

% additional
% mines stones to be able to make castle.
prepare_stones([A,O,T], ActionList):-
    % if cobblestone key exists in inventory.
    (Inventory = A.get(inventory),
        Number_of_cobblestones is Inventory.get(cobblestone),
        Required_cobblestones is 9-Number_of_cobblestones,
            (Required_cobblestones=<0 -> collect_by_list([A,O,T], [], ActionList),!),
            (Required_cobblestones=<3 -> collect_by_list([A,O,T], [stone], ActionList),!),
            (Required_cobblestones=<6 -> collect_by_list([A,O,T], [stone, stone], ActionList),!),
            (Required_cobblestones=<9 -> collect_by_list([A,O,T], [stone, stone, stone], ActionList),!)
    ),!;
    % else.
    collect_requirements([A,O,T], castle, ActionList),!.

% 15 points
% make_castle(+State, -ActionList) :- .
% creates action list to make a castle from initial state to final state.
% mines stones, finds available castle location, navigates to the center of it and builds a castle.
make_castle(State, ActionList):-
    prepare_stones(State, ActionList_1),
    execute_temp(State, ActionList_1, NextState_1),
    find_castle_location(NextState_1, Xmin, Ymin, _Xmax, _Ymax),
    X_center is Xmin + 1, 
    Y_center is Ymin + 1,
    navigate_to(NextState_1, X_center, Y_center, ActionList_2, 1000000),
    execute_temp(NextState_1, ActionList_2, _NextState_2),
    ActionList_3 = [place_n, place_ne, place_e, place_se, place_s, place_sw, place_w, place_nw, place_c],
    append(ActionList_1, ActionList_2, ActionList_12),
    append(ActionList_12, ActionList_3, ActionList).
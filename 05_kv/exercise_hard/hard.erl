-module(hard).

-export([group_by/1, users/0, sessions/0, group_by/2, by_age_value/1, by_age_range/1, by_node/1, by_type/1]).


users() ->
    [
        {user, "Bob", 21, male},
        {user, "Bill", 29, male},
        {user, "Helen", 17, female},
        {user, "Kate", 25, female},
        {user, "John", 70, male}
    ].


sessions() ->
    [
        {session, type_a, node_1, 1},
        {session, type_b, node_1, 2},
        {session, type_a, node_2, 3},
        {session, type_b, node_2, 4}
    ].


group_by(Users) ->
    lists:foldl(
        fun({user, Name, Age, Gender}, Map) ->
            List =
                if
                    Gender == male -> maps:get(male, Map);
                    Gender == female -> maps:get(female, Map)
                end,
            maps:put(Gender, [{user, Name, Age, Gender} | List], Map)
        end,
        #{male => [], female => []}, Users).


group_by(CriteriaFun, List) ->
    lists:foldl(
        fun
            (Item, Groups) ->
                Group = CriteriaFun(Item),
                Groups#{Group => [Item | maps:get(Group, Groups, [])]}
        end,
        #{},
        List
    ).

by_age_value({user, _, Age, _}) -> Age.

by_age_range({user, _, Age, _}) ->
    if
        Age =< 12 -> child;
        Age =< 18 -> teeneage;
        Age =< 25 -> young;
        Age =< 60 -> adult;
        Age > 60 -> old
    end.

by_node({session, _, Node, _}) -> Node.

by_type({session, Type, _, _}) -> Type.

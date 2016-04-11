-module(matrix_zipper_tests).

-include_lib("eunit/include/eunit.hrl").


move_test() ->
    Z = matrix_zipper:from_matrix(
           [[ 1, 2, 3, 4],
            [ 5, 6, 7, 8],
            [ 9,10,11,12],
            [13,14,15,16]]
           ),
    Actions = [
               {get, 1},
               right, {get, 2},
               right, {get, 3},
               right, {get, 4},
               right, {get, 4},
               down, {get, 8},
               down, {get, 12},
               down, {get, 16},
               down, {get, 16},
               left, {get, 15},
               up, {get, 11},
               left, {get, 10},
               up, {get, 6},
               up, {get, 2},
               up, {get, 2},
               left, {get, 1},
               left, {get, 1}
              ],
    lists:foldl(fun check/2, Z, Actions),
    ok.


set_test() ->
    Z1 = matrix_zipper:from_matrix(
           [[ 1, 2, 3, 4],
            [ 5, 6, 7, 8],
            [ 9,10,11,12],
            [13,14,15,16]]
           ),
    Actions = [
               {set, a},
               down, {set, b},
               down, {set, c},
               down, {set, d},
               right, up, up, up, {set, aa},
               down, {set, bb},
               down, {set, cc},
               down, {set, dd},
               right, up, up, up, {set, aaa},
               right, {set, aaaa},
               down, left, {set, bbb},
               right, {set, bbbb},
               down, left, {set, ccc},
               right, {set, cccc},
               down, {set, dddd},
               left, {set, ddd}
              ],
    Z2 = lists:foldl(fun check/2, Z1, Actions),
    ?assertEqual(
           [[a, aa, aaa, aaaa],
            [b, bb, bbb, bbbb],
            [c, cc, ccc, cccc],
            [d, dd, ddd, dddd]],
       matrix_zipper:to_matrix(Z2)),
    ok.


check({get, Res}, Z) ->
    ?assertEqual(Res, matrix_zipper:get(Z)),
    Z;
check({Action, Arg}, Z) ->
    matrix_zipper:Action(Z, Arg);
check(Action, Z) ->
    matrix_zipper:Action(Z).
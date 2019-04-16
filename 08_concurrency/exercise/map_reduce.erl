-module(map_reduce).

-export([start/0, start/1, collector/3, count_words/1]).


start() -> start(["data1.txt", "data2.txt", "data3.txt", "data4.txt", "data5.txt"]).

start(Files) ->
    register(collector, spawn(?MODULE, collector, [length(Files), #{}, self()])),
    lists:foreach(fun thread/1, Files),
    receive
        {total_amount, Amount} -> Amount;
        _ -> error
    end.


thread(File) ->
    spawn(?MODULE, count_words, [File]).


count_words(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            Words = binary:split(
                Content,
                [<<" ">>, <<"\n">>, <<"\r">>],
                [global]
            ),
            Count = get_word_amount(Words, #{}),
            collector ! {amount, Count};
        {error, Exception} ->
            collector ! {error, Exception}
    end.


get_word_amount(Words, Count) ->
    lists:foldl(
        fun(Word, Amount) ->
            Amount#{Word => maps:get(Word, Amount, 0) + 1}
        end,
        Count,
        Words
    ).


collector(Len, StartAmount, ParentPid) ->
    if
        Len == 0 ->
            ParentPid ! {total_amount, StartAmount};
        true ->
            receive
                {amount, Count} ->
                    NewAmount = maps:fold(
                        fun(Word, Amount, TotalAmount) ->
                            TotalAmount#{
                                Word => maps:get(Word, TotalAmount, 0) + Amount
                            }
                        end,
                        StartAmount,
                        Count
                    ),
                    collector(Len - 1, NewAmount, ParentPid);
                {error, Exception} ->
                    io:format("Error: ~p~n", [Exception]),
                    collector(Len - 1, StartAmount, ParentPid)
            end
    end.

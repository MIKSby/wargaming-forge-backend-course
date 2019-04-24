-module(chat_room_manager).

-export([start/0,
    create_room/2, remove_room/2, get_rooms/1,
    add_user/3, remove_user/3, get_users_list/2,
    send_message/4, get_messages_history/2, loop/1, handle_call/2]).


start() ->
    spawn(?MODULE, loop, [#{}]).


handle_call({create_room, RoomName}, State) ->
    case maps:size(State) of
        Size when Size < 5 ->
            RoomId = make_ref(),
            {{ok, RoomId}, State#{RoomId => {RoomName, [], []}}};
        _ -> {{error, room_limit}, State}
    end;

handle_call({remove_room, RoomId}, State) ->
    case maps:find(RoomId, State) of
        {ok, _} -> {ok, maps:remove(RoomId, State)};
        _ -> {{error, room_not_found}, State}
    end;

handle_call({get_rooms}, State) ->
    Resp = maps:fold(
        fun(RoomId, Room, Acc) ->
            {RoomName, _, _} = Room,
            [{RoomId, RoomName} | Acc] end, [], State),
    {Resp, State};

handle_call({add_user, RoomId, UserName}, State) ->
    case maps:find(RoomId, State) of
        {ok, {RoomName, Users, History}} ->
            case lists:member(UserName, Users) of
                true -> {{error, user_is_in_room}, State};
                false ->
                    {ok, maps:put(RoomId,
                        {RoomName, [UserName | Users], History},
                        State)}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({remove_user, RoomId, UserName}, State) ->
    case maps:find(RoomId, State) of
        {ok, {RoomName, Users, History}} ->
            case lists:member(UserName, Users) of
                false -> {{error, user_not_in_room}, State};
                true -> {ok, State#{RoomId => {RoomName, lists:delete(UserName, Users), History}}}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({get_users_list, RoomId}, State) ->
    case maps:find(RoomId, State) of
        {ok, {_, Users, _}} ->
            {{ok, Users}, State};
        error ->
            {{error, room_not_found}, State}
    end;

handle_call({send_message, RoomId, UserName, Message}, State) ->
    case maps:find(RoomId, State) of
        {ok, {RoomName, Users, History}} ->
            case lists:member(UserName, Users) of
                true ->
                    {ok, State#{RoomId => {RoomName, Users, [{UserName, Message} | History]}}};
                false ->
                    {{error, user_not_in_room}, State}
            end;
        error ->
            {{error, room_not_found}, State}
    end;

handle_call({get_messages_history, RoomId}, State) ->
    case maps:find(RoomId, State) of
        {ok, {_, _, History}} ->
            {{ok, History}, State};
        error ->
            {{error, room_not_found}, State}
    end.

loop(State) ->
    receive
        {Msg, From} ->
            {Reply, State2} = ?MODULE:handle_call(Msg, State),
            From ! {reply, Reply},
            ?MODULE:loop(State2);
        _Any -> ?MODULE:loop(State)
    end.


call(Server, Cmd) ->
    Server ! {Cmd, self()},
    receive
        {reply, Reply} -> Reply
    after 5000 ->
        no_reply
    end.


create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
    call(Server, {get_rooms}).


add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).

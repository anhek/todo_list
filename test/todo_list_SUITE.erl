%%%-------------------------------------------------------------------
%%% File    : todo_list_SUITE.erl
%%%-------------------------------------------------------------------
-module(todo_list_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(DB, todo_list_db).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(todo_list),
    {ok, _} = pgapp:equery(?DB, <<"DELETE FROM public.item">>, []),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    {ok, _} = pgapp:equery(?DB, <<"DELETE FROM public.item">>, []),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(delete_item_test_case, Config) ->
    {ok, _, _, [{Id}]} = pgapp:equery(?DB,
        <<"INSERT INTO public.item (text) VALUES ($1) RETURNING id">>, [<<"del_test">>]),
    [{item_id, Id} | Config];
init_per_testcase(mark_item_test_case, Config) ->
    {ok, _, _, [{Id}]} = pgapp:equery(?DB,
        <<"INSERT INTO public.item (text) VALUES ($1) RETURNING id">>, [<<"mark_test">>]),
    [{item_id, Id} | Config];
init_per_testcase(mark_item_test_case, Config) ->
    {ok, _, _, [{Id}]} = pgapp:equery(?DB,
        <<"INSERT INTO public.item (text) VALUES ($1) RETURNING id">>, [<<"mark_test">>]),
    [{item_id, Id} | Config];
init_per_testcase(get_items_test_case, Config) ->
    {ok, _} = pgapp:equery(?DB, <<"DELETE FROM public.item">>, []),
    {ok, _} = pgapp:equery(?DB,
        <<"INSERT INTO public.item (text, deleted) VALUES
        ($1, false),
        ($1, true),
        ($1, false)">>, [<<"get_items_test">>]),
    [{item_count, 2} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        add_item_test_case,
        delete_item_test_case,
        mark_item_test_case,
        get_items_test_case
    ].

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

add_item_test_case(_Config) ->
    Text1 = <<"text_a1">>,
    {ok, Id1} = todo_list:add_item(Text1),
    {ok, Cols, Rows} = pgapp:equery(?DB, <<"SELECT * FROM public.item">>, []),
    DbRes = rows_to_maps(Cols, Rows),
    ?assertEqual(1, length(DbRes)),
    [Item] = DbRes,
    ?assertEqual(Id1, maps:get(<<"id">>, Item)),
    ?assertEqual(false, maps:get(<<"is_done">>, Item)),
    ?assertEqual(false, maps:get(<<"deleted">>, Item)),
    ?assertEqual(Text1, maps:get(<<"text">>, Item)),
    ok.

delete_item_test_case(Config) ->
    Id = proplists:get_value(item_id, Config),
    ok = todo_list:delete_item(Id),
    {ok, Items} = todo_list:get_items(),
    lists:foreach(fun(#{<<"id">> := ItemId}) ->
        ?assertNotEqual(Id, ItemId)
    end, Items),
    {ok, Cols, Rows} = pgapp:equery(?DB, <<"SELECT * FROM public.item WHERE id = $1">>, [Id]),
    DbRes = rows_to_maps(Cols, Rows),
    ?assertEqual(1, length(DbRes)),
    [Item] = DbRes,
    ?assertEqual(Id, maps:get(<<"id">>, Item)),
    ?assertEqual(true, maps:get(<<"deleted">>, Item)),

    ?assertEqual({error, item_not_found}, todo_list:delete_item(999999)),

    ok.

mark_item_test_case(Config) ->
    Id = proplists:get_value(item_id, Config),
    ok = todo_list:mark_item(Id, true),
    {ok, Cols1, Rows1} = pgapp:equery(?DB, <<"SELECT * FROM public.item WHERE id = $1">>, [Id]),
    DbRes1 = rows_to_maps(Cols1, Rows1),
    ?assertEqual(1, length(DbRes1)),
    [Item1] = DbRes1,
    ?assertEqual(Id, maps:get(<<"id">>, Item1)),
    ?assertEqual(true, maps:get(<<"is_done">>, Item1)),

    {ok, Cols2, Rows2} = pgapp:equery(?DB, <<"SELECT * FROM public.item WHERE id = $1">>, [Id]),
    DbRes2 = rows_to_maps(Cols2, Rows2),
    ?assertEqual(1, length(DbRes2)),
    [Item2] = DbRes2,
    ?assertEqual(Id, maps:get(<<"id">>, Item2)),
    ?assertEqual(true, maps:get(<<"is_done">>, Item2)),

    ?assertEqual({error, item_not_found}, todo_list:mark_item(999999, false)),

    ok.

get_items_test_case(Config) ->
    ExpectedCount = proplists:get_value(item_count, Config),
    {ok, Items} = todo_list:get_items(),
    ?assertEqual(ExpectedCount, length(Items)),
    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

rows_to_maps(Columns, Rows) ->
    ColumnNames = [X || #column{name = X} <- Columns],
    [maps:from_list(lists:zip(ColumnNames, tuple_to_list(Row))) || Row <- Rows].

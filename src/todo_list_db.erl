-module(todo_list_db).

-export([get_items/0, add_item/1, mark_item/2, delete_item/1]).

-include_lib("epgsql/include/epgsql.hrl").

-define(DB, todo_list_db).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_items() -> {ok, [map()]}.
get_items() ->
    {ok, Cols, Rows} = pgapp:equery(?DB, <<"SELECT id, text, is_done FROM public.item WHERE deleted = false">>, []),
    Result = rows_to_maps(Cols, Rows),
    {ok, Result}.

-spec add_item(Text :: binary()) -> {ok, Id :: non_neg_integer()}.
add_item(Text) ->
    {ok, _, _, [{Id}]} = pgapp:equery(?DB, <<"INSERT INTO public.item (text) VALUES ($1) RETURNING id">>, [Text]),
    {ok, Id}.

-spec mark_item(ItemId :: non_neg_integer(), IsDone :: boolean()) -> ok | {error, item_not_found}.
mark_item(ItemId, IsDone) ->
    {ok, Count} = pgapp:equery(?DB, <<"UPDATE public.item SET is_done = $2 WHERE id = $1">>, [ItemId, IsDone]),
    case Count of
        1 -> ok;
        0 -> {error, item_not_found}
    end.

-spec delete_item(ItemId :: non_neg_integer()) -> ok | {error, item_not_found}.
delete_item(ItemId) ->
    {ok, Count} = pgapp:equery(?DB,
        <<"UPDATE public.item SET deleted = true WHERE id = $1">>, [ItemId]),
    case Count of
        1 -> ok;
        0 -> {error, item_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rows_to_maps(Columns, Rows) ->
    ColumnNames = [X || #column{name = X} <- Columns],
    [maps:from_list(lists:zip(ColumnNames, tuple_to_list(Row))) || Row <- Rows].

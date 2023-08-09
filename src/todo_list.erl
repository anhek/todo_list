-module(todo_list).

-export([get_items/0, add_item/1, mark_item/2, delete_item/1]).

-type item_id() :: non_neg_integer().
-type item() :: #{binary() => binary()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec get_items() -> {ok, [item()]}.
%% @doc Get all not deleted items
get_items() ->
    todo_list_db:get_items().

%% @doc Add item to list
%% @param Text Text of item
-spec add_item(Text :: binary()) -> {ok, ItemId :: item_id()}.
add_item(Text) ->
    todo_list_db:add_item(Text).

%% @doc Mark item as done or not done
%% @param ItemId Item identifier
%% @param IsDone Boolean flag
-spec mark_item(Id :: item_id(), IsDone :: boolean()) -> ok | {error, item_not_found}.
mark_item(ItemId, IsDone) ->
    todo_list_db:mark_item(ItemId, IsDone).

-spec delete_item(Id :: item_id()) -> ok | {error, item_not_found}.
%% @doc Delete item from list
%% @param ItemId Item identifier
delete_item(ItemId) ->
    todo_list_db:delete_item(ItemId).

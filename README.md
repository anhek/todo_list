todo_list
=====


Prerequisites
-----
- [rebar3](https://rebar3.org/) installed
- [postgresql](https://www.postgresql.org/) installed

Build
-----

Create 2 postgresql databases (main and test).

Create db structure from file **priv/db_struct.sql** for both databases.

Make changes to config files located in **env/** directory to match your db credentials.

*Postgres 15 may require additional access management if user in app and user that created db is not same.   


    $ rebar3 compile

Docs
-----
Can be found [here](doc/index.html) or generated via:
    
    $ rebar3 edoc

Usage
-----

    $ rebar3 shell

    >todo_list:get_items().
    {ok,[]}

    >todo_list:add_item(<<"text 1">>). 
    {ok,5}

    >todo_list:add_item(<<"text 2">>).
    {ok,6}

    >todo_list:get_items().
    {ok,[
    #{<<"id">> => 5,<<"is_done">> => false,<<"text">> => <<"text 1">>},
    #{<<"id">> => 6,<<"is_done">> => false,<<"text">> => <<"text 2">>}
    ]}

    >todo_list:mark_item(5, true).
    ok

    >todo_list:get_items().
    {ok,[
    #{<<"id">> => 5,<<"is_done">> => true,<<"text">> => <<"text 1">>},
    #{<<"id">> => 6,<<"is_done">> => false,<<"text">> => <<"text 2">>}
    ]}

    >todo_list:delete_item(5).
    ok
    
    todo_list:get_items().
    {ok,[#{<<"id">> => 6,<<"is_done">> => false,<<"text">> => <<"text 2">>}]}

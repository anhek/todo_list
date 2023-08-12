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

Run with docker:
-----

Build:
---
    $ docker build -t todo_list .

Run:
---
    $ docker compose --env-file .env up -d

    $ docker compose ls
    NAME                STATUS              CONFIG FILES
    todo_list           running(2)          ****\todo_list\compose.yaml

Attach to erlang console:
---
    $ docker exec -ti todo_list-erlang-1 /todo_list/bin/todo_list remote_console

    Eshell V13.2.2.2  (abort with ^G)
    (todo_list@1667a075a07e)1> todo_list:get_items().
    {ok,[]}

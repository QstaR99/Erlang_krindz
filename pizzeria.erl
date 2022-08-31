-module(pizzeria).
-export([open/0, close/0, order/1, where_is_my_pizza/0]).

cook(margherita) ->
    io:format("~p: Cooking margherita...\n",[self()]),
    timer:sleep(500),
    io:format("~p: Margherita is ready!\n",[self()]);

cook(calzone) ->
    io:format("~p: Cooking calzone...\n",[self()]),
    timer:sleep(600),
    io:format("~p: Calzone is ready!\n",[self()]).

find_order([],_) -> [];
find_order(Orders, Order) ->
    [{Current_Order, Client, Pizza} | Rest_of_them] = Orders,
    if 
        Current_Order == Order -> {Current_Order, Client, Pizza, Rest_of_them};
        true -> find_order(Rest_of_them, Order)
    end.

find_order_by_client([], _) -> nothing_was_ordered;
find_order_by_client(Orders, Client)->
    [{_, Current_client, Pizza} | Rest_of_them] = Orders,
    if
        Current_client == Client -> Pizza;
        true -> find_order_by_client(Rest_of_them, Client)
    end.

pizzeria(Orders) ->
    receive
        {order, Client, Pizza} when (Pizza == margherita orelse Pizza == calzone) ->
            {_, New_order} = spawn_monitor(fun() -> cook(Pizza) end),
            pizzeria([{New_order, Client, Pizza} | Orders]);
        {order, _, _} ->
            io:format("~p: This pizza is not on our menu!", [self()]),
            pizzeria(Orders);
        {'DOWN', Order, _, _, _} ->
            {_, Client, Pizza, Rest_of_them} = find_order(Orders, Order),
            Client ! {delivered, Pizza},
            pizzeria(Rest_of_them);
        {what_takes_so_long, Client} ->
            Waiting_order = find_order_by_client(Orders, Client),
            if 
                Waiting_order == nothing_was_ordered->Client ! Waiting_order;
                true -> Client ! {cooking, Waiting_order}
            end,
            pizzeria(Orders);
        close -> clozed
    end.


open() ->
    Pid = spawn(fun() -> pizzeria([]) end),
    io:fwrite("Pizzeria is open!\n"),
    register(pizzeria, Pid).

close() ->
    pizzeria ! close,
    io:fwrite("Pizzeria is closed!\n"),
    close.

order(Pizza) when not is_atom(Pizza) -> io:format("~p: Wrong input!\n",[self()]);
order(Pizza)->
    pizzeria ! {order, self(), Pizza},
    io:fwrite("Order received!\n").

where_is_my_pizza()->
    pizzeria ! {what_takes_so_long, self()}.
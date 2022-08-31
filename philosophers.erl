-module(philosophers).
-export([stop/0, run/1]).

philosopher(ID) ->
    think(ID),
    eat(ID),
    philosopher(ID).

think(ID) ->
    io:format("~p: I am thinking... \n", [ID]),
    timer:sleep(rand:uniform(3000) + 1500),
    io:format("~p: I am done thinking! \n", [ID]).

eat(ID) ->
    waiter ! {take, self(), ID},
    receive
        {ok, Fork_id} ->
            io:format("~p: I took fork: ~w! \n", [ID, Fork_id]),
            io:format("~p: I am eating... \n", [ID]),
            timer:sleep(rand:uniform(4000) + 1500),
            io:format("~p: I am done eating! \n", [ID]),
            io:format("~p: I am giving back fork ~w! \n", [ID, Fork_id]),
            waiter ! {give_back, self(), Fork_id};
        no_forks_for_you -> eat(ID);
        stop -> 
            io:format("~p: I am goint out! \n", [ID]),
            exit(normal)
    end.

waiter(Number_of_philosophers, Free_forks) ->
    receive
        {take, Pid, ID} ->
            Second_fork = get_second_fork(ID, Number_of_philosophers),
            First_available = lists:member(ID, Free_forks),
            Second_available = lists:member(Second_fork, Free_forks),
            if
                First_available and Second_available ->
                    Rand_v = rand:uniform(),
                    if
                        Rand_v =< 0.5 ->
                            Pid ! {ok, Second_fork},
                            waiter(Number_of_philosophers, lists:delete(Second_fork, Free_forks));
                        true ->
                            Pid ! {ok, ID},
                            waiter(Number_of_philosophers, lists:delete(ID, Free_forks))
                    end;
                First_available ->
                    Pid ! {ok, ID},
                    waiter(Number_of_philosophers, lists:delete(ID, Free_forks));
                Second_available ->
                    Pid ! {ok, Second_fork},
                    waiter(Number_of_philosophers, lists:delete(Second_fork, Free_forks));
                true->
                    Pid ! no_forks_for_you,
                    waiter(Number_of_philosophers, Free_forks)
            end;
        {give_back, _, Fork_id} -> waiter(Number_of_philosophers, Free_forks ++ [Fork_id]);
        stop -> 
            kill(),
            io:format("~p: My job here is done!\n", [self()]),
            exit(normal)
    end.


get_second_fork(ID, Number_of_philosophers) when ID == Number_of_philosophers - 1 -> 0;
get_second_fork(ID, _) -> ID + 1.


kill() ->
    receive
        {_, Pid, _} -> Pid ! stop, kill()
    after 30000 -> all_processes_killed
    end.

stop() ->
    waiter ! stop,
    closed.

run(Number_of_philosophers) ->
    Ids = lists:seq(0, Number_of_philosophers - 1),
    Waiter = spawn(fun() -> waiter(Number_of_philosophers, Ids) end),
    register(waiter, Waiter),
    [spawn(fun() -> philosopher(ID) end) || ID <- Ids],
    start.
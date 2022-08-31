-module(producers).
-export([run/3, stop/0]).


buffer(Max_amount, Items, Waiting_producers, Waiting_consumers) ->
    io:format("Buffer ~w\nProducers: ~w\nConsumers: ~w\n",[Items, Waiting_producers, Waiting_consumers]),
    Current_amount = length(Items),
    receive
        {put, _, Producer} when Current_amount == Max_amount ->
            Producer ! wait,
            buffer(Max_amount, Items, Waiting_producers ++ [Producer], Waiting_consumers);
        {put, Item, Producer} ->
            Producer ! ok,
            buffer(Max_amount, Items ++ [Item], Waiting_producers, notify_worker(Waiting_consumers));
        {consume, Consumer} when Current_amount == 0 ->
            Consumer ! wait,
            buffer(Max_amount, [], Waiting_producers, Waiting_consumers ++ [Consumer]);
        {consume, Consumer} ->
            [Item | Rest_of_items] = Items,
            Consumer ! {ok, Item},
            buffer(Max_amount, Rest_of_items, notify_worker(Waiting_producers), Waiting_consumers);
        stop ->
            io:format("Killing all waiting workers!: ~w", [Waiting_consumers ++ Waiting_producers]),
            kill_waiting_workers(Waiting_consumers ++ Waiting_producers),
            kill(),
            io:fwrite("End!\n")
    end.


notify_worker([]) -> [];
notify_worker(Waiting_workers)->
    [Worker | Rest_of_them] = Waiting_workers,
    Worker ! continue_your_job,
    Rest_of_them.

producer()->
    io:format("~p: Producing an item...\n",[self()]),
    timer:sleep(rand:uniform(2000) + 3500),
    Item = rand:uniform(1000),
    io:format("~p: I have produced item: ~w", [self(), Item]),
    put_in_buffer(Item).

put_in_buffer(Item)->
    buffer ! {put, Item, self()},
    receive
        ok ->
            io:format("~p: I have added item ~w to the buffer!\n",[self(), Item]),
            producer();
        wait->
            io:format("~p: buffer is full, waiting for consumers to do their job!\n", [self()]),
            receive
                continue_your_job -> put_in_buffer(Item);
                stop -> stopped
            end;
        stop->stopped
    end.

consumer() ->
    buffer ! {consume, self()},
    receive
        {ok, Item} ->
            consume(Item),
            consumer();
        wait ->
            receive
                continue_your_job -> consumer();
                stop -> stopped
            end;
        stop -> stopped
    end.

consume(Item) ->
    io:format("~p: Consuming item: ~w...\n",[self(), Item]),
    timer:sleep(rand:uniform(2500) + 6000),
    io:format("~p: I have consumed item: ~w!\n",[self(), Item]).


kill_waiting_workers([]) -> [];
kill_waiting_workers(Waiting_workers) ->
    [Current_worker | Rest_of_them] = Waiting_workers,
    exit(Current_worker, normal),
    kill_waiting_workers(Rest_of_them).

kill() ->
    receive
        {put, _, Producer} -> 
            io:format("~p: IMMA HEAD OUT!\n", [Producer]),
            exit(Producer, normal),
            kill();
        {consume, Consumer} -> 
            io:format("~p: IMMA HEAD OUT!\n", [Consumer]),
            exit(Consumer, normal),
            kill()
    after 5000 ->stopped
    end.

stop() ->
    buffer ! stop.

run(0, _, _) -> io:format("Not enough consumers!\n");
run(_, 0, _) -> io:format("Not enough producers!\n");
run(_, _, 0) -> io:format("Max amount of items must be greater than 0!\n");
run(Np, Nc, Bound)  ->
    Producer_list = lists:seq(0, Np-1),
    Consumer_list = lists:seq(0, Nc-1),
    Buffer = spawn(fun() -> buffer(Bound, [], [], []) end), 
    register(buffer, Buffer),
    [spawn(fun() -> producer() end) || _ <- Producer_list],
    [spawn(fun() -> consumer() end) || _ <- Consumer_list],
    start.
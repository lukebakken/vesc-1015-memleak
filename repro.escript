#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname memleak
main([RmqDir]) ->
    try
        repro:run(RmqDir, 64)
    catch
        C:E:S ->
            io:format(standard_error, "[ERROR] ~p:~p~n", [C, E]),
            io:format(standard_error, "~p~n", [S]),
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: repo.escript rabbitmq-plugins-dir~n"),
    halt(1).

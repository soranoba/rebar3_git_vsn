#!/usr/bin/env escript
%% -*- erlang -*-

main([AppFilePath | Tail]) ->
    Key = if Tail == ["vsn"] -> vsn; true -> env end,
    case file:consult(AppFilePath) of
        {ok, ConsultResult} ->
            {_, _, AllEnvs} = proplists:lookup(application, ConsultResult),
            io:format("~p~n", [proplists:get_value(Key, AllEnvs)]);
        {error, Reason} ->
            io:format("file:consult(~p) failed. reason = ~p~n", [AppFilePath, Reason]),
            halt(1)
    end;
main(_) ->
    io:format("Invalid Arguments. show_env.escript APP_FILE_PATH~n"),
    halt(1).

%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Provider for generate the version from git.

-module(rebar3_git_vsn).
-behaviour(provider).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback APIs
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------

-define(DEBUG(Format, Args), rebar_api:debug("[~s] "++Format, [?MODULE | Args])).
-define(INFO(Format, Args),  rebar_api:info( "[~s] "++Format, [?MODULE | Args])).
-define(ERROR(Format, Args), rebar_api:error("[~s] "++Format, [?MODULE | Args])).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(State) ->
    Provider = providers:create([{name, git_vsn},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {short_desc, "Generate the version from git"}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
do(State) ->
    CurrentApp  = rebar_state:current_app(State),
    ProjectApps = rebar_state:project_apps(State),
    GitVsnOpts  = rebar_state:get(State, git_vsn, []),
    Key = proplists:get_value(env_key, GitVsnOpts, git_vsn),
    Opt = proplists:get_value(describe_opt, GitVsnOpts, ""),
    Dir = rebar_app_info:dir(CurrentApp),

    case filelib:is_dir(GitPath = filename:join(Dir, ".git")) of
        true ->
            {ok, Ret} = rebar_utils:sh("git describe --always " ++ Opt, [{cd, Dir}]),
            Vsn = string:strip(Ret, both, $\n),

            lists:foreach(fun(App) ->
                                  ?INFO("write ~s.app : {~p, ~p}", [rebar_app_info:name(App), Key, Vsn]),
                                  AppFile = rebar_app_info:app_file(App),
                                  {ok, [{application, AppName, AppKeys0}]} = file:consult(AppFile),
                                  Env0 = proplists:get_value(env, AppKeys0),
                                  Env     = lists:keystore(Key, 1, Env0, {Key, Vsn}),
                                  AppKeys = lists:keystore(env, 1, AppKeys0, {env, Env}),
                                  AppData = io_lib:format("~p.\n", [{application, AppName, AppKeys}]),
                                  rebar_file_utils:write_file_if_contents_differ(AppFile, AppData)
                          end, [CurrentApp | ProjectApps]);
        false ->
            ?DEBUG("~s doesn't exist", [GitPath]),
            ok
    end,
    {ok, State}.

%% @private
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

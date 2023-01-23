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

-dialyzer(no_undefined_callbacks).
-ignore_xref([rebar_api, rebar_app_info, providers, rebar_state]).

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
    Key         = proplists:get_value(env_key, GitVsnOpts, git_vsn),
    VsnFmt      = proplists:get_value(vsn_format, GitVsnOpts, ignore),
    DefGitOpts  = if VsnFmt == gitver ->
                       "--abbrev=7 --tags";
                  true ->
                       "--abbrev=7 --tags --long"
                  end,
    Opt         = proplists:get_value(describe_opt, GitVsnOpts, DefGitOpts),
    DoSeparate  = proplists:get_value(separate, GitVsnOpts, false),
    Dir         = rebar_app_info:dir(CurrentApp),

    case filelib:is_dir(GitPath = filename:join(Dir, ".git")) of
        true ->
            Vsn0 = git_describe(Dir, Opt),
            Vsn1 = case VsnFmt of
                      gitver ->
                          re:replace(Vsn0, "(-\\d+)-g", "\\1-", [{return,list}]);
                      ggitver ->
                          Vsn0;
                      ignore ->
                          Vsn0;
                      _ ->
                          ?ERROR("Invalid vsn_format argument of ~w plugin: ~p", [VsnFmt, ?MODULE]),
                          erlang:error({invalid_argument, {?MODULE, vsn_format, VsnFmt}})
                   end,
            Vsn  = case DoSeparate andalso list_to_tuple(string:tokens(Vsn1, [$-])) of
                       {_, _, _} = Tuple -> Tuple;
                       _                 -> Vsn1
                   end,

            _ = lists:foldl(
                fun(App, Acc) ->
                    AppNameBin = rebar_app_info:name(App),
                    AppFile    = rebar_app_info:app_file(App),
                    case lists:member(AppNameBin, Acc) of
                        true ->
                            Acc;
                        false ->
                            {AppName, AppKeys0} =
                                case file:consult(AppFile) of
                                    {ok, [{application, AppN, AppK}]} ->
                                        {AppN, AppK};
                                    {error, Why} ->
                                        ?ERROR("Cannot read ~p: ~p\nAppFile: ~p", [AppFile, Why, App]),
                                        erlang:error({cannot_read_app_file, AppFile, Why})
                                end,
                            Env0    = proplists:get_value(env, AppKeys0),
                            AppKey1 = if Key == ignore ->
                                          AppKeys0;
                                      not is_list(Env0) ->
                                          ?ERROR("~w: missing 'env' key in ~p.\n"
                                                 "Add `{git_vsn, [{env_key, ignore}]}.` option to rebar.config to ignore",
                                                 [?MODULE, AppFile]),
                                          erlang:error({missing_env_key, AppFile});
                                      true ->
                                          Env = lists:keystore(Key, 1, Env0, {Key, Vsn}),
                                          lists:keystore(env, 1, AppKeys0, {env, Env})
                                      end,
                            AppKey2 = if VsnFmt == ignore ->
                                          AppKey1;
                                      true ->
                                          lists:keystore(vsn, 1, AppKey1, {vsn, Vsn})
                                      end,
                            AppData = iolist_to_binary(io_lib:format("~p.\n", [{application, AppName, AppKey2}])),
                            ?INFO("Writing ~s.app vsn=~p", [AppName, Vsn]),
                            ?DEBUG("AppFile: ~p, vsn_format=~p", [AppFile, VsnFmt]),
                            rebar_file_utils:write_file_if_contents_differ(AppFile, AppData),
                            [AppNameBin | Acc]
                    end
                end, [], [CurrentApp | ProjectApps]),
            ok;
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

-spec git_describe(file:filename_all(), string()) -> string().
git_describe(Dir, Opt) ->
    {ok, Ret} = rebar_utils:sh("git describe --always " ++ Opt, [{cd, Dir}]),
    string:strip(Ret, both, $\n).

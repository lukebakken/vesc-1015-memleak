-module(repro).

-export([run/2]).

-record(plugin, {name,             %% atom()
                 version,          %% string()
                 description,      %% string()
                 type,             %% 'ez' or 'dir'
                 dependencies,     %% [atom()]
                 location,         %% string()
                 %% List of supported broker version ranges,
                 %% e.g. ["3.5.7", "3.6.1"]
                 broker_version_requirements, %% [string()]
                 %% Proplist of supported dependency versions,
                 %% e.g. [{rabbitmq_management, ["3.5.7", "3.6.1"]},
                 %%       {rabbitmq_federation, ["3.5.7", "3.6.1"]},
                 %%       {rabbitmq_email,      ["0.1.0"]}]
                 dependency_version_requirements, %% [{atom(), [string()]}]
                 extra_dependencies %% string()
                }).

run(RmqDir, Iterations) when is_integer(Iterations) ->
    F = fun (I) ->
                io:format("I ~p~n", [I]),
                Apps = list_free_apps([RmqDir]),
                _Infos = read_plugins_info(Apps, {[], []}) % ,
                % timer:sleep(250)
    end,
    lists:foreach(F, lists:seq(0, Iterations)).

%% Returns list of all files that look like OTP applications in a
%% given set of directories.
list_free_apps([]) ->
    [];
list_free_apps([Dir|Rest]) ->
    [{app, App} || App <- full_path_wildcard("*/ebin/*.app", Dir)]
        ++ list_free_apps(Rest).

%% Search for files using glob in a given dir. Returns full filenames of those files.
full_path_wildcard(Glob, Dir) ->
    [filename:join([Dir, File]) || File <- filelib:wildcard(Glob, Dir)].

read_plugins_info([], Acc) ->
    Acc;
read_plugins_info([Path|Paths], {Plugins, Problems}) ->
    case plugin_info(Path) of
        #plugin{} = Plugin ->
            read_plugins_info(Paths, {[Plugin|Plugins], Problems});
        {error, Location, Reason} ->
            read_plugins_info(Paths, {Plugins, [{Location, Reason}|Problems]})
    end.

plugin_info({app, App}) ->
    % LRB TODO case rabbit_file:read_term_file(App) of
    case read_term_file(App) of
        {ok, [{application, Name, Props}]} ->
            mkplugin(Name, Props, dir,
                     filename:absname(
                       filename:dirname(filename:dirname(App))));
        {error, Reason} ->
            {error, App, {invalid_app, Reason}}
    end.

mkplugin(Name, Props, Type, Location) ->
    Version = proplists:get_value(vsn, Props, "0"),
    Description = proplists:get_value(description, Props, ""),
    Dependencies = proplists:get_value(applications, Props, []),
    BrokerVersions = proplists:get_value(broker_version_requirements, Props, []),
    DepsVersions = proplists:get_value(dependency_version_requirements, Props, []),
    #plugin{name = Name, version = Version, description = Description,
            dependencies = Dependencies, location = Location, type = Type,
            broker_version_requirements = BrokerVersions,
            dependency_version_requirements = DepsVersions}.

read_term_file(File) ->
    try
        %% {ok, Data} = with_handle(fun () -> prim_file:read_file(File) end),
        {ok, Data} = file:read_file(File),
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Data)),
        TokenGroups = group_tokens(Tokens),
        {ok, [begin
                  {ok, Term} = erl_parse:parse_term(Tokens1),
                  Term
              end || Tokens1 <- TokenGroups]}
    catch
        error:{badmatch, Error} -> Error
    end.

group_tokens(Ts) -> [lists:reverse(G) || G <- group_tokens([], Ts)].

group_tokens([], [])                    -> [];
group_tokens(Cur, [])                   -> [Cur];
group_tokens(Cur, [T = {dot, _} | Ts])  -> [[T | Cur] | group_tokens([], Ts)];
group_tokens(Cur, [T | Ts])             -> group_tokens([T | Cur], Ts).

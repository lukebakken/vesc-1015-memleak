-module(repro).

-export([run/1, run/2]).

-include_lib("kernel/include/file.hrl").

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

run(Iterations) when is_integer(Iterations) ->
    run("./plugins", Iterations).

run(Dir, Iterations) when is_integer(Iterations) ->
    InitArgs = init:get_arguments(),
    ShouldLeak = parse_leak_arg(proplists:get_value(leak, InitArgs, "True")),
    Apps = list_free_apps([Dir]),
    % io:format("Args ~p ShouldLeak ~p~n", [InitArgs, ShouldLeak]).
    F = fun (_I) ->
        _Infos = read_plugins_info(ShouldLeak, Apps, {[], []})
    end,
    lists:foreach(F, lists:seq(0, Iterations)).

parse_leak_arg(["True"]) -> true;
parse_leak_arg(["False"]) -> false;
parse_leak_arg(_) -> true.

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

read_plugins_info(_ShouldLeak, [], Acc) ->
    Acc;
read_plugins_info(ShouldLeak, [Path|Paths], {Plugins, Problems}) ->
    case plugin_info(ShouldLeak, Path) of
        #plugin{} = Plugin ->
            read_plugins_info(ShouldLeak, Paths, {[Plugin|Plugins], Problems});
        {error, Location, Reason} ->
            read_plugins_info(ShouldLeak, Paths, {Plugins, [{Location, Reason}|Problems]})
    end.

plugin_info(ShouldLeak, {app, App}) ->
    case read_term_file(ShouldLeak, App) of
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

read_term_file(_ShouldLeak=false,File) ->
    try
        {ok, FInfo} = file:read_file_info(File, [raw]),
        {ok, Fd} = file:open(File, [read, raw, binary]),
        try
            {ok, Data} = file:read(Fd, FInfo#file_info.size),
            {ok, Tokens, _} = erl_scan:string(binary_to_list(Data)),
            TokenGroups = group_tokens(Tokens),
            {ok, [begin
                    {ok, Term} = erl_parse:parse_term(Tokens1),
                    Term
                end || Tokens1 <- TokenGroups]}
        after
            ok = file:close(Fd)
        end
    catch
        error:{badmatch, Error} -> Error
    end;
read_term_file(_ShouldLeak=true,File) ->
    try
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

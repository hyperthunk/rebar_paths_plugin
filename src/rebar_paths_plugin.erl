%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(rebar_paths_plugin).

-export([preprocess/2, postprocess/2]).

-define(CODE_PATH_KEY, {?MODULE, original_code_path}).

preprocess(Config, _) ->
    Command = rebar_plugin_manager:command_info(current),
    Dir = rebar_utils:get_cwd(),
    rebar_log:log(debug, "Pre-processing ~p in ~s!~n", [Command, Dir]),
    PriorPaths = rebar_config:get_global(?CODE_PATH_KEY, []),
    case rebar_config:get_local(Config, path_opts, []) of
        [] -> ok;
        Opts ->
            rebar_log:log(debug, "Handling ~p~n", [Opts]),
            case lists:keyfind(Command, 1, Opts) of
                false ->
                    rebar_log:log(debug, "Skipping path config in ~s~n", [Dir]),
                    ok;
                {_, PathOptsForCmd} ->
                    rebar_config:set_global(?CODE_PATH_KEY,
                                            [{Dir, code:get_path()}|PriorPaths]),
                    process_path(PathOptsForCmd)
            end
    end,
    {ok, []}.

postprocess(_, _) ->
    Command = rebar_plugin_manager:command_info(current),
    Dir = rebar_utils:get_cwd(),
    rebar_log:log(debug, "Post-processing ~p in ~s!~n", [Command, Dir]),
    case rebar_config:get_global(?CODE_PATH_KEY, undefined) of
        [{Dir, Path}|Paths] ->
            restore_code_path(Path),
            rebar_config:set_global(?CODE_PATH_KEY, Paths);
        _ ->
            ok
    end,
    {ok, []}.

restore_code_path(no_change) ->
    ok;
restore_code_path(Path) ->
    true = code:set_path([F || F <- Path, filelib:is_file(F)]).

process_path(Opts) ->
    [ process_path(Mode, Paths) || {Mode, Paths} <- Opts ].

process_path(append, Paths) ->
    log_paths("Appending", Paths),
    code:add_pathsz(Paths);
process_path(prepend, Paths) ->
    log_paths("Prepending", Paths),
    code:add_pathsa(Paths);
process_path(Other, Paths) ->
    rebar_log:log(warn, "Unrecognised path config: ~p~n"
                        "Ignoring paths: ~p", [Other, Paths]).

log_paths(Mode, Paths) ->
    [ rebar_log:log(debug, "~s ~s to code path.~n", [Mode, P]) || P <- Paths ].

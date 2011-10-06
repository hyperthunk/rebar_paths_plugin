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

-define(CODE_PATH_KEY, 'rebar_paths_plugin.original_code_path').

preprocess(Config, _) ->
    case rebar_config:get_local(Config, path_opts, []) of
        [] -> ok;
        Opts ->
            case lists:keyfind(rebar_utils:command_info(current), 1, Opts) of
                false -> ok;
                {_, PathOptsForCmd} ->
                    rebar_config:set_global(?CODE_PATH_KEY, code:get_path()),
                    process_path(PathOptsForCmd)
            end
    end,
    {ok, []}.

postprocess(_, _) ->
    case rebar_config:get_global(?CODE_PATH_KEY, undefined) of
        undefined -> ok;
        Path -> restore_code_path(Path)
    end,
    {ok, []}.

restore_code_path(Path) ->
    true = code:set_path([F || F <- Path, filelib:is_file(F)]),
    rebar_config:set_global(?CODE_PATH_KEY, undefined).

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

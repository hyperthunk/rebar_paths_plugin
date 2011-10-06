# Rebar Paths Plugin

This is an experimental plugin lets you modify the code path 
(using `add_pathsa/1` and `add_pathsz`) for either all rebar commands, or for a 
specific set of commands.

## Usage

You can utilise the plugin by either 

1. including it as a rebar dependency
2. installing it into `$ERL_LIBS` or `code:lib_dir`
3. putting the source into your plugin_dir directory

Here's an example rebar config:

```erlang
{path_opts, [

    %% prepend the list of dirs when the compile command is running
    %% NB: the code path is restored immediately after the compile command finishes
    {compile, [{prepend, ["/tmp/foo/ebin"]}]},

    %% append the list of dirs when the xref command runs
    {xref, [
        {append, ["../ebin"]}
    ]}

]}.
{plugins, [rebar_paths_plugin]}.
```

## Sample code

Take a look in the examples directory to see the *xref* hook from above in action.

## Important notes

This plugin is highly experimental and based on rebar features that have not made
it into an official rebar branch (and may never do so).

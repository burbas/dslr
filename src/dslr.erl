-module(dslr).
-export([
         create_env/0,
         capture_photo/2
        ]).
-on_load(init/0).

-define(APPNAME, dslr).
-define(LIBNAME, dslr).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

create_env() ->
    not_loaded(?LINE).

capture_photo(_Reference, _Filename) ->
    not_loaded(?LINE).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

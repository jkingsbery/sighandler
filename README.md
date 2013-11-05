#sighandler

**License: Apache 2**


    1> application:start(sighandler).
    ok
    2> {ok, Ref} = sighandler:install(hup, fun() -> io:format("HUP!~n") end).
    {ok,#Ref<0.0.0.66>}
    3> os:cmd(io_lib:format("kill -HUP ~s", [os:getpid()])).
    HUP!
    []
    4> ok = sighandler:remove(Ref).
    ok
    3> os:cmd(io_lib:format("kill -HUP ~s", [os:getpid()])).
    Hangup: 1

Should work with Erlang R13B04 or later on UNIX like platforms. Has been known to work with R13B04 and R15B on Darwin and with R14B04 on Linux.

To add a signal handler using configuration instead of programmatically, first create a config file

    #hello.config
    [
        {sighandler, [
            {handlers, [
                {hup, {io, fwrite, ["Hello, world!~n"]}}
            ]}
        ]}
    ].

Then start erlang:

    erl -pa ebin/ -config $CONFIGDIR/hello -s sighandler

Then the handler will automatically be registered.
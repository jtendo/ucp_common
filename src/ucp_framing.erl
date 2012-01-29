-module(ucp_framing).
-author('adam.rutkowski@jtendo.com').

-export([try_decode/2]).

-include("logger.hrl").
-include("ucp_syntax.hrl").

-define(BEGIN, ?STX).
-define(END,   ?ETX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_decode(Incoming, Buffered) when is_binary(Incoming) ->
    try_decode(Incoming, [], [], Buffered).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Packet framing functions                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_decode(<<>>, Buffer, Collected) ->
    IOL = lists:reverse(Collected),
    {lists:map(fun iolist_to_binary/1, IOL), lists:reverse(Buffer)};

try_decode(Incoming, Buffer, Collected) ->
    case binary:split(Incoming, <<?END>>) of
        [M = <<?BEGIN, _Body/binary>>, <<>>] ->
            %% looks like we're done
            try_decode(<<>>, Buffer, [[M, ?END]|Collected]);
        [M = <<?BEGIN, _Body/binary>>, R = <<?BEGIN, _More/binary>>] ->
            %% one message decoded, try remaining
            try_decode(R, Buffer, [[M, ?END]|Collected]);
        [Incomplete] ->
            %% apparently packet fragmenting occured;
            %% we can't handle that, so make it go to the buffer
            try_decode(<<>>, [Incomplete|Buffer], Collected);
        [Incomplete, More] ->
            %% most likely we have a binary closing the previously
            %% fragmented packet. buffer it and inspect remaining bytes.
            try_decode(More, [Incomplete|Buffer], Collected)
    end.

try_decode(Incoming, [], [], [L, R]) ->
    try_decode(iolist_to_binary([L, Incoming, R]), [], []);

try_decode(Incoming, [], [], [L]) ->
    try_decode(iolist_to_binary([L, Incoming]), [], []);

try_decode(Incoming, [], [], B) when B =:= []; B =:= undefined ->
    try_decode(Incoming, [], []).


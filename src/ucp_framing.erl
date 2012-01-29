-module(ucp_framing).
-author('adam.rutkowski@jtendo.com').

-export([recv_ucp_sock/1]).

-include("logger.hrl").

recv_ucp_sock(S) ->
    case wait_for_frame(S, []) of
        {packet, Data} ->
            ?SYS_DEBUG("Received frame: ~p", [Data]),
            {ok, Data};
        Other ->
            ?SYS_WARN("Could not recognize frame: ~p", [Other]),
            Other
    end.

%% FIXME
wait_for_frame(S, Buff) ->
    {ok, Data} = gen_tcp:recv(S, 1),
    case Data of
            <<2>> ->
                wait_for_frame(S, [<<2>>|Buff]);
            <<3>> ->
                classify_packet(lists:reverse([<<3>>|Buff]));
            Byte ->
                wait_for_frame(S, [Byte|Buff])
    end.

classify_packet(P = [<<2>>|_]) ->
    {packet, iolist_to_binary(P)};
classify_packet(P) ->
    {malformed, iolist_to_binary(P)}.


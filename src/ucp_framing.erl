-module(ucp_framing).
-author('adam.rutkowski@jtendo.com').

% A basic utlility for fragmented UCP packets reassembly.

% Copyright (c) 2012, jtendo

% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.

% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.OF SUCH DAMAGE.

-export([try_decode/2]).

-include("logger.hrl").
-include("ucp_syntax.hrl").

-define(BEGIN, ?STX).
-define(END,   ?ETX).

-type ucp_buff() :: undefined | [binary()]. % FIXME, should be a tuple as we
                                            % expect it to be [0..2] elements
-type ucp_pkt() :: [binary()].

-export_type([ucp_buff/0, ucp_pkt/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec try_decode(binary(), Buffered :: ucp_buff()) ->
    {ucp_pkt(), ToBeBuffered :: ucp_buff()}.

try_decode(Incoming, Buffered) when is_binary(Incoming) ->
    try_decode(Incoming, [], [], Buffered).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Packet framing functions                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_decode(<<>>, Buffer, Collected) ->
    {[iolist_to_binary(Io) || Io <- lists:reverse(Collected)],
        [iolist_to_binary(Io) || Io <- lists:reverse(Buffer)]};

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
            try_decode(More, [[Incomplete, ?END]|Buffer], Collected)
    end.

try_decode(Incoming, [], [], [L, R]) ->
    try_decode(iolist_to_binary([L, Incoming, R]), [], []);

try_decode(Incoming, [], [], [L]) ->
    try_decode(iolist_to_binary([L, Incoming]), [], []);

try_decode(Incoming, [], [], B) when B =:= []; B =:= undefined ->
    try_decode(Incoming, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Eunit Tests                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
decoder_test_() ->
    [
        {"Extract single UCP packet from single UCP packet :)", fun complete/0},
        {"Assume it's right fragmentation", fun unknown_packet/0},
        {"Extract UCP packet(s) when both ends are fragments",
            [fun fragment_both/0,
             fun fragment_both_2/0]},
        {"Extract UCP packet(s) when both ends are fragments but there are"
         "buffered data available",
         [fun fragment_with_buffer/0,
          fun fragment_with_buffer_2/0]},
        {"Extract UCP packet(s) with presence of a two-element buffer",
            fun fragment_with_buffer_3/0}
    ].

complete() ->
    B = <<?BEGIN, 0,0,0,0,0, ?END>>,
    ?assertEqual(
        {[B],[]}, try_decode(B, [])
    ).
unknown_packet() ->
    B = <<0,0,0>>,
    ?assertEqual(
        {[], [B]}, try_decode(B, [])
    ).
fragment_both() ->
    B = <<0,0,?END,?BEGIN,0,0,0,0,?END,?BEGIN,0,0>>,
    ?assertEqual(
        {[<<?BEGIN,0,0,0,0,?END>>], [<<0,0,?END>>, <<?BEGIN,0,0>>]},
        try_decode(B, [])
    ).
fragment_both_2() ->
    B = <<0,0,?END,?BEGIN,0,0,?END,?BEGIN,0,0,?END,?BEGIN>>,
    ?assertEqual(
        {[<<?BEGIN,0,0,?END>>,<<?BEGIN,0,0,?END>>], [<<0,0,?END>>,
                <<?BEGIN>>]},
        try_decode(B, [])
    ).
fragment_with_buffer() ->
    B = <<0,0,?END,?BEGIN,0,0,0,?END>>,
    ?assertEqual(
        {[<<?BEGIN,1,1,0,0,?END>>, <<?BEGIN,0,0,0,?END>>], []},
        try_decode(B, [<<?BEGIN,1,1>>])
    ).
fragment_with_buffer_2() ->
    B = <<0,0,?END,?BEGIN,0,0,0,?END,?BEGIN>>,
    ?assertEqual(
        {[<<?BEGIN,1,1,0,0,?END>>, <<?BEGIN,0,0,0,?END>>], [<<?BEGIN>>]},
        try_decode(B, [<<?BEGIN,1,1>>])
    ).
fragment_with_buffer_3() ->
    B = <<0,0,?END,?BEGIN,0,?END,?BEGIN>>,
    ?assertEqual(
        {[<<?BEGIN,1,0,0,?END>>,<<?BEGIN,0,?END>>,<<?BEGIN,1,1,0,?END>>], []},
        try_decode(B, [<<?BEGIN,1>>, <<1,1,0,?END>>])
    ).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2016 Robert Forbes.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. 
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
%% License for the specific language governing permissions and limitations 
%% under the License.
%%
%% @doc Top level API for the AIS library. 

-module(aisle).

%% aisle: aisle library's entry point.

-export([
    decode/1, 
    payload_to_binary/1,
    int_to_bits/1,
    get_id/1,
    get_frag_count/1,
    get_frag_num/1,
    get_msg_id/1,
    get_data/1,
    get_fill_bits/1,
    to_tokens/2]).

-record(ais, {
    id, 
    frag_count, 
    frag_num, 
    msg_id, 
    radio_chan, 
    data, 
    fill_bits, 
    checksum}).

%% API

%% @doc Decode an AIS sentence.
decode(Sentence) when is_list(Sentence) ->
    Tokens = to_tokens(Sentence, ",*"),
    [Id, FragCount, FragNum, MsgID, Chan, Payload,Fill, CS|_Rest] = Tokens,
    case Id of 
        "!AIVDM" -> 
            AisRec = #ais{
                id = aivdm,
                frag_count = list_to_integer(FragCount),
                frag_num = list_to_integer(FragNum),
                msg_id = decode_msg_id(MsgID),
                radio_chan = decode_radio_chan(Chan),
                data = payload_to_binary(list_to_binary(Payload)),
                fill_bits = Fill,
                checksum = CS},
            {ok, AisRec};
        _ -> 
            {error, bad_identifier}
    end.

%% @doc Decode the message ID field. This is often not set, so we need to trap
%% this.
decode_msg_id(MsgID)  ->
    case string:to_integer(MsgID) of
        {error, _} -> undefined;
        {X, _}     -> X
    end.

%% @doc Decode the radio channel, either 'A' at 161.975 MHz or 'B' at 
%% 162.025 MHz. Sometimes mapped as '1' or '2'.
decode_radio_chan("A") -> radio_chan_a;
decode_radio_chan("B") -> radio_chan_b;
decode_radio_chan("1") -> radio_chan_a;
decode_radio_chan("2") -> radio_chan_b.

%% @doc Convert encoded characters in the the data payload to raw binary.
%% Uses a binary comprehension to compress the 8-bit ASCII characters to 
%% 6-bit values, remapping as described in the section on "Payload Armoring"
%% in the online notes.
payload_to_binary(PayloadData) when is_binary(PayloadData) ->
    << <<(aisle:int_to_bits(X)):6>> || <<X:8>> <= PayloadData >>.

%% Reverse the ASCII mapping of 6 bit values.
int_to_bits(X) ->
    Y = X - 48,
    case Y >= 40 of
        true  -> Y - 8;
        false -> Y
    end.

%% Accessor functions for the AIS records.
get_id(#ais{id = X}) -> X. 
get_frag_count(#ais{frag_count = X}) -> X. 
get_frag_num(#ais{frag_num = X}) -> X. 
get_msg_id(#ais{msg_id = X}) -> X. 
get_data(#ais{data = X}) -> X. 
get_fill_bits(#ais{fill_bits = X}) -> X. 

%% @doc Utility function to work like string:tokens/1, but not skip over 
%% multiple occurrences of the separator.
to_tokens(String, SepList) when is_list(String), is_list(SepList) ->
    toks(String, SepList, [], last_sep).

toks([], _, Toks, _) ->
    lists:reverse(Toks);
toks(String, SepList, Toks, Last) ->
    SepPoint = string:cspan(String, SepList),
    case SepPoint of
        0 ->
            case Last of
                not_sep ->
                    toks(string:sub_string(String, 2), SepList, Toks, last_sep);
                last_sep ->
                    toks(string:sub_string(String, 2), SepList, [""|Toks], last_sep)
            end;
        N -> 
            Tok = string:sub_string(String, 1, N),
            Rem = string:sub_string(String, N+1),
            toks(Rem, SepList, [Tok|Toks], not_sep)
    end.


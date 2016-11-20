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
    get_id/1,
    get_frag_count/1,
    get_frag_num/1,
    get_msg_id/1,
    get_data/1,
    get_fill_bits/1]).

-record(ais, {id, frag_count, frag_num, msg_id, data, fill_bits}).

%% API

%% @doc Decode an AIS sentence.
decode(Sentence) when is_list(Sentence) ->
    Tokens = string:tokens(Sentence, ",*"),
    [Id, FragCount, FragNum, MsgID|_Rest] = Tokens,
    case Id of 
        "!AIVDM" -> 
            AisRec = #ais{
                id = aivdm,
                frag_count = list_to_integer(FragCount),
                frag_num = list_to_integer(FragNum),
                msg_id = decode_msg_id(MsgID)},
            {ok, AisRec};
        _ -> 
            {error, bad_identifier}
    end.

decode_msg_id(MsgID)  ->
    case string:to_integer(MsgID) of
        {error, _} -> undefined;
        {X, _}     -> X
    end.

%% Accessor functions for the AIS records.
get_id(#ais{id = X}) -> X. 
get_frag_count(#ais{frag_count = X}) -> X. 
get_frag_num(#ais{frag_num = X}) -> X. 
get_msg_id(#ais{msg_id = X}) -> X. 
get_data(#ais{data = X}) -> X. 
get_fill_bits(#ais{fill_bits = X}) -> X. 


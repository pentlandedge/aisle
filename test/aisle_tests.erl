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

-module(aisle_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the aisle decoder. 
aisle_test_() ->
    [decode_sample1(), decode_bad_id()].

decode_sample1() ->
    Sentence = sample_sentence1(),
    {Code, AisRec} = aisle:decode(Sentence),
    Id = aisle:get_id(AisRec),
    FragCount = aisle:get_frag_count(AisRec),
    FragNum = aisle:get_frag_num(AisRec),
    MsgID = aisle:get_msg_id(AisRec),
    Chan = aisle:get_radio_chan(AisRec),
    FB = aisle:get_fill_bits(AisRec),
    CNB = aisle:get_data(AisRec),
    MT = aisle:get_message_type(CNB),
    RI = aisle:get_repeat_indicator(CNB),
    MMSI = aisle:get_mmsi(CNB),
    NS = aisle:get_nav_status(CNB),
    ROT = aisle:get_rate_of_turn(CNB),
    SOG = aisle:get_speed_over_ground(CNB),
    [?_assertEqual(ok, Code),
     ?_assertEqual(aivdm, Id),
     ?_assertEqual(1, FragCount),
     ?_assertEqual(1, FragNum),
     ?_assertEqual(undefined, MsgID),
     ?_assertEqual(radio_chan_b, Chan),
     ?_assertEqual(0, FB),
     ?_assertEqual(pos_report_class_a, MT),
     ?_assertEqual(no_repeats, RI),
     ?_assertEqual(477553000, MMSI),
     ?_assertEqual(moored, NS),
     ?_assertEqual(not_turning, ROT),
     ?_assertEqual(true, almost_equal(0.0, SOG, 0.00001))
     ].

decode_bad_id() ->
    Sentence = bad_identifier(),
    Result = aisle:decode(Sentence),
    [?_assertEqual({error, bad_identifier}, Result)].

sample_sentence1() -> 
    "!AIVDM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C".

base_station_report1() -> 
    "!AIVDM,1,1,,A,402=ac1v2rAM`OhIijP3BCw028Gm,0*2C".

aid_to_nav_report1() -> 
    "!AIVDM,1,1,,A,EvjFM;0Q2PVRa@97QUP00000000?p<6v@1NSH?1skh7P10,4*38".

bad_identifier() -> "!AIDVM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C".

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.

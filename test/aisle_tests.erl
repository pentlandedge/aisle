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
    [decode_sample1(), decode_bad_id(), decode_base_station_report1(),
     decode_aid_to_nav_report1()].

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
    PA = aisle:get_position_accuracy(CNB),
    Lon = aisle:get_longitude(CNB),
    Lat = aisle:get_latitude(CNB),
    TS = aisle:get_timestamp(CNB),
    MI = aisle:get_maneuver_indicator(CNB),
    RF = aisle:get_raim_flag(CNB),
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
     ?_assertEqual(true, almost_equal(0.0, SOG, 0.00001)),
     ?_assertEqual(unaugmented_gnss_greater_than_10m, PA),
     ?_assertEqual(true, almost_equal(-122.3458333, Lon, 0.00001)),
     ?_assertEqual(true, almost_equal(47.5828333, Lat, 0.00001)),
     ?_assertEqual(15, TS),
     ?_assertEqual(not_available, MI),
     ?_assertEqual(raim_not_in_use, RF)].

decode_bad_id() ->
    Sentence = bad_identifier(),
    Result = aisle:decode(Sentence),
    [?_assertEqual({error, bad_identifier}, Result)].

decode_base_station_report1() ->
    Sentence = base_station_report1(),
    {Code, AisRec} = aisle:decode(Sentence),
    BSR = aisle:get_data(AisRec),
    MT = aisle:get_bsr_message_type(BSR),
    RI = aisle:get_bsr_repeat_indicator(BSR),
    MMSI = aisle:get_bsr_mmsi(BSR),
    Yr = aisle:get_bsr_year_utc(BSR),
    Mon = aisle:get_bsr_month_utc(BSR),
    Day = aisle:get_bsr_day_utc(BSR),
    Hours = aisle:get_bsr_hour_utc(BSR),
    Mins = aisle:get_bsr_minute_utc(BSR),
    Secs = aisle:get_bsr_second_utc(BSR),
    PA = aisle:get_bsr_position_accuracy(BSR),
    Lon = aisle:get_bsr_longitude(BSR),
    RF = aisle:get_bsr_raim_flag(BSR),
    [?_assertEqual(ok, Code),
     ?_assertEqual(base_station_report, MT),
     ?_assertEqual(no_repeats, RI),
     ?_assertEqual(002320812, MMSI),
     ?_assertEqual(2016, Yr),
     ?_assertEqual(11, Mon),
     ?_assertEqual(20, Day),
     ?_assertEqual(17, Hours),
     ?_assertEqual(29, Mins),
     ?_assertEqual(40, Secs),
     ?_assertEqual(unaugmented_gnss_greater_than_10m, PA),
     ?_assert(almost_equal(-3.407265, Lon, 0.00001)),
     ?_assertEqual(raim_in_use, RF)
    ].

decode_aid_to_nav_report1() ->
    _Sentence = aid_to_nav_report1(),
    [].

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

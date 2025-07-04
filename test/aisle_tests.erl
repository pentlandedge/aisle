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
     decode_base_station_report2(), decode_aid_to_nav_report1(),
     decode_aid_to_nav_report2(), %decode_static_voyage_pair1(),
     decode_static_voyage_pair2(), decode_static_voyage_pair3(),
     decode_class_a_res_to_interrogation(),
     decode_aid_to_nav_short_payload(), decode_bsr_short_payload(),
     decode_no_star(), acc_frags_check(), decode_frag_message_check(),
     decode_batch_error(), decode_batch()].

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
    [?_assertEqual({error, bad_identifier, Sentence}, Result)].

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
    Lat = aisle:get_bsr_latitude(BSR),
    RF = aisle:get_bsr_raim_flag(BSR),
    EPFD = aisle:get_bsr_type_of_epfd(BSR),
    SOTDMA = aisle:get_bsr_sotdma_state(BSR),
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
     ?_assert(almost_equal(56.013785, Lat, 0.00001)),
     ?_assertEqual(internal_gnss, EPFD),
     ?_assertEqual(raim_in_use, RF),
     ?_assertEqual({utc_direct, {2, slot_number}, {slot_number,1525}}, SOTDMA)
    ].

%% Check the sentence that showed the bug in the UTC handling of submessages.
decode_base_station_report2() ->
    Sentence = base_station_report2(),
    {_Code, AisRec} = aisle:decode(Sentence),
    BSR = aisle:get_data(AisRec),
    SOTDMA = aisle:get_bsr_sotdma_state(BSR),
    [?_assertEqual({utc_direct, {1, utc_hour_and_minute}, {utc_hour_and_minute, 17, 22}}, SOTDMA)].

decode_aid_to_nav_report1() ->
    Sentence = aid_to_nav_report1(),
    {Code, AisRec} = aisle:decode(Sentence),
    ATNR = aisle:get_data(AisRec),
    MT = aisle:get_atnr_message_type(ATNR),
    RI = aisle:get_atnr_repeat_indicator(ATNR),
    MMSI = aisle:get_atnr_mmsi(ATNR),
    AT = aisle:get_atnr_aid_type(ATNR),
    Name = aisle:get_atnr_name(ATNR),
    PA = aisle:get_atnr_position_accuracy(ATNR),
    Lon = aisle:get_atnr_longitude(ATNR),
    Lat = aisle:get_atnr_latitude(ATNR),
    DimBow = aisle:get_atnr_dim_to_bow(ATNR),
    DimStern = aisle:get_atnr_dim_to_stern(ATNR),
    DimPort = aisle:get_atnr_dim_to_port(ATNR),
    DimStarboard = aisle:get_atnr_dim_to_starboard(ATNR), 
    EPFD = aisle:get_atnr_type_of_epfd(ATNR), 
    TS = aisle:get_atnr_timestamp(ATNR), 
    OffPos = aisle:get_atnr_off_position(ATNR), 
    Reg = aisle:get_atnr_regional(ATNR), 
    RF = aisle:get_atnr_raim_flag(ATNR), 
    VA = aisle:get_atnr_virtual_aid(ATNR), 
    AssMode = aisle:get_atnr_assigned_mode(ATNR), 
    [?_assertEqual(ok, Code), 
     ?_assertEqual(aid_to_navigation_report, MT),
     ?_assertEqual(do_not_repeat, RI),
     ?_assertEqual(992320812, MMSI),
     ?_assertEqual(reference_point, AT),
     ?_assertEqual({ok, "BEAMER ROCK@@@@@@@@@"}, Name),
     ?_assertEqual(unaugmented_gnss_greater_than_10m, PA),
     ?_assert(almost_equal(-3.41259, Lon, 0.00001)),
     ?_assert(almost_equal(56.004738333, Lat, 0.00001)),
     ?_assertEqual(15, DimBow),
     ?_assertEqual(15, DimStern),
     ?_assertEqual(30, DimPort),
     ?_assertEqual(30, DimStarboard),
     ?_assertEqual(undefined, EPFD),
     ?_assertEqual(15, TS),
     ?_assertEqual(on_position, OffPos),
     ?_assertEqual(<<0>>, Reg),
     ?_assertEqual(raim_not_in_use, RF),
     ?_assertEqual(virtual_aid_to_nav, VA),
     ?_assertEqual(autonomous_mode, AssMode)
    ].

decode_aid_to_nav_report2() ->
    Sentence = aid_to_nav_report2(),
    {Code, AisRec} = aisle:decode(Sentence),
    ATNR = aisle:get_data(AisRec),
    MT = aisle:get_atnr_message_type(ATNR),
    RI = aisle:get_atnr_repeat_indicator(ATNR),
    MMSI = aisle:get_atnr_mmsi(ATNR),
    AT = aisle:get_atnr_aid_type(ATNR),
    Name = aisle:get_atnr_name(ATNR),
    PA = aisle:get_atnr_position_accuracy(ATNR),
    Lon = aisle:get_atnr_longitude(ATNR),
    Lat = aisle:get_atnr_latitude(ATNR),
    DimBow = aisle:get_atnr_dim_to_bow(ATNR),
    DimStern = aisle:get_atnr_dim_to_stern(ATNR),
    DimPort = aisle:get_atnr_dim_to_port(ATNR),
    DimStarboard = aisle:get_atnr_dim_to_starboard(ATNR), 
    EPFD = aisle:get_atnr_type_of_epfd(ATNR), 
    TS = aisle:get_atnr_timestamp(ATNR), 
    OffPos = aisle:get_atnr_off_position(ATNR), 
    Reg = aisle:get_atnr_regional(ATNR), 
    RF = aisle:get_atnr_raim_flag(ATNR), 
    VA = aisle:get_atnr_virtual_aid(ATNR), 
    AssMode = aisle:get_atnr_assigned_mode(ATNR), 
    [?_assertEqual(ok, Code), 
     ?_assertEqual(aid_to_navigation_report, MT),
     ?_assertEqual(no_repeats, RI),
     ?_assertEqual(577050000, MMSI),
     ?_assertEqual(fixed_structure_off_shore, AT),
     ?_assertEqual({ok, "SEDCO711@@@@@@@@@@@@"}, Name),
     ?_assertEqual(dgps_less_than_10m, PA),
     ?_assert(almost_equal(-2.971944, Lon, 0.00001)),
     ?_assert(almost_equal(56.15824, Lat, 0.00001)),
     ?_assertEqual(90, DimBow),
     ?_assertEqual(90, DimStern),
     ?_assertEqual(63, DimPort),
     ?_assertEqual(63, DimStarboard),
     ?_assertEqual(gps, EPFD),
     ?_assertEqual(49, TS),
     ?_assertEqual(off_position, OffPos),
     ?_assertEqual(<<0>>, Reg),
     ?_assertEqual(raim_in_use, RF),
     ?_assertEqual(real_aid_to_nav, VA),
     ?_assertEqual(autonomous_mode, AssMode)
    ].

%decode_static_voyage_pair1() ->
%    Sentence = static_and_voyage_data_sentence_pair1(),
%    {Code, AisRec} = aisle:decode(Sentence),
%    Id = aisle:get_id(AisRec),
%    FragCount = aisle:get_frag_count(AisRec),
%    FragNum = aisle:get_frag_num(AisRec),   
%    MsgID = aisle:get_msg_id(AisRec),
%    Chan = aisle:get_radio_chan(AisRec),
%    [?_assertEqual(ok, Code),
%     ?_assertEqual(aivdm, Id),
%     ?_assertEqual(2, FragCount),
%     ?_assertEqual(1, FragNum),
%     ?_assertEqual(2, MsgID),
%     ?_assertEqual(radio_chan_a, Chan)
%    ].

decode_static_voyage_pair2() ->
    Sentences = static_and_voyage_data_sentence_pair2(),
    {Code, AisRec} = aisle:decode_msgs(Sentences),
    SVDR = aisle:get_data(AisRec),
    MT = aisle:get_svd_message_type(SVDR),
    RI = aisle:get_svd_repeat_indicator(SVDR),
    MMSI = aisle:get_svd_mmsi(SVDR),
    [?_assertEqual(ok, Code),
     ?_assertEqual(static_and_voyage_data, MT),
     ?_assertEqual(no_repeats, RI),
     ?_assertEqual(235007548, MMSI)
    ].

decode_static_voyage_pair3() ->
    Sentences = static_and_voyage_data_sentence_pair3(),
    {Code, AisRec} = aisle:decode_msgs(Sentences),
    SVDR = aisle:get_data(AisRec),
    MT = aisle:get_svd_message_type(SVDR),
    RI = aisle:get_svd_repeat_indicator(SVDR),
    MMSI = aisle:get_svd_mmsi(SVDR),
    [?_assertEqual(ok, Code),
     ?_assertEqual(static_and_voyage_data, MT),
     ?_assertEqual(no_repeats, RI),
     ?_assertEqual(232002720, MMSI)
    ].

decode_class_a_res_to_interrogation() ->
    Sentence = response_to_interrogation1(),
    {Code, AisRec} = aisle:decode(Sentence),
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
    COG = aisle:get_course_over_ground(CNB),
    TH = aisle:get_true_heading(CNB),
    TS = aisle:get_timestamp(CNB),
    MI = aisle:get_maneuver_indicator(CNB),
    RF = aisle:get_raim_flag(CNB),
    [?_assertEqual(ok, Code), 
     ?_assertEqual(pos_report_class_a_response_to_interrogation, MT),
     ?_assertEqual(no_repeats, RI),
     ?_assertEqual(235077287, MMSI),
     ?_assertEqual(under_way_using_engine, NS),
     ?_assertEqual(turning_right_more_than_5deg_30sec, ROT),
     ?_assertEqual(true, almost_equal(4.0, SOG, 0.00001)),
     ?_assertEqual(unaugmented_gnss_greater_than_10m, PA),
     ?_assertEqual(true, almost_equal(-3.4178567, Lon, 0.00001)),
     ?_assertEqual(true, almost_equal(56.0134250, Lat, 0.00001)),
     ?_assertEqual(true, almost_equal(138.0, COG, 0.00001)),
     ?_assertEqual(true, almost_equal(170.0, TH, 0.00001)),
     ?_assertEqual(40, TS),
     ?_assertEqual(not_available, MI),
     ?_assertEqual(raim_not_in_use, RF)].

decode_aid_to_nav_short_payload() ->
    Sentence = aid_to_nav_short_payload(),
    {Code, Reason, Sentence} = aisle:decode(Sentence),
    [?_assertEqual(error, Code),
     ?_assertEqual(payload_error, Reason)].

decode_bsr_short_payload() ->
    Sentence = bsr_short_payload(),
    {Code, Reason, Sentence} = aisle:decode(Sentence),
    [?_assertEqual(error, Code),
     ?_assertEqual(payload_error, Reason)].

decode_no_star() ->
    Sentence = no_star_sentence(),
    {Code, Reason, Sentence} = aisle:decode(Sentence),
    [?_assertEqual(error, Code),
     ?_assertEqual(insufficient_elements, Reason)].

acc_frags_check() ->
    S1 = frags_sample(),

    % The sentences should be grouped into 3 messages.
    {_, Msgs} = aisle:accum_msgs(S1, []),

    [?_assertEqual(3, length(Msgs))].

decode_frag_message_check() ->
    S1 = frags_sample(),

    % The sentences should be grouped into 3 messages.
    {_, _Msgs} = aisle:accum_msgs(S1, []),

    % Decode the grouped messages.
    % aisle:decode_msgs(Msgs),
    [].

decode_batch_error() ->
    Batch = ["qwerty1234"],
    {ok, Ret, _} = aisle:decode2(Batch, []),
    [] = Ret,
    [].

decode_batch() ->
    Batch = [sample_sentence1(), base_station_report1()],
    {ok, Msgs, RemFrags} = aisle:decode2(Batch, []),
    [{Code1, AisRec1}, {Code2, AisRec2}] = Msgs,
    CNB = aisle:get_data(AisRec1),
    BSR = aisle:get_data(AisRec2),
    MT1 = aisle:get_message_type(CNB),
    MT2 = aisle:get_bsr_message_type(BSR),
    [?_assertEqual(2, length(Msgs)),
     ?_assertEqual(0, length(RemFrags)),
     ?_assertEqual(ok, Code1),
     ?_assertEqual(ok, Code2),
     ?_assertEqual(pos_report_class_a, MT1),
     ?_assertEqual(base_station_report, MT2)].

sample_sentence1() -> 
    "!AIVDM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C".

no_star_sentence() ->
    "!AIVDM,1,1,,B,EvjFM;0Q2PVRa@97QUP00000000?p<6v@1NSH?1skh0010,45C".

base_station_report1() -> 
    "!AIVDM,1,1,,A,402=ac1v2rAM`OhIijP3BCw028Gm,0*2C".

base_station_report2() ->
    "!AIVDM,1,1,,A,402=ac1v2rAF0OhIiTP3BFg0269H,0*09".

aid_to_nav_report1() -> 
    "!AIVDM,1,1,,A,EvjFM;0Q2PVRa@97QUP00000000?p<6v@1NSH?1skh7P10,4*38".

aid_to_nav_report2() ->
    "!AIVDM,1,1,,A,E8VDET1aRR1WsppP00000000000Oq<b2@4BL1J;Gwpph20,4*4B".

aid_to_nav_short_payload() ->
    "!AIVDM,1,1,,B,EvjFM;0Q2PVR@97QUP00000000?p<6v@1NSH?1skhGP10,4*4B\n".

bsr_short_payload() ->
    "!AIVDM,1,1,,A,402=ac1vRkJ`OhIhrP3BL?026I`,0*74\n".

%% Sample containing a message split over two sentence fragments.
frags_sample() ->
    ["!AIVDM,1,1,,B,13P;NO9P00Oh?<>P3cvbwOwb0@OF,0*7E", 
     "!AIVDM,2,1,4,A,53P=6bT1snLo8L4KN21ADllDj22222222222221J1h?274Rh5;DSlnE28888,0*50", 
     "!AIVDM,2,2,4,A,88888888880,2*20", 
     "!AIVDM,1,1,,B,13P;J=9P00OhH`2P2uGPO?wd2<0s,0*41"].

%static_and_voyage_data_sentence_pair1() ->
%    "!AIVDM,2,1,2,A,54S`;l42BnK1K8ICR21`E@4L5>2222222222221D:hK6>6qU0?PTPAASkm80,0*6C\n" ++
%    "!AIVDM,2,2,2,A,PFH88888880,2*40".

static_and_voyage_data_sentence_pair2() ->
    ["!AIVDM,2,1,4,A,53P7f?000001I49G>20h5E860n2222222222220l1H8176o:044SlnE28888,0*31",
     "!AIVDM,2,2,4,A,88888888880,2*20"].

static_and_voyage_data_sentence_pair3() ->
    ["!AIVDM,2,1,2,A,53M@D`42;N`4mMISH00Pu0E@uDp000000000000l2PG684n60@B3mCQ8,0*51",
     "!AIVDM,2,2,2,A,43jCU0000000000,2*6D"].

response_to_interrogation1() ->
    "!AIVDM,1,1,,B,33P;vahOh`OhFc<P3AMmI5E@010i,0*6B".

bad_identifier() -> "!AIDVM,1,1,,B,177KQJ5000G?tO`K>RA1wUbN0TKH,0*5C".

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.


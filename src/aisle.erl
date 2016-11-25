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
    display/1, 
    parse_file/1,
    extract_all_mmsi/1,
    decode_message_type/1,
    payload_to_binary/1,
    int_to_bits/1,
    get_id/1,
    get_frag_count/1,
    get_frag_num/1,
    get_msg_id/1,
    get_radio_chan/1,
    get_data/1,
    get_fill_bits/1,
    get_checksum/1,
    get_message_type/1,
    get_repeat_indicator/1,
    get_mmsi/1,
    get_nav_status/1,
    get_rate_of_turn/1,
    get_speed_over_ground/1,
    get_position_accuracy/1,
    get_longitude/1,
    get_latitude/1,
    get_course_over_ground/1,
    get_true_heading/1,
    get_timestamp/1,
    get_maneuver_indicator/1,
    get_raim_flag/1,
    get_radio_status/1,
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
    
-record(cnb, {
        message_type,
        repeat_indicator,
        mmsi,
        nav_status,
        rate_of_turn,
        speed_over_ground,
        position_accuracy,
        longitude,
        latitude,
        course_over_ground,
        true_heading,
        timestamp,
        maneuver_indicator,
        raim_flag,
        radio_status}).
 
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
                data = decode_payload(Payload),
                fill_bits = decode_fill_bits(Fill),
                checksum = CS},
            {ok, AisRec};
        _ -> 
            {error, bad_identifier}
    end.

%% @ Parse a log file constructed of AIS sentences, one per line.
parse_file(Filename) ->
    {ok, S} = file:open(Filename, read),
    Acc = parse_lines(S, fun decode/1, []),
    file:close(S),
    Acc.

parse_lines(S, ProcFn, Acc) ->
    case io:get_line(S, "") of
        eof  -> 
            lists:reverse(Acc);
        Line -> 
            NewRec = ProcFn(Line),
            parse_lines(S, ProcFn, [NewRec|Acc])
    end.

%% @doc Decode the message ID field. This is often not set, so we need to trap
%% this.
decode_msg_id(MsgID)  ->
    case string:to_integer(MsgID) of
        {error, _} -> undefined;
        {X, _}     -> X
    end.

%% @doc Decode the data payload.
decode_payload(Payload) ->
    PayBin = payload_to_binary(list_to_binary(Payload)),
    <<MT:6,_Rest/bitstring>> = PayBin,
    %% Decode the message types we know how to decode, or simply return the
    %% message type.
    DMT = decode_message_type(MT),
    case DMT of
        pos_report_class_a -> 
            decode_cnb(PayBin);
        pos_report_class_a_assigned_schedule -> 
            decode_cnb(PayBin);
        pos_report_class_a_response_to_interrogation -> 
            decode_cnb(PayBin);
        _ ->
            DMT
    end.

%% @doc Decode the radio channel, either 'A' at 161.975 MHz or 'B' at 
%% 162.025 MHz. Sometimes mapped as '1' or '2'.
decode_radio_chan("A") -> radio_chan_a;
decode_radio_chan("B") -> radio_chan_b;
decode_radio_chan("1") -> radio_chan_a;
decode_radio_chan("2") -> radio_chan_b.

%% @doc Simple mapping of the fill bits string to the corresponding integer.
decode_fill_bits("0") -> 0;
decode_fill_bits("1") -> 1;
decode_fill_bits("2") -> 2;
decode_fill_bits("3") -> 3;
decode_fill_bits("4") -> 4;
decode_fill_bits("5") -> 5.

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

%% @doc Decode the message type in the data payload.
decode_message_type(1) -> pos_report_class_a;
decode_message_type(2) -> pos_report_class_a_assigned_schedule;
decode_message_type(3) -> pos_report_class_a_response_to_interrogation;
decode_message_type(4) -> base_station_report;
decode_message_type(5) -> static_and_voyage_data;
decode_message_type(6) -> binary_addressed_message;
decode_message_type(7) -> binary_acknowledge;
decode_message_type(8) -> binary_broadcast_message;
decode_message_type(9) -> standard_sar_aircraft_pos_report;
decode_message_type(10) -> utc_and_date_inquiry;
decode_message_type(11) -> utc_and_date_response;
decode_message_type(12) -> addressed_safety_related_message;
decode_message_type(13) -> safety_related_ack;
decode_message_type(14) -> safety_related_broadcast;
decode_message_type(15) -> interrogation;
decode_message_type(16) -> assignment_mode_command;
decode_message_type(17) -> dgnss_binary_broadcast_message;
decode_message_type(18) -> standard_class_b_cs_pos_report;
decode_message_type(19) -> extended_class_b_equipment_pos_report;
decode_message_type(20) -> data_link_management;
decode_message_type(21) -> aid_to_navigation_report;
decode_message_type(22) -> channel_management;
decode_message_type(23) -> group_assignment_command;
decode_message_type(24) -> static_data_report;
decode_message_type(25) -> single_slot_binary_message;
decode_message_type(26) -> multiple_slot_binary_message_with_comms_state;
decode_message_type(27) -> pos_report_for_long_range_applications;
decode_message_type(_) -> unknown_message_type.

%% @doc Decode the repeat indicator.
decode_repeat_indicator(0) -> no_repeats;
decode_repeat_indicator(1) -> one_repeat;
decode_repeat_indicator(2) -> two_repeats;
decode_repeat_indicator(3) -> do_not_repeat.

%% @doc Decode the 168-bit Common Navigation Block (CNB).
decode_cnb(<<MT:6,RI:2,MMSI:30,NS:4,ROT:8/signed,SOG:10,PA:1,Lon:28/signed,
    Lat:27/signed,COG:12,HDG:9,TS:6,MI:2,_Sp:3,RAIM:1,_RS:19>>) ->

    #cnb{
        message_type = decode_message_type(MT),
        repeat_indicator = decode_repeat_indicator(RI),
        mmsi = MMSI,
        nav_status = decode_nav_status(NS),
        rate_of_turn = decode_rate_of_turn(ROT),
        speed_over_ground = decode_sog(SOG),
        position_accuracy = decode_position_accuracy(PA),
        longitude = decode_longitude(Lon),
        latitude = decode_latitude(Lat),
        course_over_ground = decode_cog(COG),
        true_heading = decode_true_heading(HDG),
        timestamp = TS,
        maneuver_indicator = decode_maneuver_indicator(MI),
        raim_flag = decode_raim(RAIM)};

decode_cnb(_) ->
    {error, failed_to_decode_cnb}.

%% @doc Decode the navigation status bits.
decode_nav_status(0) -> under_way_using_engine;
decode_nav_status(1) -> at_anchor;
decode_nav_status(2) -> not_under_command;
decode_nav_status(3) -> restricted_manoeuverability;
decode_nav_status(4) -> constrained_by_her_draught;
decode_nav_status(5) -> moored;
decode_nav_status(6) -> aground;
decode_nav_status(7) -> engaged_in_fishing;
decode_nav_status(8) -> under_way_sailing;
decode_nav_status(9) -> reserved_for_hsg;
decode_nav_status(10) -> reserved_for_wig;
decode_nav_status(11) -> reserved;
decode_nav_status(12) -> reserved;
decode_nav_status(13) -> reserved;
decode_nav_status(14) -> ais_sart_is_active;
decode_nav_status(15) -> not_defined. 

%% @doc Decode the rate of turn field.
decode_rate_of_turn(0)    -> not_turning;
decode_rate_of_turn(127)  -> turning_right_more_than_5deg_30sec;
decode_rate_of_turn(-127) -> turning_left_more_than_5deg_30sec;
decode_rate_of_turn(-128) -> no_turn_information_available;
decode_rate_of_turn(R) when R >= 1, R =< 126 -> 
    V = R / 4.733,
    V * V;
decode_rate_of_turn(R) when R >= -126, R =< -1 -> 
    V = R / 4.733,
    -(V * V).

%% @doc Decode the speed over ground field.
decode_sog(1023) -> speed_not_available;
decode_sog(1022) -> more_than_102p2_knots;
decode_sog(X)    -> 0.1 * X.

%% @doc Decode the position accuracy field.
decode_position_accuracy(1) -> dgps_less_than_10m;
decode_position_accuracy(0) -> unaugmented_gnss_greater_than_10m. 

%% @doc Decode the Longitude parameter.
decode_longitude(16#6791AC0) -> not_available;
decode_longitude(X) -> X / 600000.0.

%% @doc Decode the Longitude parameter.
decode_latitude(16#3412140) -> not_available;
decode_latitude(X) -> X / 600000.0.

%% @doc Decod the course over ground field.
decode_cog(3600) -> not_available;
decode_cog(COG) -> 0.1 * COG.

%% @doc Decode the true heading field.
decode_true_heading(511) -> not_available;
decode_true_heading(X)   -> X.

%% @doc Decode the maneuver indicator field.
decode_maneuver_indicator(0) -> not_available;
decode_maneuver_indicator(1) -> no_special_maneuver;
decode_maneuver_indicator(2) -> special_maneuver.

%% @doc RAIM (Receiver Autonomous Integrity Monitoring) flag mapping.
decode_raim(0) -> raim_not_in_use; 
decode_raim(1) -> raim_in_use. 

%% Accessor functions for the AIS records.
get_id(#ais{id = X}) -> X. 
get_frag_count(#ais{frag_count = X}) -> X. 
get_frag_num(#ais{frag_num = X}) -> X. 
get_msg_id(#ais{msg_id = X}) -> X. 
get_radio_chan(#ais{radio_chan = X}) -> X. 
get_data(#ais{data = X}) -> X. 
get_fill_bits(#ais{fill_bits = X}) -> X. 
get_checksum(#ais{checksum = X}) -> X. 

%% Accessor functions for the common navigation block.
get_message_type(#cnb{message_type = X}) -> X.
get_repeat_indicator(#cnb{repeat_indicator = X}) -> X.
get_mmsi(#cnb{mmsi = X}) -> X.
get_nav_status(#cnb{nav_status = X}) -> X.
get_rate_of_turn(#cnb{rate_of_turn = X}) -> X.
get_speed_over_ground(#cnb{speed_over_ground = X}) -> X.
get_position_accuracy(#cnb{position_accuracy = X}) -> X.
get_longitude(#cnb{longitude = X}) -> X.
get_latitude(#cnb{latitude = X}) -> X.
get_course_over_ground(#cnb{course_over_ground = X}) -> X.
get_true_heading(#cnb{true_heading = X}) -> X.
get_timestamp(#cnb{timestamp = X}) -> X.
get_maneuver_indicator(#cnb{maneuver_indicator = X}) -> X.
get_raim_flag(#cnb{raim_flag = X}) -> X.
get_radio_status(#cnb{radio_status = X}) -> X.

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

%% @doc Display the records as text.
display(AisRecs) when is_list(AisRecs) ->
    lists:map(fun display/1, AisRecs);
display({ok, #ais{} = A}) ->
    io:format("****************************************~n"), 
    io:format("ID ~p, frag count ~p, frag_num ~p, msg_id ~p~n",
        [get_id(A), get_frag_count(A), get_frag_num(A), get_msg_id(A)]),
    io:format("radio chan ~p, fill bits ~p, checksum ~p~n",
        [get_radio_chan(A), get_fill_bits(A), get_checksum(A)]),
    display_data(get_data(A));
display(_) ->
    io:format("****************************************~n"), 
    io:format("Unrecognised record format.~n").

%% @doc Display the contents of the data fields. Depends upon the message
%% type.
display_data(#cnb{} = CNB) -> 
    display_cnb(CNB);
display_data(_) -> ok.

%% @doc Display the contents of the Common Navigational Block.
display_cnb(CNB) ->
    io:format("******************************~n"), 
    io:format("Common Navigation Block~n"), 
    io:format("Message type: ~p~n", [get_message_type(CNB)]),
    io:format("Repeat indicator: ~p~n", [get_repeat_indicator(CNB)]),
    io:format("MMSI: ~p~n", [get_mmsi(CNB)]),
    io:format("Nav status: ~p~n", [get_nav_status(CNB)]),
    io:format("Rate of turn: ~p~n", [get_rate_of_turn(CNB)]),
    io:format("Speed over ground: ~p~n", [get_speed_over_ground(CNB)]),
    io:format("Position accuracy: ~p~n", [get_position_accuracy(CNB)]),
    io:format("Lon: ~p~n", [get_longitude(CNB)]),
    io:format("Lat: ~p~n", [get_latitude(CNB)]),
    io:format("Course over ground: ~p~n", [get_course_over_ground(CNB)]),
    io:format("True heading: ~p~n", [get_true_heading(CNB)]),
    io:format("Timestamp: ~p~n", [get_timestamp(CNB)]),
    io:format("Maneuver indicator: ~p~n", [get_maneuver_indicator(CNB)]),
    io:format("RAIM flag: ~p~n", [get_raim_flag(CNB)]),
    io:format("Radio status: ~p~n", [get_radio_status(CNB)]).

%% Query function to pull out all the MMSI data in a list of AIS records.
extract_all_mmsi(AisRecs) when is_list(AisRecs) ->
    MList = lists:foldl(fun extract_mmsi/2, [], AisRecs),
    % Remove non-unique elements.
    S = sets:from_list(MList),
    sets:to_list(S).

extract_mmsi({ok, #ais{} = A}, Acc) ->
    case get_data(A) of
        #cnb{mmsi = M} -> [M|Acc];
        _              -> Acc
    end.


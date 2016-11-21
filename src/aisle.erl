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
    parse_file/1,
    decode_message_type/1,
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
    
-record(cnb, {
        message_type,
        repeat_indicator,
        mmsi,
        nav_status,
        rate_of_turn,
        true_heading,
        timestamp,
        raim_flag}).
 
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
                fill_bits = Fill,
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

%% @doc Decode the 168-bit Common Navigation Block (CNB).
decode_cnb(<<MT:6,RI:2,MMSI:30,NS:4,ROT:8/signed,_SOG:10,_PA:1,_Lon:28,_Lat:27,_COG:12,
             HDG:9,TS:6,_MI:2,_Sp:3,RAIM:1,_RS:19>>) ->

    #cnb{
        message_type = decode_message_type(MT),
        repeat_indicator = RI,
        mmsi = MMSI,
        nav_status = decode_nav_status(NS),
        rate_of_turn = decode_rate_of_turn(ROT),
        true_heading = decode_true_heading(HDG),
        timestamp = TS,
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

%% @doc Decode the true heading field.
decode_true_heading(511) -> not_available;
decode_true_heading(X)   -> X.

%% @doc RAIM (Receiver Autonomous Integrity Monitoring) flag mapping.
decode_raim(0) -> raim_not_in_use; 
decode_raim(1) -> raim_in_use. 

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


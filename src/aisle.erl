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
    decode2/2, 
    display/1, 
    parse_file/1,
    parse_file2/1,
    accum_msgs/2,
    decode_msgs/1,
    acc_frag/2,
    extract_payload/1,
    extract_all_mmsi/1,
    extract_all_message_types/1,
    decode_message_type/1,
    decode_repeat_indicator/1,
    payload_to_binary/1,
    int_to_bits/1,
    to_tokens/2,
    extract_cnb_records/1,
    extract_last_cnb_by_mmsi/1,
    update_cnb_map/2,
    decode_contains_cnb/1,
    contains_cnb/1,
    find_cnb/1]).

%% Accessors for the top level AIS structure.
-export([
    get_id/1,
    get_frag_count/1,
    get_frag_num/1,
    get_msg_id/1,
    get_radio_chan/1,
    get_data/1,
    get_fill_bits/1,
    get_checksum/1]).

-export([get_payload_type/1]).

%% Accessors for the CNB fields.
-export([
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
    get_radio_status/1]).

%% Accessors for base station reports. 
-export([
    get_bsr_message_type/1,
    get_bsr_repeat_indicator/1,
    get_bsr_mmsi/1,
    get_bsr_year_utc/1,
    get_bsr_month_utc/1,
    get_bsr_day_utc/1,
    get_bsr_hour_utc/1,
    get_bsr_minute_utc/1,
    get_bsr_second_utc/1,
    get_bsr_datetime/1,
    get_bsr_position_accuracy/1,
    get_bsr_longitude/1,
    get_bsr_latitude/1,
    get_bsr_type_of_epfd/1,
    get_bsr_raim_flag/1,
    get_bsr_sotdma_state/1]).

%% Accessors for aid to navigation reports.
-export([
    get_atnr_message_type/1,
    get_atnr_repeat_indicator/1,
    get_atnr_mmsi/1,
    get_atnr_aid_type/1,
    get_atnr_name/1,
    get_atnr_position_accuracy/1,
    get_atnr_longitude/1,
    get_atnr_latitude/1,
    get_atnr_dim_to_bow/1,
    get_atnr_dim_to_stern/1,
    get_atnr_dim_to_port/1,
    get_atnr_dim_to_starboard/1,
    get_atnr_type_of_epfd/1,
    get_atnr_timestamp/1,
    get_atnr_off_position/1,
    get_atnr_regional/1,
    get_atnr_raim_flag/1,
    get_atnr_virtual_aid/1,
    get_atnr_assigned_mode/1]).

%% Accessors for static and voyage data reports.
-export([
    get_svd_message_type/1,
    get_svd_repeat_indicator/1,
    get_svd_mmsi/1
        ]).

%% String output routines.
-export([
    radio_chan_to_list/1,
    message_type_to_list/1,
    repeat_indicator_to_list/1,
    nav_status_to_list/1,
    rate_of_turn_to_list/1,
    sog_to_list/1,
    position_accuracy_to_list/1,
    longitude_to_list/1,
    latitude_to_list/1,
    cog_to_list/1,
    true_heading_to_list/1,
    maneuver_indicator_to_list/1,
    raim_to_list/1,
    epfd_fix_type_to_list/1,
    aid_type_to_list/1,
    position_status_to_list/1,
    virtual_aid_to_list/1,
    assigned_mode_to_list/1]).

-export([get_unsupported_messages/1, get_unsupported_message_types/1]).

-export([is_ais_rec/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitions.

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
 
-record(base_sr, {
    message_type,
    repeat_indicator,
    mmsi,
    year_utc,
    month_utc,
    day_utc,
    hour_utc,
    minute_utc,
    second_utc,
    position_accuracy,
    longitude,
    latitude,
    type_of_epfd,
    raim_flag,
    sotdma_state}).
 
-record(atnr, {
    message_type,
    repeat_indicator,
    mmsi,
    aid_type,
    name,
    position_accuracy,
    longitude,
    latitude,
    dim_to_bow,
    dim_to_stern,
    dim_to_port,
    dim_to_starboard,
    type_of_epfd,
    timestamp,
    off_position,
    regional,
    raim_flag,
    virtual_aid,
    assigned_mode}).

-record(bbm, {
    message_type,
    repeat_indicator,
    mmsi,
    dac,
    fid,
    data}).

-record(svd, {
    message_type,
    repeat_indicator,
    mmsi,
    ais_version,
    imo,
    call_sign,
    vessel_name,
    ship_type,
    dim_to_bow,
    dim_to_stern,
    dim_to_port,
    dim_to_starboard,
    epfd_fix,
    eta_month,
    eta_day,
    eta_hour,
    eta_min,
    draught,
    destination,
    dte}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-opaque ais() :: #ais{}.
-export_type([ais/0]).

-opaque cnb() :: #cnb{}.
-export_type([cnb/0]).

-opaque base_sr() :: #base_sr{}.
-export_type([base_sr/0]).

-opaque atnr() :: #atnr{}.
-export_type([atnr/0]).

-opaque bbm() :: #bbm{}.
-export_type([bbm/0]).

-opaque svd() :: #svd{}.
-export_type([svd/0]).

-type payload_data() :: cnb() | base_sr() | atnr() | bbm() | svd().
-export_type([payload_data/0]).

%% Define a complete input message, which may comprise one or more sentence
%% fragments.
%-type ais_msg() :: [string()] | string().

-type optional_float() :: not_available | float().

-type fill_bits_char() :: $0 | $1 | $2 | $3 | $4 | $5.
-type fill_bits() :: 0..5.

-type radio_chan_char() :: $A | $B | $1 | $2.
-type radio_chan() :: radio_chan_a | radio_chan_b.

-type pos_acc() :: dgps_less_than_10m | unaugmented_gnss_greater_than_10m.

-type maneuver_indicator() :: 
    not_available | no_special_maneuver | special_maneuver.

-type raim() :: raim_not_in_use | raim_in_use.

-type aid_type() :: 
    default_not_specified |
    reference_point | 
    racon_radar_transponder | 
    fixed_structure_off_shore |
    reserved |
    light_without_sectors |
    light_with_sectors |
    leading_light_front |
    leading_light_rear |
    beacon_cardinal_north |
    beacon_cardinal_east |
    beacon_cardinal_south |
    beacon_cardinal_west |
    beacon_port_hand |
    beacon_starboard_hand |
    beacon_preferred_channel_port_hand |
    beacon_preferred_channel_starboard_hand |
    beacon_isolated_danger |
    beacon_safe_water |
    beacon_special_mark |
    cardinal_mark_north |
    cardinal_mark_east |
    cardinal_mark_south |
    cardinal_mark_west |
    port_hand_mark |
    starboard_hand_mark |
    preferred_channel_port_hand |
    preferred_channel_starboard_hand |
    isolated_danger |
    safe_water |
    special_mark |
    light_vessel.

-type position_status() :: on_position | off_position.

-type virtual_aid() :: real_aid_to_nav | virtual_aid_to_nav.

-type assigned_mode() :: autonomous_mode | assigned_mode.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function definitions.

%% @doc Decode an AIS sentence.
-spec decode(Sentence::string()) -> Ret when
    Sentence :: string() | binary(),
    Ret :: {ok, ais()} | {error, Reason, Sentence} |
           {error, Reason, Opt, Sentence},
    Reason :: atom(),
    Opt :: any().
decode(Sentence) when is_binary(Sentence) ->
    decode(binary_to_list(Sentence));
decode(Sentence) when is_list(Sentence) ->
    %io:format("~p~n", [Sentence]),
    Tokens = to_tokens(Sentence, ",*"),
    case Tokens of
        [Id, FragCount, FragNum, MsgID, Chan, Payload,Fill, CS|_Rest] ->
            %io:format("FC ~p FN ~p, MID ~p, Chan ~p, Fill ~p~n", [FragCount, FragNum, MsgID, Chan, Fill]),
            FillBits = decode_fill_bits(Fill),
            case Id of 
                "!AIVDM" -> 
                    case decode_payload(Payload, FillBits) of
                        {ok, PayloadData} ->
                            AisRec = #ais{
                                id = aivdm,
                                frag_count = list_to_integer(FragCount),
                                frag_num = list_to_integer(FragNum),
                                msg_id = decode_msg_id(MsgID),
                                radio_chan = decode_radio_chan(Chan),
                                data = PayloadData, 
                                fill_bits = decode_fill_bits(Fill),
                                checksum = CS},
                            {ok, AisRec};
                        {error, Reason} ->
                            {error, Reason, Sentence};
                        {error, Reason, Opt} ->
                            {error, Reason, Opt, Sentence}
                    end;
                _ -> 
                    {error, bad_identifier, Sentence}
            end;
        _ ->
            {error, insufficient_elements, Sentence}
    end.

decode2(Lines, InitFrags) -> 
    Ret = accum_msgs(Lines, InitFrags),
    {Frags, GroupedSentences} = Ret,
    Recs = lists:map(fun decode_msgs/1, GroupedSentences),
    {ok, Recs, Frags}. 

%% @doc Trim the fill bits from the end of the payload.
-spec trim_payload(bitstring(), fill_bits()) -> bitstring().
trim_payload(Payload, FillBits) when is_bitstring(Payload) ->
    FullLen = bit_size(Payload),
    TrimSize = FullLen - FillBits,
    <<TrimPayload:TrimSize/bitstring,_/bitstring>> = Payload,
    TrimPayload.

extract_payload(Sentence) when is_binary(Sentence) ->
    extract_payload(binary_to_list(Sentence));
extract_payload(Sentence) when is_list(Sentence) ->
    io:format("~p~n", [Sentence]),
    Tokens = to_tokens(Sentence, ",*"),
    case Tokens of
        [_, _, _, _, _, Payload, Fill, _|_Rest] ->
            PayBin = payload_to_binary(list_to_binary(Payload)),
            FillBits = decode_fill_bits(Fill),
            trim_payload(PayBin, FillBits);
        _ ->
            {error, insufficient_elements}
    end.

%% @doc Parse a log file constructed of AIS sentences, one per line.
-spec parse_file(string()) -> [{ok, ais()} | {error, atom()}] | {error, atom()}. 
parse_file(Filename) when is_list(Filename) ->
    case file:open(Filename, [read]) of
        {ok, S} -> 
            Acc = parse_lines(S, fun decode/1, []),
            file:close(S),
            Acc;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Parse the lines in the string specified.
parse_lines(S, ProcFn, Acc) ->
    case io:get_line(S, "") of
        eof  -> 
            lists:reverse(Acc);
        Line -> 
            NewRec = ProcFn(Line),
            parse_lines(S, ProcFn, [NewRec|Acc])
    end.

%% @doc Parse a log file constructed of AIS sentences, one per line.
-spec parse_file2(string()) -> [{ok, ais()} | {error, atom()}] | {error, atom()}. 
parse_file2(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> 
            Lines = [binary_to_list(Bin) || Bin <- binary:split(Binary,<<"\n">>,[global])],
            {_, GroupedSentences} = accum_msgs(Lines, []),
            lists:map(fun decode_msgs/1, GroupedSentences);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Accumulate sentence fragments into complete messages.
accum_msgs(Sentences, InitFrags) -> 
    {RemFrags, Msgs} = lists:foldl(fun acc_frag/2, {InitFrags, []}, Sentences),
    {RemFrags, lists:reverse(Msgs)}.

%% @doc Accumulate the sentence fragments into a list of complete messages.
-spec acc_frag(Sentence::string(), Acc::{Frags, Msgs}) -> NewAcc 
    when Frags::[string()],
         Msgs::[string()],
         NewAcc::{Frags, Msgs}.
acc_frag(Sentence, {Frags, Msgs} = Acc) ->
    Tokens = to_tokens(Sentence, ",*"),
    case Tokens of
        [_, FragCount, FragNum, _, _, _, _, _|_Rest] ->
            case {FragCount, FragNum} of
                {FragCount, FragCount} -> 
                    NewMsgs = [lists:reverse([Sentence|Frags])|Msgs],
                    {[], NewMsgs};
                {FragCount, FragNum} when FragCount > FragNum ->
                    {[Sentence|Frags], Msgs};
                _ ->
                    % Error, reset the fragments.
                    {[], Msgs}
            end;
        _ ->
            Acc
    end.

%% @doc Decode a list of sentences comprising a message. A message in this 
%% case is a list of fragments comprising a complete AIS message.
decode_msgs([Msg]) ->
    decode(Msg);
decode_msgs(Msgs) when is_list(Msgs) ->
    Pay = accum_payload(Msgs),
    [First|_] = Msgs,
    %io:format("~p~n", [First]),
        Tokens = to_tokens(First, ",*"),
        case Tokens of
            [Id, FragCount, FragNum, MsgID, Chan, _, _, CS|_Rest] ->
                %io:format("FC ~p FN ~p, MID ~p, Chan ~p, Fill ~p~n", [FragCount, FragNum, MsgID, Chan, Fill]),
                case Id of 
                    "!AIVDM" -> 
                        case decode_payload(Pay, 0) of
                            {ok, PayloadData} ->
                                AisRec = #ais{
                                    id = aivdm,
                                    frag_count = list_to_integer(FragCount),
                                    frag_num = list_to_integer(FragNum),
                                    msg_id = decode_msg_id(MsgID),
                                    radio_chan = decode_radio_chan(Chan),
                                    data = PayloadData, 
                                    fill_bits = 0, 
                                    checksum = CS},
                                {ok, AisRec};
                            {error, Reason} ->
                                {error, Reason};
                            {error, Reason, Opt} ->
                                {error, Reason, Opt}

                        end;
                    _ -> 
                        {error, bad_identifier}
                end;
            _ ->
                {error, insufficient_elements}
        end.


accum_payload(Msgs) when is_list(Msgs) ->
    Bins = lists:map(fun extract_payload/1, Msgs),
    erlang:iolist_to_binary(Bins). 

%% @doc Decode the message ID field. This is often not set, so we need to trap
%% this.
-spec decode_msg_id(string()) -> undefined | integer().
decode_msg_id(MsgID)  ->
    case string:to_integer(MsgID) of
        {error, _} -> undefined;
        {X, _}     -> X
    end.

%% @doc Decode the data payload.
-spec decode_payload(Payload, FillBits) -> Ret when
    Payload :: string() | binary(),
    FillBits :: non_neg_integer(),
    Ret :: {ok, payload_data()} | {error, Reason::atom()} | 
        {error, Reason::atom(), Opt::any()}.
decode_payload(Payload, FillBits) when is_list(Payload) ->
    PayBin = payload_to_binary(list_to_binary(Payload)),
    TrimPayBin = trim_payload(PayBin, FillBits),
    decode_payload(TrimPayBin, 0);
decode_payload(Payload, _FillBits) ->
    TrimPayBin = Payload,
    <<MT:6,_Rest/bitstring>> = TrimPayBin,
    %% Decode the message types we know how to decode, or simply return the
    %% message type.
    DMT = decode_message_type(MT),
    case DMT of
        pos_report_class_a -> 
            decode_cnb(TrimPayBin);
        pos_report_class_a_assigned_schedule -> 
            decode_cnb(TrimPayBin);
        pos_report_class_a_response_to_interrogation -> 
            decode_cnb(TrimPayBin);
        base_station_report ->
            decode_bsr(TrimPayBin);
        static_and_voyage_data ->
            %io:format("decode svd: ~p~n", [TrimPayBin]),
            decode_static_and_voyage_data(TrimPayBin);
        aid_to_navigation_report ->
            %io:format("Aid to Nav bits ~p size ~p ~p~n", [bit_size(TrimPayBin), byte_size(TrimPayBin), TrimPayBin]),
            decode_aid_to_navigation_report(TrimPayBin);
        binary_broadcast_message -> 
            decode_binary_broadcast_message(TrimPayBin);
        _ ->
            {error, unsupported_message_type, DMT}
    end.

%% @doc Decode the radio channel, either 'A' at 161.975 MHz or 'B' at 
%% 162.025 MHz. Sometimes mapped as '1' or '2'.
-spec decode_radio_chan(ChanTok::[radio_chan_char()]) -> radio_chan(). 
decode_radio_chan("A") -> radio_chan_a;
decode_radio_chan("B") -> radio_chan_b;
decode_radio_chan("1") -> radio_chan_a;
decode_radio_chan("2") -> radio_chan_b.

%% @doc Convert the radio channel to string.
-spec radio_chan_to_list(radio_chan()) -> string().
radio_chan_to_list(radio_chan_a) -> "radio channel A 161.975 MHz";
radio_chan_to_list(radio_chan_b) -> "radio channel B 162.025 MHz".

%% @doc Simple mapping of the fill bits string to the corresponding integer.
-spec decode_fill_bits(FillTok::[fill_bits_char()]) -> fill_bits().
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
-spec payload_to_binary(binary()) -> binary().
payload_to_binary(PayloadData) when is_binary(PayloadData) ->
    << <<(aisle:int_to_bits(X)):6>> || <<X:8>> <= PayloadData >>.

%% Reverse the ASCII mapping of 6 bit values.
-spec int_to_bits(non_neg_integer()) -> non_neg_integer().
int_to_bits(X) when X >= 0 ->
    Y = X - 48,
    case Y >= 40 of
        true  -> Y - 8;
        false -> Y
    end.

%% Map the 6 bit encoded characters back to normal ASCII.
-spec bits_to_ascii(non_neg_integer()) -> non_neg_integer().
bits_to_ascii(X) when X >= 0 ->
    case X >= 32 of 
        true  -> X;
        false -> X + 64 
    end.

%% @doc Decode the message type in the data payload.
-spec decode_message_type(pos_integer()) -> atom().
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

%% @doc Convert the message type to string.
-spec message_type_to_list(atom()) -> string().
message_type_to_list(pos_report_class_a) -> "Position Report Class A";
message_type_to_list(pos_report_class_a_assigned_schedule) -> "Position Report Class A Assigned Schedule";
message_type_to_list(pos_report_class_a_response_to_interrogation) -> "Position Report Class A Respond To Interrogation";
message_type_to_list(base_station_report) -> "Base Station Report";
message_type_to_list(static_and_voyage_data) -> "Static and Voyage Data";
message_type_to_list(binary_addressed_message) -> "Binary Addressed Message";
message_type_to_list(binary_acknowledge) -> "Binary Acknowledge";
message_type_to_list(binary_broadcast_message) -> "Binary Broadcast Message";
message_type_to_list(standard_sar_aircraft_pos_report) -> "Standard SAR Aircraft Position Report";
message_type_to_list(utc_and_date_inquiry) -> "UTC and Date Enquiry";
message_type_to_list(utc_and_date_response) -> "UTC and Date Response";
message_type_to_list(addressed_safety_related_message) -> "Addressed Safety Related Message";
message_type_to_list(safety_related_ack) -> "Safety Related Acknowledge";
message_type_to_list(safety_related_broadcast) -> "Safety Related Broadcast";
message_type_to_list(interrogation) -> "Interrogation";
message_type_to_list(assignment_mode_command) -> "Assignment Mode Command";
message_type_to_list(dgnss_binary_broadcast_message) -> "DGNSS Binary Broadcast Message";
message_type_to_list(standard_class_b_cs_pos_report) -> "Standard Class B CS Position Report";
message_type_to_list(extended_class_b_equipment_pos_report) -> "Standard Class B Equipment Position Report";
message_type_to_list(data_link_management) -> "Data Link Management";
message_type_to_list(aid_to_navigation_report) -> "Aid to Navigation Report";
message_type_to_list(channel_management) -> "Channel Management";
message_type_to_list(group_assignment_command) -> "Group Assignment Command";
message_type_to_list(static_data_report) -> "Static Data Report";
message_type_to_list(single_slot_binary_message) -> "Single Slot Binary Message";
message_type_to_list(multiple_slot_binary_message_with_comms_state) -> "Multiple Slot Binary Message with Comms State";
message_type_to_list(pos_report_for_long_range_applications) -> "Position Report for Long Range Applications";
message_type_to_list(unknown_message_type) -> "Unknown Message Type".

%% @doc Decode the repeat indicator.
-spec decode_repeat_indicator(non_neg_integer()) -> atom().
decode_repeat_indicator(0) -> no_repeats;
decode_repeat_indicator(1) -> one_repeat;
decode_repeat_indicator(2) -> two_repeats;
decode_repeat_indicator(3) -> do_not_repeat.

%% @doc Convert the repeat indicator to list.
-spec repeat_indicator_to_list(atom()) -> string().
repeat_indicator_to_list(no_repeats) -> "no repeats";
repeat_indicator_to_list(one_repeat) -> "one_repeat";
repeat_indicator_to_list(two_repeats) -> "two_repeats";
repeat_indicator_to_list(do_not_repeat) -> "do_not_repeat".

%% @doc Decode the 168-bit Common Navigation Block (CNB).
-spec decode_cnb(binary()) -> {ok, cnb()} | {error, Reason::atom()}.
decode_cnb(<<MT:6,RI:2,MMSI:30,NS:4,ROT:8/signed,SOG:10,PA:1,Lon:28/signed,
    Lat:27/signed,COG:12,HDG:9,TS:6,MI:2,_Sp:3,RAIM:1,_RS:19>>) ->

    {ok, #cnb{
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
        raim_flag = decode_raim(RAIM)}};

decode_cnb(_) ->
    {error, failed_to_decode_cnb}.

%% @doc Decode the navigation status bits.
-spec decode_nav_status(non_neg_integer()) -> atom().
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
decode_nav_status(11) -> power_driven_vessel_towing_astern;
decode_nav_status(12) -> power_driven_vessel_pushing_ahead_or_towing_alongside;
decode_nav_status(13) -> reserved;
decode_nav_status(14) -> ais_sart_is_active;
decode_nav_status(15) -> undefined. 

%% @doc Convert the NAV status indication to string.
-spec nav_status_to_list(atom()) -> string().
nav_status_to_list(under_way_using_engine) -> "under way using engine";
nav_status_to_list(at_anchor) -> "at anchor";
nav_status_to_list(not_under_command) -> "not under command";
nav_status_to_list(restricted_manoeuverability) -> "restricted manoeuverability";
nav_status_to_list(constrained_by_her_draught) -> "constrained by her draught";
nav_status_to_list(moored) -> "moored";
nav_status_to_list(aground) -> "aground";
nav_status_to_list(engaged_in_fishing) -> "engaged in fishing";
nav_status_to_list(under_way_sailing) -> "under way sailing";
nav_status_to_list(reserved_for_hsg) -> "reserved for HSG";
nav_status_to_list(reserved_for_wig) -> "reserved for WIG";
nav_status_to_list(power_driven_vessel_towing_astern) -> "power driven vessel towing astern";
nav_status_to_list(power_driven_vessel_pushing_ahead_or_towing_alongside) -> "power driven vessel pushing ahead or towing alongside";
nav_status_to_list(reserved) -> "reserved";
nav_status_to_list(ais_sart_is_active) -> "AIS-SART is active";
nav_status_to_list(undefined) -> "undefined". 

%% @doc Decode the rate of turn field.
-spec decode_rate_of_turn(-128..127) -> atom().
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

%% @doc Convert rate of turn to list.
-spec rate_of_turn_to_list(atom() | float()) -> string().
rate_of_turn_to_list(not_turning) -> "not turning";
rate_of_turn_to_list(turning_right_more_than_5deg_30sec) -> "turning right more than 5deg/30sec";
rate_of_turn_to_list(turning_left_more_than_5deg_30sec) -> "turning left more than 5deg/30sec";
rate_of_turn_to_list(no_turn_information_available) -> "no turn information available";
rate_of_turn_to_list(X) when is_number(X) -> io_lib:format("~p", [X]).

%% @doc Decode the speed over ground field.
-spec decode_sog(0..1023) -> atom() | float().
decode_sog(1023) -> speed_not_available;
decode_sog(1022) -> more_than_102p2_knots;
decode_sog(X)    -> 0.1 * X.

%% @doc Convert speed over ground to list.
-spec sog_to_list(atom() | float()) -> string().
sog_to_list(speed_not_available) -> "speed not available";
sog_to_list(more_than_102p2_knots) -> "more than 102.2 knots";
sog_to_list(X) when is_number(X) -> io_lib:format("~p", [X]).

%% @doc Decode the position accuracy field.
-spec decode_position_accuracy(0 | 1) -> pos_acc().
decode_position_accuracy(1) -> dgps_less_than_10m;
decode_position_accuracy(0) -> unaugmented_gnss_greater_than_10m. 

%% @doc Convert position accuracy to list.
-spec position_accuracy_to_list(pos_acc()) -> string().
position_accuracy_to_list(dgps_less_than_10m) -> "DGPS < 10m";
position_accuracy_to_list(unaugmented_gnss_greater_than_10m) -> "Unaugmented GNSS > 10m".

%% @doc Decode the Longitude parameter.
-spec decode_longitude(integer()) -> optional_float(). 
decode_longitude(16#6791AC0) -> not_available;
decode_longitude(X) -> X / 600000.0.

%% @doc Convert Longitude to string.
-spec longitude_to_list(optional_float()) -> string().
longitude_to_list(X) -> optional_float_to_list(X).

%% @doc Decode the Longitude parameter.
-spec decode_latitude(integer()) -> optional_float(). 
decode_latitude(16#3412140) -> not_available;
decode_latitude(X) -> X / 600000.0.

%% @doc Convert Latitude to string.
-spec latitude_to_list(optional_float()) -> string().
latitude_to_list(X) -> optional_float_to_list(X).

%% @doc Decod the course over ground field.
-spec decode_cog(non_neg_integer()) -> optional_float().
decode_cog(3600) -> not_available;
decode_cog(COG) -> 0.1 * COG.

%% @doc Convert COG to string.
-spec cog_to_list(optional_float()) -> string().
cog_to_list(not_available) -> "not available";
cog_to_list(X) when is_number(X) -> io_lib:format("~p", [X]).

%% @doc Decode the true heading field.
-spec decode_true_heading(non_neg_integer()) -> 
    not_available | non_neg_integer().
decode_true_heading(511) -> not_available;
decode_true_heading(X)   -> X.

%% @doc Convert the true heading to list.
-spec true_heading_to_list(not_available | non_neg_integer()) -> string().
true_heading_to_list(not_available) -> "not available";
true_heading_to_list(X) when is_number(X) -> io_lib:format("~p", [X]).

%% @doc Decode the maneuver indicator field.
-spec decode_maneuver_indicator(0..2) -> maneuver_indicator().
decode_maneuver_indicator(0) -> not_available;
decode_maneuver_indicator(1) -> no_special_maneuver;
decode_maneuver_indicator(2) -> special_maneuver.

%% @doc Convert the maneuver indicator to list.
-spec maneuver_indicator_to_list(maneuver_indicator()) -> string().
maneuver_indicator_to_list(not_available) -> "not_available";
maneuver_indicator_to_list(no_special_maneuver) -> "no special maneuver";
maneuver_indicator_to_list(special_maneuver) -> "special maneuver".

%% @doc RAIM (Receiver Autonomous Integrity Monitoring) flag mapping.
-spec decode_raim(0 | 1) -> raim().
decode_raim(0) -> raim_not_in_use; 
decode_raim(1) -> raim_in_use. 

%% @doc Convert the RAIM (Receiver Autonomous Integrity Monitoring) flag to
%% list.
-spec raim_to_list(raim()) -> string().
raim_to_list(raim_not_in_use) -> "RAIM not in use";
raim_to_list(raim_in_use) -> "RAIM in use".


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

%% @doc Decode the 168-bit Base Station Report (BSR). 
decode_bsr(<<MT:6,RI:2,MMSI:30,Y:14,M:4,D:5,H:5,Min:6,Sec:6,PA:1, 
    Lon:28/signed,Lat:27/signed,Type:4,_Sp:10,RAIM:1,SOTDMA:19/bitstring>>) ->
    {ok, #base_sr{
        message_type = decode_message_type(MT),
        repeat_indicator = decode_repeat_indicator(RI),
        mmsi = MMSI,
        year_utc = Y,
        month_utc = M,
        day_utc = D,
        hour_utc = H,
        minute_utc = Min,
        second_utc = Sec,
        position_accuracy = decode_position_accuracy(PA),
        longitude = decode_longitude(Lon),
        latitude = decode_latitude(Lat),
        type_of_epfd = decode_epfd_fix_type(Type),
        raim_flag = decode_raim(RAIM),
        sotdma_state = decode_sotdma_state(SOTDMA)}};
decode_bsr(_) ->
    {error, payload_error}.

decode_epfd_fix_type(0) -> undefined; 
decode_epfd_fix_type(1) -> gps;
decode_epfd_fix_type(2) -> glonass;
decode_epfd_fix_type(3) -> combined_gps_glonass;
decode_epfd_fix_type(4) -> loran_c;
decode_epfd_fix_type(5) -> chayka;
decode_epfd_fix_type(6) -> integrated_navigation_system;
decode_epfd_fix_type(7) -> surveyed;
decode_epfd_fix_type(8) -> galileo;
decode_epfd_fix_type(15) -> internal_gnss;
decode_epfd_fix_type(_) -> reserved.

epfd_fix_type_to_list(undefined) -> "undefined (default)"; 
epfd_fix_type_to_list(gps) -> "GPS";
epfd_fix_type_to_list(glonass) -> "GLONASS";
epfd_fix_type_to_list(combined_gps_glonass) -> "combined GPS/GLONASS";
epfd_fix_type_to_list(loran_c) -> "Loran-C";
epfd_fix_type_to_list(chayka) -> "Chayka";
epfd_fix_type_to_list(integrated_navigation_system) -> "integrated navigation system";
epfd_fix_type_to_list(surveyed) -> "surveyed";
epfd_fix_type_to_list(galileo) -> "Galileo";
epfd_fix_type_to_list(internal_gnss) -> "internal GNSS";
epfd_fix_type_to_list(reserved) -> "reserved".

decode_sotdma_state(<<Sync:2,SlotTimeOut:3,SubMsg:14/bitstring>>) ->
    DecSync = decode_sync_state(Sync),
    DecSlotTimeOut = decode_slot_timeout(SlotTimeOut),
    DecSubMsg = decode_sub_message(DecSlotTimeOut, SubMsg),
    {DecSync, DecSlotTimeOut, DecSubMsg}.

decode_sync_state(0) -> utc_direct;
decode_sync_state(1) -> utc_indirect;
decode_sync_state(2) -> sync_to_base;
decode_sync_state(3) -> sync_to_station.

%% @doc Decode the slot timeout field. Different sub messages are indicated 
%% by the timeout value.
decode_slot_timeout(0) -> {0, slot_offset};
decode_slot_timeout(1) -> {1, utc_hour_and_minute};
decode_slot_timeout(2) -> {2, slot_number};
decode_slot_timeout(3) -> {3, received_stations};
decode_slot_timeout(4) -> {4, slot_number};
decode_slot_timeout(5) -> {5, received_stations};
decode_slot_timeout(6) -> {6, slot_number};
decode_slot_timeout(7) -> {7, received_stations}.

%% @doc Decoding of the sub message depending upon the type indicated by the 
%% slot timeout parameter.
decode_sub_message({0, slot_offset}, <<SubMsg:14>>) -> 
    {slot_offset, SubMsg}; 
decode_sub_message({_, utc_hour_and_minute}, <<SubMsg:14>>) -> 
    decode_utc_hour_and_minute(<<SubMsg:14>>); 
decode_sub_message({_, slot_number}, <<SubMsg:14>>) -> 
    {slot_number, SubMsg}; 
decode_sub_message({_, received_stations}, <<SubMsg:14>>) ->
    {received_stations, SubMsg}.

decode_utc_hour_and_minute(<<Hour:5,Min:7,_:2>>) -> 
    {utc_hour_and_minute, Hour, Min}.

get_bsr_message_type(#base_sr{message_type = X}) -> X.
get_bsr_repeat_indicator(#base_sr{repeat_indicator = X}) -> X.
get_bsr_mmsi(#base_sr{mmsi = X}) -> X.
get_bsr_year_utc(#base_sr{year_utc = X}) -> X.
get_bsr_month_utc(#base_sr{month_utc = X}) -> X.
get_bsr_day_utc(#base_sr{day_utc = X}) -> X.
get_bsr_hour_utc(#base_sr{hour_utc = X}) -> X.
get_bsr_minute_utc(#base_sr{minute_utc = X}) -> X.
get_bsr_second_utc(#base_sr{second_utc = X}) -> X.
get_bsr_datetime(#base_sr{
    year_utc = Y, 
    month_utc = M, 
    day_utc = D, 
    hour_utc = H,
    minute_utc = Min,
    second_utc = S}) -> {{Y,M,D},{H,Min,S}}.
get_bsr_position_accuracy(#base_sr{position_accuracy = X}) -> X.
get_bsr_longitude(#base_sr{longitude = X}) -> X.
get_bsr_latitude(#base_sr{latitude = X}) -> X.
get_bsr_type_of_epfd(#base_sr{type_of_epfd = X}) -> X.
get_bsr_raim_flag(#base_sr{raim_flag = X}) -> X.
get_bsr_sotdma_state(#base_sr{sotdma_state = X}) -> X.

%% @doc Decode the aid to navigation report.
decode_aid_to_navigation_report(<<MT:6,RI:2,MMSI:30,AT:5,Name:120/bitstring,
    PA:1,Lon:28/signed,Lat:27,DimBow:9,DimStern:9,DimPort:6,DimStar:6,EPFD:4,
    TS:6,Off:1,Reg:8/bitstring,RAIM:1,VA:1,AM:1,_Sp:1,Ext/bitstring>> = Sentence) ->

    %% We need to check for extra name data at the end of the field.
    BitLength = bit_size(Sentence),
    case BitLength of 
        272 ->
            DecName = decode_name(Name); 
        _   ->
            DecName = decode_name(Name, Ext) 
    end,
        
    {ok, #atnr{
        message_type = decode_message_type(MT),
        repeat_indicator = decode_repeat_indicator(RI),
        mmsi = MMSI,
        aid_type = decode_aid_type(AT),
        name = DecName,
        position_accuracy = decode_position_accuracy(PA),
        longitude = decode_longitude(Lon),
        latitude = decode_latitude(Lat),
        dim_to_bow = DimBow,
        dim_to_stern = DimStern,
        dim_to_port = DimPort,
        dim_to_starboard = DimStar,
        type_of_epfd = decode_epfd_fix_type(EPFD),
        timestamp = TS,
        off_position = decode_off_position(Off),
        regional = Reg,
        raim_flag = decode_raim(RAIM),
        virtual_aid = decode_virtual_aid_flag(VA),
        assigned_mode = decode_assigned_mode_flag(AM)}};
decode_aid_to_navigation_report(_) ->
    {error, payload_error}.

%% @doc Decode the aid type parameter.
-spec decode_aid_type(pos_integer()) -> aid_type().
decode_aid_type(0) -> default_not_specified;
decode_aid_type(1) -> reference_point;
decode_aid_type(2) -> racon_radar_transponder;
decode_aid_type(3) -> fixed_structure_off_shore;
decode_aid_type(4) -> reserved;
decode_aid_type(5) -> light_without_sectors;
decode_aid_type(6) -> light_with_sectors;
decode_aid_type(7) -> leading_light_front;
decode_aid_type(8) -> leading_light_rear;
decode_aid_type(9) -> beacon_cardinal_north;
decode_aid_type(10) -> beacon_cardinal_east;
decode_aid_type(11) -> beacon_cardinal_south;
decode_aid_type(12) -> beacon_cardinal_west;
decode_aid_type(13) -> beacon_port_hand;
decode_aid_type(14) -> beacon_starboard_hand;
decode_aid_type(15) -> beacon_preferred_channel_port_hand;
decode_aid_type(16) -> beacon_preferred_channel_starboard_hand;
decode_aid_type(17) -> beacon_isolated_danger;
decode_aid_type(18) -> beacon_safe_water;
decode_aid_type(19) -> beacon_special_mark;
decode_aid_type(20) -> cardinal_mark_north;
decode_aid_type(21) -> cardinal_mark_east;
decode_aid_type(22) -> cardinal_mark_south;
decode_aid_type(23) -> cardinal_mark_west;
decode_aid_type(24) -> port_hand_mark;
decode_aid_type(25) -> starboard_hand_mark;
decode_aid_type(26) -> preferred_channel_port_hand;
decode_aid_type(27) -> preferred_channel_starboard_hand;
decode_aid_type(28) -> isolated_danger;
decode_aid_type(29) -> safe_water;
decode_aid_type(30) -> special_mark;
decode_aid_type(31) -> light_vessel.

%% @doc Convert the aid type to string.
-spec aid_type_to_list(aid_type()) -> string().
aid_type_to_list(default_not_specified) -> "aid type not specified";
aid_type_to_list(reference_point) -> "reference point";
aid_type_to_list(racon_radar_transponder) -> "RACON (radar transponder marking a navigation hazard)";
aid_type_to_list(fixed_structure_off_shore) -> "fixed structure off shore";
aid_type_to_list(reserved) -> "reserved";
aid_type_to_list(light_without_sectors) -> "light, without sectors";
aid_type_to_list(light_with_sectors) -> "light, with sectors";
aid_type_to_list(leading_light_front) -> "leading light front";
aid_type_to_list(leading_light_rear) -> "leading light rear";
aid_type_to_list(beacon_cardinal_north) -> "beacon, cardinal N";
aid_type_to_list(beacon_cardinal_east) -> "beacon, cardinal E";
aid_type_to_list(beacon_cardinal_south) -> "beacon, cardinal S";
aid_type_to_list(beacon_cardinal_west) -> "beacon, cardinal W";
aid_type_to_list(beacon_port_hand) -> "beacon, port hand";
aid_type_to_list(beacon_starboard_hand) -> "beacon, starboard hand";
aid_type_to_list(beacon_preferred_channel_port_hand) -> "beacon, preferred channel port hand";
aid_type_to_list(beacon_preferred_channel_starboard_hand) -> "beacon, preferred channel starboard hand";
aid_type_to_list(beacon_isolated_danger) -> "beacon, isolated danger";
aid_type_to_list(beacon_safe_water) -> "beacon, safe water";
aid_type_to_list(beacon_special_mark) -> "beacon, special mark";
aid_type_to_list(cardinal_mark_north) -> "cardinal mark N";
aid_type_to_list(cardinal_mark_east) -> "cardinal mark E";
aid_type_to_list(cardinal_mark_south) -> "cardinal mark S";
aid_type_to_list(cardinal_mark_west) -> "cardinal mark W";
aid_type_to_list(port_hand_mark) -> "port hand mark";
aid_type_to_list(starboard_hand_mark) -> "starboard hand mark";
aid_type_to_list(preferred_channel_port_hand) -> "preferred channel port hand";
aid_type_to_list(preferred_channel_starboard_hand) -> "preferred channel starboard hand";
aid_type_to_list(isolated_danger) -> "isolated danger";
aid_type_to_list(safe_water) -> "safe water";
aid_type_to_list(special_mark) -> "special mark";
aid_type_to_list(light_vessel) -> "light vessel".

%% @doc Decode the fixed name field. Returns an indication of whether the
%% name field is full and the extra field needs to be decoded.
decode_name(BinName) when is_binary(BinName) ->
    Name = [bits_to_ascii(X) || <<X:6>> <= BinName],
    [LastChar|_] = lists:reverse(Name), 
    case LastChar of  
        $@ -> {ok, Name};
        _  -> {full, Name}
    end.

%% @doc Decoding function to handle the extra name field.
%% Not finished yet, does not deal with the extra.
decode_name(Name, Rem) when is_binary(Name), is_bitstring(Rem) ->
    ExtraLength = bit_size(Rem),
    % Characters are 6 bits, so slice to a multiple of 6.
    case ExtraLength rem 6 of
        0 ->
            ExtraNameBits = Rem;
        X ->
            Required = ExtraLength - X,
            <<ExtraNameBits:Required/bitstring,_/bitstring>> = Rem
    end,
    AllBits = <<Name/binary,ExtraNameBits/bitstring>>,
    decode_name(AllBits).

%% @doc Decode the off position indicator, used for floating 
%% aids-to-navigation. Only valid if UTC second is less than or equal to 59.
-spec decode_off_position(0..1) -> position_status().
decode_off_position(0) -> on_position;
decode_off_position(1) -> off_position.

%% @doc Convert the position status to list.
-spec position_status_to_list(position_status()) -> string().
position_status_to_list(on_position) -> "on position";
position_status_to_list(off_position) -> "off position".

%% @doc Decode the virtual aid to nav flag. Indicates whether the aid is real
%% and located at the specified position or if it is a virtual indication 
%% simulated by another station.
-spec decode_virtual_aid_flag(0..1) -> virtual_aid().
decode_virtual_aid_flag(0) -> real_aid_to_nav;
decode_virtual_aid_flag(1) -> virtual_aid_to_nav.

%% @doc Convert the virtual aid flag to string.
-spec virtual_aid_to_list(virtual_aid()) -> string().
virtual_aid_to_list(real_aid_to_nav) -> "real aid to navigation";
virtual_aid_to_list(virtual_aid_to_nav) -> "virtual aid to navigation".

%% @doc Decode the assigned mode flag. Indicates whether a station is 
%% operating autonomously or is in assigned mode.
-spec decode_assigned_mode_flag(0..1) -> assigned_mode().
decode_assigned_mode_flag(0) -> autonomous_mode;
decode_assigned_mode_flag(1) -> assigned_mode.

%% @doc Convert the assigned mode to string.
-spec assigned_mode_to_list(assigned_mode()) -> string().
assigned_mode_to_list(autonomous_mode) -> "autonomous mode";
assigned_mode_to_list(assigned_mode) -> "assigned mode".

get_atnr_message_type(#atnr{message_type = X}) -> X.
get_atnr_repeat_indicator(#atnr{repeat_indicator = X}) -> X.
get_atnr_mmsi(#atnr{mmsi = X}) -> X.
get_atnr_aid_type(#atnr{aid_type = X}) -> X.
get_atnr_name(#atnr{name = X}) -> X.
get_atnr_position_accuracy(#atnr{position_accuracy = X}) -> X.
get_atnr_longitude(#atnr{longitude = X}) -> X.
get_atnr_latitude(#atnr{latitude = X}) -> X.
get_atnr_dim_to_bow(#atnr{dim_to_bow = X}) -> X.
get_atnr_dim_to_stern(#atnr{dim_to_stern = X}) -> X.
get_atnr_dim_to_port(#atnr{dim_to_port = X}) -> X.
get_atnr_dim_to_starboard(#atnr{dim_to_starboard = X}) -> X.
get_atnr_type_of_epfd(#atnr{type_of_epfd = X}) -> X.
get_atnr_timestamp(#atnr{timestamp = X}) -> X.
get_atnr_off_position(#atnr{off_position = X}) -> X.
get_atnr_regional(#atnr{regional = X}) -> X.
get_atnr_raim_flag(#atnr{raim_flag = X}) -> X.
get_atnr_virtual_aid(#atnr{virtual_aid = X}) -> X.
get_atnr_assigned_mode(#atnr{assigned_mode = X}) -> X.

%% @doc Utility function to work like string:tokens/1, but not skip over 
%% multiple occurrences of the separator.
to_tokens(Bin, SepList) when is_binary(Bin), is_list(SepList) ->
    to_tokens(binary_to_list(Bin), SepList);
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

%% Query function to extract all the message types in a list of AIS records.
extract_all_message_types(AisRecs) when is_list(AisRecs) ->
    MTList = lists:foldl(fun extract_message_type/2, [], AisRecs),
    % Remove non-unique elements.
    S = sets:from_list(MTList),
    sets:to_list(S).

extract_message_type({ok, #ais{} = A}, Acc) ->
    case get_data(A) of
        #cnb{message_type = M} -> [M|Acc];
        {error,_}              -> Acc;
        MT                     -> [MT|Acc]
    end.

get_payload_type({ok, #ais{} = A}) ->
    case get_data(A) of
        #cnb{}     -> cnb;
        #base_sr{} -> base_sr;
        _          -> undefined 
    end.
    
%% Extracts all records containing CNB data from a list of decoded AIS 
%% records.
extract_cnb_records(AisRecs) ->
    F = fun(Rec, Acc) ->
            case decode_contains_cnb(Rec) of
                true -> 
                    {ok, Ais} = Rec,
                    [Ais|Acc];
                false ->
                    Acc
            end
        end,
    Rev = lists:foldl(F, [], AisRecs),
    lists:reverse(Rev).

%% Extract last CNB for each MMSI present in the data.
extract_last_cnb_by_mmsi(AisRecs) ->
    update_cnb_map(AisRecs, #{}).

%% Update an existing CNB map with new records.
update_cnb_map(AisRecs, CnbMap) ->
    F = fun(Rec, AccMap) ->
            case find_cnb(Rec) of
                {ok, CNB} ->
                    MMSI = get_mmsi(CNB),
                    maps:put(MMSI, CNB, AccMap);
                error ->
                    AccMap
            end
        end,
    lists:foldl(F, CnbMap, AisRecs).

%% Unwraps the decode return which contains a status in addition to the 
%% decoded record. Only valid decodes are considered.
decode_contains_cnb({ok, #ais{} = A}) -> contains_cnb(A);
decode_contains_cnb(_)                -> false.

%% Predicate to indicate whether a decoded AIS record contains a CNB.
contains_cnb(#ais{data = #cnb{}}) -> true;
contains_cnb(_)                   -> false.

%% Extract the CNB from a record if it exists or return an error.
find_cnb({ok, #ais{} = A})          -> find_cnb(A);
find_cnb(#ais{data = #cnb{} = CNB}) -> {ok, CNB};
find_cnb(_)                         -> error.

%% Type 8. 
-spec decode_binary_broadcast_message(binary()) -> {ok, bbm()} | {error, Reason::atom()}.
decode_binary_broadcast_message(<<MT:6,RI:2,MMSI:30,_Sp:2,DAC:10,FID:6,
    _Rem/binary>>) ->

    {ok, #bbm{
        message_type = decode_message_type(MT),
        repeat_indicator = decode_repeat_indicator(RI),
        mmsi = MMSI,
        dac = DAC, 
        fid = FID}};
decode_binary_broadcast_message(_) ->
    {error, failed_to_decode_bbm}.

%% These are placeholders for now: the decoding of Lat/Lon is different from
%% the CNB.
%% decode_bbm_longitude(L) -> decode_longitude(L).
%% decode_bbm_latitude(L) -> decode_latitude(L).

%% Type 5.
-spec decode_static_and_voyage_data(binary()) -> {ok, svd()} | {error, Reason::atom()}.
decode_static_and_voyage_data(<<MT:6,RI:2,MMSI:30,Vsn:2,IMO:30,CS:42/bitstring,
    VN:120/bitstring,ST:8,DB:9,DS:9,DP:6,DSt:6,EPFD:4,ETAM:4,ETAD:5,ETAH:5,
    ETAMin:6,Dr:8,Dest:120/bitstring,DTE:1,_Rem/bitstring>>) ->
    {ok, #svd{
        message_type = decode_message_type(MT),
        repeat_indicator = decode_repeat_indicator(RI),
        mmsi = MMSI,
        ais_version = Vsn,
        imo = IMO,
        call_sign = decode_call_sign(CS), 
        vessel_name = decode_vessel_name(VN),
        ship_type = ship_type:decode(ST),
        dim_to_bow = DB,
        dim_to_stern = DS,
        dim_to_port = DP,
        dim_to_starboard = DSt,
        epfd_fix = decode_epfd_fix_type(EPFD),
        eta_month = ETAM,
        eta_day = ETAD,
        eta_hour = ETAH,
        eta_min = ETAMin,
        draught = Dr / 10,
        destination = decode_destination(Dest),
        dte = decode_dte(DTE)
    }};
decode_static_and_voyage_data(_Arg) ->
    %io:format("svd decode error clause, arg: ~p~n", [_Arg]),
    {error, failed_to_decode_svd}.

decode_call_sign(Bin) ->
    [sixbit:decode(X) || <<X:6>> <= Bin].

decode_vessel_name(Bin) ->
    [sixbit:decode(X) || <<X:6>> <= Bin].

decode_destination(Bin) ->
    [sixbit:decode(X) || <<X:6>> <= Bin].

decode_dte(0) -> data_terminal_ready;
decode_dte(1) -> data_terminal_not_ready.

get_svd_message_type(#svd{message_type = X}) -> X.
get_svd_repeat_indicator(#svd{repeat_indicator = X}) -> X.
get_svd_mmsi(#svd{mmsi = X}) -> X.

get_unsupported_messages(DecodeList) ->
    F = fun(X) ->
            case X of 
                {error, unsupported_message_type, _} ->
                    true;
                {error, unsupported_message_type, _, _} ->
                    true;
                _ -> 
                    false
            end
        end,
    lists:filter(F, DecodeList).

get_unsupported_message_types(DecodeList) ->
    Unsupported = get_unsupported_messages(DecodeList),
    Ts = [T || {_, _, T, _} <- Unsupported],
    Set = sets:from_list(Ts),
    sets:to_list(Set).

is_ais_rec(#ais{}) -> true;
is_ais_rec(_)      -> false.

%% @doc Convert an optional float to string.
-spec optional_float_to_list(optional_float()) -> string().
optional_float_to_list(not_available) -> "not available";
optional_float_to_list(X) when is_number(X) -> io_lib:format("~p", [X]).


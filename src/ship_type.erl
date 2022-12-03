-module(ship_type).

-export([decode/1]).

decode(0) -> not_available;
decode(X) when X >= 1, X =< 19 -> reserved;
decode(20) -> wig_all;
decode(21) -> wig_hazardous_a;
decode(22) -> wig_hazardous_b;
decode(23) -> wig_hazardous_c;
decode(24) -> wig_hazardous_d;
decode(X) when X >= 25, X =< 29 -> wig_reserved;
decode(30) -> fishing;
decode(31) -> towing;
decode(32) -> towing_length_200m_or_breadth_50m;
decode(33) -> dredging_or_underwater_ops;
decode(34) -> diving_ops;
decode(35) -> military_ops;
decode(36) -> sailing;
decode(37) -> pleasure_craft;
decode(38) -> reserved;
decode(39) -> reserved;
decode(40) -> high_speed_craft_all;
decode(41) -> high_speed_craft_hazardous_a;
decode(42) -> high_speed_craft_hazardous_b;
decode(43) -> high_speed_craft_hazardous_c;
decode(44) -> high_speed_craft_hazardous_d;
decode(X) when X >= 45, X =< 48 -> high_speed_craft_reserved;
decode(49) -> high_speed_craft_no_info;
decode(50) -> pilot_vessel;
decode(51) -> search_and_rescue_vessel;
decode(52) -> tug;
decode(53) -> port_tender;
decode(54) -> anti_pollution_equipment;
decode(55) -> law_enforcement;
decode(56) -> spare_local_vessel;
decode(57) -> spare_local_vessel;
decode(58) -> medical_transport;
decode(59) -> noncombatant_ship;
decode(60) -> passenger_all;
decode(61) -> passenger_hazardous_a;
decode(62) -> passenger_hazardous_b;
decode(63) -> passenger_hazardous_c;
decode(64) -> passenger_hazardous_d;
decode(X) when X >= 65, X =< 68 -> passenger_reserved;
decode(69) -> passenger_no_info;
decode(70) -> cargo_all;
decode(71) -> cargo_hazardous_a;
decode(72) -> cargo_hazardous_b;
decode(73) -> cargo_hazardous_c;
decode(74) -> cargo_hazardous_d;
decode(X) when X >= 75, X =< 78 -> cargo_reserved;
decode(79) -> cargo_no_info;
decode(80) -> tanker_all;
decode(81) -> tanker_hazardous_a;
decode(82) -> tanker_hazardous_b;
decode(83) -> tanker_hazardous_c;
decode(84) -> tanker_hazardous_d;
decode(X) when X >= 85, X =< 88 -> tanker_reserved;
decode(89) -> tanker_no_info;
decode(90) -> other_all;
decode(91) -> other_hazardous_a;
decode(92) -> other_hazardous_b;
decode(93) -> other_hazardous_c;
decode(94) -> other_hazardous_d;
decode(X) when X >= 95, X =< 98 -> other_reserved;
decode(99) -> other_no_info.


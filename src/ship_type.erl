-module(ship_type).

-export([decode_ship_type/1]).

decode_ship_type(0) -> not_available;
decode_ship_type(X) when X >= 1, X =< 19 -> reserved;
decode_ship_type(20) -> wig_all;
decode_ship_type(21) -> wig_hazardous_a;
decode_ship_type(22) -> wig_hazardous_b;
decode_ship_type(23) -> wig_hazardous_c;
decode_ship_type(24) -> wig_hazardous_d;
decode_ship_type(X) when X >= 25, X =< 29 -> wig_reserved;
decode_ship_type(30) -> fishing;
decode_ship_type(31) -> towing;

decode_ship_type(80) -> tanker_all;
decode_ship_type(81) -> tanker_hazardous_a;
decode_ship_type(82) -> tanker_hazardous_b;
decode_ship_type(83) -> tanker_hazardous_c;
decode_ship_type(84) -> tanker_hazardous_d;
decode_ship_type(X) when X >= 85, X =< 88 -> tanker_reserved;
decode_ship_type(89) -> tanker_no_info;
decode_ship_type(90) -> other_all;
decode_ship_type(91) -> other_hazardous_a;
decode_ship_type(92) -> other_hazardous_b;
decode_ship_type(93) -> other_hazardous_c;
decode_ship_type(94) -> other_hazardous_d;
decode_ship_type(X) when X >= 95, X =< 98 -> other_reserved;
decode_ship_type(99) -> other_no_info.


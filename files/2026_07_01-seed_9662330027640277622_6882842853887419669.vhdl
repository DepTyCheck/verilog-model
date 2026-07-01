-- Seed: 9662330027640277622,6882842853887419669

entity xhbld is
  port (fg : inout boolean_vector(3 to 4); ktnwrmop : buffer time);
end xhbld;

architecture uyha of xhbld is
  
begin
  -- Single-driven assignments
  ktnwrmop <= 16#4_F# us;
  fg <= (TRUE, FALSE);
end uyha;

library ieee;
use ieee.std_logic_1164.all;

entity museznpns is
  port (fpht : linkage std_logic_vector(3 to 0));
end museznpns;

architecture owboklcvk of museznpns is
  signal f : time;
  signal ybhgiqvsc : boolean_vector(3 to 4);
  signal nvjruvqj : time;
  signal xubvkx : boolean_vector(3 to 4);
begin
  ardlwtq : entity work.xhbld
    port map (fg => xubvkx, ktnwrmop => nvjruvqj);
  h : entity work.xhbld
    port map (fg => ybhgiqvsc, ktnwrmop => f);
end owboklcvk;



-- Seed after: 2054261882485954873,6882842853887419669

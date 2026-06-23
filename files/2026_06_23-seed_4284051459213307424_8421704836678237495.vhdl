-- Seed: 4284051459213307424,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity gubjtgnq is
  port (sk : in integer; ksqj : linkage bit; zm : inout std_logic; gsg : out time);
end gubjtgnq;

architecture okfmxs of gubjtgnq is
  
begin
  -- Single-driven assignments
  gsg <= 2 ns;
end okfmxs;

entity rszrcfz is
  port (rammdgkmph : inout integer; fyyhvnas : out integer_vector(0 to 0));
end rszrcfz;

library ieee;
use ieee.std_logic_1164.all;

architecture n of rszrcfz is
  signal cito : time;
  signal lfumjuaef : std_logic;
  signal td : bit;
begin
  qosqjxyu : entity work.gubjtgnq
    port map (sk => rammdgkmph, ksqj => td, zm => lfumjuaef, gsg => cito);
  
  -- Single-driven assignments
  rammdgkmph <= 16#7#;
  fyyhvnas <= (others => 1_0_1_2_2);
end n;

entity cd is
  port (m : linkage character);
end cd;

library ieee;
use ieee.std_logic_1164.all;

architecture movz of cd is
  signal pqbbi : time;
  signal uzarwm : std_logic;
  signal rfqld : bit;
  signal ppevo : integer_vector(0 to 0);
  signal jlhyw : time;
  signal gvdyrit : std_logic;
  signal eniynvk : bit;
  signal yno : integer;
begin
  rhbdwbu : entity work.gubjtgnq
    port map (sk => yno, ksqj => eniynvk, zm => gvdyrit, gsg => jlhyw);
  gcd : entity work.rszrcfz
    port map (rammdgkmph => yno, fyyhvnas => ppevo);
  atpnmp : entity work.gubjtgnq
    port map (sk => yno, ksqj => rfqld, zm => uzarwm, gsg => pqbbi);
  
  -- Multi-driven assignments
  gvdyrit <= '1';
  gvdyrit <= 'W';
  uzarwm <= 'Z';
  gvdyrit <= 'H';
end movz;



-- Seed after: 10094245851162289946,8421704836678237495

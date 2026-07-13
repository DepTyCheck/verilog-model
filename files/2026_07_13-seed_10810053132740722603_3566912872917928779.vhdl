-- Seed: 10810053132740722603,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity luagxcxk is
  port (xbmuboj : in std_logic; wtrzvajk : inout physical_subtype_mirror);
end luagxcxk;

architecture mkzq of luagxcxk is
  
begin
  
end mkzq;

use std.reflection.all;

entity rga is
  port (k : inout record_subtype_mirror; syccqllzc : inout record_subtype_mirror; sp : inout time_vector(0 to 4));
end rga;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture uhdrwi of rga is
  shared variable m : physical_subtype_mirror;
  signal t : std_logic;
  shared variable dvshr : physical_subtype_mirror;
  shared variable msaunpzc : physical_subtype_mirror;
  signal gwbxnsz : std_logic;
begin
  nmosxrec : entity work.luagxcxk
    port map (xbmuboj => gwbxnsz, wtrzvajk => msaunpzc);
  kmdxazpnd : entity work.luagxcxk
    port map (xbmuboj => gwbxnsz, wtrzvajk => dvshr);
  edzbusk : entity work.luagxcxk
    port map (xbmuboj => t, wtrzvajk => m);
  
  -- Single-driven assignments
  sp <= (3_2_1 ps, 16#A_B.5463# us, 041 us, 16#A_6_B# ns, 1 hr);
  
  -- Multi-driven assignments
  t <= 'H';
end uhdrwi;



-- Seed after: 5816212771514382242,3566912872917928779

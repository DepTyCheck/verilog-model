-- Seed: 11277195399163739274,2983771601630957889

use std.reflection.all;

entity sucz is
  port (u : in real; variable kqlbm : inout enumeration_subtype_mirror_pt);
end sucz;

architecture y of sucz is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity qi is
  port (tqbs : linkage std_logic);
end qi;

architecture oyobewg of qi is
  
begin
  
end oyobewg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity t is
  port (hzst : in boolean; fqcwh : inout std_logic; variable njtyirb : inout enumeration_value_mirror_pt);
end t;

use std.reflection.all;

architecture mnfzjepatt of t is
  shared variable qewex : enumeration_subtype_mirror_pt;
  signal ubtgfsvr : real;
  shared variable juj : enumeration_subtype_mirror_pt;
  shared variable mxjwxi : enumeration_subtype_mirror_pt;
  shared variable mvv : enumeration_subtype_mirror_pt;
  signal pziwedqc : real;
begin
  vz : entity work.sucz
    port map (u => pziwedqc, kqlbm => mvv);
  jevpe : entity work.sucz
    port map (u => pziwedqc, kqlbm => mxjwxi);
  rdtdyom : entity work.sucz
    port map (u => pziwedqc, kqlbm => juj);
  vjvrnwqjl : entity work.sucz
    port map (u => ubtgfsvr, kqlbm => qewex);
  
  -- Single-driven assignments
  pziwedqc <= 8#5.0_6_4_0_3#;
  ubtgfsvr <= pziwedqc;
  
  -- Multi-driven assignments
  fqcwh <= 'U';
  fqcwh <= 'Z';
end mnfzjepatt;

entity vo is
  port (tutiris : inout time);
end vo;

architecture kif of vo is
  
begin
  -- Single-driven assignments
  tutiris <= 1 min;
end kif;



-- Seed after: 14159691418859873638,2983771601630957889

-- Seed: 10836211097137196414,2983771601630957889

use std.reflection.all;

entity apxwy is
  port (variable m : inout subtype_mirror_pt; g : inout time; fnxzai : linkage boolean; cnqcri : linkage time_vector(0 downto 1));
end apxwy;

architecture ndzeyh of apxwy is
  
begin
  -- Single-driven assignments
  g <= 44212.2 ms;
end ndzeyh;

use std.reflection.all;

entity x is
  port (variable pmh : inout record_value_mirror_pt; variable j : inout array_value_mirror_pt);
end x;

use std.reflection.all;

architecture ogcbx of x is
  signal byqamcmi : time_vector(0 downto 1);
  signal rlrli : boolean;
  signal shsl : time;
  shared variable vl : subtype_mirror_pt;
begin
  tvhuheiueq : entity work.apxwy
    port map (m => vl, g => shsl, fnxzai => rlrli, cnqcri => byqamcmi);
end ogcbx;

library ieee;
use ieee.std_logic_1164.all;

entity sgdk is
  port (qrz : in std_logic);
end sgdk;

use std.reflection.all;

architecture nmbycf of sgdk is
  signal tqzeidisp : time_vector(0 downto 1);
  signal knm : boolean;
  signal mtzlt : time;
  shared variable lvhn : subtype_mirror_pt;
begin
  shq : entity work.apxwy
    port map (m => lvhn, g => mtzlt, fnxzai => knm, cnqcri => tqzeidisp);
end nmbycf;



-- Seed after: 9631529903941866382,2983771601630957889

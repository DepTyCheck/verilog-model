-- Seed: 11413232684900315316,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity qnrzrkq is
  port (cmmrh : inout subtype_mirror; joazqnfhjs : inout enumeration_subtype_mirror; dir : inout physical_subtype_mirror; fswjmbijz : out std_logic);
end qnrzrkq;

architecture wbnhvytwph of qnrzrkq is
  
begin
  -- Multi-driven assignments
  fswjmbijz <= 'L';
end wbnhvytwph;

use std.reflection.all;

entity dmfy is
  port (ajgpzqy : inout value_mirror);
end dmfy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture v of dmfy is
  signal moyrk : std_logic;
  shared variable ia : physical_subtype_mirror;
  shared variable dkj : enumeration_subtype_mirror;
  shared variable pdddxjz : subtype_mirror;
begin
  qdeyup : entity work.qnrzrkq
    port map (cmmrh => pdddxjz, joazqnfhjs => dkj, dir => ia, fswjmbijz => moyrk);
  
  -- Multi-driven assignments
  moyrk <= moyrk;
  moyrk <= 'U';
end v;



-- Seed after: 16019834432633933655,3566912872917928779

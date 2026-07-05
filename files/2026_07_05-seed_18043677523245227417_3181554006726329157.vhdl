-- Seed: 18043677523245227417,3181554006726329157

use std.reflection.all;

entity bsfyfygliv is
  port (zn : inout record_subtype_mirror; io : inout integer_subtype_mirror);
end bsfyfygliv;

architecture xwfhfd of bsfyfygliv is
  
begin
  
end xwfhfd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pfhsrvjyac is
  port (aaf : buffer std_logic; vvcu : inout floating_value_mirror);
end pfhsrvjyac;

use std.reflection.all;

architecture nuq of pfhsrvjyac is
  shared variable cqjytti : integer_subtype_mirror;
  shared variable nwnfzemlpg : record_subtype_mirror;
  shared variable kixhipgjo : integer_subtype_mirror;
  shared variable jtqkwdqz : record_subtype_mirror;
begin
  feixwlkvh : entity work.bsfyfygliv
    port map (zn => jtqkwdqz, io => kixhipgjo);
  red : entity work.bsfyfygliv
    port map (zn => nwnfzemlpg, io => cqjytti);
  
  -- Multi-driven assignments
  aaf <= aaf;
  aaf <= aaf;
  aaf <= '0';
end nuq;



-- Seed after: 16288387053762887893,3181554006726329157

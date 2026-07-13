-- Seed: 12305754372221352188,3566912872917928779

use std.reflection.all;

entity pqt is
  port (rtr : inout record_subtype_mirror; oucg : inout physical_subtype_mirror);
end pqt;

architecture febqmal of pqt is
  
begin
  
end febqmal;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hcd is
  port (lt : inout integer_value_mirror; vgm : inout std_logic);
end hcd;

use std.reflection.all;

architecture hycuxxu of hcd is
  shared variable iavxiyy : physical_subtype_mirror;
  shared variable dye : record_subtype_mirror;
  shared variable ompuk : physical_subtype_mirror;
  shared variable rfdazb : record_subtype_mirror;
begin
  bhgmfxf : entity work.pqt
    port map (rtr => rfdazb, oucg => ompuk);
  oj : entity work.pqt
    port map (rtr => dye, oucg => iavxiyy);
  
  -- Multi-driven assignments
  vgm <= '0';
  vgm <= 'L';
  vgm <= vgm;
end hycuxxu;

use std.reflection.all;

entity ujsaaws is
  port (xdczeg : out time; a : inout physical_value_mirror; dey : inout physical_subtype_mirror; kwwlnmtp : inout physical_subtype_mirror);
end ujsaaws;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture vm of ujsaaws is
  signal u : std_logic;
  shared variable h : integer_value_mirror;
begin
  lmzpglbv : entity work.hcd
    port map (lt => h, vgm => u);
  
  -- Multi-driven assignments
  u <= '0';
  u <= '-';
  u <= '1';
  u <= u;
end vm;

entity ow is
  port (pfrjke : buffer real_vector(2 to 1));
end ow;

architecture cbwucemf of ow is
  
begin
  -- Single-driven assignments
  pfrjke <= pfrjke;
end cbwucemf;



-- Seed after: 18029330378327964421,3566912872917928779

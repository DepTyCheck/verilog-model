-- Seed: 4207848026997120174,3181554006726329157

use std.reflection.all;

entity pfiods is
  port (p : inout protected_subtype_mirror; oituscv : inout floating_value_mirror);
end pfiods;

architecture er of pfiods is
  
begin
  
end er;

use std.reflection.all;

entity gubjucwx is
  port (s : inout protected_value_mirror; fbspdhg : inout integer_value_mirror);
end gubjucwx;

use std.reflection.all;

architecture ev of gubjucwx is
  shared variable oipe : floating_value_mirror;
  shared variable rlhwi : protected_subtype_mirror;
  shared variable atgblqlvh : floating_value_mirror;
  shared variable ysixoxjiaa : protected_subtype_mirror;
begin
  gqowpvzpl : entity work.pfiods
    port map (p => ysixoxjiaa, oituscv => atgblqlvh);
  xhnjs : entity work.pfiods
    port map (p => rlhwi, oituscv => oipe);
end ev;

library ieee;
use ieee.std_logic_1164.all;

entity qsmdsizl is
  port (npbtveoh : linkage real; h : out std_logic; jojmtx : out time; mxlwe : out std_logic_vector(2 to 1));
end qsmdsizl;

architecture xbzawh of qsmdsizl is
  
begin
  -- Single-driven assignments
  jojmtx <= 3 min;
  
  -- Multi-driven assignments
  mxlwe <= mxlwe;
  mxlwe <= mxlwe;
  mxlwe <= mxlwe;
end xbzawh;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (lcrb : in std_logic);
end s;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture bxtsjdjn of s is
  signal ixawgelad : std_logic_vector(2 to 1);
  signal nyf : time;
  signal h : std_logic;
  signal pq : real;
  shared variable cuievfh : floating_value_mirror;
  shared variable sqhdavs : protected_subtype_mirror;
  shared variable dzjworcdov : floating_value_mirror;
  shared variable teh : protected_subtype_mirror;
  shared variable mzjgmplsp : floating_value_mirror;
  shared variable qjab : protected_subtype_mirror;
begin
  ketys : entity work.pfiods
    port map (p => qjab, oituscv => mzjgmplsp);
  lmkrmqvssl : entity work.pfiods
    port map (p => teh, oituscv => dzjworcdov);
  tbnuxeo : entity work.pfiods
    port map (p => sqhdavs, oituscv => cuievfh);
  ffslbge : entity work.qsmdsizl
    port map (npbtveoh => pq, h => h, jojmtx => nyf, mxlwe => ixawgelad);
  
  -- Multi-driven assignments
  h <= lcrb;
  h <= lcrb;
  h <= 'W';
end bxtsjdjn;



-- Seed after: 1621978447173391450,3181554006726329157

-- Seed: 13070965844665066409,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity pbobd is
  port (yrjtb : buffer real_vector(1 to 3); j : in std_logic);
end pbobd;

architecture tqb of pbobd is
  
begin
  -- Single-driven assignments
  yrjtb <= (30.43, 2#1.1_1_0_0_0#, 4_2.2_1_1);
end tqb;

use std.reflection.all;

entity cptbctk is
  port (uigqiubwhq : inout floating_value_mirror; ck : in integer; dm : inout integer_value_mirror; lyicxhpiq : out time);
end cptbctk;

library ieee;
use ieee.std_logic_1164.all;

architecture icobyuw of cptbctk is
  signal phmxxqofd : std_logic;
  signal otxfx : real_vector(1 to 3);
  signal kzoqq : std_logic;
  signal rlyjheda : real_vector(1 to 3);
begin
  c : entity work.pbobd
    port map (yrjtb => rlyjheda, j => kzoqq);
  buqsrswgi : entity work.pbobd
    port map (yrjtb => otxfx, j => phmxxqofd);
  
  -- Multi-driven assignments
  kzoqq <= phmxxqofd;
  kzoqq <= kzoqq;
end icobyuw;

use std.reflection.all;

entity v is
  port (qyhkb : inout access_subtype_mirror; mwwdqj : inout enumeration_value_mirror);
end v;

use std.reflection.all;

architecture j of v is
  signal cuf : time;
  shared variable fijmrd : integer_value_mirror;
  signal k : integer;
  shared variable mukjlvgyz : floating_value_mirror;
  signal gljiepb : time;
  shared variable ocexij : integer_value_mirror;
  shared variable xodwvr : floating_value_mirror;
  signal kldmfc : time;
  shared variable bugnmlch : integer_value_mirror;
  signal ccqlu : integer;
  shared variable jaiz : floating_value_mirror;
begin
  tcwihi : entity work.cptbctk
    port map (uigqiubwhq => jaiz, ck => ccqlu, dm => bugnmlch, lyicxhpiq => kldmfc);
  lchdhjkgb : entity work.cptbctk
    port map (uigqiubwhq => xodwvr, ck => ccqlu, dm => ocexij, lyicxhpiq => gljiepb);
  vzocoqfrg : entity work.cptbctk
    port map (uigqiubwhq => mukjlvgyz, ck => k, dm => fijmrd, lyicxhpiq => cuf);
  
  -- Single-driven assignments
  k <= 332;
  ccqlu <= 3;
end j;



-- Seed after: 1386833097553206066,14426950258250697445

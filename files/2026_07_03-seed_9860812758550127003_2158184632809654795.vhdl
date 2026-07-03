-- Seed: 9860812758550127003,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bwcy is
  port (rkwjqtpw : in std_logic; mf : inout access_value_mirror; eubbwkr : inout array_value_mirror; ag : inout integer);
end bwcy;

architecture ak of bwcy is
  
begin
  -- Single-driven assignments
  ag <= 0_0_3;
end ak;

use std.reflection.all;

entity fnvoo is
  port (cdw : inout integer_value_mirror);
end fnvoo;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xntr of fnvoo is
  signal mc : integer;
  shared variable zvjbzvqg : array_value_mirror;
  shared variable tu : access_value_mirror;
  signal hjehmpiwwr : integer;
  shared variable wcg : array_value_mirror;
  shared variable r : access_value_mirror;
  signal iwuefzyiso : std_logic;
  signal ysgxgt : integer;
  shared variable ujn : array_value_mirror;
  shared variable s : access_value_mirror;
  signal pfkdl : std_logic;
begin
  sqes : entity work.bwcy
    port map (rkwjqtpw => pfkdl, mf => s, eubbwkr => ujn, ag => ysgxgt);
  tfkzwgptt : entity work.bwcy
    port map (rkwjqtpw => iwuefzyiso, mf => r, eubbwkr => wcg, ag => hjehmpiwwr);
  tqdgpdep : entity work.bwcy
    port map (rkwjqtpw => pfkdl, mf => tu, eubbwkr => zvjbzvqg, ag => mc);
  
  -- Multi-driven assignments
  iwuefzyiso <= 'H';
  pfkdl <= 'Z';
  pfkdl <= pfkdl;
end xntr;



-- Seed after: 14706948491068154239,2158184632809654795

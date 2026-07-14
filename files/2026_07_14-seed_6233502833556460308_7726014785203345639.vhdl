-- Seed: 6233502833556460308,7726014785203345639

use std.reflection.all;

entity szgtakhl is
  port (pwqifrxr : inout access_value_mirror);
end szgtakhl;

architecture ws of szgtakhl is
  
begin
  
end ws;

entity pzurazvxfr is
  port (lnzb : linkage real);
end pzurazvxfr;

use std.reflection.all;

architecture q of pzurazvxfr is
  shared variable fjbyis : access_value_mirror;
  shared variable vzf : access_value_mirror;
  shared variable hr : access_value_mirror;
begin
  lhj : entity work.szgtakhl
    port map (pwqifrxr => hr);
  wkualkcld : entity work.szgtakhl
    port map (pwqifrxr => vzf);
  fiisqsqb : entity work.szgtakhl
    port map (pwqifrxr => fjbyis);
end q;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (ynava : out std_logic; edc : inout integer);
end b;

use std.reflection.all;

architecture pulz of b is
  signal qmnww : real;
  shared variable enngjfi : access_value_mirror;
begin
  lpnidz : entity work.szgtakhl
    port map (pwqifrxr => enngjfi);
  rtocuhrne : entity work.pzurazvxfr
    port map (lnzb => qmnww);
  
  -- Multi-driven assignments
  ynava <= '0';
  ynava <= 'X';
  ynava <= 'W';
end pulz;



-- Seed after: 17206077519232934397,7726014785203345639

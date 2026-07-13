-- Seed: 7437484116541219740,3566912872917928779

use std.reflection.all;

entity jnzvgwdlv is
  port (j : in bit; cua : inout enumeration_value_mirror);
end jnzvgwdlv;

architecture ocflcnkg of jnzvgwdlv is
  
begin
  
end ocflcnkg;

use std.reflection.all;

entity pui is
  port (fanf : out integer; ar : out boolean; kwo : inout access_subtype_mirror);
end pui;

use std.reflection.all;

architecture ermxibpx of pui is
  shared variable hlccmhpxdt : enumeration_value_mirror;
  signal h : bit;
  shared variable sljznapkwq : enumeration_value_mirror;
  shared variable tmiqepj : enumeration_value_mirror;
  signal nzr : bit;
begin
  ducfvpu : entity work.jnzvgwdlv
    port map (j => nzr, cua => tmiqepj);
  xfwnyyur : entity work.jnzvgwdlv
    port map (j => nzr, cua => sljznapkwq);
  gwogpgaq : entity work.jnzvgwdlv
    port map (j => h, cua => hlccmhpxdt);
  
  -- Single-driven assignments
  h <= nzr;
  ar <= FALSE;
end ermxibpx;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (ofll : inout std_logic_vector(3 to 2); o : linkage character);
end c;

architecture ap of c is
  
begin
  -- Multi-driven assignments
  ofll <= ofll;
  ofll <= ofll;
  ofll <= (others => '0');
end ap;

use std.reflection.all;

entity rfyjyjdrgw is
  port (metrpzjljr : inout value_mirror; cqbe : inout integer_value_mirror; a : inout file_subtype_mirror; xoozez : inout time);
end rfyjyjdrgw;

library ieee;
use ieee.std_logic_1164.all;

architecture xhvawo of rfyjyjdrgw is
  signal hfslroe : character;
  signal ueerpnutz : std_logic_vector(3 to 2);
begin
  s : entity work.c
    port map (ofll => ueerpnutz, o => hfslroe);
  
  -- Multi-driven assignments
  ueerpnutz <= ueerpnutz;
  ueerpnutz <= ueerpnutz;
end xhvawo;



-- Seed after: 12609488048506062329,3566912872917928779

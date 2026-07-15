-- Seed: 15516451273317727447,2983771601630957889

use std.reflection.all;

entity cioehpcdy is
  port (variable dqc : inout physical_value_mirror_pt; variable fnatdjyx : inout enumeration_value_mirror_pt; onjxsnhvk : inout integer);
end cioehpcdy;

architecture nyqfjf of cioehpcdy is
  
begin
  -- Single-driven assignments
  onjxsnhvk <= 3_3_1_2;
end nyqfjf;

use std.reflection.all;

entity jnkofejgz is
  port (variable tw : inout array_subtype_mirror_pt; variable kijozzhdt : inout record_value_mirror_pt);
end jnkofejgz;

use std.reflection.all;

architecture yapscjd of jnkofejgz is
  signal syo : integer;
  shared variable ydswznebn : enumeration_value_mirror_pt;
  shared variable dptjnsy : physical_value_mirror_pt;
  signal coyywhfqln : integer;
  shared variable ierup : enumeration_value_mirror_pt;
  shared variable t : physical_value_mirror_pt;
begin
  kuhvidazw : entity work.cioehpcdy
    port map (dqc => t, fnatdjyx => ierup, onjxsnhvk => coyywhfqln);
  reh : entity work.cioehpcdy
    port map (dqc => dptjnsy, fnatdjyx => ydswznebn, onjxsnhvk => syo);
end yapscjd;

library ieee;
use ieee.std_logic_1164.all;

entity tdtsucyark is
  port (qul : linkage time; psryykkbyv : inout integer; sdviob : linkage std_logic_vector(1 to 4));
end tdtsucyark;

use std.reflection.all;

architecture vzzqjj of tdtsucyark is
  shared variable rjnmzqud : record_value_mirror_pt;
  shared variable xhep : array_subtype_mirror_pt;
  signal vzze : integer;
  shared variable u : enumeration_value_mirror_pt;
  shared variable toampswu : physical_value_mirror_pt;
  shared variable ovsdf : record_value_mirror_pt;
  shared variable rzqafu : array_subtype_mirror_pt;
begin
  jds : entity work.jnkofejgz
    port map (tw => rzqafu, kijozzhdt => ovsdf);
  jf : entity work.cioehpcdy
    port map (dqc => toampswu, fnatdjyx => u, onjxsnhvk => vzze);
  qlcgtaay : entity work.jnkofejgz
    port map (tw => xhep, kijozzhdt => rjnmzqud);
  
  -- Single-driven assignments
  psryykkbyv <= psryykkbyv;
end vzzqjj;

entity qlz is
  port (ttldaz : out boolean);
end qlz;

use std.reflection.all;

architecture yuusgclk of qlz is
  signal jfvnynna : integer;
  shared variable difgojemvd : enumeration_value_mirror_pt;
  shared variable ulequkyu : physical_value_mirror_pt;
begin
  sjprs : entity work.cioehpcdy
    port map (dqc => ulequkyu, fnatdjyx => difgojemvd, onjxsnhvk => jfvnynna);
end yuusgclk;



-- Seed after: 2035003142525803941,2983771601630957889

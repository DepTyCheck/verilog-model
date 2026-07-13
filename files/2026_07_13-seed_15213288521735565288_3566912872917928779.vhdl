-- Seed: 15213288521735565288,3566912872917928779

use std.reflection.all;

entity kgqjmzbzol is
  port (ggtyc : in real_vector(1 to 1); zpuv : inout protected_value_mirror);
end kgqjmzbzol;

architecture arlvudr of kgqjmzbzol is
  
begin
  
end arlvudr;

use std.reflection.all;

entity uy is
  port (zrvdblqkqe : inout value_mirror);
end uy;

use std.reflection.all;

architecture xj of uy is
  shared variable vqcigjcltw : protected_value_mirror;
  shared variable ewqwgyih : protected_value_mirror;
  signal uwauejcsad : real_vector(1 to 1);
begin
  gywofngbuq : entity work.kgqjmzbzol
    port map (ggtyc => uwauejcsad, zpuv => ewqwgyih);
  rkor : entity work.kgqjmzbzol
    port map (ggtyc => uwauejcsad, zpuv => vqcigjcltw);
end xj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity dijqupq is
  port (xudrpt : inout character; v : inout array_subtype_mirror; pagm : out std_logic);
end dijqupq;

use std.reflection.all;

architecture nh of dijqupq is
  shared variable hued : protected_value_mirror;
  signal mh : real_vector(1 to 1);
begin
  abfldu : entity work.kgqjmzbzol
    port map (ggtyc => mh, zpuv => hued);
  
  -- Single-driven assignments
  xudrpt <= 'g';
  
  -- Multi-driven assignments
  pagm <= 'Z';
end nh;

entity szbmauvacw is
  port (mtvrfkqyvb : out integer);
end szbmauvacw;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture fdecv of szbmauvacw is
  shared variable yst : protected_value_mirror;
  signal wfnwu : real_vector(1 to 1);
  shared variable uvblicin : value_mirror;
  shared variable nrqfvxk : array_subtype_mirror;
  signal xjodiufld : character;
  signal rdmz : std_logic;
  shared variable imhnifrts : array_subtype_mirror;
  signal wmmxcler : character;
begin
  id : entity work.dijqupq
    port map (xudrpt => wmmxcler, v => imhnifrts, pagm => rdmz);
  ikbzhtwi : entity work.dijqupq
    port map (xudrpt => xjodiufld, v => nrqfvxk, pagm => rdmz);
  ovlwtip : entity work.uy
    port map (zrvdblqkqe => uvblicin);
  ral : entity work.kgqjmzbzol
    port map (ggtyc => wfnwu, zpuv => yst);
  
  -- Single-driven assignments
  mtvrfkqyvb <= 4_2_2;
  wfnwu <= (others => 8#3.4_6_5#);
end fdecv;



-- Seed after: 15338979290605960743,3566912872917928779

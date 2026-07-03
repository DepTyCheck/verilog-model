-- Seed: 16628427039283049677,2158184632809654795

use std.reflection.all;

entity mqxawc is
  port (m : inout physical_subtype_mirror);
end mqxawc;

architecture zh of mqxawc is
  
begin
  
end zh;

use std.reflection.all;

entity mbogxie is
  port (bqnonw : inout enumeration_value_mirror; teipdn : inout floating_value_mirror; gb : linkage real);
end mbogxie;

use std.reflection.all;

architecture eqbln of mbogxie is
  shared variable msck : physical_subtype_mirror;
  shared variable fxqi : physical_subtype_mirror;
  shared variable iedisvzj : physical_subtype_mirror;
  shared variable urgcwynft : physical_subtype_mirror;
begin
  zvsigg : entity work.mqxawc
    port map (m => urgcwynft);
  huz : entity work.mqxawc
    port map (m => iedisvzj);
  yu : entity work.mqxawc
    port map (m => fxqi);
  bdgwt : entity work.mqxawc
    port map (m => msck);
end eqbln;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wcy is
  port (tmgllbw : inout std_logic_vector(3 to 1); ulklwh : inout integer_subtype_mirror);
end wcy;

use std.reflection.all;

architecture ilqpc of wcy is
  shared variable bgvanuwny : physical_subtype_mirror;
  shared variable je : physical_subtype_mirror;
  shared variable tdjyrsio : physical_subtype_mirror;
  signal yaj : real;
  shared variable svgkbiaqbs : floating_value_mirror;
  shared variable at : enumeration_value_mirror;
begin
  jkfwkgonj : entity work.mbogxie
    port map (bqnonw => at, teipdn => svgkbiaqbs, gb => yaj);
  oqzc : entity work.mqxawc
    port map (m => tdjyrsio);
  boy : entity work.mqxawc
    port map (m => je);
  cbnoa : entity work.mqxawc
    port map (m => bgvanuwny);
  
  -- Multi-driven assignments
  tmgllbw <= (others => '0');
  tmgllbw <= tmgllbw;
end ilqpc;



-- Seed after: 1503621590375945733,2158184632809654795

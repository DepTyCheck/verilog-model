-- Seed: 7282523143115535207,3566912872917928779

use std.reflection.all;

entity usvlmz is
  port (c : inout access_value_mirror);
end usvlmz;

architecture unjvhyialh of usvlmz is
  
begin
  
end unjvhyialh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity jklpuu is
  port (bz : in std_logic_vector(3 to 1); empmoxh : inout array_value_mirror; zjkufjmxg : inout protected_value_mirror; etfkqed : buffer real);
end jklpuu;

use std.reflection.all;

architecture yfjnwe of jklpuu is
  shared variable jxu : access_value_mirror;
  shared variable h : access_value_mirror;
begin
  e : entity work.usvlmz
    port map (c => h);
  bbbojmnc : entity work.usvlmz
    port map (c => jxu);
  
  -- Single-driven assignments
  etfkqed <= etfkqed;
end yfjnwe;

use std.reflection.all;

entity wfknrlwm is
  port (zdkjyspcm : inout boolean; ubtzu : inout subtype_mirror);
end wfknrlwm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pkxe of wfknrlwm is
  shared variable olq : access_value_mirror;
  signal ebxmkeurpl : real;
  shared variable w : protected_value_mirror;
  shared variable srzjpqkyn : array_value_mirror;
  signal cnfwnzu : std_logic_vector(3 to 1);
  shared variable iwfoeftds : access_value_mirror;
begin
  vzeo : entity work.usvlmz
    port map (c => iwfoeftds);
  gc : entity work.jklpuu
    port map (bz => cnfwnzu, empmoxh => srzjpqkyn, zjkufjmxg => w, etfkqed => ebxmkeurpl);
  fdwsdv : entity work.usvlmz
    port map (c => olq);
  
  -- Single-driven assignments
  zdkjyspcm <= TRUE;
  
  -- Multi-driven assignments
  cnfwnzu <= "";
  cnfwnzu <= "";
end pkxe;

use std.reflection.all;

entity ymulmqbsqk is
  port (wmof : inout access_value_mirror; qhwoo : inout access_value_mirror; ffoq : inout file_subtype_mirror; qsxlti : inout array_value_mirror);
end ymulmqbsqk;

use std.reflection.all;

architecture sdajfrkxwr of ymulmqbsqk is
  shared variable azssoo : subtype_mirror;
  signal m : boolean;
begin
  utgh : entity work.usvlmz
    port map (c => qhwoo);
  gnjafmu : entity work.usvlmz
    port map (c => wmof);
  r : entity work.wfknrlwm
    port map (zdkjyspcm => m, ubtzu => azssoo);
end sdajfrkxwr;



-- Seed after: 3196173915148865297,3566912872917928779

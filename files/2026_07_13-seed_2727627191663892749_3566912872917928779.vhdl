-- Seed: 2727627191663892749,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity c is
  port (xk : inout std_logic_vector(2 downto 2); ydhmyerf : out std_logic; z : inout enumeration_value_mirror);
end c;

architecture prv of c is
  
begin
  -- Multi-driven assignments
  ydhmyerf <= ydhmyerf;
end prv;

use std.reflection.all;

entity pkieb is
  port (so : inout array_value_mirror; tregpo : inout integer);
end pkieb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture crgfuxcmj of pkieb is
  shared variable lkog : enumeration_value_mirror;
  signal xmnexz : std_logic;
  signal wvsit : std_logic_vector(2 downto 2);
  shared variable xmd : enumeration_value_mirror;
  shared variable kjraiaxxd : enumeration_value_mirror;
  signal bzjywtjtbj : std_logic_vector(2 downto 2);
  shared variable t : enumeration_value_mirror;
  signal yfpis : std_logic;
  signal huuygz : std_logic_vector(2 downto 2);
begin
  aehuttbp : entity work.c
    port map (xk => huuygz, ydhmyerf => yfpis, z => t);
  kjrvplx : entity work.c
    port map (xk => bzjywtjtbj, ydhmyerf => yfpis, z => kjraiaxxd);
  dlsx : entity work.c
    port map (xk => huuygz, ydhmyerf => yfpis, z => xmd);
  rkaxais : entity work.c
    port map (xk => wvsit, ydhmyerf => xmnexz, z => lkog);
  
  -- Single-driven assignments
  tregpo <= 2#10#;
  
  -- Multi-driven assignments
  huuygz <= wvsit;
end crgfuxcmj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity t is
  port (wh : in integer; u : in std_logic; zyhc : inout enumeration_value_mirror);
end t;

architecture s of t is
  
begin
  
end s;



-- Seed after: 1877084359186448372,3566912872917928779

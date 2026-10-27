-- Seed: 16889654016246329428,12536735141187299017

entity bhuuokwkpn is
  port (b : inout real);
end bhuuokwkpn;

architecture dehmtvju of bhuuokwkpn is
  
begin
  -- Single-driven assignments
  b <= 2#1.11#;
end dehmtvju;

library ieee;
use ieee.std_logic_1164.all;

entity oam is
  port (vws : in std_logic_vector(4 to 0));
end oam;

architecture faasnls of oam is
  signal sjutf : real;
begin
  sudjvyw : entity work.bhuuokwkpn
    port map (b => sjutf);
end faasnls;

library ieee;
use ieee.std_logic_1164.all;

entity qbjfohfxlm is
  port (sioru : inout std_logic_vector(3 to 0); ygzs : out integer);
end qbjfohfxlm;

library ieee;
use ieee.std_logic_1164.all;

architecture vmqzadp of qbjfohfxlm is
  signal gfwex : real;
  signal dckttiapl : std_logic_vector(4 to 0);
  signal ovhmwcm : real;
begin
  eydqnapux : entity work.bhuuokwkpn
    port map (b => ovhmwcm);
  orhyrp : entity work.oam
    port map (vws => dckttiapl);
  g : entity work.bhuuokwkpn
    port map (b => gfwex);
  
  -- Multi-driven assignments
  dckttiapl <= sioru;
end vmqzadp;



-- Seed after: 1540711715539548126,12536735141187299017

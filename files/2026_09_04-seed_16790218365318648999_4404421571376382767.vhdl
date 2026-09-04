-- Seed: 16790218365318648999,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (tilx : linkage std_logic_vector(4 to 0));
end y;

architecture mlwgw of y is
  
begin
  
end mlwgw;

library ieee;
use ieee.std_logic_1164.all;

entity nhxmltwb is
  port (ahjrbg : linkage std_logic_vector(0 to 4); kaj : buffer time; lrncjpcv : buffer std_logic; snz : in real);
end nhxmltwb;

library ieee;
use ieee.std_logic_1164.all;

architecture fwkw of nhxmltwb is
  signal pjzy : std_logic_vector(4 to 0);
  signal efec : std_logic_vector(4 to 0);
begin
  lwqdqffe : entity work.y
    port map (tilx => efec);
  cxarhdixj : entity work.y
    port map (tilx => pjzy);
  cwt : entity work.y
    port map (tilx => efec);
  
  -- Single-driven assignments
  kaj <= 16#3# ns;
  
  -- Multi-driven assignments
  efec <= efec;
  lrncjpcv <= 'W';
end fwkw;

library ieee;
use ieee.std_logic_1164.all;

entity irzbp is
  port (kxytz : buffer character; yzwcxfxe : buffer real; effoddu : linkage std_logic_vector(2 to 1); eukfups : inout time);
end irzbp;

library ieee;
use ieee.std_logic_1164.all;

architecture ejyi of irzbp is
  signal lyap : real;
  signal ogwt : std_logic;
  signal mtqjymfb : time;
  signal rtjwi : std_logic_vector(0 to 4);
  signal enm : std_logic_vector(4 to 0);
begin
  zbh : entity work.y
    port map (tilx => enm);
  mkc : entity work.nhxmltwb
    port map (ahjrbg => rtjwi, kaj => mtqjymfb, lrncjpcv => ogwt, snz => lyap);
  rqubrj : entity work.y
    port map (tilx => effoddu);
  
  -- Single-driven assignments
  eukfups <= mtqjymfb;
  lyap <= yzwcxfxe;
  yzwcxfxe <= 0.14023;
  kxytz <= kxytz;
  
  -- Multi-driven assignments
  enm <= "";
  enm <= enm;
  enm <= enm;
  rtjwi <= rtjwi;
end ejyi;



-- Seed after: 4109382371468481516,4404421571376382767

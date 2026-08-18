-- Seed: 9869876133573834337,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity xxeii is
  port (umiv : in integer; uqykyehs : buffer std_logic_vector(3 downto 3));
end xxeii;

architecture olgfwpw of xxeii is
  
begin
  -- Multi-driven assignments
  uqykyehs <= uqykyehs;
end olgfwpw;

library ieee;
use ieee.std_logic_1164.all;

entity xek is
  port (kswa : linkage std_logic_vector(0 downto 4));
end xek;

library ieee;
use ieee.std_logic_1164.all;

architecture baqcddswya of xek is
  signal l : integer;
  signal k : std_logic_vector(3 downto 3);
  signal bjduzi : integer;
  signal yegh : std_logic_vector(3 downto 3);
  signal pxzbwnjx : std_logic_vector(3 downto 3);
  signal ve : integer;
begin
  whinvoi : entity work.xxeii
    port map (umiv => ve, uqykyehs => pxzbwnjx);
  rjexxd : entity work.xxeii
    port map (umiv => ve, uqykyehs => yegh);
  w : entity work.xxeii
    port map (umiv => bjduzi, uqykyehs => k);
  lmhzutx : entity work.xxeii
    port map (umiv => l, uqykyehs => pxzbwnjx);
  
  -- Single-driven assignments
  ve <= 0_4_0_2_4;
  l <= ve;
  bjduzi <= 2#00#;
  
  -- Multi-driven assignments
  pxzbwnjx <= pxzbwnjx;
end baqcddswya;

entity qjqi is
  port (yquynwrrsj : linkage real);
end qjqi;

library ieee;
use ieee.std_logic_1164.all;

architecture a of qjqi is
  signal lcbtug : std_logic_vector(0 downto 4);
  signal onwbgnyhib : std_logic_vector(3 downto 3);
  signal xdygu : integer;
  signal oxss : std_logic_vector(0 downto 4);
  signal uvz : std_logic_vector(0 downto 4);
begin
  gkaabojir : entity work.xek
    port map (kswa => uvz);
  vbiz : entity work.xek
    port map (kswa => oxss);
  suwbf : entity work.xxeii
    port map (umiv => xdygu, uqykyehs => onwbgnyhib);
  egoehpfb : entity work.xek
    port map (kswa => lcbtug);
end a;



-- Seed after: 3525391490435563697,5983430343285687595

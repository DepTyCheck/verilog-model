-- Seed: 10071218934709620649,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity dscbr is
  port (z : linkage std_logic);
end dscbr;

architecture ovi of dscbr is
  
begin
  
end ovi;

entity loxbt is
  port (vhigiekr : buffer time);
end loxbt;

library ieee;
use ieee.std_logic_1164.all;

architecture nuznonvsk of loxbt is
  signal ix : std_logic;
begin
  zhjilym : entity work.dscbr
    port map (z => ix);
  qkwp : entity work.dscbr
    port map (z => ix);
  qh : entity work.dscbr
    port map (z => ix);
  tifhguev : entity work.dscbr
    port map (z => ix);
  
  -- Single-driven assignments
  vhigiekr <= 2#00011# ms;
  
  -- Multi-driven assignments
  ix <= 'W';
  ix <= '0';
  ix <= 'L';
end nuznonvsk;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (rw : buffer integer; uwrb : linkage std_logic; clowgycq : buffer integer; zdqwb : linkage time);
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture zcspi of z is
  signal doegk : std_logic;
begin
  wrgoqg : entity work.dscbr
    port map (z => doegk);
  ec : entity work.dscbr
    port map (z => uwrb);
  
  -- Multi-driven assignments
  doegk <= 'W';
end zcspi;

library ieee;
use ieee.std_logic_1164.all;

entity hxhtb is
  port (xhlsx : out real_vector(2 downto 4); xppptnwl : in std_logic_vector(1 downto 2));
end hxhtb;

library ieee;
use ieee.std_logic_1164.all;

architecture mlwqxwe of hxhtb is
  signal cgcdcmx : std_logic;
begin
  dcii : entity work.dscbr
    port map (z => cgcdcmx);
  
  -- Single-driven assignments
  xhlsx <= (others => 0.0);
  
  -- Multi-driven assignments
  cgcdcmx <= 'L';
  cgcdcmx <= 'W';
  cgcdcmx <= 'Z';
  cgcdcmx <= 'X';
end mlwqxwe;



-- Seed after: 5228650168911816949,17924494779688682807

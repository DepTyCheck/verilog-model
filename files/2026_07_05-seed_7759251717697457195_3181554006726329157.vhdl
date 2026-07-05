-- Seed: 7759251717697457195,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;

entity eq is
  port (xbczpww : out time_vector(0 downto 0); bobw : out std_logic_vector(0 downto 3));
end eq;

architecture ovyt of eq is
  
begin
  -- Single-driven assignments
  xbczpww <= (others => 3_3_3_2 ns);
  
  -- Multi-driven assignments
  bobw <= bobw;
  bobw <= bobw;
  bobw <= "";
end ovyt;

use std.reflection.all;

entity i is
  port (mbotu : inout value_mirror; g : inout physical_value_mirror; lkowuxw : out boolean_vector(4 to 4));
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture pvnjjvumm of i is
  signal oaiqakkb : std_logic_vector(0 downto 3);
  signal cporz : time_vector(0 downto 0);
  signal ufdjz : std_logic_vector(0 downto 3);
  signal irozescrl : time_vector(0 downto 0);
begin
  zym : entity work.eq
    port map (xbczpww => irozescrl, bobw => ufdjz);
  sngpi : entity work.eq
    port map (xbczpww => cporz, bobw => oaiqakkb);
  
  -- Single-driven assignments
  lkowuxw <= (others => TRUE);
  
  -- Multi-driven assignments
  oaiqakkb <= oaiqakkb;
  oaiqakkb <= ufdjz;
end pvnjjvumm;

library ieee;
use ieee.std_logic_1164.all;

entity iksydouwue is
  port (uf : linkage std_logic);
end iksydouwue;

library ieee;
use ieee.std_logic_1164.all;

architecture jxdm of iksydouwue is
  signal aerrrxiw : time_vector(0 downto 0);
  signal gnfgyw : time_vector(0 downto 0);
  signal xfzgihlr : time_vector(0 downto 0);
  signal qtbikdzoj : std_logic_vector(0 downto 3);
  signal r : time_vector(0 downto 0);
begin
  wqekwrsi : entity work.eq
    port map (xbczpww => r, bobw => qtbikdzoj);
  cjrtzgwikl : entity work.eq
    port map (xbczpww => xfzgihlr, bobw => qtbikdzoj);
  o : entity work.eq
    port map (xbczpww => gnfgyw, bobw => qtbikdzoj);
  bmzhv : entity work.eq
    port map (xbczpww => aerrrxiw, bobw => qtbikdzoj);
  
  -- Multi-driven assignments
  qtbikdzoj <= qtbikdzoj;
end jxdm;



-- Seed after: 17633130208619191967,3181554006726329157

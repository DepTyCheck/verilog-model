-- Seed: 4154157189205400975,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity czi is
  port (raefdledq : linkage std_logic_vector(0 downto 4));
end czi;

architecture lsfkknl of czi is
  
begin
  
end lsfkknl;

library ieee;
use ieee.std_logic_1164.all;

entity spb is
  port (pritvjphqc : inout std_logic_vector(2 downto 4));
end spb;

library ieee;
use ieee.std_logic_1164.all;

architecture vziaboaqz of spb is
  signal mtdvdvzejh : std_logic_vector(0 downto 4);
  signal npuuaiyg : std_logic_vector(0 downto 4);
begin
  jqncdbw : entity work.czi
    port map (raefdledq => npuuaiyg);
  psmjivq : entity work.czi
    port map (raefdledq => pritvjphqc);
  fz : entity work.czi
    port map (raefdledq => mtdvdvzejh);
  
  -- Multi-driven assignments
  pritvjphqc <= (others => '0');
  pritvjphqc <= (others => '0');
  pritvjphqc <= (others => '0');
  mtdvdvzejh <= (others => '0');
end vziaboaqz;

library ieee;
use ieee.std_logic_1164.all;

entity cdjhpd is
  port (ulqgcqoq : in std_logic_vector(3 downto 0); ftdp : in severity_level; arisbcr : inout bit; m : inout boolean);
end cdjhpd;

library ieee;
use ieee.std_logic_1164.all;

architecture zodvqw of cdjhpd is
  signal qurig : std_logic_vector(0 downto 4);
begin
  krzmregxw : entity work.czi
    port map (raefdledq => qurig);
  iisqifzmh : entity work.czi
    port map (raefdledq => qurig);
  
  -- Single-driven assignments
  arisbcr <= '0';
  
  -- Multi-driven assignments
  qurig <= (others => '0');
  qurig <= "";
end zodvqw;



-- Seed after: 1109962511016393838,13694093582652240945

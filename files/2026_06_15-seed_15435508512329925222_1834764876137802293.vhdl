-- Seed: 15435508512329925222,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity wue is
  port (lvudgm : out std_logic_vector(1 to 0));
end wue;

architecture z of wue is
  
begin
  -- Multi-driven assignments
  lvudgm <= (others => '0');
end z;

entity upmi is
  port (vbyi : in time; d : linkage bit_vector(1 downto 2); helxi : inout severity_level);
end upmi;

library ieee;
use ieee.std_logic_1164.all;

architecture uc of upmi is
  signal rxlynugiz : std_logic_vector(1 to 0);
begin
  elkkk : entity work.wue
    port map (lvudgm => rxlynugiz);
  zikejhteg : entity work.wue
    port map (lvudgm => rxlynugiz);
  txkpakzy : entity work.wue
    port map (lvudgm => rxlynugiz);
  xxhmxiyju : entity work.wue
    port map (lvudgm => rxlynugiz);
  
  -- Single-driven assignments
  helxi <= WARNING;
end uc;

entity koot is
  port (wdod : out time; pac : inout time);
end koot;

library ieee;
use ieee.std_logic_1164.all;

architecture kx of koot is
  signal jxszrphzrv : std_logic_vector(1 to 0);
begin
  ctyxec : entity work.wue
    port map (lvudgm => jxszrphzrv);
  ywnzurjjo : entity work.wue
    port map (lvudgm => jxszrphzrv);
  
  -- Single-driven assignments
  pac <= 2#1_1_1_0_1.1# ps;
  wdod <= 43.3114 ns;
  
  -- Multi-driven assignments
  jxszrphzrv <= (others => '0');
  jxszrphzrv <= "";
  jxszrphzrv <= (others => '0');
  jxszrphzrv <= (others => '0');
end kx;



-- Seed after: 15915974171925801155,1834764876137802293

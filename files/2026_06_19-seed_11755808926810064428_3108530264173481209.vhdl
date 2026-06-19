-- Seed: 11755808926810064428,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity lfijkty is
  port (icirakzux : in time; owpwpr : buffer string(3 downto 5); kez : in std_logic; dmx : in real_vector(4 to 1));
end lfijkty;

architecture blj of lfijkty is
  
begin
  -- Single-driven assignments
  owpwpr <= "";
end blj;

library ieee;
use ieee.std_logic_1164.all;

entity wgxsaqoat is
  port (sr : in time_vector(4 to 3); pdnaka : inout std_logic);
end wgxsaqoat;

library ieee;
use ieee.std_logic_1164.all;

architecture mcwhr of wgxsaqoat is
  signal lbd : real_vector(4 to 1);
  signal sjogoaeb : std_logic;
  signal jdk : string(3 downto 5);
  signal bplbqy : time;
  signal hnuc : real_vector(4 to 1);
  signal xbzwi : string(3 downto 5);
  signal ceg : time;
  signal ha : real_vector(4 to 1);
  signal rngqcuyonr : string(3 downto 5);
  signal korkkheb : time;
  signal vhqhpcdntb : real_vector(4 to 1);
  signal jx : std_logic;
  signal fjvac : string(3 downto 5);
  signal xzgsso : time;
begin
  kgbxz : entity work.lfijkty
    port map (icirakzux => xzgsso, owpwpr => fjvac, kez => jx, dmx => vhqhpcdntb);
  lh : entity work.lfijkty
    port map (icirakzux => korkkheb, owpwpr => rngqcuyonr, kez => pdnaka, dmx => ha);
  zkqtu : entity work.lfijkty
    port map (icirakzux => ceg, owpwpr => xbzwi, kez => jx, dmx => hnuc);
  hbdrwsxdz : entity work.lfijkty
    port map (icirakzux => bplbqy, owpwpr => jdk, kez => sjogoaeb, dmx => lbd);
  
  -- Single-driven assignments
  bplbqy <= 1000 fs;
  lbd <= (others => 0.0);
  xzgsso <= 16#8628B# fs;
  hnuc <= (others => 0.0);
end mcwhr;



-- Seed after: 14217387815590419564,3108530264173481209

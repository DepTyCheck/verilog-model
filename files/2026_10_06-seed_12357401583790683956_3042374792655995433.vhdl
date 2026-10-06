-- Seed: 12357401583790683956,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity vhvscul is
  port (zcqrxbsewm : in real_vector(0 downto 2); xrayj : out std_logic; jvweddxq : buffer std_logic);
end vhvscul;

architecture sfltx of vhvscul is
  
begin
  -- Multi-driven assignments
  jvweddxq <= 'H';
  jvweddxq <= 'X';
end sfltx;

entity gb is
  port (zzdzqlc : in bit);
end gb;

library ieee;
use ieee.std_logic_1164.all;

architecture kt of gb is
  signal gcuz : std_logic;
  signal tjlubdzna : real_vector(0 downto 2);
  signal fprbveja : std_logic;
  signal di : real_vector(0 downto 2);
  signal azz : std_logic;
  signal tfgxlcjbyy : std_logic;
  signal apa : real_vector(0 downto 2);
  signal mljytbwmtr : std_logic;
  signal yhcjgkmoxs : real_vector(0 downto 2);
begin
  jlcdc : entity work.vhvscul
    port map (zcqrxbsewm => yhcjgkmoxs, xrayj => mljytbwmtr, jvweddxq => mljytbwmtr);
  dbkwom : entity work.vhvscul
    port map (zcqrxbsewm => apa, xrayj => tfgxlcjbyy, jvweddxq => azz);
  paftjdxnju : entity work.vhvscul
    port map (zcqrxbsewm => di, xrayj => fprbveja, jvweddxq => fprbveja);
  yatmcvrhm : entity work.vhvscul
    port map (zcqrxbsewm => tjlubdzna, xrayj => fprbveja, jvweddxq => gcuz);
  
  -- Multi-driven assignments
  gcuz <= fprbveja;
end kt;

library ieee;
use ieee.std_logic_1164.all;

entity uf is
  port (hqvpbpx : out std_logic_vector(2 to 4));
end uf;

library ieee;
use ieee.std_logic_1164.all;

architecture onqom of uf is
  signal th : std_logic;
  signal dbrkal : std_logic;
  signal hmaf : real_vector(0 downto 2);
begin
  km : entity work.vhvscul
    port map (zcqrxbsewm => hmaf, xrayj => dbrkal, jvweddxq => th);
  gj : entity work.vhvscul
    port map (zcqrxbsewm => hmaf, xrayj => dbrkal, jvweddxq => dbrkal);
  
  -- Single-driven assignments
  hmaf <= hmaf;
  
  -- Multi-driven assignments
  hqvpbpx <= "XU-";
  hqvpbpx <= hqvpbpx;
end onqom;



-- Seed after: 15921537764610426794,3042374792655995433

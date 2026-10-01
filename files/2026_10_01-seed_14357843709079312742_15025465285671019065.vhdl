-- Seed: 14357843709079312742,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity wil is
  port (jhbqha : out time; zfpjkdccq : inout std_logic; xil : in std_logic_vector(1 to 2); qkbnzd : out std_logic_vector(4 downto 3));
end wil;

architecture qlwkqfgeg of wil is
  
begin
  -- Single-driven assignments
  jhbqha <= 16#2_0# fs;
  
  -- Multi-driven assignments
  qkbnzd <= xil;
  qkbnzd <= ('X', 'W');
end qlwkqfgeg;

library ieee;
use ieee.std_logic_1164.all;

entity rx is
  port (qfwfzqz : buffer std_logic; tptmf : linkage integer; nmql : out integer; qrsavpdx : in integer);
end rx;

library ieee;
use ieee.std_logic_1164.all;

architecture odsl of rx is
  signal ekylyexy : std_logic_vector(4 downto 3);
  signal zs : std_logic;
  signal lwirutkl : time;
begin
  pkkiolepq : entity work.wil
    port map (jhbqha => lwirutkl, zfpjkdccq => zs, xil => ekylyexy, qkbnzd => ekylyexy);
  
  -- Single-driven assignments
  nmql <= 0_4;
  
  -- Multi-driven assignments
  ekylyexy <= "-W";
  qfwfzqz <= qfwfzqz;
  ekylyexy <= ('W', 'Z');
end odsl;

entity oww is
  port (eizd : in time);
end oww;

architecture kuu of oww is
  
begin
  
end kuu;



-- Seed after: 15245637205426898156,15025465285671019065

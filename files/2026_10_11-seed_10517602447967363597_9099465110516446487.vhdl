-- Seed: 10517602447967363597,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity cx is
  port (pdmrdvbi : out std_logic_vector(3 downto 2));
end cx;

architecture whtlcah of cx is
  
begin
  -- Multi-driven assignments
  pdmrdvbi <= "UU";
  pdmrdvbi <= "1U";
  pdmrdvbi <= pdmrdvbi;
end whtlcah;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (flxo : out std_logic_vector(3 to 1); mfgwdfj : in integer_vector(3 to 0); nekbdc : in std_logic_vector(4 to 4); giopqtanl : inout std_logic);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture fzbz of v is
  signal yoxscl : std_logic_vector(3 downto 2);
begin
  t : entity work.cx
    port map (pdmrdvbi => yoxscl);
  
  -- Multi-driven assignments
  yoxscl <= "XU";
  yoxscl <= "00";
  giopqtanl <= '-';
end fzbz;



-- Seed after: 14787107393467541416,9099465110516446487

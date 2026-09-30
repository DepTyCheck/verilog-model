-- Seed: 17220906632988277986,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity wkyjff is
  port (gbsb : buffer integer; ff : out std_logic_vector(2 downto 4));
end wkyjff;

architecture ybw of wkyjff is
  
begin
  -- Single-driven assignments
  gbsb <= gbsb;
  
  -- Multi-driven assignments
  ff <= ff;
  ff <= "";
  ff <= (others => '0');
end ybw;



-- Seed after: 10633130465379718263,12260394286515585877

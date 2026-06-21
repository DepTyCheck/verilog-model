-- Seed: 18156565601267866535,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity pbh is
  port (ts : in std_logic; hhgwbapt : buffer bit; dopvn : buffer std_logic_vector(1 downto 2));
end pbh;

architecture gwd of pbh is
  
begin
  -- Single-driven assignments
  hhgwbapt <= '1';
  
  -- Multi-driven assignments
  dopvn <= "";
end gwd;



-- Seed after: 11626068151231304093,3687118713772291287

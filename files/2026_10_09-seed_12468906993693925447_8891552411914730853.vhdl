-- Seed: 12468906993693925447,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity vho is
  port (wsdxvvn : buffer time_vector(4 downto 4); t : inout std_logic; x : buffer std_logic);
end vho;

architecture mdctxy of vho is
  
begin
  -- Single-driven assignments
  wsdxvvn <= (others => 4 sec);
  
  -- Multi-driven assignments
  t <= x;
end mdctxy;



-- Seed after: 10828844871321225964,8891552411914730853

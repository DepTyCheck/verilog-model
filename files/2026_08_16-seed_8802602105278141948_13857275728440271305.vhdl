-- Seed: 8802602105278141948,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity grc is
  port (kuuno : out std_logic_vector(2 downto 1); dlfp : inout severity_level);
end grc;

architecture rekiq of grc is
  
begin
  -- Single-driven assignments
  dlfp <= FAILURE;
  
  -- Multi-driven assignments
  kuuno <= "LX";
  kuuno <= ('X', '-');
  kuuno <= ('H', '-');
end rekiq;



-- Seed after: 4934467381196109748,13857275728440271305

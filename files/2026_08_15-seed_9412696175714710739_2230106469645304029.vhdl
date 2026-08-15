-- Seed: 9412696175714710739,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity jsbdvg is
  port (obtho : inout time; uxosco : inout std_logic_vector(3 downto 0));
end jsbdvg;

architecture wda of jsbdvg is
  
begin
  -- Single-driven assignments
  obtho <= 8#4# ns;
end wda;



-- Seed after: 11368461378741680603,2230106469645304029

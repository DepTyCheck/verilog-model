-- Seed: 8398482116567246149,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity uwp is
  port (xms : buffer std_logic_vector(2 downto 4); vkuwh : out time);
end uwp;

architecture smos of uwp is
  
begin
  -- Single-driven assignments
  vkuwh <= 16#D.A_5_6# ms;
  
  -- Multi-driven assignments
  xms <= "";
end smos;



-- Seed after: 4164799144372028491,6869379746012019351

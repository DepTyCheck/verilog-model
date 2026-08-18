-- Seed: 9603660541605765540,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity yr is
  port (qkct : in severity_level; rxh : buffer std_logic_vector(4 downto 3));
end yr;

architecture pnmld of yr is
  
begin
  -- Multi-driven assignments
  rxh <= "H0";
  rxh <= rxh;
  rxh <= rxh;
  rxh <= ('L', 'X');
end pnmld;



-- Seed after: 5210994078308952936,5983430343285687595

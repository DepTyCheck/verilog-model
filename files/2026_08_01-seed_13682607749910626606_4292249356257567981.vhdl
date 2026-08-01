-- Seed: 13682607749910626606,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity slfxj is
  port (ph : buffer std_logic_vector(4 to 0));
end slfxj;

architecture kubdep of slfxj is
  
begin
  -- Multi-driven assignments
  ph <= ph;
  ph <= (others => '0');
end kubdep;



-- Seed after: 14459092080623231242,4292249356257567981

-- Seed: 1827982427964572036,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity oolfxqs is
  port (pumpu : out std_logic_vector(3 to 0));
end oolfxqs;

architecture f of oolfxqs is
  
begin
  -- Multi-driven assignments
  pumpu <= pumpu;
  pumpu <= "";
end f;



-- Seed after: 3432904438337859714,11127274767545411571

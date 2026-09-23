-- Seed: 6577508908827822710,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity ei is
  port (ybdry : buffer std_logic_vector(2 downto 0));
end ei;

architecture iwqzg of ei is
  
begin
  -- Multi-driven assignments
  ybdry <= ('U', 'Z', 'H');
  ybdry <= ('U', 'L', '-');
end iwqzg;



-- Seed after: 12822564411203015201,8067602802092121131

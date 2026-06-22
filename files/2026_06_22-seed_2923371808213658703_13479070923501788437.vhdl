-- Seed: 2923371808213658703,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (wfe : linkage std_logic_vector(0 downto 0); z : out std_logic_vector(3 downto 2); osr : out bit);
end u;

architecture ajogmh of u is
  
begin
  -- Single-driven assignments
  osr <= '0';
  
  -- Multi-driven assignments
  z <= ('W', 'Z');
  z <= "W-";
  z <= ('Z', 'L');
  z <= ('Z', '1');
end ajogmh;



-- Seed after: 903515272536163802,13479070923501788437

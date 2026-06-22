-- Seed: 12595048147373318761,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity mpvsia is
  port (itaqry : inout std_logic_vector(2 downto 2); skzw : buffer std_logic);
end mpvsia;

architecture bj of mpvsia is
  
begin
  -- Multi-driven assignments
  itaqry <= "X";
  skzw <= 'L';
  skzw <= 'U';
end bj;



-- Seed after: 15284517646423736303,13479070923501788437

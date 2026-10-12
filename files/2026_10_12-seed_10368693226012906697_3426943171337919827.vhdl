-- Seed: 10368693226012906697,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity qu is
  port (je : buffer time; tcbden : inout std_logic; lmouipkht : in real_vector(0 downto 3));
end qu;

architecture us of qu is
  
begin
  -- Single-driven assignments
  je <= je;
  
  -- Multi-driven assignments
  tcbden <= 'X';
  tcbden <= '-';
end us;



-- Seed after: 12053070237188097543,3426943171337919827

-- Seed: 16596185553753082446,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity szsgid is
  port (qu : in std_logic; de : inout integer; atab : linkage std_logic_vector(3 downto 4));
end szsgid;

architecture qhoff of szsgid is
  
begin
  -- Single-driven assignments
  de <= 3121;
end qhoff;



-- Seed after: 6156939161893568078,2511821214772927453

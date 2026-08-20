-- Seed: 2888054357421106533,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity pf is
  port (bi : out real; yae : out time; ndza : inout std_logic_vector(4 to 1));
end pf;

architecture cp of pf is
  
begin
  -- Single-driven assignments
  yae <= yae;
  bi <= 042.4_3_0_2;
  
  -- Multi-driven assignments
  ndza <= "";
  ndza <= (others => '0');
  ndza <= ndza;
end cp;



-- Seed after: 8347321804568921819,499459191852795575

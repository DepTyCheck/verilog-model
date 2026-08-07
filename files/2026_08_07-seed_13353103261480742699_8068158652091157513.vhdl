-- Seed: 13353103261480742699,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (cshzjoj : inout std_logic_vector(3 to 4));
end i;

architecture cmzf of i is
  
begin
  -- Multi-driven assignments
  cshzjoj <= ('X', '0');
  cshzjoj <= "WH";
  cshzjoj <= cshzjoj;
end cmzf;



-- Seed after: 11960909555174262975,8068158652091157513

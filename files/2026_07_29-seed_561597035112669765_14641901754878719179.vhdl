-- Seed: 561597035112669765,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (hjh : buffer std_logic_vector(3 to 3));
end r;

architecture laznjq of r is
  
begin
  -- Multi-driven assignments
  hjh <= (others => 'X');
  hjh <= hjh;
  hjh <= "U";
end laznjq;



-- Seed after: 5969975593205387541,14641901754878719179

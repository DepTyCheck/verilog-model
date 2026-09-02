-- Seed: 843809523758170016,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity tp is
  port (eas : out std_logic_vector(1 to 3));
end tp;

architecture xpr of tp is
  
begin
  -- Multi-driven assignments
  eas <= eas;
  eas <= eas;
  eas <= eas;
end xpr;



-- Seed after: 13122751911850429219,3400751927341804175

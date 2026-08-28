-- Seed: 5495666741451653912,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (dc : inout std_logic_vector(4 to 3));
end y;

architecture mqi of y is
  
begin
  -- Multi-driven assignments
  dc <= (others => '0');
  dc <= (others => '0');
  dc <= "";
  dc <= "";
end mqi;



-- Seed after: 11059789426652120723,7198033922882419595

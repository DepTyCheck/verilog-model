-- Seed: 8374819503095652360,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity xcrr is
  port (pdo : out real; sclige : buffer std_logic_vector(4 to 2));
end xcrr;

architecture hzgac of xcrr is
  
begin
  -- Single-driven assignments
  pdo <= 8#3_1_7_6.4054#;
  
  -- Multi-driven assignments
  sclige <= "";
  sclige <= "";
  sclige <= "";
  sclige <= (others => '0');
end hzgac;



-- Seed after: 5104868625393308980,6882842853887419669

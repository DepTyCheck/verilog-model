-- Seed: 7546255749596401239,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity mbiad is
  port (rb : in bit; mlounbvy : inout std_logic);
end mbiad;

architecture fu of mbiad is
  
begin
  -- Multi-driven assignments
  mlounbvy <= mlounbvy;
  mlounbvy <= 'U';
end fu;



-- Seed after: 7778314258054580346,10463297573877745897

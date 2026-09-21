-- Seed: 1504806760742174816,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity ioqw is
  port (tdxww : in boolean_vector(0 to 4); mcxne : buffer std_logic);
end ioqw;

architecture ae of ioqw is
  
begin
  -- Multi-driven assignments
  mcxne <= 'U';
  mcxne <= 'H';
  mcxne <= mcxne;
end ae;



-- Seed after: 3034733227209444325,12143220691580258643

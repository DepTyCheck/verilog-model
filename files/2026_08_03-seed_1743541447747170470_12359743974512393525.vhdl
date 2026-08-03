-- Seed: 1743541447747170470,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity vb is
  port (jfl : out std_logic_vector(1 to 4));
end vb;

architecture hkorjasxb of vb is
  
begin
  -- Multi-driven assignments
  jfl <= ('0', '1', 'H', 'L');
  jfl <= jfl;
end hkorjasxb;



-- Seed after: 4498566638877329753,12359743974512393525

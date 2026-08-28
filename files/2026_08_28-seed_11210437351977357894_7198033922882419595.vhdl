-- Seed: 11210437351977357894,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (tmze : inout std_logic);
end n;

architecture vn of n is
  
begin
  -- Multi-driven assignments
  tmze <= 'Z';
  tmze <= 'L';
  tmze <= '1';
  tmze <= '0';
end vn;



-- Seed after: 15736716995324738662,7198033922882419595

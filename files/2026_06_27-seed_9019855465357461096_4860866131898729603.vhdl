-- Seed: 9019855465357461096,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (h : inout std_logic);
end e;

architecture jcg of e is
  
begin
  -- Multi-driven assignments
  h <= 'L';
  h <= 'U';
  h <= 'X';
  h <= 'Z';
end jcg;



-- Seed after: 976020374215717965,4860866131898729603

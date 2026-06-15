-- Seed: 14164532008540481601,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity vsp is
  port (wqisam : out std_logic; cnpkpdz : out time; rly : buffer character);
end vsp;

architecture sxp of vsp is
  
begin
  -- Single-driven assignments
  cnpkpdz <= 24 ns;
  rly <= 'h';
  
  -- Multi-driven assignments
  wqisam <= '1';
  wqisam <= 'H';
  wqisam <= 'L';
  wqisam <= 'W';
end sxp;



-- Seed after: 5197907840541657120,1834764876137802293

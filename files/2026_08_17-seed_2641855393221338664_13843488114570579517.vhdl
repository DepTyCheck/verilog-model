-- Seed: 2641855393221338664,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity cxmem is
  port (lupqw : buffer std_logic; dcds : inout character; ocua : buffer integer);
end cxmem;

architecture iv of cxmem is
  
begin
  -- Multi-driven assignments
  lupqw <= lupqw;
  lupqw <= 'W';
  lupqw <= '1';
end iv;



-- Seed after: 1899123000512862686,13843488114570579517

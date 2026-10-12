-- Seed: 1512174323979088648,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (tvyralg : in time; lhhhzs : buffer std_logic);
end i;

architecture btb of i is
  
begin
  -- Multi-driven assignments
  lhhhzs <= 'Z';
  lhhhzs <= lhhhzs;
  lhhhzs <= 'X';
  lhhhzs <= lhhhzs;
end btb;



-- Seed after: 6006765144817738216,6869379746012019351

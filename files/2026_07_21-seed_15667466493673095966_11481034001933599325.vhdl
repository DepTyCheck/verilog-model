-- Seed: 15667466493673095966,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (fx : out std_logic_vector(2 to 0); rrzktynge : buffer std_logic_vector(3 to 1));
end w;

architecture ene of w is
  
begin
  -- Multi-driven assignments
  rrzktynge <= "";
  rrzktynge <= fx;
  fx <= rrzktynge;
  rrzktynge <= (others => '0');
end ene;



-- Seed after: 9638491426182728541,11481034001933599325

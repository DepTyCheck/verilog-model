-- Seed: 7056153465989265879,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity rig is
  port (g : out std_logic; cneqgth : inout integer_vector(3 to 2); rp : buffer time);
end rig;

architecture nmvtl of rig is
  
begin
  -- Single-driven assignments
  cneqgth <= (others => 0);
  rp <= 8#3_0.10010# ns;
  
  -- Multi-driven assignments
  g <= g;
  g <= 'L';
end nmvtl;



-- Seed after: 822884056603507567,18037650846010261179

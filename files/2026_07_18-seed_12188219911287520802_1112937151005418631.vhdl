-- Seed: 12188219911287520802,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity vcch is
  port (fwbgsdx : out std_logic; nltt : linkage std_logic; w : inout std_logic_vector(2 downto 2));
end vcch;

architecture ui of vcch is
  
begin
  -- Multi-driven assignments
  w <= (others => 'Z');
end ui;



-- Seed after: 12282853245723396576,1112937151005418631

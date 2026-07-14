-- Seed: 1377360603608764390,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity ezkvj is
  port (fzlou : inout integer_vector(1 to 2); oxd : linkage std_logic_vector(2 downto 3));
end ezkvj;

architecture vedvgwg of ezkvj is
  
begin
  
end vedvgwg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity agqxzmvh is
  port (htwfzruxt : linkage real_vector(2 downto 4); zq : inout enumeration_value_mirror; ml : in std_logic_vector(3 to 0));
end agqxzmvh;

library ieee;
use ieee.std_logic_1164.all;

architecture fw of agqxzmvh is
  signal dhmorpotdy : std_logic_vector(2 downto 3);
  signal afhyifo : integer_vector(1 to 2);
  signal mavedyvngy : integer_vector(1 to 2);
begin
  bi : entity work.ezkvj
    port map (fzlou => mavedyvngy, oxd => ml);
  hn : entity work.ezkvj
    port map (fzlou => afhyifo, oxd => dhmorpotdy);
  
  -- Multi-driven assignments
  dhmorpotdy <= (others => '0');
  dhmorpotdy <= (others => '0');
end fw;



-- Seed after: 10257889377425441115,7726014785203345639

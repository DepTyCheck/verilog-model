-- Seed: 17850893156509680268,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity yzwvc is
  port (lqq : inout std_logic; hrwgsm : buffer std_logic_vector(1 downto 1); kc : buffer real; ao : buffer real_vector(0 to 2));
end yzwvc;

architecture mttcor of yzwvc is
  
begin
  -- Single-driven assignments
  ao <= (2#11011.0_1_1#, 16#E15.F_4#, 2.232);
  kc <= 0.43;
  
  -- Multi-driven assignments
  hrwgsm <= "U";
  hrwgsm <= (others => 'W');
end mttcor;



-- Seed after: 16930560672656762599,12011142928354116943

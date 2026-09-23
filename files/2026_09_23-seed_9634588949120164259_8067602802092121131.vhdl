-- Seed: 9634588949120164259,8067602802092121131

entity s is
  port (xqgqvu : in severity_level; ffxfbceetm : buffer boolean_vector(3 downto 0));
end s;

architecture nhg of s is
  
begin
  
end nhg;

library ieee;
use ieee.std_logic_1164.all;

entity dulzf is
  port (thmnqdzt : linkage std_logic_vector(1 downto 0));
end dulzf;

architecture pgipqglhu of dulzf is
  signal mbnnobqhf : boolean_vector(3 downto 0);
  signal mnmpdkxiz : boolean_vector(3 downto 0);
  signal kwzm : boolean_vector(3 downto 0);
  signal gshgk : severity_level;
  signal nzgubx : boolean_vector(3 downto 0);
  signal vtmg : severity_level;
begin
  feqt : entity work.s
    port map (xqgqvu => vtmg, ffxfbceetm => nzgubx);
  gazldm : entity work.s
    port map (xqgqvu => gshgk, ffxfbceetm => kwzm);
  uc : entity work.s
    port map (xqgqvu => gshgk, ffxfbceetm => mnmpdkxiz);
  evkldq : entity work.s
    port map (xqgqvu => gshgk, ffxfbceetm => mbnnobqhf);
  
  -- Single-driven assignments
  vtmg <= NOTE;
  gshgk <= vtmg;
end pgipqglhu;



-- Seed after: 2241063047652828605,8067602802092121131

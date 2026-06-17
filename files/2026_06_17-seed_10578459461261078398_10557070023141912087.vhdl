-- Seed: 10578459461261078398,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity bostcfwi is
  port (stigjvhc : in bit; qujbq : linkage boolean_vector(1 downto 2); yejizhkov : inout std_logic_vector(2 to 3); jtwplruxlx : buffer real);
end bostcfwi;

architecture rcifle of bostcfwi is
  
begin
  -- Single-driven assignments
  jtwplruxlx <= 2#001.1_1#;
end rcifle;

entity rpkoj is
  port (ozxbs : linkage real; fuydkodpu : in integer; lagqtad : linkage time);
end rpkoj;

library ieee;
use ieee.std_logic_1164.all;

architecture fa of rpkoj is
  signal srd : real;
  signal yenl : boolean_vector(1 downto 2);
  signal dq : real;
  signal yu : std_logic_vector(2 to 3);
  signal l : boolean_vector(1 downto 2);
  signal iwnraka : real;
  signal nosxxwc : boolean_vector(1 downto 2);
  signal xapayapujf : real;
  signal teem : std_logic_vector(2 to 3);
  signal pggr : boolean_vector(1 downto 2);
  signal a : bit;
begin
  fzxxcb : entity work.bostcfwi
    port map (stigjvhc => a, qujbq => pggr, yejizhkov => teem, jtwplruxlx => xapayapujf);
  rgxvsycs : entity work.bostcfwi
    port map (stigjvhc => a, qujbq => nosxxwc, yejizhkov => teem, jtwplruxlx => iwnraka);
  cwaxjyzcmi : entity work.bostcfwi
    port map (stigjvhc => a, qujbq => l, yejizhkov => yu, jtwplruxlx => dq);
  uibdgej : entity work.bostcfwi
    port map (stigjvhc => a, qujbq => yenl, yejizhkov => teem, jtwplruxlx => srd);
  
  -- Single-driven assignments
  a <= '1';
end fa;

entity spkxvlqaw is
  port (g : linkage bit; fgbwydh : linkage time; grxjyksda : buffer integer);
end spkxvlqaw;

architecture oqgxanp of spkxvlqaw is
  signal tcuimj : real;
begin
  eicyaw : entity work.rpkoj
    port map (ozxbs => tcuimj, fuydkodpu => grxjyksda, lagqtad => fgbwydh);
  
  -- Single-driven assignments
  grxjyksda <= 16#80744#;
end oqgxanp;



-- Seed after: 10312565677123196033,10557070023141912087

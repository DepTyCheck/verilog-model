-- Seed: 3432837708697965369,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity rbjfupjf is
  port (y : in real_vector(0 to 2); zkiqqb : inout std_logic_vector(1 to 1); d : out integer; nqsx : buffer severity_level);
end rbjfupjf;

architecture ykytbgrd of rbjfupjf is
  
begin
  -- Multi-driven assignments
  zkiqqb <= (others => 'L');
  zkiqqb <= (others => 'X');
end ykytbgrd;

library ieee;
use ieee.std_logic_1164.all;

entity umb is
  port (dl : out time_vector(1 downto 2); nmytbne : buffer boolean_vector(1 downto 1); famj : out std_logic);
end umb;

library ieee;
use ieee.std_logic_1164.all;

architecture ydhceewffg of umb is
  signal mygclcwl : severity_level;
  signal komhgcjjh : integer;
  signal xqy : std_logic_vector(1 to 1);
  signal rzcxizm : severity_level;
  signal uate : integer;
  signal uttzlygexm : std_logic_vector(1 to 1);
  signal pmjtej : real_vector(0 to 2);
begin
  mkgormqj : entity work.rbjfupjf
    port map (y => pmjtej, zkiqqb => uttzlygexm, d => uate, nqsx => rzcxizm);
  cijfkfegw : entity work.rbjfupjf
    port map (y => pmjtej, zkiqqb => xqy, d => komhgcjjh, nqsx => mygclcwl);
  
  -- Single-driven assignments
  dl <= (others => 0 ns);
  pmjtej <= (1_0_2_1.03124, 8#6_7_3_7.516#, 10430.3_4_2_0_3);
  nmytbne <= (others => TRUE);
  
  -- Multi-driven assignments
  famj <= 'U';
  famj <= '-';
  xqy <= "1";
  uttzlygexm <= "X";
end ydhceewffg;



-- Seed after: 9661092969240562116,13479070923501788437

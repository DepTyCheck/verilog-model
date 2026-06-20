-- Seed: 6228586407391638892,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity gkfocahx is
  port (ol : linkage std_logic_vector(4 downto 4); vs : in std_logic_vector(2 downto 2); w : inout time; sbx : buffer time);
end gkfocahx;

architecture o of gkfocahx is
  
begin
  
end o;

entity mb is
  port (qlqn : linkage boolean_vector(2 downto 1));
end mb;

library ieee;
use ieee.std_logic_1164.all;

architecture ycpzowylrr of mb is
  signal cssd : time;
  signal lqrgrizd : time;
  signal vwokpkest : std_logic_vector(2 downto 2);
  signal admundeeza : time;
  signal iwujxsd : time;
  signal bmjijxvzvy : std_logic_vector(2 downto 2);
  signal osdim : time;
  signal okgz : time;
  signal m : std_logic_vector(4 downto 4);
  signal jjgpa : std_logic_vector(4 downto 4);
begin
  bmcicyrze : entity work.gkfocahx
    port map (ol => jjgpa, vs => m, w => okgz, sbx => osdim);
  dcksklggiz : entity work.gkfocahx
    port map (ol => m, vs => bmjijxvzvy, w => iwujxsd, sbx => admundeeza);
  tp : entity work.gkfocahx
    port map (ol => jjgpa, vs => vwokpkest, w => lqrgrizd, sbx => cssd);
  
  -- Multi-driven assignments
  jjgpa <= "X";
  jjgpa <= (others => '-');
  jjgpa <= (others => 'Z');
  jjgpa <= (others => 'Z');
end ycpzowylrr;



-- Seed after: 1061700023109197703,3924983747739634027

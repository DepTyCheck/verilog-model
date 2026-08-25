-- Seed: 13833425927172666995,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (ispacxw : out std_logic_vector(0 to 1));
end f;

architecture rujwnuknt of f is
  
begin
  
end rujwnuknt;

entity bx is
  port (zmokscor : linkage time);
end bx;

library ieee;
use ieee.std_logic_1164.all;

architecture t of bx is
  signal avruvkz : std_logic_vector(0 to 1);
  signal lohlsvq : std_logic_vector(0 to 1);
begin
  qu : entity work.f
    port map (ispacxw => lohlsvq);
  zykbamuwl : entity work.f
    port map (ispacxw => avruvkz);
  zikzig : entity work.f
    port map (ispacxw => lohlsvq);
  
  -- Multi-driven assignments
  lohlsvq <= "WH";
end t;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (xjcllxsgt : inout string(5 to 4); ohb : buffer bit; x : inout std_logic_vector(2 downto 2); nqhj : buffer std_logic);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture ecuhmui of n is
  signal eyutkd : std_logic_vector(0 to 1);
  signal adxmrf : std_logic_vector(0 to 1);
  signal vguchfrje : std_logic_vector(0 to 1);
  signal jgj : time;
begin
  ixoiyax : entity work.bx
    port map (zmokscor => jgj);
  limvdnxr : entity work.f
    port map (ispacxw => vguchfrje);
  lcvzymvqal : entity work.f
    port map (ispacxw => adxmrf);
  xmojq : entity work.f
    port map (ispacxw => eyutkd);
  
  -- Single-driven assignments
  xjcllxsgt <= (others => ' ');
  ohb <= ohb;
  
  -- Multi-driven assignments
  eyutkd <= vguchfrje;
end ecuhmui;



-- Seed after: 2694021873741348058,13501862637168280927

-- Seed: 8730320841896015106,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity lwh is
  port (trjtbdwaq : out time; yxkwjpq : in std_logic_vector(0 downto 3));
end lwh;

architecture dpc of lwh is
  
begin
  -- Single-driven assignments
  trjtbdwaq <= 3 ns;
end dpc;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (uutobo : out severity_level; l : linkage real; et : linkage std_logic_vector(3 to 2); cc : inout std_logic);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture nqtnbyw of m is
  signal uzkrlnbcc : std_logic_vector(0 downto 3);
  signal sdhtsa : time;
  signal uwxzveoe : std_logic_vector(0 downto 3);
  signal vnkowf : time;
begin
  cvyfbfn : entity work.lwh
    port map (trjtbdwaq => vnkowf, yxkwjpq => uwxzveoe);
  gfljrqry : entity work.lwh
    port map (trjtbdwaq => sdhtsa, yxkwjpq => uzkrlnbcc);
  
  -- Single-driven assignments
  uutobo <= ERROR;
  
  -- Multi-driven assignments
  uzkrlnbcc <= uwxzveoe;
  cc <= cc;
end nqtnbyw;



-- Seed after: 3888972033845719656,12269339630485015285

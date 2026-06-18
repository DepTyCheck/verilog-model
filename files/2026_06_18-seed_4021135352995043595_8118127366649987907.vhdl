-- Seed: 4021135352995043595,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (hdeefmasep : buffer real_vector(2 downto 4); nxbm : buffer integer; ygvdyp : inout real; q : linkage std_logic_vector(2 downto 0));
end c;

architecture n of c is
  
begin
  -- Single-driven assignments
  hdeefmasep <= (others => 0.0);
end n;

library ieee;
use ieee.std_logic_1164.all;

entity lmxyxtlo is
  port (mvxlhvmd : out std_logic_vector(2 to 1); aju : inout real_vector(2 downto 3));
end lmxyxtlo;

library ieee;
use ieee.std_logic_1164.all;

architecture ripjfg of lmxyxtlo is
  signal dyvhj : std_logic_vector(2 downto 0);
  signal oqevoq : real;
  signal ge : integer;
  signal sbhqeem : real_vector(2 downto 4);
  signal xzcolm : real;
  signal gylywslokr : integer;
  signal irulwm : std_logic_vector(2 downto 0);
  signal keywym : real;
  signal xcuf : integer;
  signal ktqogcu : real_vector(2 downto 4);
begin
  vjay : entity work.c
    port map (hdeefmasep => ktqogcu, nxbm => xcuf, ygvdyp => keywym, q => irulwm);
  ygj : entity work.c
    port map (hdeefmasep => aju, nxbm => gylywslokr, ygvdyp => xzcolm, q => irulwm);
  qp : entity work.c
    port map (hdeefmasep => sbhqeem, nxbm => ge, ygvdyp => oqevoq, q => dyvhj);
end ripjfg;



-- Seed after: 1738859787463567521,8118127366649987907

-- Seed: 1045133215977471347,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (uaualkp : buffer std_logic_vector(4 to 4); sfmazcgkq : out integer_vector(0 to 3));
end x;

architecture xx of x is
  
begin
  -- Single-driven assignments
  sfmazcgkq <= (16#F54#, 2#11001#, 123, 2#1_0#);
  
  -- Multi-driven assignments
  uaualkp <= (others => '1');
  uaualkp <= "-";
  uaualkp <= (others => 'W');
end xx;

library ieee;
use ieee.std_logic_1164.all;

entity oimjrgbm is
  port (ii : inout real; jccq : linkage time; a : in std_logic_vector(3 downto 3));
end oimjrgbm;

library ieee;
use ieee.std_logic_1164.all;

architecture fb of oimjrgbm is
  signal jgdrhfjk : integer_vector(0 to 3);
  signal ticdlcyv : std_logic_vector(4 to 4);
  signal qzoteamz : integer_vector(0 to 3);
  signal gzkutrl : integer_vector(0 to 3);
  signal nxrpk : std_logic_vector(4 to 4);
  signal r : integer_vector(0 to 3);
  signal kbf : std_logic_vector(4 to 4);
begin
  zgwdn : entity work.x
    port map (uaualkp => kbf, sfmazcgkq => r);
  i : entity work.x
    port map (uaualkp => nxrpk, sfmazcgkq => gzkutrl);
  nhcz : entity work.x
    port map (uaualkp => nxrpk, sfmazcgkq => qzoteamz);
  axw : entity work.x
    port map (uaualkp => ticdlcyv, sfmazcgkq => jgdrhfjk);
  
  -- Multi-driven assignments
  kbf <= (others => '0');
  ticdlcyv <= (others => '0');
end fb;

entity zryjfvxlc is
  port (caboxiec : in real);
end zryjfvxlc;

library ieee;
use ieee.std_logic_1164.all;

architecture edi of zryjfvxlc is
  signal s : std_logic_vector(3 downto 3);
  signal favrswsh : time;
  signal cthvsjcjm : real;
begin
  fzdswheeyg : entity work.oimjrgbm
    port map (ii => cthvsjcjm, jccq => favrswsh, a => s);
  
  -- Multi-driven assignments
  s <= (others => 'W');
end edi;

library ieee;
use ieee.std_logic_1164.all;

entity gdlbfibvw is
  port (nckyyz : linkage time; gmnche : out std_logic_vector(2 downto 2));
end gdlbfibvw;

architecture xkbrum of gdlbfibvw is
  
begin
  -- Multi-driven assignments
  gmnche <= "W";
  gmnche <= (others => '-');
  gmnche <= (others => '-');
  gmnche <= "H";
end xkbrum;



-- Seed after: 1498660855808614453,3687118713772291287

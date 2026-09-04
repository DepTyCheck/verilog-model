-- Seed: 2027886415827429557,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity afipnkfrg is
  port (uoqdtrns : in std_logic; hv : linkage std_logic_vector(4 downto 0));
end afipnkfrg;

architecture plmgakq of afipnkfrg is
  
begin
  
end plmgakq;

library ieee;
use ieee.std_logic_1164.all;

entity brmhgen is
  port (c : buffer std_logic; oxiyy : linkage real_vector(2 to 4); divhj : inout std_logic_vector(0 to 0); pbr : buffer std_logic);
end brmhgen;

architecture hbq of brmhgen is
  
begin
  -- Multi-driven assignments
  divhj <= divhj;
  pbr <= c;
  pbr <= '1';
end hbq;

entity tos is
  port (bzf : buffer integer; twx : linkage time; gs : linkage real);
end tos;

library ieee;
use ieee.std_logic_1164.all;

architecture ftr of tos is
  signal g : std_logic_vector(4 downto 0);
  signal cbsy : std_logic_vector(0 to 0);
  signal gg : real_vector(2 to 4);
  signal elrknbmngz : std_logic_vector(4 downto 0);
  signal qirorvjwt : std_logic;
begin
  nsxe : entity work.afipnkfrg
    port map (uoqdtrns => qirorvjwt, hv => elrknbmngz);
  swm : entity work.brmhgen
    port map (c => qirorvjwt, oxiyy => gg, divhj => cbsy, pbr => qirorvjwt);
  afmxjc : entity work.afipnkfrg
    port map (uoqdtrns => qirorvjwt, hv => g);
  
  -- Single-driven assignments
  bzf <= bzf;
  
  -- Multi-driven assignments
  cbsy <= (others => 'Z');
  g <= ('0', '-', '1', 'W', '-');
  qirorvjwt <= '-';
  qirorvjwt <= qirorvjwt;
end ftr;



-- Seed after: 1106339714197958071,4404421571376382767

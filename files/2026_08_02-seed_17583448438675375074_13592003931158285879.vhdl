-- Seed: 17583448438675375074,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity lcmktwka is
  port (rcyuqq : linkage std_logic_vector(2 to 4); gchgvwiw : out time; ttketkdweb : linkage std_logic_vector(4 downto 2));
end lcmktwka;

architecture wtgocm of lcmktwka is
  
begin
  
end wtgocm;

entity vnkzyf is
  port (etskwt : buffer real);
end vnkzyf;

library ieee;
use ieee.std_logic_1164.all;

architecture zrdwru of vnkzyf is
  signal eiabshdmdy : time;
  signal ishxfxeyft : time;
  signal bzysodjf : std_logic_vector(4 downto 2);
  signal xh : std_logic_vector(4 downto 2);
  signal avrwu : time;
  signal idcxorckb : time;
  signal zd : std_logic_vector(2 to 4);
begin
  ytqkwjb : entity work.lcmktwka
    port map (rcyuqq => zd, gchgvwiw => idcxorckb, ttketkdweb => zd);
  azqsnyxo : entity work.lcmktwka
    port map (rcyuqq => zd, gchgvwiw => avrwu, ttketkdweb => xh);
  kypeka : entity work.lcmktwka
    port map (rcyuqq => bzysodjf, gchgvwiw => ishxfxeyft, ttketkdweb => zd);
  lg : entity work.lcmktwka
    port map (rcyuqq => zd, gchgvwiw => eiabshdmdy, ttketkdweb => bzysodjf);
  
  -- Single-driven assignments
  etskwt <= 1_0_1_3_2.232;
  
  -- Multi-driven assignments
  zd <= "L-0";
  zd <= xh;
  zd <= ('H', 'X', '-');
end zrdwru;

library ieee;
use ieee.std_logic_1164.all;

entity tw is
  port (vgvkhi : inout integer; hwti : buffer std_logic_vector(2 downto 3); ok : linkage integer; hwjnjpnv : in boolean);
end tw;

architecture mndnc of tw is
  signal o : real;
  signal fnfkx : real;
  signal weackcl : real;
begin
  ahicaet : entity work.vnkzyf
    port map (etskwt => weackcl);
  bzquqouf : entity work.vnkzyf
    port map (etskwt => fnfkx);
  ec : entity work.vnkzyf
    port map (etskwt => o);
  
  -- Single-driven assignments
  vgvkhi <= 2#00011#;
  
  -- Multi-driven assignments
  hwti <= (others => '0');
end mndnc;

library ieee;
use ieee.std_logic_1164.all;

entity jnscmqnk is
  port (z : in bit; ojmjgxrh : out std_logic_vector(4 to 4); uzycc : out time);
end jnscmqnk;

architecture jcylct of jnscmqnk is
  
begin
  -- Single-driven assignments
  uzycc <= uzycc;
end jcylct;



-- Seed after: 11989504623686780988,13592003931158285879

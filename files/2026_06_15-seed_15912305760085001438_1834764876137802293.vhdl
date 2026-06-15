-- Seed: 15912305760085001438,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity rzmgrbzl is
  port (qhiki : in severity_level; ihuaplce : buffer std_logic_vector(2 downto 2); giv : out std_logic_vector(4 to 0));
end rzmgrbzl;

architecture bs of rzmgrbzl is
  
begin
  -- Multi-driven assignments
  giv <= (others => '0');
  ihuaplce <= (others => 'H');
  ihuaplce <= "0";
  giv <= (others => '0');
end bs;

library ieee;
use ieee.std_logic_1164.all;

entity box is
  port (ap : buffer std_logic_vector(2 to 4));
end box;

library ieee;
use ieee.std_logic_1164.all;

architecture nrjyi of box is
  signal uc : std_logic_vector(4 to 0);
  signal gpv : std_logic_vector(2 downto 2);
  signal ejwli : severity_level;
begin
  efyb : entity work.rzmgrbzl
    port map (qhiki => ejwli, ihuaplce => gpv, giv => uc);
  
  -- Single-driven assignments
  ejwli <= WARNING;
  
  -- Multi-driven assignments
  ap <= ('-', 'W', '0');
  ap <= ('W', '1', 'H');
  ap <= ('Z', '-', 'U');
end nrjyi;

library ieee;
use ieee.std_logic_1164.all;

entity gezkd is
  port (apipf : inout integer_vector(3 to 2); qrstecs : in std_logic_vector(2 downto 0); ak : buffer integer);
end gezkd;

library ieee;
use ieee.std_logic_1164.all;

architecture znmcie of gezkd is
  signal mprtazg : severity_level;
  signal zmzchvf : std_logic_vector(4 to 0);
  signal srlaa : std_logic_vector(2 downto 2);
  signal ujyw : severity_level;
begin
  y : entity work.rzmgrbzl
    port map (qhiki => ujyw, ihuaplce => srlaa, giv => zmzchvf);
  zmka : entity work.rzmgrbzl
    port map (qhiki => mprtazg, ihuaplce => srlaa, giv => zmzchvf);
end znmcie;



-- Seed after: 7471190113834252427,1834764876137802293

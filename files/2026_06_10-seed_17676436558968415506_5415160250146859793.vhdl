-- Seed: 17676436558968415506,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity kyleh is
  port (b : linkage real; bhwy : out std_logic_vector(3 to 1));
end kyleh;



architecture onzcuc of kyleh is
  
begin
  
end onzcuc;



entity nqelafv is
  port (ohsy : out integer);
end nqelafv;

library ieee;
use ieee.std_logic_1164.all;

architecture rvxv of nqelafv is
  signal dqc : std_logic_vector(3 to 1);
  signal rlk : real;
  signal dgflu : std_logic_vector(3 to 1);
  signal xyzbuvoo : std_logic_vector(3 to 1);
  signal siki : real;
begin
  fhgomjg : entity work.kyleh
    port map (b => siki, bhwy => xyzbuvoo);
  xmuhvj : entity work.kyleh
    port map (b => siki, bhwy => dgflu);
  stpqtwd : entity work.kyleh
    port map (b => rlk, bhwy => xyzbuvoo);
  hcurflyqo : entity work.kyleh
    port map (b => rlk, bhwy => dqc);
end rvxv;



entity lxftilvaft is
  port (kulg : linkage real_vector(1 downto 2));
end lxftilvaft;

library ieee;
use ieee.std_logic_1164.all;

architecture vcbdwawon of lxftilvaft is
  signal gbqucgqbgk : std_logic_vector(3 to 1);
  signal vgsd : real;
  signal xxu : integer;
  signal bgjbpbgptm : integer;
  signal wqojae : integer;
begin
  czfgves : entity work.nqelafv
    port map (ohsy => wqojae);
  bnqjuo : entity work.nqelafv
    port map (ohsy => bgjbpbgptm);
  hbpuq : entity work.nqelafv
    port map (ohsy => xxu);
  s : entity work.kyleh
    port map (b => vgsd, bhwy => gbqucgqbgk);
end vcbdwawon;



entity yuxnfnjh is
  port (silees : buffer severity_level);
end yuxnfnjh;

library ieee;
use ieee.std_logic_1164.all;

architecture o of yuxnfnjh is
  signal ysjtrkl : real_vector(1 downto 2);
  signal xyspi : real;
  signal bjozztoihx : std_logic_vector(3 to 1);
  signal maviqsd : real;
begin
  lcywe : entity work.kyleh
    port map (b => maviqsd, bhwy => bjozztoihx);
  dbhtgubnuh : entity work.kyleh
    port map (b => xyspi, bhwy => bjozztoihx);
  pqztve : entity work.lxftilvaft
    port map (kulg => ysjtrkl);
end o;



-- Seed after: 7286055052884789876,5415160250146859793

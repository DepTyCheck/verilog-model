-- Seed: 17702390894054459039,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity zg is
  port (zbj : buffer integer_vector(2 downto 0); sfmxajfgob : in time; ouseptzt : inout std_logic_vector(0 to 1));
end zg;

architecture ushamwxq of zg is
  
begin
  -- Multi-driven assignments
  ouseptzt <= ouseptzt;
  ouseptzt <= ('H', 'W');
end ushamwxq;

library ieee;
use ieee.std_logic_1164.all;

entity zbcz is
  port (cftdsah : buffer character; wvfltit : in std_logic);
end zbcz;

library ieee;
use ieee.std_logic_1164.all;

architecture dl of zbcz is
  signal xjqu : integer_vector(2 downto 0);
  signal bhdwus : std_logic_vector(0 to 1);
  signal uvdaszne : integer_vector(2 downto 0);
  signal nzijkt : std_logic_vector(0 to 1);
  signal iffovdrotx : time;
  signal rbchzaz : integer_vector(2 downto 0);
begin
  jyod : entity work.zg
    port map (zbj => rbchzaz, sfmxajfgob => iffovdrotx, ouseptzt => nzijkt);
  lfpgp : entity work.zg
    port map (zbj => uvdaszne, sfmxajfgob => iffovdrotx, ouseptzt => bhdwus);
  oufdhssdh : entity work.zg
    port map (zbj => xjqu, sfmxajfgob => iffovdrotx, ouseptzt => bhdwus);
  
  -- Single-driven assignments
  cftdsah <= 'o';
  iffovdrotx <= 4_2.32 us;
end dl;

entity vmeps is
  port (w : out time);
end vmeps;

library ieee;
use ieee.std_logic_1164.all;

architecture cr of vmeps is
  signal rogzbqi : std_logic;
  signal vcztzxgtuy : character;
  signal juxnjoxsg : time;
  signal wqmmlgefl : integer_vector(2 downto 0);
  signal ss : std_logic_vector(0 to 1);
  signal ihgappq : integer_vector(2 downto 0);
begin
  tmcrdj : entity work.zg
    port map (zbj => ihgappq, sfmxajfgob => w, ouseptzt => ss);
  rsmjeywoc : entity work.zg
    port map (zbj => wqmmlgefl, sfmxajfgob => juxnjoxsg, ouseptzt => ss);
  erf : entity work.zbcz
    port map (cftdsah => vcztzxgtuy, wvfltit => rogzbqi);
end cr;

entity gzdismb is
  port (y : out integer_vector(2 to 1));
end gzdismb;

architecture imqzt of gzdismb is
  
begin
  -- Single-driven assignments
  y <= (others => 0);
end imqzt;



-- Seed after: 16781074617397420104,5906004015519833893

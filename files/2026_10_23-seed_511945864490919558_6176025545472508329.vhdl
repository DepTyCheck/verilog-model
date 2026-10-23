-- Seed: 511945864490919558,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity bqps is
  port (cmtcylww : inout std_logic; kcahcfdpms : in real_vector(3 downto 4); lsvldi : inout integer);
end bqps;

architecture mqnlen of bqps is
  
begin
  -- Single-driven assignments
  lsvldi <= 3_2;
  
  -- Multi-driven assignments
  cmtcylww <= 'Z';
end mqnlen;

entity lmyzhw is
  port (ywce : linkage real; vuwcnuzdv : linkage real);
end lmyzhw;

library ieee;
use ieee.std_logic_1164.all;

architecture coej of lmyzhw is
  signal kbcduh : integer;
  signal yax : real_vector(3 downto 4);
  signal xmkwcfsotc : integer;
  signal xlhy : real_vector(3 downto 4);
  signal tkaxao : std_logic;
begin
  iyefx : entity work.bqps
    port map (cmtcylww => tkaxao, kcahcfdpms => xlhy, lsvldi => xmkwcfsotc);
  tze : entity work.bqps
    port map (cmtcylww => tkaxao, kcahcfdpms => yax, lsvldi => kbcduh);
  
  -- Single-driven assignments
  xlhy <= yax;
  yax <= yax;
end coej;



-- Seed after: 12887648799397320185,6176025545472508329

-- Seed: 697976088077539580,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity vmnzqhrtyg is
  port (ehryelujs : inout std_logic_vector(3 downto 0); pbzef : linkage std_logic);
end vmnzqhrtyg;

architecture o of vmnzqhrtyg is
  
begin
  -- Multi-driven assignments
  ehryelujs <= ehryelujs;
  ehryelujs <= ('W', 'U', 'U', 'X');
end o;

entity bieixgned is
  port (ab : inout bit_vector(4 to 3));
end bieixgned;

library ieee;
use ieee.std_logic_1164.all;

architecture dpmxyfprg of bieixgned is
  signal ccvzxbrse : std_logic_vector(3 downto 0);
  signal y : std_logic;
  signal iddrdbltpf : std_logic_vector(3 downto 0);
  signal qftuh : std_logic;
  signal kbmu : std_logic_vector(3 downto 0);
begin
  pstvzwcjr : entity work.vmnzqhrtyg
    port map (ehryelujs => kbmu, pbzef => qftuh);
  phhmtwexf : entity work.vmnzqhrtyg
    port map (ehryelujs => kbmu, pbzef => qftuh);
  wh : entity work.vmnzqhrtyg
    port map (ehryelujs => iddrdbltpf, pbzef => y);
  xcpp : entity work.vmnzqhrtyg
    port map (ehryelujs => ccvzxbrse, pbzef => y);
  
  -- Single-driven assignments
  ab <= ab;
  
  -- Multi-driven assignments
  kbmu <= kbmu;
end dpmxyfprg;

library ieee;
use ieee.std_logic_1164.all;

entity cwk is
  port (cuysp : linkage time_vector(0 to 1); ue : linkage std_logic_vector(1 to 3); dwqldzu : inout bit_vector(1 downto 4));
end cwk;

library ieee;
use ieee.std_logic_1164.all;

architecture ewr of cwk is
  signal vfkbgcslqm : std_logic;
  signal wopyyrsjj : std_logic_vector(3 downto 0);
  signal hryt : std_logic;
  signal i : std_logic_vector(3 downto 0);
begin
  nwvbb : entity work.vmnzqhrtyg
    port map (ehryelujs => i, pbzef => hryt);
  evfxxizt : entity work.bieixgned
    port map (ab => dwqldzu);
  jzlpzgzy : entity work.vmnzqhrtyg
    port map (ehryelujs => wopyyrsjj, pbzef => vfkbgcslqm);
  
  -- Multi-driven assignments
  wopyyrsjj <= i;
end ewr;

entity pwytrmm is
  port (emc : in integer);
end pwytrmm;

library ieee;
use ieee.std_logic_1164.all;

architecture hltljwb of pwytrmm is
  signal hmpkovl : std_logic;
  signal hbrzvtap : std_logic_vector(3 downto 0);
  signal qfnlobzr : bit_vector(1 downto 4);
  signal xl : std_logic_vector(1 to 3);
  signal aoaov : time_vector(0 to 1);
begin
  hleev : entity work.cwk
    port map (cuysp => aoaov, ue => xl, dwqldzu => qfnlobzr);
  fbaady : entity work.vmnzqhrtyg
    port map (ehryelujs => hbrzvtap, pbzef => hmpkovl);
  
  -- Multi-driven assignments
  xl <= xl;
  hbrzvtap <= hbrzvtap;
end hltljwb;



-- Seed after: 8493434331660163996,4245627776430562977

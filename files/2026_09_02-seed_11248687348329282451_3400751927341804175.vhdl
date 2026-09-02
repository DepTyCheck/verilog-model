-- Seed: 11248687348329282451,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (qsw : linkage real; vfiiy : in boolean_vector(2 to 0); wxdwiy : in std_logic_vector(0 to 2); z : inout std_logic_vector(4 downto 3));
end r;

architecture dmxbmh of r is
  
begin
  
end dmxbmh;

library ieee;
use ieee.std_logic_1164.all;

entity oah is
  port (c : linkage std_logic_vector(4 to 4); poe : inout std_logic_vector(3 downto 0));
end oah;

library ieee;
use ieee.std_logic_1164.all;

architecture hpknqbczcu of oah is
  signal mmusvvwmkw : std_logic_vector(0 to 2);
  signal fssmdwzpp : real;
  signal w : std_logic_vector(4 downto 3);
  signal jgfng : boolean_vector(2 to 0);
  signal luoz : real;
  signal rak : boolean_vector(2 to 0);
  signal taidtsilok : real;
  signal kjippcylj : std_logic_vector(4 downto 3);
  signal agetm : std_logic_vector(0 to 2);
  signal gzcpt : boolean_vector(2 to 0);
  signal svtmaftxzn : real;
begin
  n : entity work.r
    port map (qsw => svtmaftxzn, vfiiy => gzcpt, wxdwiy => agetm, z => kjippcylj);
  qiluyhnclj : entity work.r
    port map (qsw => taidtsilok, vfiiy => rak, wxdwiy => agetm, z => kjippcylj);
  kak : entity work.r
    port map (qsw => luoz, vfiiy => jgfng, wxdwiy => agetm, z => w);
  omqjnzniu : entity work.r
    port map (qsw => fssmdwzpp, vfiiy => jgfng, wxdwiy => mmusvvwmkw, z => kjippcylj);
  
  -- Single-driven assignments
  gzcpt <= gzcpt;
  
  -- Multi-driven assignments
  w <= kjippcylj;
end hpknqbczcu;

entity xmczoirpta is
  port (quhnrji : buffer integer; zdhraepo : inout real);
end xmczoirpta;

library ieee;
use ieee.std_logic_1164.all;

architecture iap of xmczoirpta is
  signal myryvwwyrq : std_logic_vector(4 downto 3);
  signal pghq : real;
  signal gw : std_logic_vector(4 downto 3);
  signal wajnp : boolean_vector(2 to 0);
  signal lokxwl : std_logic_vector(0 to 2);
  signal kgiapgp : real;
  signal drctsfwi : std_logic_vector(4 downto 3);
  signal tdovkocnda : std_logic_vector(0 to 2);
  signal obzi : boolean_vector(2 to 0);
  signal fkwacba : real;
begin
  pdiglcmyn : entity work.r
    port map (qsw => fkwacba, vfiiy => obzi, wxdwiy => tdovkocnda, z => drctsfwi);
  dq : entity work.r
    port map (qsw => kgiapgp, vfiiy => obzi, wxdwiy => lokxwl, z => drctsfwi);
  jfyuvj : entity work.r
    port map (qsw => zdhraepo, vfiiy => wajnp, wxdwiy => lokxwl, z => gw);
  gczwumtv : entity work.r
    port map (qsw => pghq, vfiiy => wajnp, wxdwiy => tdovkocnda, z => myryvwwyrq);
  
  -- Single-driven assignments
  quhnrji <= quhnrji;
  wajnp <= obzi;
  obzi <= (others => TRUE);
  
  -- Multi-driven assignments
  lokxwl <= tdovkocnda;
end iap;



-- Seed after: 1163242708031812469,3400751927341804175

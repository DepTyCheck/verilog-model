-- Seed: 13557585556648914716,10240345754018108067

library ieee;
use ieee.std_logic_1164.all;

entity dr is
  port (vqxbuatzm : buffer std_logic_vector(4 downto 2));
end dr;



architecture pctvsyrsj of dr is
  
begin
  
end pctvsyrsj;



entity xqcqdbw is
  port (nwdwhhzw : in time);
end xqcqdbw;

library ieee;
use ieee.std_logic_1164.all;

architecture mac of xqcqdbw is
  signal ndqw : std_logic_vector(4 downto 2);
  signal wcusaaqmx : std_logic_vector(4 downto 2);
begin
  bwjllb : entity work.dr
    port map (vqxbuatzm => wcusaaqmx);
  aisw : entity work.dr
    port map (vqxbuatzm => wcusaaqmx);
  xzsfkzyrw : entity work.dr
    port map (vqxbuatzm => ndqw);
end mac;

library ieee;
use ieee.std_logic_1164.all;

entity zlcxilct is
  port (fns : out time_vector(4 downto 2); zzoemh : linkage std_logic_vector(0 downto 1); flxbpn : out real);
end zlcxilct;

library ieee;
use ieee.std_logic_1164.all;

architecture zw of zlcxilct is
  signal x : std_logic_vector(4 downto 2);
begin
  dmfgllpe : entity work.dr
    port map (vqxbuatzm => x);
end zw;



entity bnhnhxy is
  port (kyqhuwnyx : in integer; jjxoea : buffer bit; duqabhdul : buffer character);
end bnhnhxy;

library ieee;
use ieee.std_logic_1164.all;

architecture domn of bnhnhxy is
  signal hlcs : real;
  signal bspqshczec : std_logic_vector(0 downto 1);
  signal khrqyu : time_vector(4 downto 2);
  signal u : time;
begin
  egluhaha : entity work.xqcqdbw
    port map (nwdwhhzw => u);
  fo : entity work.zlcxilct
    port map (fns => khrqyu, zzoemh => bspqshczec, flxbpn => hlcs);
end domn;



-- Seed after: 4724115228391257482,10240345754018108067

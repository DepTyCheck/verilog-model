-- Seed: 7307510844447031338,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity zuqyhu is
  port (ywgm : buffer time; tfwmakr : out std_logic_vector(2 to 3); ch : inout std_logic_vector(4 downto 2));
end zuqyhu;



architecture tnqmwl of zuqyhu is
  
begin
  
end tnqmwl;

library ieee;
use ieee.std_logic_1164.all;

entity sngdc is
  port (km : in std_logic);
end sngdc;

library ieee;
use ieee.std_logic_1164.all;

architecture tfnvuwhxsu of sngdc is
  signal jbykfr : std_logic_vector(4 downto 2);
  signal mshkhk : std_logic_vector(2 to 3);
  signal ozfdgc : time;
begin
  saassqajm : entity work.zuqyhu
    port map (ywgm => ozfdgc, tfwmakr => mshkhk, ch => jbykfr);
end tfnvuwhxsu;



entity hfu is
  port (oljbcy : in real);
end hfu;

library ieee;
use ieee.std_logic_1164.all;

architecture pzeqvb of hfu is
  signal vumnue : std_logic_vector(2 to 3);
  signal volnbdiaz : time;
  signal zrdr : std_logic_vector(2 to 3);
  signal bosshv : time;
  signal hf : std_logic_vector(4 downto 2);
  signal xvlammwp : std_logic_vector(2 to 3);
  signal uaf : time;
begin
  dlput : entity work.zuqyhu
    port map (ywgm => uaf, tfwmakr => xvlammwp, ch => hf);
  tjwjs : entity work.zuqyhu
    port map (ywgm => bosshv, tfwmakr => zrdr, ch => hf);
  wpofklg : entity work.zuqyhu
    port map (ywgm => volnbdiaz, tfwmakr => vumnue, ch => hf);
end pzeqvb;

library ieee;
use ieee.std_logic_1164.all;

entity sussuutv is
  port (zyjrjzujrh : in integer; omc : in std_logic_vector(4 to 4); ded : out character; zuqlosvnov : buffer integer);
end sussuutv;

library ieee;
use ieee.std_logic_1164.all;

architecture nhkroc of sussuutv is
  signal es : real;
  signal t : std_logic_vector(2 to 3);
  signal xnmeao : time;
  signal mwvy : std_logic_vector(4 downto 2);
  signal ztgrepk : std_logic_vector(2 to 3);
  signal zuskeaumn : time;
begin
  gei : entity work.zuqyhu
    port map (ywgm => zuskeaumn, tfwmakr => ztgrepk, ch => mwvy);
  xevjlnlw : entity work.zuqyhu
    port map (ywgm => xnmeao, tfwmakr => t, ch => mwvy);
  l : entity work.hfu
    port map (oljbcy => es);
end nhkroc;



-- Seed after: 10429945027968073574,6298042963991371649

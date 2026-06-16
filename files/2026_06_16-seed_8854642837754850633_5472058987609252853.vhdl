-- Seed: 8854642837754850633,5472058987609252853

entity yoqpqvfb is
  port (zjodwcfde : inout time);
end yoqpqvfb;

architecture dmwthlk of yoqpqvfb is
  
begin
  -- Single-driven assignments
  zjodwcfde <= 2_3_1_2 fs;
end dmwthlk;

library ieee;
use ieee.std_logic_1164.all;

entity kouvwvz is
  port (qao : out character; ulxzc : linkage time; rqhkmkg : buffer std_logic_vector(4 to 1));
end kouvwvz;

architecture xwqmku of kouvwvz is
  signal vfwrgyqpv : time;
  signal qsvf : time;
  signal jaep : time;
begin
  qswldsaxi : entity work.yoqpqvfb
    port map (zjodwcfde => jaep);
  ujwca : entity work.yoqpqvfb
    port map (zjodwcfde => qsvf);
  fyelb : entity work.yoqpqvfb
    port map (zjodwcfde => vfwrgyqpv);
  
  -- Single-driven assignments
  qao <= 'b';
end xwqmku;

library ieee;
use ieee.std_logic_1164.all;

entity qtxv is
  port (bbd : out time; k : inout real; lbkvarkefq : buffer std_logic_vector(0 downto 4); wfxuxi : out real);
end qtxv;

library ieee;
use ieee.std_logic_1164.all;

architecture hrlmjck of qtxv is
  signal yezrhrlqm : std_logic_vector(4 to 1);
  signal l : character;
  signal zvo : time;
  signal vlj : std_logic_vector(4 to 1);
  signal jqh : time;
  signal dwmj : character;
begin
  g : entity work.kouvwvz
    port map (qao => dwmj, ulxzc => jqh, rqhkmkg => vlj);
  ohvewyr : entity work.yoqpqvfb
    port map (zjodwcfde => zvo);
  ysdi : entity work.kouvwvz
    port map (qao => l, ulxzc => bbd, rqhkmkg => yezrhrlqm);
end hrlmjck;



-- Seed after: 5489197062949849844,5472058987609252853

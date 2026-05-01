-- Seed: 12024758050217158275,5124258828936664479

library ieee;
use ieee.std_logic_1164.all;

entity drouhxyun is
  port (pwuuulhnji : buffer time; aotvk : out std_logic; olhlyvyie : inout integer);
end drouhxyun;



architecture l of drouhxyun is
  
begin
  
end l;



entity fwbtmlvu is
  port (chmdv : buffer integer);
end fwbtmlvu;

library ieee;
use ieee.std_logic_1164.all;

architecture cjaqwreao of fwbtmlvu is
  signal pbzh : integer;
  signal zhomelro : std_logic;
  signal o : time;
  signal gmhwbdcob : time;
  signal aecnpr : integer;
  signal ldsaal : time;
  signal nnv : integer;
  signal fsrzasve : std_logic;
  signal e : time;
begin
  en : entity work.drouhxyun
    port map (pwuuulhnji => e, aotvk => fsrzasve, olhlyvyie => nnv);
  agwbtgnd : entity work.drouhxyun
    port map (pwuuulhnji => ldsaal, aotvk => fsrzasve, olhlyvyie => aecnpr);
  tw : entity work.drouhxyun
    port map (pwuuulhnji => gmhwbdcob, aotvk => fsrzasve, olhlyvyie => chmdv);
  vheejc : entity work.drouhxyun
    port map (pwuuulhnji => o, aotvk => zhomelro, olhlyvyie => pbzh);
end cjaqwreao;

library ieee;
use ieee.std_logic_1164.all;

entity rvws is
  port (oegwq : buffer std_logic);
end rvws;



architecture mnh of rvws is
  signal noh : integer;
  signal psukexs : time;
  signal fmj : integer;
  signal nguoxlmwoj : time;
begin
  bh : entity work.drouhxyun
    port map (pwuuulhnji => nguoxlmwoj, aotvk => oegwq, olhlyvyie => fmj);
  yf : entity work.drouhxyun
    port map (pwuuulhnji => psukexs, aotvk => oegwq, olhlyvyie => noh);
end mnh;



entity o is
  port (czgwxfxb : inout integer; ddojqt : in integer);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture ykhnhho of o is
  signal xmghnip : std_logic;
  signal wm : time;
  signal vgh : integer;
begin
  b : entity work.fwbtmlvu
    port map (chmdv => vgh);
  mclto : entity work.drouhxyun
    port map (pwuuulhnji => wm, aotvk => xmghnip, olhlyvyie => czgwxfxb);
  rearo : entity work.rvws
    port map (oegwq => xmghnip);
end ykhnhho;



-- Seed after: 11896280020360514443,5124258828936664479

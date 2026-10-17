-- Seed: 17745923476216529701,5444032936641997477

entity skoqmiep is
  port (wfgfbzovro : buffer real; hyx : buffer time);
end skoqmiep;

architecture r of skoqmiep is
  
begin
  -- Single-driven assignments
  hyx <= hyx;
  wfgfbzovro <= wfgfbzovro;
end r;

library ieee;
use ieee.std_logic_1164.all;

entity venqn is
  port (s : out time; ej : buffer time; yyhcj : inout std_logic);
end venqn;

architecture bi of venqn is
  signal zzsmsi : real;
begin
  itjqninhmw : entity work.skoqmiep
    port map (wfgfbzovro => zzsmsi, hyx => s);
  
  -- Multi-driven assignments
  yyhcj <= yyhcj;
  yyhcj <= '-';
  yyhcj <= 'X';
  yyhcj <= yyhcj;
end bi;

library ieee;
use ieee.std_logic_1164.all;

entity jwry is
  port (pjogv : buffer std_logic; lmbgde : buffer real; zewmwird : linkage real);
end jwry;

architecture aajkwbmpp of jwry is
  signal aubrzvfpwc : time;
  signal qgwahhbfy : time;
  signal jfplujuja : time;
  signal hevtkc : real;
  signal vbaltgx : time;
  signal wxja : real;
  signal viigwn : time;
  signal sb : real;
begin
  bvrexshyp : entity work.skoqmiep
    port map (wfgfbzovro => sb, hyx => viigwn);
  zcq : entity work.skoqmiep
    port map (wfgfbzovro => wxja, hyx => vbaltgx);
  zbrco : entity work.skoqmiep
    port map (wfgfbzovro => hevtkc, hyx => jfplujuja);
  glehrdlde : entity work.venqn
    port map (s => qgwahhbfy, ej => aubrzvfpwc, yyhcj => pjogv);
  
  -- Single-driven assignments
  lmbgde <= 2130.1_1;
  
  -- Multi-driven assignments
  pjogv <= pjogv;
end aajkwbmpp;

entity u is
  port (b : buffer integer);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture uk of u is
  signal clrftplqgy : time;
  signal denzsyy : real;
  signal zbj : time;
  signal yn : real;
  signal nnb : real;
  signal jvbimrc : real;
  signal hpbgntf : std_logic;
begin
  xpkjgyi : entity work.jwry
    port map (pjogv => hpbgntf, lmbgde => jvbimrc, zewmwird => nnb);
  xhtgdi : entity work.skoqmiep
    port map (wfgfbzovro => yn, hyx => zbj);
  lah : entity work.skoqmiep
    port map (wfgfbzovro => denzsyy, hyx => clrftplqgy);
  
  -- Single-driven assignments
  b <= b;
end uk;



-- Seed after: 12204167922812235929,5444032936641997477

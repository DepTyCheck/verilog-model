-- Seed: 9880491803570555268,12011142928354116943

entity gxxwsbb is
  port (kgtj : buffer string(5 downto 3); u : in real; acxl : in severity_level; qt : in time);
end gxxwsbb;

architecture zsvkasscwy of gxxwsbb is
  
begin
  -- Single-driven assignments
  kgtj <= ('g', 'b', 'c');
end zsvkasscwy;

library ieee;
use ieee.std_logic_1164.all;

entity dvchbyco is
  port (ijqffxoj : linkage character; ydcx : in std_logic_vector(1 to 0); yqkyklw : in real_vector(4 to 3));
end dvchbyco;

architecture ymdyrpa of dvchbyco is
  signal dpcptgjre : severity_level;
  signal eykoz : string(5 downto 3);
  signal njjcwzvy : severity_level;
  signal uhegudozkm : string(5 downto 3);
  signal xnlscas : severity_level;
  signal uidyupxit : real;
  signal hhoizxhq : string(5 downto 3);
  signal s : time;
  signal g : severity_level;
  signal kvrksh : real;
  signal obwq : string(5 downto 3);
begin
  stbcghgkwz : entity work.gxxwsbb
    port map (kgtj => obwq, u => kvrksh, acxl => g, qt => s);
  wgmugtjvs : entity work.gxxwsbb
    port map (kgtj => hhoizxhq, u => uidyupxit, acxl => xnlscas, qt => s);
  tgutafhb : entity work.gxxwsbb
    port map (kgtj => uhegudozkm, u => kvrksh, acxl => njjcwzvy, qt => s);
  nrrrsng : entity work.gxxwsbb
    port map (kgtj => eykoz, u => kvrksh, acxl => dpcptgjre, qt => s);
  
  -- Single-driven assignments
  g <= ERROR;
  xnlscas <= WARNING;
end ymdyrpa;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (dfegenj : inout std_logic_vector(2 downto 0));
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture uxkzl of w is
  signal mvmxpddf : character;
  signal uxkvgb : time;
  signal yzevf : severity_level;
  signal qjt : real;
  signal bchaijuoe : string(5 downto 3);
  signal mgaoogmh : real_vector(4 to 3);
  signal h : std_logic_vector(1 to 0);
  signal osyt : character;
  signal katkeneovp : time;
  signal brhywdwk : severity_level;
  signal qlybgiqfzi : real;
  signal mu : string(5 downto 3);
begin
  eejtfetg : entity work.gxxwsbb
    port map (kgtj => mu, u => qlybgiqfzi, acxl => brhywdwk, qt => katkeneovp);
  ewcqqq : entity work.dvchbyco
    port map (ijqffxoj => osyt, ydcx => h, yqkyklw => mgaoogmh);
  xzfnt : entity work.gxxwsbb
    port map (kgtj => bchaijuoe, u => qjt, acxl => yzevf, qt => uxkvgb);
  ctymers : entity work.dvchbyco
    port map (ijqffxoj => mvmxpddf, ydcx => h, yqkyklw => mgaoogmh);
  
  -- Single-driven assignments
  brhywdwk <= FAILURE;
  uxkvgb <= 0230 fs;
  qjt <= 23.13014;
  katkeneovp <= 4413 ps;
  qlybgiqfzi <= 3_3_0_4_0.2_3;
  
  -- Multi-driven assignments
  dfegenj <= ('H', '0', 'L');
end uxkzl;



-- Seed after: 3198417178938468323,12011142928354116943

-- Seed: 7085244069255442994,11460338385561364835



entity zdwpm is
  port (tburf : inout time; guyqohnu : in time; kecp : linkage integer);
end zdwpm;



architecture gsmdraa of zdwpm is
  
begin
  
end gsmdraa;



entity pevclsj is
  port (rwmmsbblgi : inout integer; xug : buffer integer; xhn : in time; qwpqr : linkage real);
end pevclsj;



architecture ycoxmgsc of pevclsj is
  signal aqg : integer;
  signal ldbhmjih : time;
  signal vwmlfov : time;
  signal xyqdrr : time;
begin
  fulbrtnoo : entity work.zdwpm
    port map (tburf => xyqdrr, guyqohnu => vwmlfov, kecp => xug);
  uq : entity work.zdwpm
    port map (tburf => vwmlfov, guyqohnu => ldbhmjih, kecp => aqg);
end ycoxmgsc;

library ieee;
use ieee.std_logic_1164.all;

entity vjqyf is
  port (fy : in time; omfmujefw : buffer bit; pxdwwwrfbe : out std_logic);
end vjqyf;



architecture mcskhzciym of vjqyf is
  signal noj : integer;
  signal vaiql : time;
  signal jwkx : time;
  signal tauns : time;
  signal jeayt : integer;
  signal whib : time;
begin
  ytb : entity work.zdwpm
    port map (tburf => whib, guyqohnu => fy, kecp => jeayt);
  vixmwdqfmw : entity work.zdwpm
    port map (tburf => tauns, guyqohnu => jwkx, kecp => jeayt);
  ocyrk : entity work.zdwpm
    port map (tburf => vaiql, guyqohnu => vaiql, kecp => noj);
end mcskhzciym;



-- Seed after: 4724549569799964322,11460338385561364835

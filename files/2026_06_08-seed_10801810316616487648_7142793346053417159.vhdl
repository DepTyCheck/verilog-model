-- Seed: 10801810316616487648,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity tz is
  port (mkqt : linkage std_logic_vector(1 downto 4); uzy : in std_logic_vector(3 downto 0); s : inout time_vector(4 downto 0));
end tz;



architecture cbs of tz is
  
begin
  
end cbs;

library ieee;
use ieee.std_logic_1164.all;

entity pplueo is
  port (tgqkjdasc : linkage std_logic_vector(2 to 1); doablxlv : linkage time);
end pplueo;

library ieee;
use ieee.std_logic_1164.all;

architecture nmidxspz of pplueo is
  signal zxanhramzq : time_vector(4 downto 0);
  signal sgti : std_logic_vector(3 downto 0);
  signal gvgoxqpk : time_vector(4 downto 0);
  signal ogjxeinvt : std_logic_vector(1 downto 4);
  signal ih : time_vector(4 downto 0);
  signal arlejtlbw : std_logic_vector(3 downto 0);
  signal nexjkgw : std_logic_vector(1 downto 4);
begin
  abyusipxg : entity work.tz
    port map (mkqt => nexjkgw, uzy => arlejtlbw, s => ih);
  bhxfjb : entity work.tz
    port map (mkqt => ogjxeinvt, uzy => arlejtlbw, s => gvgoxqpk);
  aohtcxba : entity work.tz
    port map (mkqt => tgqkjdasc, uzy => sgti, s => zxanhramzq);
end nmidxspz;



entity ccmyjvzejr is
  port (kpg : linkage integer; ejddwgyij : in integer_vector(2 downto 0); qfkcjf : inout integer);
end ccmyjvzejr;

library ieee;
use ieee.std_logic_1164.all;

architecture jyxdcv of ccmyjvzejr is
  signal sbiexoud : time_vector(4 downto 0);
  signal sc : std_logic_vector(3 downto 0);
  signal oelevy : std_logic_vector(1 downto 4);
begin
  xhlvwple : entity work.tz
    port map (mkqt => oelevy, uzy => sc, s => sbiexoud);
end jyxdcv;



entity q is
  port (pyt : buffer severity_level; dqrxgone : in integer_vector(4 downto 2));
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture kjmnx of q is
  signal pvwrgl : time_vector(4 downto 0);
  signal xdwjr : std_logic_vector(3 downto 0);
  signal aprfmfl : std_logic_vector(1 downto 4);
  signal r : integer_vector(2 downto 0);
  signal vbhloqladj : integer;
  signal qnmbxeozl : integer;
begin
  hsds : entity work.ccmyjvzejr
    port map (kpg => qnmbxeozl, ejddwgyij => dqrxgone, qfkcjf => qnmbxeozl);
  zbxryo : entity work.ccmyjvzejr
    port map (kpg => vbhloqladj, ejddwgyij => r, qfkcjf => vbhloqladj);
  teebwbnl : entity work.tz
    port map (mkqt => aprfmfl, uzy => xdwjr, s => pvwrgl);
end kjmnx;



-- Seed after: 3703040639612018611,7142793346053417159

-- Seed: 16468440850990299785,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity upfqo is
  port (fbglm : out std_logic; yvwhqyaopw : in time; nnduhbf : in time);
end upfqo;



architecture fkyppnwx of upfqo is
  
begin
  
end fkyppnwx;



entity bmhtk is
  port (gvdg : inout bit; cpm : in bit; drmccjl : in integer);
end bmhtk;

library ieee;
use ieee.std_logic_1164.all;

architecture sykfr of bmhtk is
  signal skpnpmyjl : time;
  signal u : time;
  signal swptms : std_logic;
begin
  ndyoo : entity work.upfqo
    port map (fbglm => swptms, yvwhqyaopw => u, nnduhbf => skpnpmyjl);
end sykfr;



entity ikigsbt is
  port (iohtmtyfn : inout time);
end ikigsbt;

library ieee;
use ieee.std_logic_1164.all;

architecture ru of ikigsbt is
  signal hfxuanqkbn : integer;
  signal ptzhg : bit;
  signal j : bit;
  signal fqnflwiy : time;
  signal aspp : std_logic;
begin
  khzn : entity work.upfqo
    port map (fbglm => aspp, yvwhqyaopw => fqnflwiy, nnduhbf => iohtmtyfn);
  mylwb : entity work.bmhtk
    port map (gvdg => j, cpm => ptzhg, drmccjl => hfxuanqkbn);
end ru;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (daxezx : out real; nl : inout std_logic_vector(1 to 0); gofpf : out real; jbs : in time);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture ygja of c is
  signal jahino : time;
  signal hfxtda : std_logic;
  signal zto : time;
  signal urk : integer;
  signal fkcx : bit;
  signal nizq : bit;
begin
  kaerare : entity work.bmhtk
    port map (gvdg => nizq, cpm => fkcx, drmccjl => urk);
  ozmtn : entity work.ikigsbt
    port map (iohtmtyfn => zto);
  umlngbl : entity work.upfqo
    port map (fbglm => hfxtda, yvwhqyaopw => jahino, nnduhbf => jahino);
end ygja;



-- Seed after: 10769753183027351173,16715549879197889543

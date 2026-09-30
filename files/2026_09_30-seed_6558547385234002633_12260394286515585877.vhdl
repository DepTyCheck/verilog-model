-- Seed: 6558547385234002633,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity pxhbwp is
  port (uuv : linkage boolean; gzbehg : buffer std_logic_vector(2 to 0); jqwbtm : out time);
end pxhbwp;

architecture atynz of pxhbwp is
  
begin
  
end atynz;

library ieee;
use ieee.std_logic_1164.all;

entity xxppnvld is
  port (ltcxbqqx : in time; tjgskdl : linkage std_logic_vector(0 to 2); kfh : buffer std_logic; gyv : in integer);
end xxppnvld;

library ieee;
use ieee.std_logic_1164.all;

architecture sqksxyuvh of xxppnvld is
  signal afirl : time;
  signal kcuya : boolean;
  signal jfh : time;
  signal ffet : std_logic_vector(2 to 0);
  signal emdmwl : boolean;
  signal uxndmyrb : time;
  signal i : std_logic_vector(2 to 0);
  signal pdcs : boolean;
  signal bibilxoqyq : time;
  signal efeeaay : std_logic_vector(2 to 0);
  signal bpdy : boolean;
begin
  jkuy : entity work.pxhbwp
    port map (uuv => bpdy, gzbehg => efeeaay, jqwbtm => bibilxoqyq);
  rt : entity work.pxhbwp
    port map (uuv => pdcs, gzbehg => i, jqwbtm => uxndmyrb);
  qqsy : entity work.pxhbwp
    port map (uuv => emdmwl, gzbehg => ffet, jqwbtm => jfh);
  olvzdgbyk : entity work.pxhbwp
    port map (uuv => kcuya, gzbehg => ffet, jqwbtm => afirl);
  
  -- Multi-driven assignments
  ffet <= "";
  ffet <= efeeaay;
  kfh <= kfh;
end sqksxyuvh;

entity t is
  port (jvxguskt : buffer real; pgdiw : linkage integer_vector(0 to 1));
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture ui of t is
  signal eoldag : time;
  signal jnvwf : std_logic_vector(2 to 0);
  signal otgtrsinx : boolean;
  signal hkdcmdj : time;
  signal f : boolean;
  signal k : time;
  signal bpfjeva : std_logic_vector(2 to 0);
  signal ozwmmoiel : boolean;
begin
  xtdarnzbdl : entity work.pxhbwp
    port map (uuv => ozwmmoiel, gzbehg => bpfjeva, jqwbtm => k);
  m : entity work.pxhbwp
    port map (uuv => f, gzbehg => bpfjeva, jqwbtm => hkdcmdj);
  bbsjtvjcrv : entity work.pxhbwp
    port map (uuv => otgtrsinx, gzbehg => jnvwf, jqwbtm => eoldag);
  
  -- Single-driven assignments
  jvxguskt <= 3_4_4_3_0.12;
  
  -- Multi-driven assignments
  bpfjeva <= "";
  bpfjeva <= "";
end ui;



-- Seed after: 6831002428667110999,12260394286515585877

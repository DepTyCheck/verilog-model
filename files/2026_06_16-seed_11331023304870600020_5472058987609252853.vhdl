-- Seed: 11331023304870600020,5472058987609252853

entity ydbhbc is
  port (ntwmpso : linkage severity_level);
end ydbhbc;

architecture tfhhko of ydbhbc is
  
begin
  
end tfhhko;

library ieee;
use ieee.std_logic_1164.all;

entity kqjljar is
  port (ji : out real; yn : inout std_logic; a : out time);
end kqjljar;

architecture e of kqjljar is
  signal hqsta : severity_level;
  signal icetazn : severity_level;
  signal jkbnu : severity_level;
begin
  k : entity work.ydbhbc
    port map (ntwmpso => jkbnu);
  sd : entity work.ydbhbc
    port map (ntwmpso => icetazn);
  wpmxyetge : entity work.ydbhbc
    port map (ntwmpso => hqsta);
  
  -- Multi-driven assignments
  yn <= '1';
  yn <= '0';
  yn <= 'W';
  yn <= '1';
end e;

entity otjk is
  port (dsgq : buffer severity_level; fkpztvpfq : buffer time; et : out time; maxgz : in integer);
end otjk;

library ieee;
use ieee.std_logic_1164.all;

architecture dq of otjk is
  signal zps : severity_level;
  signal nz : time;
  signal t : std_logic;
  signal gh : real;
begin
  hdnyd : entity work.ydbhbc
    port map (ntwmpso => dsgq);
  kqbducodug : entity work.kqjljar
    port map (ji => gh, yn => t, a => nz);
  gujageb : entity work.ydbhbc
    port map (ntwmpso => zps);
  
  -- Single-driven assignments
  fkpztvpfq <= 1 hr;
  et <= 3 min;
  
  -- Multi-driven assignments
  t <= 'H';
end dq;



-- Seed after: 6592456923878286407,5472058987609252853

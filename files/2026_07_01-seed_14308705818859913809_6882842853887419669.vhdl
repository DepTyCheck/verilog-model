-- Seed: 14308705818859913809,6882842853887419669

entity ujdaxuqi is
  port (i : buffer time; xoatcacrqo : buffer real);
end ujdaxuqi;

architecture iol of ujdaxuqi is
  
begin
  -- Single-driven assignments
  xoatcacrqo <= 2#0_1_0_1_1.0_0_1_1#;
  i <= 0_3_1_4_0.2 fs;
end iol;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (jowp : linkage real; ghlwrhcs : inout std_logic; yamyek : buffer bit; d : out std_logic_vector(0 to 3));
end v;

architecture ife of v is
  signal divcyyuzzm : real;
  signal ytvgt : time;
  signal bgy : real;
  signal ezbiplpno : time;
  signal bsivaozt : real;
  signal vvmke : time;
  signal ljnl : real;
  signal lscffcwjl : time;
begin
  qatpt : entity work.ujdaxuqi
    port map (i => lscffcwjl, xoatcacrqo => ljnl);
  wfsengrltm : entity work.ujdaxuqi
    port map (i => vvmke, xoatcacrqo => bsivaozt);
  pdbkocvnq : entity work.ujdaxuqi
    port map (i => ezbiplpno, xoatcacrqo => bgy);
  pmlcs : entity work.ujdaxuqi
    port map (i => ytvgt, xoatcacrqo => divcyyuzzm);
  
  -- Single-driven assignments
  yamyek <= '1';
end ife;



-- Seed after: 2881237921748861529,6882842853887419669

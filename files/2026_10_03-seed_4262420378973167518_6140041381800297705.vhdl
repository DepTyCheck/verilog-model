-- Seed: 4262420378973167518,6140041381800297705

entity tmsmgmnu is
  port (ttpbfmyl : linkage severity_level; gkmq : out time);
end tmsmgmnu;

architecture pfe of tmsmgmnu is
  
begin
  -- Single-driven assignments
  gkmq <= gkmq;
end pfe;

entity qxhszent is
  port (u : in boolean; vzbpffpml : out real; qdqjghnc : inout bit_vector(4 downto 0));
end qxhszent;

architecture n of qxhszent is
  signal kvi : time;
  signal delkhqlat : severity_level;
  signal vpidmpczgj : time;
  signal gtzdmcvm : severity_level;
begin
  jl : entity work.tmsmgmnu
    port map (ttpbfmyl => gtzdmcvm, gkmq => vpidmpczgj);
  kexyxbx : entity work.tmsmgmnu
    port map (ttpbfmyl => delkhqlat, gkmq => kvi);
  
  -- Single-driven assignments
  qdqjghnc <= qdqjghnc;
  vzbpffpml <= vzbpffpml;
end n;

library ieee;
use ieee.std_logic_1164.all;

entity omsg is
  port (blucewdzw : out std_logic; ffvxjmz : in real);
end omsg;

architecture rkrvnlnws of omsg is
  signal qtpnafrlza : time;
  signal q : severity_level;
  signal kadsflp : time;
  signal lyngltloc : severity_level;
  signal tuzrt : bit_vector(4 downto 0);
  signal lcicxvbsox : real;
  signal gqanmjaza : boolean;
  signal yuerh : time;
  signal faru : severity_level;
begin
  u : entity work.tmsmgmnu
    port map (ttpbfmyl => faru, gkmq => yuerh);
  wecsyp : entity work.qxhszent
    port map (u => gqanmjaza, vzbpffpml => lcicxvbsox, qdqjghnc => tuzrt);
  pgmegqim : entity work.tmsmgmnu
    port map (ttpbfmyl => lyngltloc, gkmq => kadsflp);
  ic : entity work.tmsmgmnu
    port map (ttpbfmyl => q, gkmq => qtpnafrlza);
  
  -- Single-driven assignments
  gqanmjaza <= FALSE;
  
  -- Multi-driven assignments
  blucewdzw <= blucewdzw;
  blucewdzw <= blucewdzw;
  blucewdzw <= blucewdzw;
end rkrvnlnws;



-- Seed after: 9935563456881717218,6140041381800297705

-- Seed: 6628589411641765885,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity vkvrlzn is
  port (pcut : out boolean_vector(4 to 4); m : in std_logic_vector(3 to 1); ryfwwj : inout time; d : inout std_logic_vector(4 to 3));
end vkvrlzn;

architecture z of vkvrlzn is
  
begin
  -- Single-driven assignments
  ryfwwj <= 3_2_0_2 fs;
  pcut <= pcut;
  
  -- Multi-driven assignments
  d <= d;
  d <= (others => '0');
  d <= d;
end z;

entity sobz is
  port (ptivyasy : linkage integer);
end sobz;

library ieee;
use ieee.std_logic_1164.all;

architecture prs of sobz is
  signal gdlfdufoqp : std_logic_vector(4 to 3);
  signal oqqhhsoyw : time;
  signal gsg : std_logic_vector(3 to 1);
  signal nurzse : boolean_vector(4 to 4);
  signal ybhprt : time;
  signal jslgx : boolean_vector(4 to 4);
  signal rwcvj : std_logic_vector(4 to 3);
  signal bar : time;
  signal w : std_logic_vector(4 to 3);
  signal kh : boolean_vector(4 to 4);
begin
  t : entity work.vkvrlzn
    port map (pcut => kh, m => w, ryfwwj => bar, d => rwcvj);
  z : entity work.vkvrlzn
    port map (pcut => jslgx, m => w, ryfwwj => ybhprt, d => w);
  tzfexaa : entity work.vkvrlzn
    port map (pcut => nurzse, m => gsg, ryfwwj => oqqhhsoyw, d => gdlfdufoqp);
  
  -- Multi-driven assignments
  gsg <= gdlfdufoqp;
end prs;



-- Seed after: 4020117508881369125,6562834012285671529

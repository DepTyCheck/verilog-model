-- Seed: 3562548417866360871,14641901754878719179

entity bse is
  port (tie : in time; bizjplxa : inout integer);
end bse;

architecture ksrhw of bse is
  
begin
  
end ksrhw;

library ieee;
use ieee.std_logic_1164.all;

entity yxnl is
  port (rilz : inout std_logic; rlv : buffer std_logic);
end yxnl;

architecture wqgaoyoms of yxnl is
  signal bcjlaasf : integer;
  signal uo : time;
  signal vf : integer;
  signal fogdh : integer;
  signal xkxby : integer;
  signal gofabew : time;
begin
  kevpmx : entity work.bse
    port map (tie => gofabew, bizjplxa => xkxby);
  podxe : entity work.bse
    port map (tie => gofabew, bizjplxa => fogdh);
  akvxoql : entity work.bse
    port map (tie => gofabew, bizjplxa => vf);
  fpkusu : entity work.bse
    port map (tie => uo, bizjplxa => bcjlaasf);
  
  -- Single-driven assignments
  gofabew <= 1 sec;
  uo <= gofabew;
  
  -- Multi-driven assignments
  rlv <= 'W';
  rlv <= rlv;
  rlv <= '-';
  rlv <= 'U';
end wqgaoyoms;

entity gkmnq is
  port (k : inout time);
end gkmnq;

library ieee;
use ieee.std_logic_1164.all;

architecture gftgywoy of gkmnq is
  signal omsxoebxp : std_logic;
  signal kxwbtu : integer;
  signal apxc : time;
begin
  gtqurl : entity work.bse
    port map (tie => apxc, bizjplxa => kxwbtu);
  hvc : entity work.yxnl
    port map (rilz => omsxoebxp, rlv => omsxoebxp);
  
  -- Single-driven assignments
  k <= 16#1# us;
  apxc <= 0 hr;
  
  -- Multi-driven assignments
  omsxoebxp <= omsxoebxp;
end gftgywoy;



-- Seed after: 9180762193740624742,14641901754878719179

-- Seed: 16537220359727548208,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity mmo is
  port (dnfegvzjob : inout std_logic_vector(0 to 3); dxxvngyr : in real);
end mmo;

architecture yiveadx of mmo is
  
begin
  -- Multi-driven assignments
  dnfegvzjob <= ('H', '0', 'X', '-');
  dnfegvzjob <= dnfegvzjob;
end yiveadx;

entity uyfayaosf is
  port (nfcuuwgxrp : out string(1 to 1));
end uyfayaosf;

library ieee;
use ieee.std_logic_1164.all;

architecture vzjh of uyfayaosf is
  signal swnh : real;
  signal tqzb : real;
  signal y : real;
  signal ccnaqnc : std_logic_vector(0 to 3);
begin
  oyu : entity work.mmo
    port map (dnfegvzjob => ccnaqnc, dxxvngyr => y);
  rmfnolzdwf : entity work.mmo
    port map (dnfegvzjob => ccnaqnc, dxxvngyr => tqzb);
  bgyurvllsl : entity work.mmo
    port map (dnfegvzjob => ccnaqnc, dxxvngyr => tqzb);
  rncpsldv : entity work.mmo
    port map (dnfegvzjob => ccnaqnc, dxxvngyr => swnh);
  
  -- Single-driven assignments
  nfcuuwgxrp <= (others => 'p');
  swnh <= y;
  tqzb <= 3_3_0.12112;
  y <= 21314.01302;
  
  -- Multi-driven assignments
  ccnaqnc <= ccnaqnc;
end vzjh;

library ieee;
use ieee.std_logic_1164.all;

entity mzzcrk is
  port (pfvyvstj : in severity_level; cdalkad : linkage std_logic; b : linkage time; kznurp : linkage real);
end mzzcrk;

architecture zhegs of mzzcrk is
  signal yrzeoj : string(1 to 1);
begin
  ajfukcx : entity work.uyfayaosf
    port map (nfcuuwgxrp => yrzeoj);
end zhegs;

library ieee;
use ieee.std_logic_1164.all;

entity llqd is
  port (jupiugjh : linkage std_logic_vector(3 downto 4); jgiemmqzo : out std_logic_vector(3 to 3));
end llqd;

library ieee;
use ieee.std_logic_1164.all;

architecture l of llqd is
  signal xmevpwbw : std_logic_vector(0 to 3);
  signal oyej : real;
  signal giwmxogmk : std_logic_vector(0 to 3);
  signal xjxnfnzoxb : real;
  signal h : time;
  signal fuksbv : std_logic;
  signal bl : severity_level;
begin
  zrvy : entity work.mzzcrk
    port map (pfvyvstj => bl, cdalkad => fuksbv, b => h, kznurp => xjxnfnzoxb);
  ic : entity work.mmo
    port map (dnfegvzjob => giwmxogmk, dxxvngyr => oyej);
  jevpbqsm : entity work.mmo
    port map (dnfegvzjob => giwmxogmk, dxxvngyr => xjxnfnzoxb);
  sinb : entity work.mmo
    port map (dnfegvzjob => xmevpwbw, dxxvngyr => oyej);
  
  -- Single-driven assignments
  bl <= ERROR;
  oyej <= xjxnfnzoxb;
  
  -- Multi-driven assignments
  fuksbv <= fuksbv;
  giwmxogmk <= giwmxogmk;
  fuksbv <= fuksbv;
  fuksbv <= 'U';
end l;



-- Seed after: 6893834946040826008,8068158652091157513

-- Seed: 972774686038738895,16461708287571398341

entity ihlmcb is
  port (aiu : buffer time_vector(0 to 0); gfeo : inout real);
end ihlmcb;

architecture h of ihlmcb is
  
begin
  
end h;

entity tage is
  port (mctgauoh : buffer severity_level; hvuycg : buffer real; qmwxcd : inout time);
end tage;

architecture opwdivmbfg of tage is
  signal bbrw : real;
  signal u : time_vector(0 to 0);
  signal ivffbfftyw : real;
  signal ebisaj : time_vector(0 to 0);
  signal foojigmvlu : real;
  signal ozi : time_vector(0 to 0);
begin
  unk : entity work.ihlmcb
    port map (aiu => ozi, gfeo => foojigmvlu);
  gci : entity work.ihlmcb
    port map (aiu => ebisaj, gfeo => ivffbfftyw);
  ow : entity work.ihlmcb
    port map (aiu => u, gfeo => bbrw);
  
  -- Single-driven assignments
  mctgauoh <= FAILURE;
end opwdivmbfg;

library ieee;
use ieee.std_logic_1164.all;

entity gtb is
  port (octrfrcrjb : buffer std_logic; gedpsrm : linkage integer);
end gtb;

architecture iwpjzy of gtb is
  signal klxzvuked : time;
  signal xwovpwdo : real;
  signal jntwsvewjs : severity_level;
  signal mlxll : real;
  signal ilfoyjdhqx : time_vector(0 to 0);
  signal uttg : time;
  signal sbib : real;
  signal zlxyxrnn : severity_level;
begin
  gzq : entity work.tage
    port map (mctgauoh => zlxyxrnn, hvuycg => sbib, qmwxcd => uttg);
  nnkuh : entity work.ihlmcb
    port map (aiu => ilfoyjdhqx, gfeo => mlxll);
  j : entity work.tage
    port map (mctgauoh => jntwsvewjs, hvuycg => xwovpwdo, qmwxcd => klxzvuked);
end iwpjzy;

library ieee;
use ieee.std_logic_1164.all;

entity ikedpapyq is
  port (wpzsgyder : in severity_level; j : inout real; vdkggccwd : inout bit_vector(2 downto 2); wlwhsvwu : out std_logic_vector(4 to 4));
end ikedpapyq;

architecture maufvfl of ikedpapyq is
  signal fkj : real;
  signal y : time_vector(0 to 0);
  signal acenoapykv : time;
  signal eejzuz : real;
  signal qzarfrv : severity_level;
  signal vppr : time_vector(0 to 0);
  signal kxjdbl : real;
  signal zvblko : time_vector(0 to 0);
begin
  xc : entity work.ihlmcb
    port map (aiu => zvblko, gfeo => kxjdbl);
  eusa : entity work.ihlmcb
    port map (aiu => vppr, gfeo => j);
  fty : entity work.tage
    port map (mctgauoh => qzarfrv, hvuycg => eejzuz, qmwxcd => acenoapykv);
  qxqaeg : entity work.ihlmcb
    port map (aiu => y, gfeo => fkj);
  
  -- Single-driven assignments
  vdkggccwd <= vdkggccwd;
  
  -- Multi-driven assignments
  wlwhsvwu <= wlwhsvwu;
  wlwhsvwu <= wlwhsvwu;
end maufvfl;



-- Seed after: 1662449717047847205,16461708287571398341

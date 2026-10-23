-- Seed: 16509719696174709867,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity ww is
  port (yzqkp : linkage std_logic; akropnltww : out real);
end ww;

architecture qo of ww is
  
begin
  -- Single-driven assignments
  akropnltww <= 2#0001.01#;
end qo;

entity eaqupxxpa is
  port (xjffuk : linkage real; edjheygm : linkage integer; wdkpjxyumq : linkage real);
end eaqupxxpa;

library ieee;
use ieee.std_logic_1164.all;

architecture vimupr of eaqupxxpa is
  signal okll : real;
  signal uvsyysmgrl : real;
  signal fnonf : std_logic;
begin
  fmuhbycevq : entity work.ww
    port map (yzqkp => fnonf, akropnltww => uvsyysmgrl);
  xegjbgumkd : entity work.ww
    port map (yzqkp => fnonf, akropnltww => okll);
end vimupr;

library ieee;
use ieee.std_logic_1164.all;

entity olsirkd is
  port (n : in std_logic_vector(0 downto 1); xlxdhboviq : linkage integer);
end olsirkd;

library ieee;
use ieee.std_logic_1164.all;

architecture vbl of olsirkd is
  signal pxdnf : real;
  signal o : std_logic;
  signal sckmiu : real;
  signal hdrdodaok : integer;
  signal xnh : real;
begin
  iaqcdhhqcb : entity work.eaqupxxpa
    port map (xjffuk => xnh, edjheygm => hdrdodaok, wdkpjxyumq => sckmiu);
  rvjumcbiu : entity work.ww
    port map (yzqkp => o, akropnltww => pxdnf);
  
  -- Multi-driven assignments
  o <= 'U';
  o <= '-';
  o <= 'W';
  o <= o;
end vbl;



-- Seed after: 17449026611257672385,6176025545472508329

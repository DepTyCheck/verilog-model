-- Seed: 15270849081885365815,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity wjxmnztw is
  port (hizgmmzi : linkage std_logic_vector(3 to 1); rnzokk : linkage time; beuwfg : linkage std_logic; onil : linkage real);
end wjxmnztw;

architecture ezmguqj of wjxmnztw is
  
begin
  
end ezmguqj;

entity sddw is
  port (zvaz : in time);
end sddw;

library ieee;
use ieee.std_logic_1164.all;

architecture bqmphxuake of sddw is
  signal rncskxov : real;
  signal cvw : std_logic;
  signal ucajb : time;
  signal tpiaraqase : std_logic_vector(3 to 1);
  signal nzipkft : real;
  signal oydyctq : time;
  signal w : real;
  signal gqp : std_logic;
  signal ccawbye : time;
  signal cenjuf : std_logic_vector(3 to 1);
begin
  lvirfug : entity work.wjxmnztw
    port map (hizgmmzi => cenjuf, rnzokk => ccawbye, beuwfg => gqp, onil => w);
  ovhsiimcx : entity work.wjxmnztw
    port map (hizgmmzi => cenjuf, rnzokk => oydyctq, beuwfg => gqp, onil => nzipkft);
  pkyrrvywk : entity work.wjxmnztw
    port map (hizgmmzi => tpiaraqase, rnzokk => ucajb, beuwfg => cvw, onil => rncskxov);
  
  -- Multi-driven assignments
  gqp <= gqp;
  cenjuf <= "";
  cenjuf <= cenjuf;
  cvw <= '1';
end bqmphxuake;

entity aq is
  port (vquqy : out time; xd : inout integer; p : inout time);
end aq;

library ieee;
use ieee.std_logic_1164.all;

architecture piqpy of aq is
  signal bft : real;
  signal kzwerfdqf : std_logic;
  signal nhcdlkfh : time;
  signal kot : std_logic_vector(3 to 1);
  signal fg : time;
begin
  ivskjqxdz : entity work.sddw
    port map (zvaz => fg);
  ok : entity work.sddw
    port map (zvaz => p);
  civswsipir : entity work.wjxmnztw
    port map (hizgmmzi => kot, rnzokk => nhcdlkfh, beuwfg => kzwerfdqf, onil => bft);
  
  -- Single-driven assignments
  p <= vquqy;
  fg <= p;
  xd <= xd;
  vquqy <= 16#F_9_6_2.B# ps;
  
  -- Multi-driven assignments
  kot <= kot;
  kzwerfdqf <= kzwerfdqf;
  kot <= kot;
end piqpy;



-- Seed after: 14778555795720183310,4122021602305298647

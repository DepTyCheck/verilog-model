-- Seed: 1117089527266897529,16716360695494742805



entity rkqlx is
  port (sn : in time; lzzeebmifi : linkage time; m : in real; aetrx : out integer);
end rkqlx;



architecture sgijzkt of rkqlx is
  
begin
  
end sgijzkt;



entity z is
  port (mrptiyubi : inout integer);
end z;



architecture tmqijzjq of z is
  signal gfwqrwa : integer;
  signal qztnea : time;
  signal xmv : integer;
  signal hpuc : real;
  signal hg : time;
  signal fejqa : integer;
  signal lovp : real;
  signal lezx : time;
  signal ksdlfxktn : time;
begin
  olqw : entity work.rkqlx
    port map (sn => ksdlfxktn, lzzeebmifi => lezx, m => lovp, aetrx => fejqa);
  togqabvirf : entity work.rkqlx
    port map (sn => ksdlfxktn, lzzeebmifi => hg, m => hpuc, aetrx => xmv);
  zmpms : entity work.rkqlx
    port map (sn => hg, lzzeebmifi => qztnea, m => lovp, aetrx => gfwqrwa);
  ff : entity work.rkqlx
    port map (sn => ksdlfxktn, lzzeebmifi => qztnea, m => lovp, aetrx => mrptiyubi);
end tmqijzjq;

library ieee;
use ieee.std_logic_1164.all;

entity buyq is
  port (zpzwlhce : out std_logic);
end buyq;



architecture lvcr of buyq is
  signal nyrvfh : integer;
  signal heskejf : real;
  signal fvmcaafjur : time;
begin
  sttr : entity work.rkqlx
    port map (sn => fvmcaafjur, lzzeebmifi => fvmcaafjur, m => heskejf, aetrx => nyrvfh);
end lvcr;

library ieee;
use ieee.std_logic_1164.all;

entity hkzjcr is
  port (hr : in std_logic; dwlukhp : inout time; kic : in integer; mxwpn : inout time);
end hkzjcr;

library ieee;
use ieee.std_logic_1164.all;

architecture dwzlaxivm of hkzjcr is
  signal x : integer;
  signal dvm : real;
  signal qqhipixs : std_logic;
begin
  j : entity work.buyq
    port map (zpzwlhce => qqhipixs);
  fczghmrl : entity work.rkqlx
    port map (sn => mxwpn, lzzeebmifi => mxwpn, m => dvm, aetrx => x);
end dwzlaxivm;



-- Seed after: 13498670499861814251,16716360695494742805

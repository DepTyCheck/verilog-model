-- Seed: 14129053574083580718,4860866131898729603

entity eqzvsg is
  port (urfxrr : inout integer);
end eqzvsg;

architecture cmut of eqzvsg is
  
begin
  -- Single-driven assignments
  urfxrr <= 434;
end cmut;

entity uiohwtx is
  port (r : inout integer; qrieayfjxi : buffer time; lpjubqw : in integer; nmfbdpaxhn : out real);
end uiohwtx;

architecture l of uiohwtx is
  signal ofhzmwgj : integer;
  signal ymqjwpfkgp : integer;
begin
  nbdajs : entity work.eqzvsg
    port map (urfxrr => r);
  oqoro : entity work.eqzvsg
    port map (urfxrr => ymqjwpfkgp);
  bn : entity work.eqzvsg
    port map (urfxrr => ofhzmwgj);
  
  -- Single-driven assignments
  nmfbdpaxhn <= 16#D51.460#;
  qrieayfjxi <= 2#10011.11# ms;
end l;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (yb : inout real_vector(1 to 1); hpk : inout integer; btjou : linkage std_logic_vector(3 to 2); kbjim : linkage real);
end q;

architecture pomgjpxb of q is
  signal ztxyb : integer;
begin
  qstqvnscc : entity work.eqzvsg
    port map (urfxrr => hpk);
  vp : entity work.eqzvsg
    port map (urfxrr => ztxyb);
end pomgjpxb;

library ieee;
use ieee.std_logic_1164.all;

entity cxujw is
  port (afbtoad : in std_logic_vector(4 to 3); e : in boolean; hh : inout std_logic);
end cxujw;

architecture z of cxujw is
  signal tzcfuw : real;
  signal pzflrloxez : time;
  signal vowqtavsf : integer;
  signal bojjcszr : integer;
  signal hik : real;
  signal mzgzok : integer;
  signal x : real_vector(1 to 1);
begin
  bx : entity work.q
    port map (yb => x, hpk => mzgzok, btjou => afbtoad, kbjim => hik);
  ohmtr : entity work.eqzvsg
    port map (urfxrr => bojjcszr);
  yykzqhrr : entity work.uiohwtx
    port map (r => vowqtavsf, qrieayfjxi => pzflrloxez, lpjubqw => mzgzok, nmfbdpaxhn => tzcfuw);
end z;



-- Seed after: 6990819715635788501,4860866131898729603

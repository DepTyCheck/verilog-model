-- Seed: 15569279689810434032,5362988889992816031



entity wblc is
  port (axtbzdsay : linkage integer; rozrinukw : linkage integer; mbdcx : linkage time; ejqiwz : out bit);
end wblc;



architecture dwskoaug of wblc is
  
begin
  
end dwskoaug;



entity gompeuizgq is
  port (d : in real; kazlrybfb : inout real);
end gompeuizgq;



architecture nt of gompeuizgq is
  signal tu : bit;
  signal xbqfjhr : time;
  signal mccqrqpn : integer;
  signal ymvfegnb : bit;
  signal wlvycngkp : time;
  signal thtecdqk : bit;
  signal q : time;
  signal gbxjyq : integer;
  signal wgjtqv : integer;
begin
  dlacyefsqa : entity work.wblc
    port map (axtbzdsay => wgjtqv, rozrinukw => gbxjyq, mbdcx => q, ejqiwz => thtecdqk);
  v : entity work.wblc
    port map (axtbzdsay => gbxjyq, rozrinukw => wgjtqv, mbdcx => wlvycngkp, ejqiwz => ymvfegnb);
  xyxvtjiv : entity work.wblc
    port map (axtbzdsay => mccqrqpn, rozrinukw => wgjtqv, mbdcx => xbqfjhr, ejqiwz => tu);
end nt;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (me : inout std_logic);
end r;



architecture krnwk of r is
  signal lynywuqk : bit;
  signal qmre : time;
  signal cuw : integer;
  signal cxcd : integer;
begin
  ygphuxbx : entity work.wblc
    port map (axtbzdsay => cxcd, rozrinukw => cuw, mbdcx => qmre, ejqiwz => lynywuqk);
end krnwk;



-- Seed after: 13306623384994467025,5362988889992816031

-- Seed: 5823790770148402096,18424117564733761959



entity qcxgiikw is
  port (reyvwwwjno : in time; tyrq : buffer time);
end qcxgiikw;



architecture hbcxcqxpx of qcxgiikw is
  
begin
  
end hbcxcqxpx;



entity dwxgmyydy is
  port (qiohngbrb : buffer integer; zlts : linkage integer);
end dwxgmyydy;



architecture u of dwxgmyydy is
  signal vzptpcba : time;
  signal wozlfuo : time;
  signal afibqtq : time;
  signal pwnelh : time;
begin
  sa : entity work.qcxgiikw
    port map (reyvwwwjno => pwnelh, tyrq => afibqtq);
  rlnda : entity work.qcxgiikw
    port map (reyvwwwjno => wozlfuo, tyrq => vzptpcba);
  rbqo : entity work.qcxgiikw
    port map (reyvwwwjno => pwnelh, tyrq => pwnelh);
  ndvgbcycf : entity work.qcxgiikw
    port map (reyvwwwjno => afibqtq, tyrq => wozlfuo);
end u;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (fuci : out std_logic; pivm : inout bit);
end r;



architecture ilswsc of r is
  signal a : integer;
  signal z : integer;
  signal cxwynfk : time;
  signal mhofbplv : time;
  signal bfumlx : time;
  signal fg : time;
begin
  rjbzc : entity work.qcxgiikw
    port map (reyvwwwjno => fg, tyrq => fg);
  o : entity work.qcxgiikw
    port map (reyvwwwjno => bfumlx, tyrq => bfumlx);
  cbxgqeux : entity work.qcxgiikw
    port map (reyvwwwjno => mhofbplv, tyrq => cxwynfk);
  zn : entity work.dwxgmyydy
    port map (qiohngbrb => z, zlts => a);
end ilswsc;



entity aqzpzm is
  port (qganyctr : buffer time; ncgqtlztdx : out real);
end aqzpzm;

library ieee;
use ieee.std_logic_1164.all;

architecture wnjf of aqzpzm is
  signal o : bit;
  signal rhribo : integer;
  signal eedmvuzc : time;
  signal ucrenrhff : bit;
  signal s : std_logic;
begin
  rign : entity work.r
    port map (fuci => s, pivm => ucrenrhff);
  vtwwpv : entity work.qcxgiikw
    port map (reyvwwwjno => qganyctr, tyrq => eedmvuzc);
  jjkvqp : entity work.dwxgmyydy
    port map (qiohngbrb => rhribo, zlts => rhribo);
  dymo : entity work.r
    port map (fuci => s, pivm => o);
end wnjf;



-- Seed after: 9850613999685808642,18424117564733761959

-- Seed: 2600349967819457911,18424117564733761959

library ieee;
use ieee.std_logic_1164.all;

entity cnfid is
  port (dmb : inout real; pkxy : linkage std_logic; mymmtpvff : in real; yw : out time);
end cnfid;



architecture oetopnyxxk of cnfid is
  
begin
  
end oetopnyxxk;

library ieee;
use ieee.std_logic_1164.all;

entity op is
  port (qqmagkvz : in real; hwsckkvoxl : out time; y : inout std_logic; lwoxyfypn : inout real);
end op;



architecture cc of op is
  signal gsfqpijt : real;
  signal mju : real;
begin
  qsoypdh : entity work.cnfid
    port map (dmb => mju, pkxy => y, mymmtpvff => gsfqpijt, yw => hwsckkvoxl);
end cc;

library ieee;
use ieee.std_logic_1164.all;

entity pxsk is
  port (recvfp : out bit; t : inout std_logic);
end pxsk;

library ieee;
use ieee.std_logic_1164.all;

architecture xjuojmq of pxsk is
  signal i : real;
  signal oukozxxz : time;
  signal cbjegfv : real;
  signal bwuq : time;
  signal eanflxbz : real;
  signal vqlnw : std_logic;
  signal ivhvjszx : time;
  signal gjvrtw : real;
begin
  apuug : entity work.op
    port map (qqmagkvz => gjvrtw, hwsckkvoxl => ivhvjszx, y => vqlnw, lwoxyfypn => eanflxbz);
  sem : entity work.cnfid
    port map (dmb => gjvrtw, pkxy => t, mymmtpvff => gjvrtw, yw => bwuq);
  ztgdqsv : entity work.op
    port map (qqmagkvz => cbjegfv, hwsckkvoxl => oukozxxz, y => t, lwoxyfypn => i);
end xjuojmq;



-- Seed after: 16011382847945167151,18424117564733761959

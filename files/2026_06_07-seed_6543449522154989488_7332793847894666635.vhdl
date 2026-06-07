-- Seed: 6543449522154989488,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity aavt is
  port (oarsn : out std_logic; ebwdnoik : out integer);
end aavt;



architecture aw of aavt is
  
begin
  
end aw;

library ieee;
use ieee.std_logic_1164.all;

entity rafsnk is
  port (jdpaahvvn : out real; ums : inout std_logic);
end rafsnk;



architecture imqzud of rafsnk is
  
begin
  
end imqzud;



entity ul is
  port (qujipilafl : buffer bit_vector(2 to 0); wyi : in real);
end ul;

library ieee;
use ieee.std_logic_1164.all;

architecture zruxpisq of ul is
  signal bggqypcgru : integer;
  signal ovzvkzpwdw : std_logic;
  signal xnmqsrv : integer;
  signal ypphdzr : std_logic;
  signal lkub : std_logic;
  signal ilqut : real;
begin
  nhc : entity work.rafsnk
    port map (jdpaahvvn => ilqut, ums => lkub);
  xrkfuiqc : entity work.aavt
    port map (oarsn => ypphdzr, ebwdnoik => xnmqsrv);
  odr : entity work.aavt
    port map (oarsn => ovzvkzpwdw, ebwdnoik => bggqypcgru);
end zruxpisq;

library ieee;
use ieee.std_logic_1164.all;

entity riqxpnqpw is
  port (kbopb : buffer real; ujxdyothyw : linkage severity_level; mx : linkage std_logic; ivqdx : inout std_logic);
end riqxpnqpw;



architecture b of riqxpnqpw is
  signal wjljztghk : real;
  signal ahjoyyleof : bit_vector(2 to 0);
  signal lqw : real;
  signal rx : real;
  signal bmp : bit_vector(2 to 0);
begin
  ykkoh : entity work.ul
    port map (qujipilafl => bmp, wyi => rx);
  h : entity work.rafsnk
    port map (jdpaahvvn => lqw, ums => ivqdx);
  jkyyf : entity work.ul
    port map (qujipilafl => ahjoyyleof, wyi => wjljztghk);
end b;



-- Seed after: 15796026839126271378,7332793847894666635

-- Seed: 15079623622168464352,15141888397681078541

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (taocyknlg : inout time; rrgumb : buffer std_logic);
end v;



architecture iazfi of v is
  
begin
  
end iazfi;

library ieee;
use ieee.std_logic_1164.all;

entity ss is
  port (izrdywufhq : linkage time; zvykiirf : linkage real; mosoz : buffer time; tq : linkage std_logic);
end ss;

library ieee;
use ieee.std_logic_1164.all;

architecture qqjnacoo of ss is
  signal wzalwpizd : std_logic;
begin
  gaw : entity work.v
    port map (taocyknlg => mosoz, rrgumb => wzalwpizd);
end qqjnacoo;

library ieee;
use ieee.std_logic_1164.all;

entity rnt is
  port (jsbkzsrce : buffer std_logic);
end rnt;

library ieee;
use ieee.std_logic_1164.all;

architecture yghgmfcx of rnt is
  signal vg : std_logic;
  signal kor : time;
  signal xtiavi : real;
  signal ywnlkutes : time;
  signal qvin : std_logic;
  signal nylyusicdf : time;
begin
  okt : entity work.v
    port map (taocyknlg => nylyusicdf, rrgumb => qvin);
  nnx : entity work.ss
    port map (izrdywufhq => ywnlkutes, zvykiirf => xtiavi, mosoz => kor, tq => qvin);
  pebnasjuf : entity work.v
    port map (taocyknlg => ywnlkutes, rrgumb => vg);
end yghgmfcx;



-- Seed after: 10978932528258794279,15141888397681078541

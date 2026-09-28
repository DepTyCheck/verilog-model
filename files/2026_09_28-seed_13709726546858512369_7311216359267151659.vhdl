-- Seed: 13709726546858512369,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity xbyjfyek is
  port (mpqebrg : buffer integer; yziwfdu : out std_logic_vector(4 to 2); po : linkage time);
end xbyjfyek;

architecture la of xbyjfyek is
  
begin
  -- Multi-driven assignments
  yziwfdu <= (others => '0');
  yziwfdu <= (others => '0');
end la;

library ieee;
use ieee.std_logic_1164.all;

entity mcddstyk is
  port (h : out real; etvii : linkage integer; u : linkage std_logic_vector(0 to 3); om : linkage std_logic_vector(2 downto 4));
end mcddstyk;

architecture aqftckzlii of mcddstyk is
  
begin
  -- Single-driven assignments
  h <= h;
end aqftckzlii;

entity icukzbik is
  port (bylabxpb : out integer; hfnt : linkage real; pfdj : out time_vector(3 downto 0); asokgrl : inout integer);
end icukzbik;

library ieee;
use ieee.std_logic_1164.all;

architecture dcyjsyp of icukzbik is
  signal xv : time;
  signal pza : std_logic_vector(4 to 2);
  signal ryg : std_logic_vector(0 to 3);
  signal o : integer;
  signal gngkcankxl : real;
  signal ah : time;
  signal nowd : std_logic_vector(2 downto 4);
  signal xmae : integer;
begin
  q : entity work.xbyjfyek
    port map (mpqebrg => xmae, yziwfdu => nowd, po => ah);
  eh : entity work.mcddstyk
    port map (h => gngkcankxl, etvii => o, u => ryg, om => nowd);
  b : entity work.xbyjfyek
    port map (mpqebrg => asokgrl, yziwfdu => pza, po => xv);
  
  -- Single-driven assignments
  pfdj <= (16#467B0# fs, 16#16267# ns, 4 min, 2_4_4_4.4 ps);
  bylabxpb <= asokgrl;
  
  -- Multi-driven assignments
  ryg <= ryg;
  pza <= (others => '0');
end dcyjsyp;



-- Seed after: 4528284335954375411,7311216359267151659

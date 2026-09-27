-- Seed: 12667451462089479127,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity pb is
  port (z : out std_logic_vector(1 downto 2); igmxeav : out bit);
end pb;

architecture mxtwhijvy of pb is
  
begin
  -- Single-driven assignments
  igmxeav <= igmxeav;
end mxtwhijvy;

entity djdhhwrh is
  port (vuztoexbxd : inout integer; nujfys : out time; vmwachzwq : inout time_vector(1 to 1));
end djdhhwrh;

library ieee;
use ieee.std_logic_1164.all;

architecture nuby of djdhhwrh is
  signal qkmy : bit;
  signal f : std_logic_vector(1 downto 2);
  signal bpzosjetpz : bit;
  signal ytrczzfgq : std_logic_vector(1 downto 2);
begin
  woe : entity work.pb
    port map (z => ytrczzfgq, igmxeav => bpzosjetpz);
  yxtmoifabx : entity work.pb
    port map (z => f, igmxeav => qkmy);
  
  -- Single-driven assignments
  vmwachzwq <= vmwachzwq;
  vuztoexbxd <= vuztoexbxd;
  nujfys <= nujfys;
end nuby;

entity ocac is
  port (iluim : buffer time_vector(3 downto 1); efvef : in time; ueuhwwywav : linkage time_vector(2 downto 1));
end ocac;

library ieee;
use ieee.std_logic_1164.all;

architecture lfjdhzft of ocac is
  signal klakis : bit;
  signal kefcgfkecz : bit;
  signal lcem : bit;
  signal drplqnezpp : std_logic_vector(1 downto 2);
begin
  gcwl : entity work.pb
    port map (z => drplqnezpp, igmxeav => lcem);
  qlamuxrft : entity work.pb
    port map (z => drplqnezpp, igmxeav => kefcgfkecz);
  q : entity work.pb
    port map (z => drplqnezpp, igmxeav => klakis);
  
  -- Single-driven assignments
  iluim <= iluim;
end lfjdhzft;

entity ogek is
  port (upjbcyd : in integer; dhae : buffer integer_vector(1 downto 3); kpc : out time);
end ogek;

library ieee;
use ieee.std_logic_1164.all;

architecture hryxzebf of ogek is
  signal e : bit;
  signal ajqnsl : bit;
  signal fmpvferjge : std_logic_vector(1 downto 2);
  signal bazbvgzr : time_vector(1 to 1);
  signal npoxoykyff : time;
  signal wsmdkith : integer;
  signal ypgzs : time_vector(2 downto 1);
  signal uq : time_vector(3 downto 1);
begin
  anrry : entity work.ocac
    port map (iluim => uq, efvef => kpc, ueuhwwywav => ypgzs);
  btmpp : entity work.djdhhwrh
    port map (vuztoexbxd => wsmdkith, nujfys => npoxoykyff, vmwachzwq => bazbvgzr);
  bglvujv : entity work.pb
    port map (z => fmpvferjge, igmxeav => ajqnsl);
  tsukeneqr : entity work.pb
    port map (z => fmpvferjge, igmxeav => e);
  
  -- Multi-driven assignments
  fmpvferjge <= (others => '0');
  fmpvferjge <= "";
end hryxzebf;



-- Seed after: 12355542713223698229,6379010654866854599

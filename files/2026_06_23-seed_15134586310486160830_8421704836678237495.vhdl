-- Seed: 15134586310486160830,8421704836678237495

entity sybfak is
  port (pahfagile : inout real_vector(1 downto 4); gwbbe : buffer time);
end sybfak;

architecture esltkw of sybfak is
  
begin
  -- Single-driven assignments
  pahfagile <= (others => 0.0);
  gwbbe <= 2 ps;
end esltkw;

library ieee;
use ieee.std_logic_1164.all;

entity ddigmqo is
  port (xjpbsii : linkage boolean_vector(2 downto 2); vytlat : buffer std_logic_vector(4 downto 2); avzvbl : inout std_logic; cmavxlihs : buffer real);
end ddigmqo;

architecture gxlrx of ddigmqo is
  signal lrc : time;
  signal vnv : real_vector(1 downto 4);
  signal suggrdk : time;
  signal ryzvwqh : real_vector(1 downto 4);
  signal rjrlvbbl : time;
  signal h : real_vector(1 downto 4);
  signal ltdaymfu : time;
  signal wwvavw : real_vector(1 downto 4);
begin
  brmknkdp : entity work.sybfak
    port map (pahfagile => wwvavw, gwbbe => ltdaymfu);
  dxnwionp : entity work.sybfak
    port map (pahfagile => h, gwbbe => rjrlvbbl);
  wdxgakbl : entity work.sybfak
    port map (pahfagile => ryzvwqh, gwbbe => suggrdk);
  wl : entity work.sybfak
    port map (pahfagile => vnv, gwbbe => lrc);
  
  -- Multi-driven assignments
  avzvbl <= '0';
  avzvbl <= 'Z';
end gxlrx;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (jn : in time_vector(4 downto 4); nbfiaoja : buffer std_logic_vector(2 downto 2));
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture vrqjffn of j is
  signal h : real;
  signal lvus : std_logic;
  signal mgjy : std_logic_vector(4 downto 2);
  signal gu : boolean_vector(2 downto 2);
  signal yqyuqrullr : time;
  signal untciqpr : real_vector(1 downto 4);
begin
  pfwei : entity work.sybfak
    port map (pahfagile => untciqpr, gwbbe => yqyuqrullr);
  hxmkwdl : entity work.ddigmqo
    port map (xjpbsii => gu, vytlat => mgjy, avzvbl => lvus, cmavxlihs => h);
  
  -- Multi-driven assignments
  lvus <= '0';
  nbfiaoja <= (others => '1');
end vrqjffn;



-- Seed after: 10497911691192634032,8421704836678237495

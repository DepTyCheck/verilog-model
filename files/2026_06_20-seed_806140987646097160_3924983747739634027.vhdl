-- Seed: 806140987646097160,3924983747739634027

entity enpc is
  port (tj : linkage time; zmwngtm : in integer; cz : in time);
end enpc;

architecture nbbwzlfy of enpc is
  
begin
  
end nbbwzlfy;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (lbm : linkage real; iqkgwcvyd : in bit; djk : in std_logic_vector(1 to 0));
end x;

architecture qbkko of x is
  signal be : time;
  signal xx : integer;
  signal rgyffcokza : time;
  signal wgkyasdkzh : time;
  signal kldkn : time;
  signal aqhvtumj : integer;
  signal l : time;
begin
  rd : entity work.enpc
    port map (tj => l, zmwngtm => aqhvtumj, cz => kldkn);
  lcdbiwqqi : entity work.enpc
    port map (tj => kldkn, zmwngtm => aqhvtumj, cz => wgkyasdkzh);
  guen : entity work.enpc
    port map (tj => rgyffcokza, zmwngtm => xx, cz => be);
end qbkko;

library ieee;
use ieee.std_logic_1164.all;

entity vjc is
  port (zn : buffer std_logic_vector(1 to 3); ljqxdki : inout bit_vector(4 downto 4); vontzmm : out std_logic_vector(0 to 0));
end vjc;

architecture agdyyvlk of vjc is
  signal npkpl : time;
  signal pzfbmkvxc : integer;
  signal ejvatd : time;
  signal tnumfygpt : time;
  signal z : integer;
  signal ktqasbm : time;
  signal hgc : integer;
  signal dhwkto : time;
begin
  unpenl : entity work.enpc
    port map (tj => dhwkto, zmwngtm => hgc, cz => dhwkto);
  kptefjqvxg : entity work.enpc
    port map (tj => ktqasbm, zmwngtm => z, cz => tnumfygpt);
  fkfpeh : entity work.enpc
    port map (tj => tnumfygpt, zmwngtm => hgc, cz => dhwkto);
  ss : entity work.enpc
    port map (tj => ejvatd, zmwngtm => pzfbmkvxc, cz => npkpl);
  
  -- Single-driven assignments
  z <= 000;
  npkpl <= 2#1_0_1_0# us;
end agdyyvlk;

entity xybmw is
  port (mt : linkage bit; jenelngezs : out real; sjcavjvlqm : out integer; pscxhq : linkage integer);
end xybmw;

library ieee;
use ieee.std_logic_1164.all;

architecture xdxnmrrh of xybmw is
  signal rqnycirc : integer;
  signal szrj : time;
  signal rnzya : std_logic_vector(1 to 0);
  signal mwlrzl : time;
  signal gexf : time;
  signal vubdrhmcj : std_logic_vector(1 to 0);
  signal alysvid : bit;
  signal r : real;
begin
  wcleq : entity work.x
    port map (lbm => r, iqkgwcvyd => alysvid, djk => vubdrhmcj);
  pu : entity work.enpc
    port map (tj => gexf, zmwngtm => sjcavjvlqm, cz => mwlrzl);
  oxfrtuni : entity work.x
    port map (lbm => jenelngezs, iqkgwcvyd => alysvid, djk => rnzya);
  v : entity work.enpc
    port map (tj => szrj, zmwngtm => rqnycirc, cz => gexf);
  
  -- Single-driven assignments
  sjcavjvlqm <= 1403;
  alysvid <= '0';
  
  -- Multi-driven assignments
  vubdrhmcj <= (others => '0');
  vubdrhmcj <= "";
  rnzya <= (others => '0');
end xdxnmrrh;



-- Seed after: 9169990078290240836,3924983747739634027

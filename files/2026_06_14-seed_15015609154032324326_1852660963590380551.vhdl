-- Seed: 15015609154032324326,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity czfhpzh is
  port (tmbgsfgurr : in std_logic_vector(4 to 0); n : inout std_logic_vector(2 downto 4); ibd : in time);
end czfhpzh;



architecture tcpq of czfhpzh is
  
begin
  
end tcpq;



entity v is
  port (hhyfm : out time; zhm : linkage integer);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture efwyvefdzi of v is
  signal qjjrrplj : std_logic_vector(2 downto 4);
  signal tmvtyjf : time;
  signal wlfw : std_logic_vector(2 downto 4);
  signal hsorxnx : std_logic_vector(4 to 0);
  signal cmoxzjvyim : std_logic_vector(4 to 0);
begin
  sjhkrodvj : entity work.czfhpzh
    port map (tmbgsfgurr => cmoxzjvyim, n => hsorxnx, ibd => hhyfm);
  xuzi : entity work.czfhpzh
    port map (tmbgsfgurr => cmoxzjvyim, n => wlfw, ibd => tmvtyjf);
  mz : entity work.czfhpzh
    port map (tmbgsfgurr => hsorxnx, n => qjjrrplj, ibd => hhyfm);
end efwyvefdzi;



entity rk is
  port (dgrvffglzz : in integer_vector(0 downto 3));
end rk;

library ieee;
use ieee.std_logic_1164.all;

architecture ukh of rk is
  signal kfq : time;
  signal hbmp : time;
  signal ihknljodgz : std_logic_vector(2 downto 4);
begin
  tkm : entity work.czfhpzh
    port map (tmbgsfgurr => ihknljodgz, n => ihknljodgz, ibd => hbmp);
  nbooulh : entity work.czfhpzh
    port map (tmbgsfgurr => ihknljodgz, n => ihknljodgz, ibd => kfq);
end ukh;



-- Seed after: 12208963030437092843,1852660963590380551

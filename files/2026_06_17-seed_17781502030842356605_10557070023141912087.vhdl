-- Seed: 17781502030842356605,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity rsrxnwx is
  port (c : linkage std_logic_vector(4 downto 1));
end rsrxnwx;

architecture sxnbpqancn of rsrxnwx is
  
begin
  
end sxnbpqancn;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (vq : in time_vector(4 to 4); l : out integer; hwjtz : buffer integer_vector(3 downto 3); oyxu : out std_logic_vector(1 downto 0));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture wccbw of p is
  signal oy : std_logic_vector(4 downto 1);
  signal gxearaob : std_logic_vector(4 downto 1);
begin
  mloi : entity work.rsrxnwx
    port map (c => gxearaob);
  nggogy : entity work.rsrxnwx
    port map (c => oy);
  jkwmitukb : entity work.rsrxnwx
    port map (c => oy);
  
  -- Single-driven assignments
  l <= 1_2;
  hwjtz <= (others => 8#5_7_0_3#);
end wccbw;

library ieee;
use ieee.std_logic_1164.all;

entity fxmdkd is
  port (rgqtprmlgc : linkage std_logic; mrwhyfeb : in std_logic_vector(4 downto 4));
end fxmdkd;

library ieee;
use ieee.std_logic_1164.all;

architecture kfqnjdn of fxmdkd is
  signal orznsm : std_logic_vector(4 downto 1);
begin
  rc : entity work.rsrxnwx
    port map (c => orznsm);
  
  -- Multi-driven assignments
  orznsm <= "X1WZ";
end kfqnjdn;



-- Seed after: 10490221224714433836,10557070023141912087

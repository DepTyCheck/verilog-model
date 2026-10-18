-- Seed: 7967064639594545307,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity wchnfplgn is
  port (nxywwqjem : out std_logic_vector(0 downto 0));
end wchnfplgn;

architecture xfwepd of wchnfplgn is
  
begin
  -- Multi-driven assignments
  nxywwqjem <= nxywwqjem;
  nxywwqjem <= (others => 'W');
  nxywwqjem <= (others => 'H');
end xfwepd;

entity xlc is
  port (aw : in integer);
end xlc;

library ieee;
use ieee.std_logic_1164.all;

architecture cpiiukx of xlc is
  signal gq : std_logic_vector(0 downto 0);
begin
  ubnfuaclp : entity work.wchnfplgn
    port map (nxywwqjem => gq);
  
  -- Multi-driven assignments
  gq <= (others => 'W');
  gq <= gq;
  gq <= gq;
  gq <= (others => 'L');
end cpiiukx;

library ieee;
use ieee.std_logic_1164.all;

entity lo is
  port (yoigcvwu : buffer std_logic);
end lo;

library ieee;
use ieee.std_logic_1164.all;

architecture sm of lo is
  signal i : std_logic_vector(0 downto 0);
  signal p : std_logic_vector(0 downto 0);
begin
  hfs : entity work.wchnfplgn
    port map (nxywwqjem => p);
  wbolbijxrh : entity work.wchnfplgn
    port map (nxywwqjem => i);
  
  -- Multi-driven assignments
  yoigcvwu <= 'W';
  yoigcvwu <= 'U';
end sm;

library ieee;
use ieee.std_logic_1164.all;

entity vqipbv is
  port (anhj : in std_logic_vector(4 downto 0); m : linkage time; yqz : in boolean; ab : inout time);
end vqipbv;

library ieee;
use ieee.std_logic_1164.all;

architecture mpcd of vqipbv is
  signal cnb : std_logic;
  signal ffh : std_logic_vector(0 downto 0);
begin
  goc : entity work.wchnfplgn
    port map (nxywwqjem => ffh);
  w : entity work.lo
    port map (yoigcvwu => cnb);
end mpcd;



-- Seed after: 5911889048408811324,14108379317686958507

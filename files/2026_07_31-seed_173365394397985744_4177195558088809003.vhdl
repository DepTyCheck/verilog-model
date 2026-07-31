-- Seed: 173365394397985744,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity msnwa is
  port (l : buffer integer; yewjixbiv : linkage time_vector(1 downto 3); q : buffer std_logic_vector(1 to 2); tkvbybjly : out std_logic);
end msnwa;

architecture vzyoe of msnwa is
  
begin
  -- Single-driven assignments
  l <= l;
end vzyoe;

library ieee;
use ieee.std_logic_1164.all;

entity mvugrg is
  port (eq : buffer real; tuljikb : buffer real; cfvxcryb : in std_logic);
end mvugrg;

library ieee;
use ieee.std_logic_1164.all;

architecture aqovwukrq of mvugrg is
  signal cizcnmjcz : time_vector(1 downto 3);
  signal lwuhagjj : integer;
  signal qff : time_vector(1 downto 3);
  signal f : integer;
  signal wukg : std_logic;
  signal sgf : std_logic_vector(1 to 2);
  signal ewz : time_vector(1 downto 3);
  signal ynhssabx : integer;
begin
  o : entity work.msnwa
    port map (l => ynhssabx, yewjixbiv => ewz, q => sgf, tkvbybjly => wukg);
  stlf : entity work.msnwa
    port map (l => f, yewjixbiv => qff, q => sgf, tkvbybjly => wukg);
  unbcahdula : entity work.msnwa
    port map (l => lwuhagjj, yewjixbiv => cizcnmjcz, q => sgf, tkvbybjly => wukg);
  
  -- Multi-driven assignments
  sgf <= sgf;
end aqovwukrq;

entity gikqpsfr is
  port (ulegyipn : in character);
end gikqpsfr;

architecture kznppjkmto of gikqpsfr is
  
begin
  
end kznppjkmto;

library ieee;
use ieee.std_logic_1164.all;

entity vnrz is
  port (b : in time; il : linkage std_logic; cgjeryu : buffer std_logic_vector(4 to 1));
end vnrz;

library ieee;
use ieee.std_logic_1164.all;

architecture kqg of vnrz is
  signal jbquekn : std_logic_vector(1 to 2);
  signal guwpdyeka : time_vector(1 downto 3);
  signal r : integer;
  signal yaf : real;
  signal dn : real;
  signal egm : std_logic;
  signal uqa : real;
  signal tkhvbnrf : real;
begin
  cmzuz : entity work.mvugrg
    port map (eq => tkhvbnrf, tuljikb => uqa, cfvxcryb => egm);
  uaclfpf : entity work.mvugrg
    port map (eq => dn, tuljikb => yaf, cfvxcryb => egm);
  txbqhaatc : entity work.msnwa
    port map (l => r, yewjixbiv => guwpdyeka, q => jbquekn, tkvbybjly => egm);
  
  -- Multi-driven assignments
  cgjeryu <= (others => '0');
end kqg;



-- Seed after: 7086020702915246315,4177195558088809003

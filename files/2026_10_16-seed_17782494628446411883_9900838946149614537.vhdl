-- Seed: 17782494628446411883,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity lvsts is
  port (bzqozpg : linkage std_logic; lqojbocgwx : buffer time);
end lvsts;

architecture ldrkysq of lvsts is
  
begin
  -- Single-driven assignments
  lqojbocgwx <= 2.2 fs;
end ldrkysq;

entity bfkxgsgpfo is
  port (osqb : linkage integer_vector(2 downto 4); ctqpwzj : buffer integer; hoxtrnm : out bit_vector(2 to 0); sknqrd : in severity_level);
end bfkxgsgpfo;

library ieee;
use ieee.std_logic_1164.all;

architecture vkrrzfra of bfkxgsgpfo is
  signal boewzzvi : time;
  signal uqfkrfotc : time;
  signal arilyfp : std_logic;
begin
  d : entity work.lvsts
    port map (bzqozpg => arilyfp, lqojbocgwx => uqfkrfotc);
  igntqowsy : entity work.lvsts
    port map (bzqozpg => arilyfp, lqojbocgwx => boewzzvi);
  
  -- Multi-driven assignments
  arilyfp <= arilyfp;
  arilyfp <= 'U';
  arilyfp <= 'H';
end vkrrzfra;

library ieee;
use ieee.std_logic_1164.all;

entity dty is
  port (vmijq : in bit; yfoyycbc : linkage integer; j : out std_logic_vector(0 downto 3));
end dty;

library ieee;
use ieee.std_logic_1164.all;

architecture axcm of dty is
  signal goeuhzgeps : time;
  signal w : time;
  signal gzxwdz : std_logic;
  signal ed : severity_level;
  signal nirb : bit_vector(2 to 0);
  signal eha : integer;
  signal rkubzvht : integer_vector(2 downto 4);
begin
  dtsxajictf : entity work.bfkxgsgpfo
    port map (osqb => rkubzvht, ctqpwzj => eha, hoxtrnm => nirb, sknqrd => ed);
  bcaqxtlxq : entity work.lvsts
    port map (bzqozpg => gzxwdz, lqojbocgwx => w);
  czcnh : entity work.lvsts
    port map (bzqozpg => gzxwdz, lqojbocgwx => goeuhzgeps);
  
  -- Single-driven assignments
  ed <= ed;
  
  -- Multi-driven assignments
  j <= j;
  j <= j;
  j <= j;
end axcm;

entity kvkjgqlilf is
  port (thxyzwif : linkage real; z : linkage integer; izf : buffer time);
end kvkjgqlilf;

library ieee;
use ieee.std_logic_1164.all;

architecture eyvpx of kvkjgqlilf is
  signal desrxje : time;
  signal reudeoqwg : std_logic;
  signal qmkg : severity_level;
  signal fdyoq : bit_vector(2 to 0);
  signal xrvnkhpdf : integer;
  signal e : integer_vector(2 downto 4);
  signal p : time;
  signal xpc : std_logic;
begin
  ryet : entity work.lvsts
    port map (bzqozpg => xpc, lqojbocgwx => p);
  yvbrjfed : entity work.bfkxgsgpfo
    port map (osqb => e, ctqpwzj => xrvnkhpdf, hoxtrnm => fdyoq, sknqrd => qmkg);
  xnjcpcz : entity work.lvsts
    port map (bzqozpg => reudeoqwg, lqojbocgwx => izf);
  bwjsht : entity work.lvsts
    port map (bzqozpg => xpc, lqojbocgwx => desrxje);
  
  -- Single-driven assignments
  qmkg <= NOTE;
  
  -- Multi-driven assignments
  reudeoqwg <= 'X';
  xpc <= '0';
  xpc <= 'H';
end eyvpx;



-- Seed after: 16350199989549864782,9900838946149614537

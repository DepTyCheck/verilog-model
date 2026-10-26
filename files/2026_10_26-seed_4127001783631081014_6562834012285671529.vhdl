-- Seed: 4127001783631081014,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity vx is
  port (bdcjnogt : out std_logic_vector(4 downto 3); imvlho : linkage std_logic; yf : out std_logic_vector(1 downto 4));
end vx;

architecture mkfh of vx is
  
begin
  -- Multi-driven assignments
  bdcjnogt <= bdcjnogt;
end mkfh;

library ieee;
use ieee.std_logic_1164.all;

entity mbhdcee is
  port (nfmlknz : out integer; wwl : in real; tsgytvdso : buffer std_logic);
end mbhdcee;

library ieee;
use ieee.std_logic_1164.all;

architecture hyunhye of mbhdcee is
  signal momhznpzg : std_logic_vector(4 downto 3);
  signal wbcrspq : std_logic_vector(1 downto 4);
  signal xpg : std_logic_vector(4 downto 3);
begin
  hyvi : entity work.vx
    port map (bdcjnogt => xpg, imvlho => tsgytvdso, yf => wbcrspq);
  ayzhgjc : entity work.vx
    port map (bdcjnogt => momhznpzg, imvlho => tsgytvdso, yf => wbcrspq);
  
  -- Single-driven assignments
  nfmlknz <= 8#70757#;
  
  -- Multi-driven assignments
  tsgytvdso <= tsgytvdso;
  tsgytvdso <= '0';
end hyunhye;

entity t is
  port (maqjeuurys : out time; uwpihdb : out real_vector(3 downto 0); dxopfwn : linkage integer; xhb : linkage severity_level);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture fsiwfn of t is
  signal ycypnlyvyx : std_logic;
  signal phlwn : std_logic_vector(1 downto 4);
  signal cihrabibnl : std_logic_vector(4 downto 3);
  signal n : std_logic;
  signal gy : real;
  signal qvuowlrj : integer;
begin
  zj : entity work.mbhdcee
    port map (nfmlknz => qvuowlrj, wwl => gy, tsgytvdso => n);
  uxjfpuokfj : entity work.vx
    port map (bdcjnogt => cihrabibnl, imvlho => n, yf => phlwn);
  ciyhnmfmeg : entity work.vx
    port map (bdcjnogt => cihrabibnl, imvlho => ycypnlyvyx, yf => phlwn);
end fsiwfn;



-- Seed after: 15281998484621361415,6562834012285671529

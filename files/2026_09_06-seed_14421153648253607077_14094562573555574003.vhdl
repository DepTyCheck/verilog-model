-- Seed: 14421153648253607077,14094562573555574003

entity eujfuei is
  port (yr : linkage integer);
end eujfuei;

architecture s of eujfuei is
  
begin
  
end s;

entity ivx is
  port (myimz : inout integer_vector(3 downto 4); ugwuj : in integer_vector(2 to 0); l : linkage time);
end ivx;

architecture eixgvger of ivx is
  signal prxvskboah : integer;
  signal fqif : integer;
  signal fuobabagk : integer;
begin
  gahu : entity work.eujfuei
    port map (yr => fuobabagk);
  ptfj : entity work.eujfuei
    port map (yr => fqif);
  sxxzwkkb : entity work.eujfuei
    port map (yr => prxvskboah);
  
  -- Single-driven assignments
  myimz <= myimz;
end eixgvger;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (nvszckh : out std_logic_vector(0 to 4); gbtmfgznfq : inout bit; wixgmviaf : inout time; pup : linkage bit);
end t;

architecture jykzfq of t is
  signal dxuutun : integer;
  signal dlmrdp : integer_vector(2 to 0);
begin
  tl : entity work.ivx
    port map (myimz => dlmrdp, ugwuj => dlmrdp, l => wixgmviaf);
  hrnxszhksx : entity work.eujfuei
    port map (yr => dxuutun);
end jykzfq;

library ieee;
use ieee.std_logic_1164.all;

entity zpuvqr is
  port (kohp : linkage std_logic_vector(2 downto 3); rymdlxl : linkage time; rlfog : out boolean_vector(4 to 0));
end zpuvqr;

library ieee;
use ieee.std_logic_1164.all;

architecture wwsazhxrq of zpuvqr is
  signal kkhvclxc : integer;
  signal w : bit;
  signal zpd : time;
  signal ppqduftc : bit;
  signal vrwjgi : bit;
  signal pqktfjjbx : time;
  signal h : bit;
  signal vkczabgwss : std_logic_vector(0 to 4);
begin
  fpcm : entity work.t
    port map (nvszckh => vkczabgwss, gbtmfgznfq => h, wixgmviaf => pqktfjjbx, pup => vrwjgi);
  pdobg : entity work.t
    port map (nvszckh => vkczabgwss, gbtmfgznfq => ppqduftc, wixgmviaf => zpd, pup => w);
  pynoyk : entity work.eujfuei
    port map (yr => kkhvclxc);
  
  -- Single-driven assignments
  rlfog <= (others => TRUE);
  
  -- Multi-driven assignments
  vkczabgwss <= ('X', '0', '0', 'X', '-');
  vkczabgwss <= vkczabgwss;
end wwsazhxrq;



-- Seed after: 3361050768427702966,14094562573555574003

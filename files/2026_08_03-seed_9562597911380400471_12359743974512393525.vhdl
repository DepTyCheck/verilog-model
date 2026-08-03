-- Seed: 9562597911380400471,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity pfbfvlufv is
  port (jfra : buffer std_logic);
end pfbfvlufv;

architecture vxzxqm of pfbfvlufv is
  
begin
  -- Multi-driven assignments
  jfra <= 'H';
end vxzxqm;

library ieee;
use ieee.std_logic_1164.all;

entity zwdxwhdoi is
  port (yxd : linkage integer; gh : out integer; ynycxheh : in bit_vector(3 downto 4); gsqpiviycn : inout std_logic);
end zwdxwhdoi;

library ieee;
use ieee.std_logic_1164.all;

architecture waswygvw of zwdxwhdoi is
  signal qcv : std_logic;
begin
  xq : entity work.pfbfvlufv
    port map (jfra => gsqpiviycn);
  zz : entity work.pfbfvlufv
    port map (jfra => qcv);
end waswygvw;

entity kehvsmerf is
  port (zamdgy : in integer; jwxurndjx : out bit; wxd : linkage integer);
end kehvsmerf;

library ieee;
use ieee.std_logic_1164.all;

architecture gzplkxc of kehvsmerf is
  signal hpgw : std_logic;
begin
  zdsiljf : entity work.pfbfvlufv
    port map (jfra => hpgw);
  
  -- Single-driven assignments
  jwxurndjx <= '0';
end gzplkxc;

entity oxztuwpdvy is
  port (lgujz : in integer; nlyedn : inout time; onyljvssax : out string(5 downto 4));
end oxztuwpdvy;

library ieee;
use ieee.std_logic_1164.all;

architecture dnufv of oxztuwpdvy is
  signal iokya : std_logic;
  signal linz : bit_vector(3 downto 4);
  signal dchrfp : integer;
  signal mlpwpxqywv : integer;
begin
  lkkhzrc : entity work.zwdxwhdoi
    port map (yxd => mlpwpxqywv, gh => dchrfp, ynycxheh => linz, gsqpiviycn => iokya);
  v : entity work.pfbfvlufv
    port map (jfra => iokya);
  
  -- Single-driven assignments
  linz <= (others => '0');
  nlyedn <= 1 hr;
  onyljvssax <= "zp";
  
  -- Multi-driven assignments
  iokya <= '0';
  iokya <= 'H';
  iokya <= 'L';
end dnufv;



-- Seed after: 8756314977898161546,12359743974512393525

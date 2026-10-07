-- Seed: 9757163654413618372,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity mllrudkx is
  port (oxmu : linkage integer; makrimtyrf : buffer std_logic; fs : buffer std_logic_vector(0 downto 4));
end mllrudkx;

architecture w of mllrudkx is
  
begin
  -- Multi-driven assignments
  makrimtyrf <= 'L';
end w;

library ieee;
use ieee.std_logic_1164.all;

entity xteaj is
  port (dquhk : in time; vgbdogw : out bit_vector(3 to 1); q : linkage std_logic_vector(0 downto 3); iifzwd : in time_vector(2 downto 2));
end xteaj;

library ieee;
use ieee.std_logic_1164.all;

architecture hdbxqm of xteaj is
  signal ek : integer;
  signal uqoosai : std_logic_vector(0 downto 4);
  signal ojh : integer;
  signal ydgnf : std_logic_vector(0 downto 4);
  signal vvxpjonry : std_logic;
  signal jhip : integer;
  signal cdvasvm : std_logic_vector(0 downto 4);
  signal vonkmemz : std_logic;
  signal tnutc : integer;
begin
  kjquwlo : entity work.mllrudkx
    port map (oxmu => tnutc, makrimtyrf => vonkmemz, fs => cdvasvm);
  jbx : entity work.mllrudkx
    port map (oxmu => jhip, makrimtyrf => vvxpjonry, fs => ydgnf);
  z : entity work.mllrudkx
    port map (oxmu => ojh, makrimtyrf => vonkmemz, fs => uqoosai);
  ojrzzaz : entity work.mllrudkx
    port map (oxmu => ek, makrimtyrf => vonkmemz, fs => cdvasvm);
  
  -- Single-driven assignments
  vgbdogw <= (others => '0');
  
  -- Multi-driven assignments
  vonkmemz <= '-';
end hdbxqm;



-- Seed after: 6051124795981800781,5906004015519833893

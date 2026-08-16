-- Seed: 3111120153149333530,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity zh is
  port (q : out std_logic_vector(2 downto 2); ba : out integer; eelaolt : in std_logic_vector(0 to 0));
end zh;

architecture kz of zh is
  
begin
  -- Single-driven assignments
  ba <= 1_4_0_1;
end kz;

library ieee;
use ieee.std_logic_1164.all;

entity pgzgqchbd is
  port (isvtr : in std_logic);
end pgzgqchbd;

library ieee;
use ieee.std_logic_1164.all;

architecture g of pgzgqchbd is
  signal w : std_logic_vector(0 to 0);
  signal uwampslbi : integer;
  signal orzlhn : integer;
  signal qwbiqj : std_logic_vector(2 downto 2);
  signal ofkt : std_logic_vector(0 to 0);
  signal f : integer;
  signal ospmmnzwki : std_logic_vector(2 downto 2);
begin
  jcoyaqaavf : entity work.zh
    port map (q => ospmmnzwki, ba => f, eelaolt => ofkt);
  grfaes : entity work.zh
    port map (q => qwbiqj, ba => orzlhn, eelaolt => qwbiqj);
  lydiriprvb : entity work.zh
    port map (q => qwbiqj, ba => uwampslbi, eelaolt => w);
  
  -- Multi-driven assignments
  ospmmnzwki <= (others => 'W');
  qwbiqj <= ospmmnzwki;
  ospmmnzwki <= (others => 'L');
end g;



-- Seed after: 2794951315932812404,13857275728440271305

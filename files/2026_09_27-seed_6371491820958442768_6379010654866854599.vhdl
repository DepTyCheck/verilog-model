-- Seed: 6371491820958442768,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity orgyjilmr is
  port (pd : out std_logic_vector(4 to 0); jeda : in time_vector(4 downto 1); lvbff : out std_logic);
end orgyjilmr;

architecture ekxjq of orgyjilmr is
  
begin
  -- Multi-driven assignments
  pd <= (others => '0');
  pd <= (others => '0');
end ekxjq;

library ieee;
use ieee.std_logic_1164.all;

entity rdchzsma is
  port (ofnbpujh : inout severity_level; uitoesqo : buffer std_logic; fwxq : inout severity_level);
end rdchzsma;

architecture exkpog of rdchzsma is
  
begin
  -- Single-driven assignments
  fwxq <= ERROR;
  ofnbpujh <= fwxq;
  
  -- Multi-driven assignments
  uitoesqo <= 'U';
  uitoesqo <= uitoesqo;
  uitoesqo <= '-';
  uitoesqo <= uitoesqo;
end exkpog;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (tn : linkage time_vector(1 downto 0); scv : inout severity_level; nhhrvaixz : out integer_vector(3 to 0); hynikxaa : buffer std_logic);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture hxjo of v is
  signal rpoiuy : std_logic;
  signal rb : time_vector(4 downto 1);
  signal miftukr : time_vector(4 downto 1);
  signal rqc : std_logic;
  signal gacvwjjf : time_vector(4 downto 1);
  signal i : std_logic_vector(4 to 0);
  signal pvuh : std_logic;
  signal nmctql : time_vector(4 downto 1);
  signal cipn : std_logic_vector(4 to 0);
begin
  t : entity work.orgyjilmr
    port map (pd => cipn, jeda => nmctql, lvbff => pvuh);
  f : entity work.orgyjilmr
    port map (pd => i, jeda => gacvwjjf, lvbff => rqc);
  dywwolucp : entity work.orgyjilmr
    port map (pd => cipn, jeda => miftukr, lvbff => hynikxaa);
  ffow : entity work.orgyjilmr
    port map (pd => cipn, jeda => rb, lvbff => rpoiuy);
  
  -- Single-driven assignments
  nhhrvaixz <= (others => 0);
  gacvwjjf <= (3 min, 1_2_2_1 ms, 343 ps, 341 ms);
  nmctql <= (2#1.0101# ns, 16#6_2_1_7_0# ns, 8#3_7_3_7.1_1# ps, 1 sec);
  scv <= FAILURE;
  
  -- Multi-driven assignments
  hynikxaa <= '0';
end hxjo;

entity rfveb is
  port (lzo : inout severity_level);
end rfveb;

library ieee;
use ieee.std_logic_1164.all;

architecture vykvif of rfveb is
  signal du : std_logic;
  signal zjarhqna : severity_level;
begin
  vjoxobn : entity work.rdchzsma
    port map (ofnbpujh => zjarhqna, uitoesqo => du, fwxq => lzo);
  
  -- Multi-driven assignments
  du <= du;
  du <= '-';
  du <= '1';
end vykvif;



-- Seed after: 12334310585306906128,6379010654866854599

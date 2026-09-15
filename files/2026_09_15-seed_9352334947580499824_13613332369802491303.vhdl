-- Seed: 9352334947580499824,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (sfutyx : buffer std_logic; rzqb : in std_logic_vector(0 downto 4); g : in std_logic_vector(3 to 1));
end i;

architecture gvumec of i is
  
begin
  -- Multi-driven assignments
  sfutyx <= sfutyx;
  sfutyx <= 'L';
  sfutyx <= '1';
  sfutyx <= 'Z';
end gvumec;

entity fbmzw is
  port (sa : buffer time; abc : out bit);
end fbmzw;

library ieee;
use ieee.std_logic_1164.all;

architecture mrj of fbmzw is
  signal jsjqwzn : std_logic_vector(3 to 1);
  signal wlzm : std_logic_vector(0 downto 4);
  signal viqkpehzsc : std_logic;
  signal siabhkc : std_logic_vector(0 downto 4);
  signal qykiev : std_logic_vector(0 downto 4);
  signal azyjyfpv : std_logic_vector(3 to 1);
  signal ilortgpr : std_logic_vector(3 to 1);
  signal sye : std_logic;
begin
  h : entity work.i
    port map (sfutyx => sye, rzqb => ilortgpr, g => azyjyfpv);
  qkjihhltrq : entity work.i
    port map (sfutyx => sye, rzqb => qykiev, g => azyjyfpv);
  yomzg : entity work.i
    port map (sfutyx => sye, rzqb => siabhkc, g => ilortgpr);
  lmsjglndad : entity work.i
    port map (sfutyx => viqkpehzsc, rzqb => wlzm, g => jsjqwzn);
  
  -- Single-driven assignments
  sa <= 4_2_4_2 ms;
  abc <= '0';
  
  -- Multi-driven assignments
  viqkpehzsc <= 'Z';
  azyjyfpv <= qykiev;
  sye <= sye;
  sye <= sye;
end mrj;

entity phsfad is
  port (uumn : in time; fltrot : inout integer_vector(0 to 2));
end phsfad;

library ieee;
use ieee.std_logic_1164.all;

architecture zhphxsbyeg of phsfad is
  signal lc : std_logic_vector(3 to 1);
  signal ysjh : std_logic_vector(0 downto 4);
  signal ld : std_logic;
begin
  lflrabgmy : entity work.i
    port map (sfutyx => ld, rzqb => ysjh, g => lc);
  
  -- Single-driven assignments
  fltrot <= (0_1, 16#02F#, 16#4#);
  
  -- Multi-driven assignments
  ld <= ld;
  ld <= '-';
  ysjh <= ysjh;
end zhphxsbyeg;



-- Seed after: 17060455925358602336,13613332369802491303

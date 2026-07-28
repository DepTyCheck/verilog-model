-- Seed: 8925841095839143598,2511821214772927453

entity wejsjkxi is
  port (uk : out integer);
end wejsjkxi;

architecture exfqjqqk of wejsjkxi is
  
begin
  -- Single-driven assignments
  uk <= uk;
end exfqjqqk;

library ieee;
use ieee.std_logic_1164.all;

entity tsi is
  port (uyehkjkz : out std_logic_vector(4 downto 2); jvxokuj : buffer std_logic);
end tsi;

architecture frf of tsi is
  signal pkzdq : integer;
  signal d : integer;
  signal dyrigjwiin : integer;
  signal uipy : integer;
begin
  w : entity work.wejsjkxi
    port map (uk => uipy);
  qjjryhjf : entity work.wejsjkxi
    port map (uk => dyrigjwiin);
  g : entity work.wejsjkxi
    port map (uk => d);
  k : entity work.wejsjkxi
    port map (uk => pkzdq);
end frf;

library ieee;
use ieee.std_logic_1164.all;

entity bxo is
  port (cszypvg : out std_logic);
end bxo;

library ieee;
use ieee.std_logic_1164.all;

architecture eb of bxo is
  signal k : integer;
  signal ff : integer;
  signal s : std_logic;
  signal lt : std_logic_vector(4 downto 2);
begin
  wycmjawuqi : entity work.tsi
    port map (uyehkjkz => lt, jvxokuj => s);
  kejswyeueo : entity work.wejsjkxi
    port map (uk => ff);
  tn : entity work.wejsjkxi
    port map (uk => k);
  
  -- Multi-driven assignments
  s <= 'U';
end eb;



-- Seed after: 11606673267992421571,2511821214772927453

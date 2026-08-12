-- Seed: 10599182246184047085,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity sptbibg is
  port (mbfgq : out std_logic; glsriltfus : out std_logic_vector(4 downto 3); k : in std_logic_vector(2 downto 3));
end sptbibg;

architecture dppy of sptbibg is
  
begin
  -- Multi-driven assignments
  glsriltfus <= glsriltfus;
end dppy;

library ieee;
use ieee.std_logic_1164.all;

entity lzczafwoby is
  port (ltmdreju : in std_logic_vector(3 to 0); hastx : inout std_logic; ikezjo : in std_logic; sroorg : in std_logic);
end lzczafwoby;

library ieee;
use ieee.std_logic_1164.all;

architecture clthrl of lzczafwoby is
  signal jyrm : std_logic_vector(2 downto 3);
  signal tqdey : std_logic_vector(4 downto 3);
  signal n : std_logic_vector(2 downto 3);
  signal kgdhpmfg : std_logic_vector(4 downto 3);
  signal eswq : std_logic;
  signal tlsu : std_logic_vector(2 downto 3);
  signal lvuqz : std_logic_vector(4 downto 3);
  signal slf : std_logic;
begin
  eseucvv : entity work.sptbibg
    port map (mbfgq => slf, glsriltfus => lvuqz, k => tlsu);
  cifn : entity work.sptbibg
    port map (mbfgq => eswq, glsriltfus => kgdhpmfg, k => ltmdreju);
  uhsgfxzhdd : entity work.sptbibg
    port map (mbfgq => hastx, glsriltfus => lvuqz, k => n);
  dyjszk : entity work.sptbibg
    port map (mbfgq => hastx, glsriltfus => tqdey, k => jyrm);
end clthrl;



-- Seed after: 3434492745236431488,8412319452373742525

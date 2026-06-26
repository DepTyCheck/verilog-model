-- Seed: 7102406833080009737,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity apepzfjy is
  port (fcxbeatulr : out std_logic_vector(0 downto 0); gjsoq : linkage std_logic);
end apepzfjy;

architecture mgztwv of apepzfjy is
  
begin
  -- Multi-driven assignments
  fcxbeatulr <= (others => 'L');
end mgztwv;

library ieee;
use ieee.std_logic_1164.all;

entity hiwavjypp is
  port (jdbtrylct : in integer; ywisjkeh : linkage time; rfvkya : out std_logic);
end hiwavjypp;

library ieee;
use ieee.std_logic_1164.all;

architecture qgn of hiwavjypp is
  signal bkuizjxj : std_logic;
  signal atpcoscq : std_logic_vector(0 downto 0);
  signal co : std_logic;
  signal qpffg : std_logic_vector(0 downto 0);
begin
  aiogrzg : entity work.apepzfjy
    port map (fcxbeatulr => qpffg, gjsoq => rfvkya);
  l : entity work.apepzfjy
    port map (fcxbeatulr => qpffg, gjsoq => co);
  darjuep : entity work.apepzfjy
    port map (fcxbeatulr => atpcoscq, gjsoq => bkuizjxj);
  
  -- Multi-driven assignments
  qpffg <= "Z";
  rfvkya <= '0';
  atpcoscq <= (others => 'X');
end qgn;



-- Seed after: 12112080203480110990,12011142928354116943

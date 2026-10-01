-- Seed: 10858809032616586050,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity ihzllanw is
  port (nrehskbn : buffer std_logic; ehaw : in integer_vector(0 downto 4));
end ihzllanw;

architecture jeyb of ihzllanw is
  
begin
  -- Multi-driven assignments
  nrehskbn <= '0';
  nrehskbn <= '0';
  nrehskbn <= '-';
end jeyb;

library ieee;
use ieee.std_logic_1164.all;

entity apz is
  port (dowcwbxp : in std_logic; vilbfy : linkage integer; yh : buffer time);
end apz;

library ieee;
use ieee.std_logic_1164.all;

architecture okgzqj of apz is
  signal ipfgek : integer_vector(0 downto 4);
  signal jhrs : std_logic;
  signal jrywiebz : std_logic;
  signal vokrdayzdc : integer_vector(0 downto 4);
  signal rnpwq : std_logic;
begin
  zrbcnymrf : entity work.ihzllanw
    port map (nrehskbn => rnpwq, ehaw => vokrdayzdc);
  sbckbgprdb : entity work.ihzllanw
    port map (nrehskbn => rnpwq, ehaw => vokrdayzdc);
  i : entity work.ihzllanw
    port map (nrehskbn => jrywiebz, ehaw => vokrdayzdc);
  u : entity work.ihzllanw
    port map (nrehskbn => jhrs, ehaw => ipfgek);
  
  -- Single-driven assignments
  ipfgek <= (others => 0);
  yh <= 1 hr;
  
  -- Multi-driven assignments
  rnpwq <= '1';
  jhrs <= dowcwbxp;
end okgzqj;



-- Seed after: 15744964145849833988,15025465285671019065

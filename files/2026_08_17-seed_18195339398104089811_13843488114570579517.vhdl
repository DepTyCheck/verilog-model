-- Seed: 18195339398104089811,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity bfqyylhieb is
  port (kiquuwo : in time; qjgtcewayx : out std_logic_vector(2 to 2));
end bfqyylhieb;

architecture ingld of bfqyylhieb is
  
begin
  -- Multi-driven assignments
  qjgtcewayx <= (others => 'W');
  qjgtcewayx <= qjgtcewayx;
  qjgtcewayx <= qjgtcewayx;
end ingld;

library ieee;
use ieee.std_logic_1164.all;

entity mbtzfexudi is
  port (rvlxqd : in integer_vector(3 downto 0); ofv : inout std_logic_vector(1 downto 3); rx : linkage string(4 to 1); oukubnfm : buffer character);
end mbtzfexudi;

library ieee;
use ieee.std_logic_1164.all;

architecture hzukxs of mbtzfexudi is
  signal rtvsrastea : time;
  signal kyndbwu : std_logic_vector(2 to 2);
  signal ghmx : std_logic_vector(2 to 2);
  signal fjsa : std_logic_vector(2 to 2);
  signal qk : time;
begin
  beivto : entity work.bfqyylhieb
    port map (kiquuwo => qk, qjgtcewayx => fjsa);
  obix : entity work.bfqyylhieb
    port map (kiquuwo => qk, qjgtcewayx => ghmx);
  yxqarihgk : entity work.bfqyylhieb
    port map (kiquuwo => qk, qjgtcewayx => kyndbwu);
  pm : entity work.bfqyylhieb
    port map (kiquuwo => rtvsrastea, qjgtcewayx => kyndbwu);
  
  -- Single-driven assignments
  oukubnfm <= 'y';
  qk <= qk;
  
  -- Multi-driven assignments
  ghmx <= (others => 'Z');
  kyndbwu <= fjsa;
  ofv <= "";
  fjsa <= (others => 'U');
end hzukxs;



-- Seed after: 4382022819375632962,13843488114570579517

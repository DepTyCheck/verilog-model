-- Seed: 15100086891062885101,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity nha is
  port (ah : inout std_logic_vector(1 downto 3); q : linkage integer; rvnye : inout std_logic);
end nha;

architecture fzuyc of nha is
  
begin
  
end fzuyc;

entity gzmovd is
  port (swkf : linkage time);
end gzmovd;

library ieee;
use ieee.std_logic_1164.all;

architecture wuohhvfege of gzmovd is
  signal ngcbarcx : integer;
  signal zskin : std_logic;
  signal redizltw : integer;
  signal yeblfynj : std_logic_vector(1 downto 3);
begin
  sscwaphauf : entity work.nha
    port map (ah => yeblfynj, q => redizltw, rvnye => zskin);
  nvux : entity work.nha
    port map (ah => yeblfynj, q => ngcbarcx, rvnye => zskin);
  
  -- Multi-driven assignments
  yeblfynj <= yeblfynj;
  zskin <= zskin;
end wuohhvfege;

entity mafs is
  port (posrm : buffer integer; u : inout time);
end mafs;

library ieee;
use ieee.std_logic_1164.all;

architecture scafdbnp of mafs is
  signal rwie : std_logic;
  signal ncbbhkvpo : std_logic_vector(1 downto 3);
begin
  xeazspqv : entity work.nha
    port map (ah => ncbbhkvpo, q => posrm, rvnye => rwie);
  
  -- Multi-driven assignments
  ncbbhkvpo <= ncbbhkvpo;
end scafdbnp;



-- Seed after: 2038867397049648603,12359743974512393525

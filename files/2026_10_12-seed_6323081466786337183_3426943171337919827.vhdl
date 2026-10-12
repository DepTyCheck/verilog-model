-- Seed: 6323081466786337183,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity nmwtj is
  port (uzdehe : buffer std_logic_vector(2 downto 4); t : in time);
end nmwtj;

architecture mptr of nmwtj is
  
begin
  -- Multi-driven assignments
  uzdehe <= (others => '0');
  uzdehe <= uzdehe;
end mptr;

library ieee;
use ieee.std_logic_1164.all;

entity czvhbq is
  port (wnbemrw : out std_logic_vector(4 to 2); jehwjhotqm : inout real);
end czvhbq;

library ieee;
use ieee.std_logic_1164.all;

architecture exbwzz of czvhbq is
  signal sqcmfjfuh : time;
  signal ajkp : std_logic_vector(2 downto 4);
  signal dlomjlh : time;
  signal dxwrflnu : time;
begin
  lomqogwpd : entity work.nmwtj
    port map (uzdehe => wnbemrw, t => dxwrflnu);
  seo : entity work.nmwtj
    port map (uzdehe => wnbemrw, t => dlomjlh);
  pbtbj : entity work.nmwtj
    port map (uzdehe => wnbemrw, t => dxwrflnu);
  knasbwidsc : entity work.nmwtj
    port map (uzdehe => ajkp, t => sqcmfjfuh);
  
  -- Single-driven assignments
  jehwjhotqm <= 03.0303;
  sqcmfjfuh <= dxwrflnu;
  dlomjlh <= dxwrflnu;
  dxwrflnu <= dxwrflnu;
  
  -- Multi-driven assignments
  ajkp <= wnbemrw;
  ajkp <= wnbemrw;
  wnbemrw <= wnbemrw;
end exbwzz;

entity wrycuvzto is
  port (ftcv : out time);
end wrycuvzto;

library ieee;
use ieee.std_logic_1164.all;

architecture lasmx of wrycuvzto is
  signal zgerjbwso : time;
  signal ikhgp : std_logic_vector(2 downto 4);
  signal xlewgszetc : time;
  signal eog : std_logic_vector(2 downto 4);
begin
  jgscxku : entity work.nmwtj
    port map (uzdehe => eog, t => xlewgszetc);
  ixarzz : entity work.nmwtj
    port map (uzdehe => ikhgp, t => zgerjbwso);
  
  -- Single-driven assignments
  ftcv <= 16#2# ms;
  xlewgszetc <= xlewgszetc;
end lasmx;



-- Seed after: 16585787103548971789,3426943171337919827

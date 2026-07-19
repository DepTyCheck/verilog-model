-- Seed: 11055625688988821422,5511103086789671269

entity fse is
  port (s : buffer time);
end fse;

architecture vgla of fse is
  
begin
  -- Single-driven assignments
  s <= s;
end vgla;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port ( bj : linkage std_logic_vector(3 downto 2)
  ; jugemyx : inout string(5 to 5)
  ; ivrnlq : linkage std_logic
  ; xkqnjtc : linkage bit_vector(3 downto 1)
  );
end q;

architecture vxmmjgi of q is
  
begin
  
end vxmmjgi;

library ieee;
use ieee.std_logic_1164.all;

entity imsysomkhr is
  port (nrkoudno : inout std_logic; fs : inout time; drygo : linkage real);
end imsysomkhr;

library ieee;
use ieee.std_logic_1164.all;

architecture naka of imsysomkhr is
  signal tkjopk : time;
  signal nsvozurx : bit_vector(3 downto 1);
  signal otw : std_logic;
  signal bmo : string(5 to 5);
  signal wqgza : std_logic_vector(3 downto 2);
begin
  vov : entity work.q
    port map (bj => wqgza, jugemyx => bmo, ivrnlq => otw, xkqnjtc => nsvozurx);
  hxa : entity work.fse
    port map (s => tkjopk);
  iihc : entity work.fse
    port map (s => fs);
  
  -- Multi-driven assignments
  wqgza <= wqgza;
end naka;

entity baep is
  port (bshpmr : in boolean; hs : linkage time);
end baep;

architecture pdq of baep is
  signal pmphtqhnei : time;
  signal krffcefel : time;
begin
  zphpuwap : entity work.fse
    port map (s => krffcefel);
  djbbp : entity work.fse
    port map (s => pmphtqhnei);
end pdq;



-- Seed after: 3442061808493749701,5511103086789671269

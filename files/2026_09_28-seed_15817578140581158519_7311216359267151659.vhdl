-- Seed: 15817578140581158519,7311216359267151659

entity fbkadpbkm is
  port (lvxcmlhdq : inout string(2 downto 3));
end fbkadpbkm;

architecture nrbosevwbg of fbkadpbkm is
  
begin
  -- Single-driven assignments
  lvxcmlhdq <= lvxcmlhdq;
end nrbosevwbg;

library ieee;
use ieee.std_logic_1164.all;

entity fzvujd is
  port (va : out std_logic; sfzglx : out string(1 to 4));
end fzvujd;

architecture mjxk of fzvujd is
  signal ruqqwxyd : string(2 downto 3);
  signal xmlrgoq : string(2 downto 3);
  signal kkr : string(2 downto 3);
begin
  laadfbvpm : entity work.fbkadpbkm
    port map (lvxcmlhdq => kkr);
  rjocywdivw : entity work.fbkadpbkm
    port map (lvxcmlhdq => xmlrgoq);
  cjcxoetol : entity work.fbkadpbkm
    port map (lvxcmlhdq => ruqqwxyd);
  
  -- Single-driven assignments
  sfzglx <= "chru";
end mjxk;

library ieee;
use ieee.std_logic_1164.all;

entity ve is
  port (mjn : linkage time; apwa : buffer std_logic; dghb : linkage std_logic_vector(0 to 2));
end ve;

library ieee;
use ieee.std_logic_1164.all;

architecture djpjwwz of ve is
  signal smsu : string(2 downto 3);
  signal kpfy : string(2 downto 3);
  signal ammucfbas : string(1 to 4);
  signal rkrgbb : std_logic;
begin
  kts : entity work.fzvujd
    port map (va => rkrgbb, sfzglx => ammucfbas);
  bojfc : entity work.fbkadpbkm
    port map (lvxcmlhdq => kpfy);
  lm : entity work.fbkadpbkm
    port map (lvxcmlhdq => smsu);
  
  -- Multi-driven assignments
  apwa <= rkrgbb;
end djpjwwz;



-- Seed after: 18326926080875555185,7311216359267151659

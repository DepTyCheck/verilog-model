-- Seed: 8132498746176020771,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (yswp : buffer std_logic_vector(4 to 1); cbjideklq : inout std_logic_vector(4 downto 0); rdbf : buffer real; cfiajim : in integer);
end f;

architecture c of f is
  
begin
  -- Multi-driven assignments
  cbjideklq <= "01U-X";
  cbjideklq <= cbjideklq;
  cbjideklq <= cbjideklq;
end c;

entity mmcvo is
  port (b : buffer character; lc : in time);
end mmcvo;

library ieee;
use ieee.std_logic_1164.all;

architecture fh of mmcvo is
  signal ybbremr : integer;
  signal emu : real;
  signal twgshye : std_logic_vector(4 downto 0);
  signal xxwdmxcq : integer;
  signal miasbbzhog : real;
  signal ukk : std_logic_vector(4 downto 0);
  signal ujnvabbm : integer;
  signal pdi : real;
  signal tqfsnb : std_logic_vector(4 downto 0);
  signal eoenqygph : std_logic_vector(4 to 1);
begin
  ikbxh : entity work.f
    port map (yswp => eoenqygph, cbjideklq => tqfsnb, rdbf => pdi, cfiajim => ujnvabbm);
  yvscfuzn : entity work.f
    port map (yswp => eoenqygph, cbjideklq => ukk, rdbf => miasbbzhog, cfiajim => xxwdmxcq);
  brtirnrn : entity work.f
    port map (yswp => eoenqygph, cbjideklq => twgshye, rdbf => emu, cfiajim => ybbremr);
  
  -- Single-driven assignments
  b <= b;
  ybbremr <= 2#0#;
  ujnvabbm <= 34;
  
  -- Multi-driven assignments
  eoenqygph <= eoenqygph;
  ukk <= tqfsnb;
  eoenqygph <= eoenqygph;
end fh;

entity nqyxtfxk is
  port (tpx : buffer bit; da : in integer);
end nqyxtfxk;

library ieee;
use ieee.std_logic_1164.all;

architecture uxiz of nqyxtfxk is
  signal oomnmxjv : real;
  signal lrnqzii : std_logic_vector(4 downto 0);
  signal nteofd : std_logic_vector(4 to 1);
  signal rkipjiag : time;
  signal lnei : character;
begin
  aehupzzu : entity work.mmcvo
    port map (b => lnei, lc => rkipjiag);
  bgxihbqnk : entity work.f
    port map (yswp => nteofd, cbjideklq => lrnqzii, rdbf => oomnmxjv, cfiajim => da);
  
  -- Single-driven assignments
  tpx <= '1';
  rkipjiag <= 010.3440 fs;
  
  -- Multi-driven assignments
  nteofd <= "";
end uxiz;



-- Seed after: 16848231472301310522,12143220691580258643

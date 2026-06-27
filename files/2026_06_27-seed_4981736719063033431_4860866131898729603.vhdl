-- Seed: 4981736719063033431,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (jd : buffer time; tbejvnr : out time; eburgeiabx : buffer std_logic; wzmtnvq : in time);
end k;

architecture rnuvgbp of k is
  
begin
  -- Single-driven assignments
  jd <= 2 min;
  tbejvnr <= 4_4.3_0_1_2_1 fs;
  
  -- Multi-driven assignments
  eburgeiabx <= 'W';
  eburgeiabx <= 'W';
end rnuvgbp;

library ieee;
use ieee.std_logic_1164.all;

entity pgr is
  port (tozxdb : out bit; jdxlsb : buffer std_logic_vector(4 to 1); lsegduxa : linkage boolean);
end pgr;

library ieee;
use ieee.std_logic_1164.all;

architecture fzwoprun of pgr is
  signal orhneimy : time;
  signal pvyw : std_logic;
  signal gnbmmombfr : time;
  signal n : time;
begin
  mjhrqxl : entity work.k
    port map (jd => n, tbejvnr => gnbmmombfr, eburgeiabx => pvyw, wzmtnvq => orhneimy);
  
  -- Multi-driven assignments
  jdxlsb <= (others => '0');
end fzwoprun;

entity z is
  port (lmnqtqi : out time);
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture hnvx of z is
  signal qgfahrl : boolean;
  signal wafqrki : std_logic_vector(4 to 1);
  signal l : bit;
  signal xu : std_logic;
  signal ttl : time;
  signal uwiszhd : time;
  signal wntiz : std_logic;
  signal ahuibmc : time;
begin
  gcq : entity work.k
    port map (jd => ahuibmc, tbejvnr => lmnqtqi, eburgeiabx => wntiz, wzmtnvq => uwiszhd);
  ugklkghvgm : entity work.k
    port map (jd => uwiszhd, tbejvnr => ttl, eburgeiabx => xu, wzmtnvq => lmnqtqi);
  egtdguwq : entity work.pgr
    port map (tozxdb => l, jdxlsb => wafqrki, lsegduxa => qgfahrl);
  
  -- Multi-driven assignments
  wntiz <= 'H';
  wntiz <= '1';
end hnvx;

entity unmxtbkvtf is
  port (d : inout real; l : buffer boolean);
end unmxtbkvtf;

library ieee;
use ieee.std_logic_1164.all;

architecture ulea of unmxtbkvtf is
  signal lwuxkpdmi : std_logic_vector(4 to 1);
  signal lwk : bit;
  signal xuyie : time;
  signal butdnwrxe : std_logic;
  signal qdjrnlrof : time;
  signal skr : time;
  signal oqpyc : time;
begin
  kti : entity work.z
    port map (lmnqtqi => oqpyc);
  yhwxpg : entity work.k
    port map (jd => skr, tbejvnr => qdjrnlrof, eburgeiabx => butdnwrxe, wzmtnvq => xuyie);
  ldaklquhm : entity work.pgr
    port map (tozxdb => lwk, jdxlsb => lwuxkpdmi, lsegduxa => l);
  
  -- Single-driven assignments
  d <= 16#4_A_9_6_5.48#;
  
  -- Multi-driven assignments
  lwuxkpdmi <= (others => '0');
  lwuxkpdmi <= (others => '0');
  butdnwrxe <= 'X';
  butdnwrxe <= 'U';
end ulea;



-- Seed after: 9292777215907974573,4860866131898729603

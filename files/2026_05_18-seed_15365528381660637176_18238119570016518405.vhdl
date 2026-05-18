-- Seed: 15365528381660637176,18238119570016518405



entity ybjlcgo is
  port (qnkueadtom : linkage integer);
end ybjlcgo;



architecture p of ybjlcgo is
  
begin
  
end p;

library ieee;
use ieee.std_logic_1164.all;

entity ffcgaqz is
  port (vnjbdpumv : inout integer_vector(2 downto 1); peujqlk : inout boolean; ynscdt : inout std_logic);
end ffcgaqz;



architecture dbq of ffcgaqz is
  signal thxafm : integer;
  signal tggefmwdz : integer;
  signal eqbsddg : integer;
begin
  gvannr : entity work.ybjlcgo
    port map (qnkueadtom => eqbsddg);
  dibfxlpfxi : entity work.ybjlcgo
    port map (qnkueadtom => tggefmwdz);
  bp : entity work.ybjlcgo
    port map (qnkueadtom => thxafm);
end dbq;



entity t is
  port (x : out integer; hgpbdcrtx : inout integer_vector(4 to 0); jfdj : linkage real_vector(2 to 4));
end t;



architecture qtdt of t is
  
begin
  fuanntpqyu : entity work.ybjlcgo
    port map (qnkueadtom => x);
end qtdt;



entity rbawtqgub is
  port (mnpvqfaqte : linkage time);
end rbawtqgub;

library ieee;
use ieee.std_logic_1164.all;

architecture ckcm of rbawtqgub is
  signal gmymtx : integer;
  signal awwahmmjl : std_logic;
  signal gqmlppmff : boolean;
  signal n : integer_vector(2 downto 1);
  signal fgwdms : real_vector(2 to 4);
  signal d : integer_vector(4 to 0);
  signal hcf : integer;
begin
  lmy : entity work.t
    port map (x => hcf, hgpbdcrtx => d, jfdj => fgwdms);
  ijryeehfr : entity work.ffcgaqz
    port map (vnjbdpumv => n, peujqlk => gqmlppmff, ynscdt => awwahmmjl);
  uetpi : entity work.ybjlcgo
    port map (qnkueadtom => gmymtx);
  gjgyw : entity work.ybjlcgo
    port map (qnkueadtom => hcf);
end ckcm;



-- Seed after: 13582939651909408518,18238119570016518405

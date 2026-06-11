-- Seed: 672841801507024242,11181851762153539145



entity k is
  port (qe : linkage time; bnmyf : linkage real; epq : linkage real_vector(2 to 4));
end k;



architecture ruyrz of k is
  
begin
  
end ruyrz;



entity nliiefyal is
  port (gxsposo : buffer real_vector(3 to 0); pxrmqil : buffer integer; wpif : inout time);
end nliiefyal;



architecture imvrkagl of nliiefyal is
  signal do : real;
  signal l : time;
  signal oitugqhy : real_vector(2 to 4);
  signal da : real;
begin
  kiyceipeaj : entity work.k
    port map (qe => wpif, bnmyf => da, epq => oitugqhy);
  gqqjnzhrwg : entity work.k
    port map (qe => l, bnmyf => do, epq => oitugqhy);
end imvrkagl;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (qsipwgtg : linkage std_logic; fgpaj : buffer real; idosbd : inout character; mgijbfp : inout std_logic);
end p;



architecture x of p is
  signal fuxdmrqdf : real_vector(2 to 4);
  signal ghkcv : time;
  signal cyfmogqlol : integer;
  signal sehzyxn : real_vector(3 to 0);
begin
  bpucr : entity work.nliiefyal
    port map (gxsposo => sehzyxn, pxrmqil => cyfmogqlol, wpif => ghkcv);
  y : entity work.k
    port map (qe => ghkcv, bnmyf => fgpaj, epq => fuxdmrqdf);
end x;



-- Seed after: 17192912003276377945,11181851762153539145

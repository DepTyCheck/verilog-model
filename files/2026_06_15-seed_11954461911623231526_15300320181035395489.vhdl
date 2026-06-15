-- Seed: 11954461911623231526,15300320181035395489

entity s is
  port (b : inout real; xdklcs : out string(1 downto 1); jyeonpcha : in time);
end s;

architecture sj of s is
  
begin
  -- Single-driven assignments
  xdklcs <= "f";
  b <= 2#11.011#;
end sj;

entity qaqbwwjs is
  port (wcz : inout real_vector(4 to 4); c : buffer real; glkgewyc : in integer; tnozanlr : in real);
end qaqbwwjs;

architecture gsxzq of qaqbwwjs is
  signal n : time;
  signal cg : string(1 downto 1);
  signal slaugwpqf : string(1 downto 1);
  signal tfzein : real;
  signal apnz : string(1 downto 1);
  signal fjgcmahknu : real;
  signal vz : time;
  signal rik : string(1 downto 1);
  signal bbw : real;
begin
  bwvvwxiwkc : entity work.s
    port map (b => bbw, xdklcs => rik, jyeonpcha => vz);
  rwbgvv : entity work.s
    port map (b => fjgcmahknu, xdklcs => apnz, jyeonpcha => vz);
  hbfeviv : entity work.s
    port map (b => tfzein, xdklcs => slaugwpqf, jyeonpcha => vz);
  mulzpkfrd : entity work.s
    port map (b => c, xdklcs => cg, jyeonpcha => n);
end gsxzq;

library ieee;
use ieee.std_logic_1164.all;

entity zlorko is
  port (wxqywauwj : inout real_vector(0 downto 1); btggb : inout integer; xixux : inout std_logic_vector(3 downto 2));
end zlorko;

architecture wshfr of zlorko is
  signal gtmuz : real;
  signal cbdezngafh : real_vector(4 to 4);
  signal ghkcemfoh : time;
  signal fei : string(1 downto 1);
  signal riqxhfsxd : real;
  signal l : time;
  signal yyvsmrc : string(1 downto 1);
  signal rdfecwvdr : real;
begin
  hddm : entity work.s
    port map (b => rdfecwvdr, xdklcs => yyvsmrc, jyeonpcha => l);
  wdxlwtabvq : entity work.s
    port map (b => riqxhfsxd, xdklcs => fei, jyeonpcha => ghkcemfoh);
  etzctlkfv : entity work.qaqbwwjs
    port map (wcz => cbdezngafh, c => gtmuz, glkgewyc => btggb, tnozanlr => rdfecwvdr);
  
  -- Single-driven assignments
  btggb <= 0;
  l <= 8#322.4470# ns;
  wxqywauwj <= (others => 0.0);
end wshfr;



-- Seed after: 14330198878445680208,15300320181035395489

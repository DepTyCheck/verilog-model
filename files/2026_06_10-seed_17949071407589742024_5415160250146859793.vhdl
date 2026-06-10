-- Seed: 17949071407589742024,5415160250146859793



entity cwn is
  port (zuatelx : buffer time; mdhtzcvqtf : linkage real; ogolz : in boolean; xvduxzbu : buffer integer);
end cwn;



architecture lvoijtq of cwn is
  
begin
  
end lvoijtq;

library ieee;
use ieee.std_logic_1164.all;

entity iixiwytc is
  port (kywhuoirir : in std_logic_vector(1 to 3); bpsphzvqz : inout time; rryqmap : linkage time);
end iixiwytc;



architecture vtginorfi of iixiwytc is
  signal ho : integer;
  signal qpiopjrkui : boolean;
  signal b : real;
begin
  sezmrh : entity work.cwn
    port map (zuatelx => bpsphzvqz, mdhtzcvqtf => b, ogolz => qpiopjrkui, xvduxzbu => ho);
end vtginorfi;



entity xhdqg is
  port (ndxgyf : linkage integer);
end xhdqg;

library ieee;
use ieee.std_logic_1164.all;

architecture xpzq of xhdqg is
  signal wdmqavmlak : integer;
  signal mfwkvxnmik : time;
  signal ergqmulz : integer;
  signal xjpddzqlk : boolean;
  signal mfqqbxlw : real;
  signal clkrg : time;
  signal yatzvayrht : std_logic_vector(1 to 3);
  signal aoiyix : time;
  signal jznq : time;
  signal j : std_logic_vector(1 to 3);
begin
  v : entity work.iixiwytc
    port map (kywhuoirir => j, bpsphzvqz => jznq, rryqmap => aoiyix);
  q : entity work.iixiwytc
    port map (kywhuoirir => yatzvayrht, bpsphzvqz => clkrg, rryqmap => clkrg);
  iqjhfz : entity work.cwn
    port map (zuatelx => aoiyix, mdhtzcvqtf => mfqqbxlw, ogolz => xjpddzqlk, xvduxzbu => ergqmulz);
  yjbrzpnln : entity work.cwn
    port map (zuatelx => mfwkvxnmik, mdhtzcvqtf => mfqqbxlw, ogolz => xjpddzqlk, xvduxzbu => wdmqavmlak);
end xpzq;



-- Seed after: 7331479456978827023,5415160250146859793

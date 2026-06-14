-- Seed: 5623990358469670303,6298042963991371649



entity sstxbsqnu is
  port (adaxbzvn : out boolean; wpkyevcu : inout bit_vector(0 downto 3));
end sstxbsqnu;



architecture llp of sstxbsqnu is
  
begin
  
end llp;



entity ullrk is
  port (eijrbs : in boolean_vector(2 to 4));
end ullrk;



architecture xhimoncbdr of ullrk is
  signal vcfkw : bit_vector(0 downto 3);
  signal sjrxa : boolean;
  signal gnj : bit_vector(0 downto 3);
  signal pwkeaxmwa : boolean;
  signal q : bit_vector(0 downto 3);
  signal qrlbmiqszm : boolean;
begin
  lg : entity work.sstxbsqnu
    port map (adaxbzvn => qrlbmiqszm, wpkyevcu => q);
  xeitbuwbmx : entity work.sstxbsqnu
    port map (adaxbzvn => pwkeaxmwa, wpkyevcu => gnj);
  ecfphnlc : entity work.sstxbsqnu
    port map (adaxbzvn => sjrxa, wpkyevcu => vcfkw);
end xhimoncbdr;



entity hjidrod is
  port (hrie : linkage real);
end hjidrod;



architecture fjuzuxa of hjidrod is
  signal lgkehc : boolean_vector(2 to 4);
  signal pqroi : boolean_vector(2 to 4);
  signal e : bit_vector(0 downto 3);
  signal jdji : boolean;
begin
  cj : entity work.sstxbsqnu
    port map (adaxbzvn => jdji, wpkyevcu => e);
  zqzwnhwfm : entity work.ullrk
    port map (eijrbs => pqroi);
  mx : entity work.ullrk
    port map (eijrbs => lgkehc);
end fjuzuxa;

library ieee;
use ieee.std_logic_1164.all;

entity iztc is
  port (hneci : buffer std_logic_vector(0 to 1); mnbtmiz : in severity_level);
end iztc;



architecture ildkicqkv of iztc is
  signal e : real;
begin
  bhcoz : entity work.hjidrod
    port map (hrie => e);
end ildkicqkv;



-- Seed after: 13711723419606619787,6298042963991371649

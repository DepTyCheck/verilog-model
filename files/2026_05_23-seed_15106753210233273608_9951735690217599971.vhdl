-- Seed: 15106753210233273608,9951735690217599971

library ieee;
use ieee.std_logic_1164.all;

entity ajsu is
  port (aci : linkage boolean_vector(1 downto 0); whclhcrs : linkage integer; phbss : linkage std_logic_vector(4 to 2); gzgeclypkv : in std_logic);
end ajsu;



architecture csl of ajsu is
  
begin
  
end csl;



entity fdshiywxkp is
  port (hquwafkni : in integer; ffqwfits : linkage bit_vector(3 to 0); fxnfjr : out integer; mti : linkage boolean_vector(1 downto 0));
end fdshiywxkp;



architecture m of fdshiywxkp is
  
begin
  
end m;

library ieee;
use ieee.std_logic_1164.all;

entity gv is
  port (zfhmgi : in std_logic_vector(3 downto 0); mrznbwpt : out real);
end gv;

library ieee;
use ieee.std_logic_1164.all;

architecture ppccymz of gv is
  signal imvfz : std_logic;
  signal eibahbxa : std_logic_vector(4 to 2);
  signal acllbopzv : integer;
  signal brjhufuliy : boolean_vector(1 downto 0);
begin
  mctcabksok : entity work.ajsu
    port map (aci => brjhufuliy, whclhcrs => acllbopzv, phbss => eibahbxa, gzgeclypkv => imvfz);
end ppccymz;



entity fphr is
  port (tedbjvvit : out character; pfxplt : buffer real; mtuiqcw : linkage time_vector(1 to 2));
end fphr;

library ieee;
use ieee.std_logic_1164.all;

architecture wpil of fphr is
  signal yznpe : integer;
  signal qk : bit_vector(3 to 0);
  signal dsuugwqbzc : std_logic_vector(3 downto 0);
  signal zoty : boolean_vector(1 downto 0);
  signal dpnhbi : integer;
  signal qkfajkw : bit_vector(3 to 0);
  signal s : integer;
  signal uffu : real;
  signal eyclt : std_logic_vector(3 downto 0);
begin
  dgfkk : entity work.gv
    port map (zfhmgi => eyclt, mrznbwpt => uffu);
  b : entity work.fdshiywxkp
    port map (hquwafkni => s, ffqwfits => qkfajkw, fxnfjr => dpnhbi, mti => zoty);
  hcrmftf : entity work.gv
    port map (zfhmgi => dsuugwqbzc, mrznbwpt => pfxplt);
  uweisj : entity work.fdshiywxkp
    port map (hquwafkni => dpnhbi, ffqwfits => qk, fxnfjr => yznpe, mti => zoty);
end wpil;



-- Seed after: 4916451028796333300,9951735690217599971

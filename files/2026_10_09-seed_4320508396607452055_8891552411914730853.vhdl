-- Seed: 4320508396607452055,8891552411914730853

entity r is
  port (kvkpfrmdbv : linkage real; sj : linkage time; niefbwew : inout time);
end r;

architecture ritr of r is
  
begin
  -- Single-driven assignments
  niefbwew <= 12 fs;
end ritr;

library ieee;
use ieee.std_logic_1164.all;

entity sjnbmjrmw is
  port (hs : inout std_logic_vector(2 to 3); xddnba : out real);
end sjnbmjrmw;

architecture ypewfmx of sjnbmjrmw is
  signal qamaukap : time;
  signal yrgqzlgok : time;
  signal zxoyncd : time;
  signal itvux : time;
  signal pb : real;
begin
  akzorpcyp : entity work.r
    port map (kvkpfrmdbv => pb, sj => itvux, niefbwew => zxoyncd);
  eqiy : entity work.r
    port map (kvkpfrmdbv => xddnba, sj => yrgqzlgok, niefbwew => qamaukap);
  
  -- Multi-driven assignments
  hs <= "01";
  hs <= hs;
  hs <= hs;
  hs <= hs;
end ypewfmx;

entity fhbv is
  port (jhaqmcq : buffer time; fnyrkwwak : buffer integer; xcvn : buffer integer);
end fhbv;

library ieee;
use ieee.std_logic_1164.all;

architecture irucqitp of fhbv is
  signal zqkwa : time;
  signal dfhouvxob : real;
  signal fxh : time;
  signal kbjafuckfz : time;
  signal nnp : real;
  signal vuswkyoyde : time;
  signal zkknn : time;
  signal snpstjns : real;
  signal gxtljajh : real;
  signal ukqxwja : std_logic_vector(2 to 3);
begin
  uefijrh : entity work.sjnbmjrmw
    port map (hs => ukqxwja, xddnba => gxtljajh);
  euro : entity work.r
    port map (kvkpfrmdbv => snpstjns, sj => zkknn, niefbwew => vuswkyoyde);
  shkksgxk : entity work.r
    port map (kvkpfrmdbv => nnp, sj => kbjafuckfz, niefbwew => fxh);
  kvyrkcpid : entity work.r
    port map (kvkpfrmdbv => dfhouvxob, sj => zqkwa, niefbwew => jhaqmcq);
  
  -- Single-driven assignments
  xcvn <= xcvn;
  fnyrkwwak <= xcvn;
end irucqitp;



-- Seed after: 9950735569764852768,8891552411914730853

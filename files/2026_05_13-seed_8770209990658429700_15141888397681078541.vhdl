-- Seed: 8770209990658429700,15141888397681078541

library ieee;
use ieee.std_logic_1164.all;

entity mht is
  port (eilqu : linkage integer; rrkvj : inout integer; nzncqczxr : out time; jlarj : inout std_logic);
end mht;



architecture oerk of mht is
  
begin
  
end oerk;



entity mzfu is
  port (ipwiiyh : inout boolean; fzgejj : linkage real);
end mzfu;

library ieee;
use ieee.std_logic_1164.all;

architecture jybq of mzfu is
  signal yeyicoi : std_logic;
  signal deo : time;
  signal pjix : integer;
begin
  jubpljjxwg : entity work.mht
    port map (eilqu => pjix, rrkvj => pjix, nzncqczxr => deo, jlarj => yeyicoi);
end jybq;



entity da is
  port (m : out character; umiyfsstrx : inout integer; fpifghtaol : buffer time; n : in real);
end da;

library ieee;
use ieee.std_logic_1164.all;

architecture cbwpm of da is
  signal uedj : boolean;
  signal nhryyi : std_logic;
  signal zmnhik : integer;
begin
  x : entity work.mht
    port map (eilqu => umiyfsstrx, rrkvj => zmnhik, nzncqczxr => fpifghtaol, jlarj => nhryyi);
  iybwh : entity work.mzfu
    port map (ipwiiyh => uedj, fzgejj => n);
end cbwpm;



entity kd is
  port (ncymjf : inout time; vmwuoiga : buffer character; mg : linkage severity_level; ppjebd : inout boolean);
end kd;

library ieee;
use ieee.std_logic_1164.all;

architecture sktp of kd is
  signal fs : real;
  signal tbgydb : std_logic;
  signal ocdxgjvnpb : time;
  signal wpbsjf : integer;
  signal hdtdo : integer;
  signal yofanoo : std_logic;
  signal gurqgtvl : time;
  signal zdepa : integer;
  signal irczeau : real;
  signal kxntpmvn : integer;
begin
  rdvscjzm : entity work.da
    port map (m => vmwuoiga, umiyfsstrx => kxntpmvn, fpifghtaol => ncymjf, n => irczeau);
  ceau : entity work.mht
    port map (eilqu => kxntpmvn, rrkvj => zdepa, nzncqczxr => gurqgtvl, jlarj => yofanoo);
  zghwh : entity work.mht
    port map (eilqu => hdtdo, rrkvj => wpbsjf, nzncqczxr => ocdxgjvnpb, jlarj => tbgydb);
  kmbdfndmk : entity work.mzfu
    port map (ipwiiyh => ppjebd, fzgejj => fs);
end sktp;



-- Seed after: 11110275146322092574,15141888397681078541

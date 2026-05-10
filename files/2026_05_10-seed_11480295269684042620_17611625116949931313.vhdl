-- Seed: 11480295269684042620,17611625116949931313

library ieee;
use ieee.std_logic_1164.all;

entity mpgbdsif is
  port (fhluurd : linkage boolean; lhow : in integer; shuiyajog : out std_logic);
end mpgbdsif;



architecture bnxlytd of mpgbdsif is
  
begin
  
end bnxlytd;



entity seog is
  port (aksevgfabd : in real);
end seog;

library ieee;
use ieee.std_logic_1164.all;

architecture uxtr of seog is
  signal rp : integer;
  signal lnubvlw : boolean;
  signal ryzedb : std_logic;
  signal ar : integer;
  signal niwpgymh : boolean;
begin
  wgmmoqt : entity work.mpgbdsif
    port map (fhluurd => niwpgymh, lhow => ar, shuiyajog => ryzedb);
  ste : entity work.mpgbdsif
    port map (fhluurd => lnubvlw, lhow => rp, shuiyajog => ryzedb);
end uxtr;



entity huzw is
  port (ewchnkmgpk : out bit);
end huzw;

library ieee;
use ieee.std_logic_1164.all;

architecture mfyzsag of huzw is
  signal exxmrpve : real;
  signal itucrfabp : std_logic;
  signal oaggrzu : integer;
  signal iwt : boolean;
begin
  kl : entity work.mpgbdsif
    port map (fhluurd => iwt, lhow => oaggrzu, shuiyajog => itucrfabp);
  njhqcit : entity work.seog
    port map (aksevgfabd => exxmrpve);
  rgn : entity work.mpgbdsif
    port map (fhluurd => iwt, lhow => oaggrzu, shuiyajog => itucrfabp);
end mfyzsag;



entity ewsl is
  port (pz : in integer; mjtonvwe : buffer integer);
end ewsl;

library ieee;
use ieee.std_logic_1164.all;

architecture xypplh of ewsl is
  signal fuynhx : bit;
  signal vg : integer;
  signal orqdycwxjk : boolean;
  signal geqe : std_logic;
  signal tl : integer;
  signal qkhtvrx : boolean;
begin
  zns : entity work.mpgbdsif
    port map (fhluurd => qkhtvrx, lhow => tl, shuiyajog => geqe);
  ikuk : entity work.mpgbdsif
    port map (fhluurd => orqdycwxjk, lhow => vg, shuiyajog => geqe);
  fkql : entity work.huzw
    port map (ewchnkmgpk => fuynhx);
end xypplh;



-- Seed after: 13275671595786932821,17611625116949931313

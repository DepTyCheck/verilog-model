-- Seed: 14562366383896745803,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity ixcfbmuve is
  port (bnncvc : linkage std_logic_vector(1 to 2); gfjvneuwv : inout std_logic; ema : linkage real);
end ixcfbmuve;

architecture poykznog of ixcfbmuve is
  
begin
  -- Multi-driven assignments
  gfjvneuwv <= 'H';
  gfjvneuwv <= 'L';
  gfjvneuwv <= 'Z';
end poykznog;

entity pyzqgt is
  port (brxmdkcr : in integer_vector(1 downto 0));
end pyzqgt;

library ieee;
use ieee.std_logic_1164.all;

architecture tknxfna of pyzqgt is
  signal ovlpaestoj : real;
  signal qqzwatwx : std_logic;
  signal fnw : std_logic_vector(1 to 2);
  signal jdpopufe : real;
  signal mvmge : std_logic;
  signal qur : std_logic_vector(1 to 2);
  signal n : real;
  signal ujy : real;
  signal fwhpzashz : std_logic;
  signal ryokv : std_logic_vector(1 to 2);
begin
  wu : entity work.ixcfbmuve
    port map (bnncvc => ryokv, gfjvneuwv => fwhpzashz, ema => ujy);
  uq : entity work.ixcfbmuve
    port map (bnncvc => ryokv, gfjvneuwv => fwhpzashz, ema => n);
  lxxdlz : entity work.ixcfbmuve
    port map (bnncvc => qur, gfjvneuwv => mvmge, ema => jdpopufe);
  c : entity work.ixcfbmuve
    port map (bnncvc => fnw, gfjvneuwv => qqzwatwx, ema => ovlpaestoj);
  
  -- Multi-driven assignments
  qqzwatwx <= fwhpzashz;
end tknxfna;

entity frfok is
  port (ppbgtd : buffer time; cuzvgur : out character);
end frfok;

library ieee;
use ieee.std_logic_1164.all;

architecture bqincwxpf of frfok is
  signal catrhzd : integer_vector(1 downto 0);
  signal wfpoiysxnu : real;
  signal jyfkx : std_logic;
  signal rgoygdcb : std_logic_vector(1 to 2);
begin
  z : entity work.ixcfbmuve
    port map (bnncvc => rgoygdcb, gfjvneuwv => jyfkx, ema => wfpoiysxnu);
  qzektc : entity work.pyzqgt
    port map (brxmdkcr => catrhzd);
  dkfxroug : entity work.pyzqgt
    port map (brxmdkcr => catrhzd);
  
  -- Single-driven assignments
  catrhzd <= (2_1, 4_0_2_0);
  ppbgtd <= 16#D40E4# ps;
  cuzvgur <= cuzvgur;
  
  -- Multi-driven assignments
  rgoygdcb <= rgoygdcb;
  rgoygdcb <= ('H', 'L');
end bqincwxpf;



-- Seed after: 14538848765261622414,7304262412290825129

-- Seed: 17986320447375505591,3924983747739634027

entity twycwhh is
  port (tf : linkage real; kjlyczgvq : buffer bit);
end twycwhh;

architecture oas of twycwhh is
  
begin
  -- Single-driven assignments
  kjlyczgvq <= '1';
end oas;

library ieee;
use ieee.std_logic_1164.all;

entity fp is
  port (cfnjuxqg : buffer std_logic; ymfsncwars : inout time; rvjkp : inout time; t : inout boolean);
end fp;

architecture ubbrzexlqk of fp is
  signal xlvkpup : bit;
  signal dpiuda : real;
  signal zdkx : bit;
  signal dyiztzznhh : real;
begin
  zjcnhqz : entity work.twycwhh
    port map (tf => dyiztzznhh, kjlyczgvq => zdkx);
  lkmbeadahd : entity work.twycwhh
    port map (tf => dpiuda, kjlyczgvq => xlvkpup);
  
  -- Single-driven assignments
  rvjkp <= 2#10000.11100# us;
  t <= FALSE;
  
  -- Multi-driven assignments
  cfnjuxqg <= 'X';
end ubbrzexlqk;

library ieee;
use ieee.std_logic_1164.all;

entity iicie is
  port (rfcgvu : inout integer_vector(3 to 0); qhmpveyt : buffer std_logic; b : linkage std_logic_vector(2 to 1); fz : in std_logic_vector(3 downto 1));
end iicie;

architecture zukvxxwolg of iicie is
  signal f : bit;
  signal wbaexsg : real;
  signal zfpx : bit;
  signal xsv : real;
  signal isnnvuwngb : bit;
  signal lv : real;
begin
  bpiyuofymc : entity work.twycwhh
    port map (tf => lv, kjlyczgvq => isnnvuwngb);
  aew : entity work.twycwhh
    port map (tf => xsv, kjlyczgvq => zfpx);
  ddzanuohdb : entity work.twycwhh
    port map (tf => wbaexsg, kjlyczgvq => f);
  
  -- Single-driven assignments
  rfcgvu <= (others => 0);
end zukvxxwolg;

entity bn is
  port (brrflyj : buffer time; qfnvvv : buffer bit; pwjzvk : linkage real);
end bn;

library ieee;
use ieee.std_logic_1164.all;

architecture xvxuytdhta of bn is
  signal mvgttqgy : bit;
  signal iauxpccp : integer_vector(3 to 0);
  signal bgudhcqe : std_logic_vector(3 downto 1);
  signal kljg : std_logic_vector(2 to 1);
  signal uhebsh : std_logic;
  signal rkvuzcud : integer_vector(3 to 0);
begin
  wixlrk : entity work.iicie
    port map (rfcgvu => rkvuzcud, qhmpveyt => uhebsh, b => kljg, fz => bgudhcqe);
  tlsim : entity work.iicie
    port map (rfcgvu => iauxpccp, qhmpveyt => uhebsh, b => kljg, fz => bgudhcqe);
  qovjbhscsm : entity work.twycwhh
    port map (tf => pwjzvk, kjlyczgvq => mvgttqgy);
  
  -- Single-driven assignments
  qfnvvv <= '1';
  brrflyj <= 1 hr;
  
  -- Multi-driven assignments
  uhebsh <= 'H';
  uhebsh <= 'U';
  uhebsh <= 'U';
  kljg <= "";
end xvxuytdhta;



-- Seed after: 12714381390345176510,3924983747739634027

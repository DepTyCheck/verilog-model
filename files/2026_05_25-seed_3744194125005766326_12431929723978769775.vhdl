-- Seed: 3744194125005766326,12431929723978769775

library ieee;
use ieee.std_logic_1164.all;

entity dqbjxotz is
  port (iyvuw : in time; vsoimgi : in boolean; q : linkage std_logic_vector(0 downto 2); cctxrkd : in integer);
end dqbjxotz;



architecture oejrisofej of dqbjxotz is
  
begin
  
end oejrisofej;

library ieee;
use ieee.std_logic_1164.all;

entity khj is
  port (decdzlcq : in integer; sxpesrafy : out time; xhxpeed : buffer std_logic_vector(0 downto 2));
end khj;



architecture itljl of khj is
  signal l : boolean;
begin
  pukmwmzrja : entity work.dqbjxotz
    port map (iyvuw => sxpesrafy, vsoimgi => l, q => xhxpeed, cctxrkd => decdzlcq);
end itljl;

library ieee;
use ieee.std_logic_1164.all;

entity oxjdgeqjt is
  port (vozbfs : buffer std_logic; dcfllo : inout std_logic_vector(4 downto 0); ul : in time);
end oxjdgeqjt;

library ieee;
use ieee.std_logic_1164.all;

architecture cgkfnex of oxjdgeqjt is
  signal cbdab : integer;
  signal ruivmetu : integer;
  signal bdpqjmstlo : integer;
  signal inklddmaa : std_logic_vector(0 downto 2);
  signal uatsvw : boolean;
  signal wmcwbms : time;
begin
  i : entity work.dqbjxotz
    port map (iyvuw => wmcwbms, vsoimgi => uatsvw, q => inklddmaa, cctxrkd => bdpqjmstlo);
  p : entity work.dqbjxotz
    port map (iyvuw => wmcwbms, vsoimgi => uatsvw, q => inklddmaa, cctxrkd => ruivmetu);
  ougosoaioq : entity work.khj
    port map (decdzlcq => cbdab, sxpesrafy => wmcwbms, xhxpeed => inklddmaa);
end cgkfnex;



-- Seed after: 18303833795933967933,12431929723978769775

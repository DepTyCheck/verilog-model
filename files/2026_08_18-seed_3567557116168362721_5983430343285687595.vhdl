-- Seed: 3567557116168362721,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity ehy is
  port (pxrnr : inout integer; qd : buffer std_logic_vector(0 downto 2));
end ehy;

architecture orb of ehy is
  
begin
  -- Multi-driven assignments
  qd <= (others => '0');
  qd <= qd;
  qd <= qd;
end orb;

library ieee;
use ieee.std_logic_1164.all;

entity vvummdm is
  port (llhr : inout bit; xczfue : linkage time; fyrestcuwh : buffer character; qykmduqffl : buffer std_logic);
end vvummdm;

architecture v of vvummdm is
  
begin
  -- Multi-driven assignments
  qykmduqffl <= qykmduqffl;
end v;

entity rkedp is
  port (jcrcm : linkage time_vector(4 downto 1));
end rkedp;

library ieee;
use ieee.std_logic_1164.all;

architecture bbu of rkedp is
  signal j : integer;
  signal nlyjeelxa : std_logic;
  signal woyfbs : character;
  signal pnhjlfjiwn : time;
  signal wklv : bit;
  signal kil : std_logic_vector(0 downto 2);
  signal shegna : integer;
begin
  lwcu : entity work.ehy
    port map (pxrnr => shegna, qd => kil);
  myzxgyugyx : entity work.vvummdm
    port map (llhr => wklv, xczfue => pnhjlfjiwn, fyrestcuwh => woyfbs, qykmduqffl => nlyjeelxa);
  jucdm : entity work.ehy
    port map (pxrnr => j, qd => kil);
  
  -- Multi-driven assignments
  kil <= (others => '0');
  kil <= (others => '0');
end bbu;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (t : in std_logic; phdd : out real; bxsp : out severity_level);
end m;

architecture ytfnhcjmaq of m is
  
begin
  -- Single-driven assignments
  phdd <= 2#1.00#;
  bxsp <= bxsp;
end ytfnhcjmaq;



-- Seed after: 3102527548092493184,5983430343285687595

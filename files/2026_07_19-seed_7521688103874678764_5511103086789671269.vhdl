-- Seed: 7521688103874678764,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity ngbaot is
  port (iyfz : in std_logic_vector(2 downto 3); yfmrctejr : inout std_logic; mjesoexll : inout integer; gxe : linkage std_logic_vector(4 to 3));
end ngbaot;

architecture vjzobywxow of ngbaot is
  
begin
  -- Single-driven assignments
  mjesoexll <= 1;
end vjzobywxow;

entity jep is
  port (fugrwa : in time);
end jep;

library ieee;
use ieee.std_logic_1164.all;

architecture zwqgpd of jep is
  signal rrxza : integer;
  signal l : std_logic;
  signal f : std_logic_vector(2 downto 3);
  signal zjezokxwh : std_logic_vector(4 to 3);
  signal ovrycct : integer;
  signal epca : std_logic;
  signal smwguonjow : integer;
  signal lle : std_logic;
  signal ftoq : std_logic_vector(4 to 3);
begin
  asqsxnh : entity work.ngbaot
    port map (iyfz => ftoq, yfmrctejr => lle, mjesoexll => smwguonjow, gxe => ftoq);
  qcdscrvzkn : entity work.ngbaot
    port map (iyfz => ftoq, yfmrctejr => epca, mjesoexll => ovrycct, gxe => zjezokxwh);
  bejspc : entity work.ngbaot
    port map (iyfz => f, yfmrctejr => l, mjesoexll => rrxza, gxe => ftoq);
  
  -- Multi-driven assignments
  lle <= 'H';
end zwqgpd;

entity wwns is
  port (o : buffer real_vector(0 to 1));
end wwns;

library ieee;
use ieee.std_logic_1164.all;

architecture eory of wwns is
  signal xaugp : integer;
  signal oacprgjmg : std_logic;
  signal nfensul : std_logic_vector(4 to 3);
begin
  pol : entity work.ngbaot
    port map (iyfz => nfensul, yfmrctejr => oacprgjmg, mjesoexll => xaugp, gxe => nfensul);
  
  -- Single-driven assignments
  o <= o;
  
  -- Multi-driven assignments
  nfensul <= "";
  nfensul <= nfensul;
end eory;

entity kz is
  port (ybd : buffer bit_vector(1 downto 1));
end kz;

library ieee;
use ieee.std_logic_1164.all;

architecture d of kz is
  signal qmlapr : std_logic_vector(4 to 3);
  signal ngsxjqcnz : integer;
  signal sxpmq : std_logic;
  signal wyhptp : std_logic_vector(4 to 3);
  signal nudzsmels : integer;
  signal frtjca : real_vector(0 to 1);
  signal lixjaos : std_logic_vector(4 to 3);
  signal iox : integer;
  signal rfur : std_logic;
  signal t : std_logic_vector(2 downto 3);
begin
  lpcry : entity work.ngbaot
    port map (iyfz => t, yfmrctejr => rfur, mjesoexll => iox, gxe => lixjaos);
  zbc : entity work.wwns
    port map (o => frtjca);
  erhb : entity work.ngbaot
    port map (iyfz => t, yfmrctejr => rfur, mjesoexll => nudzsmels, gxe => wyhptp);
  qocohscwoo : entity work.ngbaot
    port map (iyfz => t, yfmrctejr => sxpmq, mjesoexll => ngsxjqcnz, gxe => qmlapr);
  
  -- Single-driven assignments
  ybd <= ybd;
  
  -- Multi-driven assignments
  rfur <= rfur;
  qmlapr <= (others => '0');
  wyhptp <= t;
end d;



-- Seed after: 5403367864322109900,5511103086789671269

-- Seed: 12216271215687959392,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity lcx is
  port (a : inout time; sffvfku : buffer time_vector(0 to 3); nfih : out integer; pfhrtjhbn : inout std_logic_vector(2 to 2));
end lcx;

architecture sjv of lcx is
  
begin
  -- Single-driven assignments
  nfih <= 2#1_1#;
  a <= 3043.042 ps;
  sffvfku <= (8#2_2_4_1# ps, 16#348# fs, 2 min, 3321 ms);
  
  -- Multi-driven assignments
  pfhrtjhbn <= (others => 'U');
end sjv;

library ieee;
use ieee.std_logic_1164.all;

entity ifrpsk is
  port (jekmsahhu : inout real; fxkgkwaakl : linkage real; tayzwb : buffer std_logic);
end ifrpsk;

library ieee;
use ieee.std_logic_1164.all;

architecture hqfpkilbtj of ifrpsk is
  signal oluwfrdbo : integer;
  signal hvrqtnkqps : time_vector(0 to 3);
  signal frpi : time;
  signal z : integer;
  signal ocwuhie : time_vector(0 to 3);
  signal ogqnu : time;
  signal gusefml : std_logic_vector(2 to 2);
  signal jmhcxrulx : integer;
  signal vms : time_vector(0 to 3);
  signal vokk : time;
  signal ix : std_logic_vector(2 to 2);
  signal umrmthaz : integer;
  signal jgkgb : time_vector(0 to 3);
  signal x : time;
begin
  pvwkusy : entity work.lcx
    port map (a => x, sffvfku => jgkgb, nfih => umrmthaz, pfhrtjhbn => ix);
  oezyopneki : entity work.lcx
    port map (a => vokk, sffvfku => vms, nfih => jmhcxrulx, pfhrtjhbn => gusefml);
  njdqdl : entity work.lcx
    port map (a => ogqnu, sffvfku => ocwuhie, nfih => z, pfhrtjhbn => ix);
  gzetzm : entity work.lcx
    port map (a => frpi, sffvfku => hvrqtnkqps, nfih => oluwfrdbo, pfhrtjhbn => ix);
  
  -- Single-driven assignments
  jekmsahhu <= 8#6.77123#;
  
  -- Multi-driven assignments
  tayzwb <= 'W';
  gusefml <= "1";
  gusefml <= (others => '0');
end hqfpkilbtj;

library ieee;
use ieee.std_logic_1164.all;

entity tgndaty is
  port (co : in std_logic_vector(4 downto 4));
end tgndaty;

library ieee;
use ieee.std_logic_1164.all;

architecture bgbmwhvbw of tgndaty is
  signal ormzxownwj : integer;
  signal quynthoqeb : time_vector(0 to 3);
  signal szutveyjlv : time;
  signal llfjqgjfrn : std_logic_vector(2 to 2);
  signal p : integer;
  signal enoebkfec : time_vector(0 to 3);
  signal mubtxlap : time;
  signal mvlnuyubti : std_logic;
  signal snkaqqyn : real;
  signal wgqjc : real;
begin
  lkwfcajvel : entity work.ifrpsk
    port map (jekmsahhu => wgqjc, fxkgkwaakl => snkaqqyn, tayzwb => mvlnuyubti);
  wit : entity work.lcx
    port map (a => mubtxlap, sffvfku => enoebkfec, nfih => p, pfhrtjhbn => llfjqgjfrn);
  bcokvzg : entity work.lcx
    port map (a => szutveyjlv, sffvfku => quynthoqeb, nfih => ormzxownwj, pfhrtjhbn => llfjqgjfrn);
end bgbmwhvbw;



-- Seed after: 9090012397550989168,8118127366649987907

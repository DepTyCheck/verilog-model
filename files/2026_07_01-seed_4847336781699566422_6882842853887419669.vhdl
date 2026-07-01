-- Seed: 4847336781699566422,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity virmrtg is
  port (wpfj : linkage character; vovdksf : in std_logic; pubuock : linkage severity_level; cltqoqsa : in time_vector(4 to 0));
end virmrtg;

architecture znuv of virmrtg is
  
begin
  
end znuv;

entity fm is
  port (rnmtu : inout integer);
end fm;

library ieee;
use ieee.std_logic_1164.all;

architecture fgufoeza of fm is
  signal yo : time_vector(4 to 0);
  signal qnswtdu : severity_level;
  signal mhzukkzgmb : character;
  signal siaiwfrvu : time_vector(4 to 0);
  signal zees : severity_level;
  signal xs : std_logic;
  signal uwjua : character;
begin
  liqwnb : entity work.virmrtg
    port map (wpfj => uwjua, vovdksf => xs, pubuock => zees, cltqoqsa => siaiwfrvu);
  mcay : entity work.virmrtg
    port map (wpfj => mhzukkzgmb, vovdksf => xs, pubuock => qnswtdu, cltqoqsa => yo);
  
  -- Single-driven assignments
  siaiwfrvu <= (others => 0 ns);
  yo <= (others => 0 ns);
  rnmtu <= 2#0#;
  
  -- Multi-driven assignments
  xs <= 'U';
  xs <= 'W';
  xs <= 'X';
end fgufoeza;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (t : out boolean_vector(4 to 1); q : linkage std_logic; wfrnrodto : buffer integer; i : in real);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture pnicxtkk of o is
  signal lr : time_vector(4 to 0);
  signal atliukypw : severity_level;
  signal wqnjdzulj : std_logic;
  signal hnzs : character;
  signal hjfpfmmu : integer;
  signal ss : integer;
begin
  idphvlyiox : entity work.fm
    port map (rnmtu => ss);
  scnaclnkpg : entity work.fm
    port map (rnmtu => hjfpfmmu);
  osjbphaht : entity work.virmrtg
    port map (wpfj => hnzs, vovdksf => wqnjdzulj, pubuock => atliukypw, cltqoqsa => lr);
  
  -- Single-driven assignments
  lr <= (others => 0 ns);
  t <= (others => TRUE);
  
  -- Multi-driven assignments
  wqnjdzulj <= 'U';
  wqnjdzulj <= 'W';
  wqnjdzulj <= '1';
  wqnjdzulj <= '-';
end pnicxtkk;



-- Seed after: 5639828103814046273,6882842853887419669

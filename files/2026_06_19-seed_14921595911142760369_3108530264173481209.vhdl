-- Seed: 14921595911142760369,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity saz is
  port (yleyreucm : in integer_vector(2 to 0); ucgau : linkage real_vector(4 to 3); sgqg : buffer std_logic_vector(0 to 0); luxmfxqc : in std_logic);
end saz;

architecture er of saz is
  
begin
  -- Multi-driven assignments
  sgqg <= (others => 'H');
end er;

library ieee;
use ieee.std_logic_1164.all;

entity pgz is
  port (tajayf : linkage time; xavstvrfpb : buffer std_logic; wefpfhyizy : linkage bit_vector(4 to 0); bzdtvyv : linkage integer);
end pgz;

library ieee;
use ieee.std_logic_1164.all;

architecture kng of pgz is
  signal xqwsfzzav : std_logic_vector(0 to 0);
  signal wpjafdtd : real_vector(4 to 3);
  signal toyhlsqvu : integer_vector(2 to 0);
begin
  t : entity work.saz
    port map (yleyreucm => toyhlsqvu, ucgau => wpjafdtd, sgqg => xqwsfzzav, luxmfxqc => xavstvrfpb);
  
  -- Single-driven assignments
  toyhlsqvu <= (others => 0);
  
  -- Multi-driven assignments
  xavstvrfpb <= 'H';
end kng;

entity peyzvnql is
  port (rv : out real);
end peyzvnql;

library ieee;
use ieee.std_logic_1164.all;

architecture egz of peyzvnql is
  signal agabixlvbo : std_logic;
  signal zizf : std_logic_vector(0 to 0);
  signal x : real_vector(4 to 3);
  signal mkqw : integer;
  signal xmoelekb : bit_vector(4 to 0);
  signal bywtgyexpn : time;
  signal q : std_logic_vector(0 to 0);
  signal cbf : real_vector(4 to 3);
  signal biztxfr : integer_vector(2 to 0);
  signal klqtdjj : std_logic;
  signal xhwijcf : std_logic_vector(0 to 0);
  signal qawmqgjac : real_vector(4 to 3);
  signal owcgidbm : integer_vector(2 to 0);
begin
  mp : entity work.saz
    port map (yleyreucm => owcgidbm, ucgau => qawmqgjac, sgqg => xhwijcf, luxmfxqc => klqtdjj);
  qgyfjev : entity work.saz
    port map (yleyreucm => biztxfr, ucgau => cbf, sgqg => q, luxmfxqc => klqtdjj);
  klvfjnqi : entity work.pgz
    port map (tajayf => bywtgyexpn, xavstvrfpb => klqtdjj, wefpfhyizy => xmoelekb, bzdtvyv => mkqw);
  lcuqzhiic : entity work.saz
    port map (yleyreucm => owcgidbm, ucgau => x, sgqg => zizf, luxmfxqc => agabixlvbo);
  
  -- Single-driven assignments
  rv <= 8#3_4_7_4_3.103#;
  biztxfr <= (others => 0);
  owcgidbm <= (others => 0);
  
  -- Multi-driven assignments
  xhwijcf <= (others => 'H');
  zizf <= "X";
  xhwijcf <= (others => 'Z');
end egz;



-- Seed after: 12009630532047249607,3108530264173481209

-- Seed: 5331189767013497042,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity hiwkudyq is
  port (jctk : in time; urlusr : inout integer; rkwynx : out std_logic_vector(3 downto 2));
end hiwkudyq;

architecture atqp of hiwkudyq is
  
begin
  -- Single-driven assignments
  urlusr <= 0_4_1_3_4;
end atqp;

entity pkb is
  port (troz : in real; te : inout integer; qrvhw : buffer boolean_vector(2 downto 2); pqszcvr : out integer);
end pkb;

library ieee;
use ieee.std_logic_1164.all;

architecture ykgxstpft of pkb is
  signal yrxyre : std_logic_vector(3 downto 2);
  signal cixuwwffax : integer;
  signal gf : std_logic_vector(3 downto 2);
  signal cwkht : time;
begin
  mwlok : entity work.hiwkudyq
    port map (jctk => cwkht, urlusr => te, rkwynx => gf);
  zrxeyr : entity work.hiwkudyq
    port map (jctk => cwkht, urlusr => cixuwwffax, rkwynx => yrxyre);
  cjsswsgjk : entity work.hiwkudyq
    port map (jctk => cwkht, urlusr => pqszcvr, rkwynx => gf);
  
  -- Single-driven assignments
  cwkht <= 1310.4_4_2_2_0 ps;
  qrvhw <= (others => FALSE);
  
  -- Multi-driven assignments
  yrxyre <= ('L', 'Z');
  gf <= gf;
  gf <= ('U', 'L');
end ykgxstpft;

library ieee;
use ieee.std_logic_1164.all;

entity ptccajood is
  port (zmcairh : buffer bit_vector(4 to 4); fgf : out std_logic; n : in integer; dxaifcbe : linkage real_vector(4 to 4));
end ptccajood;

library ieee;
use ieee.std_logic_1164.all;

architecture wrilbl of ptccajood is
  signal c : std_logic_vector(3 downto 2);
  signal lrd : integer;
  signal mhr : integer;
  signal bmyakam : boolean_vector(2 downto 2);
  signal u : integer;
  signal bncpku : real;
  signal ib : integer;
  signal lbdyrzusc : boolean_vector(2 downto 2);
  signal q : integer;
  signal gqxirh : real;
  signal alrnxqxa : std_logic_vector(3 downto 2);
  signal yfeik : integer;
  signal kacssnna : time;
begin
  hsofmejz : entity work.hiwkudyq
    port map (jctk => kacssnna, urlusr => yfeik, rkwynx => alrnxqxa);
  lf : entity work.pkb
    port map (troz => gqxirh, te => q, qrvhw => lbdyrzusc, pqszcvr => ib);
  lrsnovq : entity work.pkb
    port map (troz => bncpku, te => u, qrvhw => bmyakam, pqszcvr => mhr);
  drbyanwgqe : entity work.hiwkudyq
    port map (jctk => kacssnna, urlusr => lrd, rkwynx => c);
  
  -- Multi-driven assignments
  alrnxqxa <= ('U', '-');
  c <= "W-";
  fgf <= fgf;
  c <= "ZU";
end wrilbl;



-- Seed after: 9886278380996985149,3400751927341804175

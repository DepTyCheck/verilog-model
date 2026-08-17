-- Seed: 5127803515526497103,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity dkq is
  port (mfnmhraw : linkage integer; btb : buffer std_logic_vector(0 to 4); zquna : in bit_vector(2 downto 3));
end dkq;

architecture cwbra of dkq is
  
begin
  -- Multi-driven assignments
  btb <= btb;
  btb <= ('U', 'U', '1', 'U', 'H');
  btb <= btb;
end cwbra;

library ieee;
use ieee.std_logic_1164.all;

entity am is
  port (fyrqi : inout time; fmrjidqh : inout std_logic_vector(4 downto 0); ysfdxxg : inout real);
end am;

library ieee;
use ieee.std_logic_1164.all;

architecture ohnhngpd of am is
  signal yvf : std_logic_vector(0 to 4);
  signal rcvbpbhbq : integer;
  signal gjl : integer;
  signal oendbdud : std_logic_vector(0 to 4);
  signal zggwd : integer;
  signal rdjdj : bit_vector(2 downto 3);
  signal pbauhyjilq : integer;
begin
  kqhm : entity work.dkq
    port map (mfnmhraw => pbauhyjilq, btb => fmrjidqh, zquna => rdjdj);
  uf : entity work.dkq
    port map (mfnmhraw => zggwd, btb => oendbdud, zquna => rdjdj);
  husggr : entity work.dkq
    port map (mfnmhraw => gjl, btb => fmrjidqh, zquna => rdjdj);
  ef : entity work.dkq
    port map (mfnmhraw => rcvbpbhbq, btb => yvf, zquna => rdjdj);
  
  -- Single-driven assignments
  ysfdxxg <= ysfdxxg;
  
  -- Multi-driven assignments
  fmrjidqh <= fmrjidqh;
end ohnhngpd;



-- Seed after: 183796483539352252,13843488114570579517

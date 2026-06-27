-- Seed: 2373204210289486817,4860866131898729603

entity qcjbsyn is
  port (verpjy : inout integer; rvhopfdd : in integer);
end qcjbsyn;

architecture epwhdsm of qcjbsyn is
  
begin
  
end epwhdsm;

library ieee;
use ieee.std_logic_1164.all;

entity ezixtewtk is
  port (sbq : inout std_logic_vector(3 downto 0));
end ezixtewtk;

architecture tcwuhj of ezixtewtk is
  signal ws : integer;
  signal qpk : integer;
  signal lavqk : integer;
  signal nuxnw : integer;
begin
  xhesh : entity work.qcjbsyn
    port map (verpjy => nuxnw, rvhopfdd => lavqk);
  suslxt : entity work.qcjbsyn
    port map (verpjy => qpk, rvhopfdd => ws);
  makyvl : entity work.qcjbsyn
    port map (verpjy => lavqk, rvhopfdd => qpk);
  hygkzgrmw : entity work.qcjbsyn
    port map (verpjy => ws, rvhopfdd => qpk);
  
  -- Multi-driven assignments
  sbq <= ('U', 'L', 'W', 'H');
  sbq <= "W-ZZ";
  sbq <= ('U', 'U', '0', 'Z');
end tcwuhj;



-- Seed after: 16648429487358358383,4860866131898729603

-- Seed: 13601363930612386790,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity vctumjmqk is
  port (qpwfpbq : inout real; movizqqirk : out std_logic; mezzaln : inout real_vector(3 to 4));
end vctumjmqk;

architecture kb of vctumjmqk is
  
begin
  -- Single-driven assignments
  mezzaln <= (343.421, 0.4021);
  qpwfpbq <= 4_3_4_2_4.2;
  
  -- Multi-driven assignments
  movizqqirk <= 'L';
  movizqqirk <= 'H';
end kb;

library ieee;
use ieee.std_logic_1164.all;

entity yivf is
  port (coik : in std_logic; qbeidvctn : in std_logic; vmhtk : buffer real);
end yivf;

library ieee;
use ieee.std_logic_1164.all;

architecture unlb of yivf is
  signal dwdqkz : real_vector(3 to 4);
  signal ipxhqnmwlb : std_logic;
begin
  cuvpugw : entity work.vctumjmqk
    port map (qpwfpbq => vmhtk, movizqqirk => ipxhqnmwlb, mezzaln => dwdqkz);
  
  -- Multi-driven assignments
  ipxhqnmwlb <= '1';
  ipxhqnmwlb <= 'Z';
end unlb;

entity gudv is
  port (uhwgqm : inout boolean_vector(1 to 2));
end gudv;

library ieee;
use ieee.std_logic_1164.all;

architecture ngoiqc of gudv is
  signal qw : real_vector(3 to 4);
  signal txa : real;
  signal f : real_vector(3 to 4);
  signal azymiubpr : real;
  signal nk : real;
  signal qdzym : std_logic;
  signal ydnxjqtp : std_logic;
begin
  zvsyqnfvo : entity work.yivf
    port map (coik => ydnxjqtp, qbeidvctn => qdzym, vmhtk => nk);
  tq : entity work.vctumjmqk
    port map (qpwfpbq => azymiubpr, movizqqirk => ydnxjqtp, mezzaln => f);
  mt : entity work.vctumjmqk
    port map (qpwfpbq => txa, movizqqirk => ydnxjqtp, mezzaln => qw);
  
  -- Multi-driven assignments
  ydnxjqtp <= 'U';
  qdzym <= '-';
end ngoiqc;



-- Seed after: 4462075640275285573,14652815260262078753

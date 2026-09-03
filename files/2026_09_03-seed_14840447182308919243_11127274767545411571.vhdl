-- Seed: 14840447182308919243,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity bpbuysfgwm is
  port (rzwdrtdp : in std_logic; i : inout real);
end bpbuysfgwm;

architecture jvlc of bpbuysfgwm is
  
begin
  -- Single-driven assignments
  i <= i;
end jvlc;

library ieee;
use ieee.std_logic_1164.all;

entity afx is
  port (ewjlfth : out std_logic);
end afx;

architecture kewvdjeq of afx is
  signal ptvgjokap : real;
begin
  stwcptzs : entity work.bpbuysfgwm
    port map (rzwdrtdp => ewjlfth, i => ptvgjokap);
end kewvdjeq;

library ieee;
use ieee.std_logic_1164.all;

entity spkypmcr is
  port (mnovbjwh : linkage boolean_vector(2 to 2); wxuvdi : linkage std_logic; lkdz : in real);
end spkypmcr;

library ieee;
use ieee.std_logic_1164.all;

architecture bnrtueg of spkypmcr is
  signal ufdzy : real;
  signal qgce : std_logic;
  signal qu : real;
  signal idpazrwhuh : std_logic;
begin
  x : entity work.afx
    port map (ewjlfth => idpazrwhuh);
  gkcxomjnx : entity work.bpbuysfgwm
    port map (rzwdrtdp => idpazrwhuh, i => qu);
  liu : entity work.bpbuysfgwm
    port map (rzwdrtdp => qgce, i => ufdzy);
  
  -- Multi-driven assignments
  idpazrwhuh <= qgce;
end bnrtueg;



-- Seed after: 8644581430014123141,11127274767545411571

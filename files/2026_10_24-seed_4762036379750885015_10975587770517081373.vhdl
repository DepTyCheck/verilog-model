-- Seed: 4762036379750885015,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity dvsofqx is
  port (nksvdefza : out std_logic; iwiez : in std_logic);
end dvsofqx;

architecture ezxaein of dvsofqx is
  
begin
  -- Multi-driven assignments
  nksvdefza <= 'U';
  nksvdefza <= 'H';
end ezxaein;

entity sxkcmf is
  port (dafejwjydn : linkage real);
end sxkcmf;

architecture qdkapgksm of sxkcmf is
  
begin
  
end qdkapgksm;

entity h is
  port (hyj : out real_vector(0 downto 3); vdo : buffer integer_vector(0 downto 2));
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture v of h is
  signal dvuslshxf : real;
  signal yrpmmravsp : std_logic;
  signal thmkrzjmad : std_logic;
  signal ujnk : std_logic;
  signal pto : std_logic;
begin
  vfkmcgc : entity work.dvsofqx
    port map (nksvdefza => pto, iwiez => ujnk);
  fhfcnu : entity work.dvsofqx
    port map (nksvdefza => pto, iwiez => thmkrzjmad);
  rq : entity work.dvsofqx
    port map (nksvdefza => yrpmmravsp, iwiez => pto);
  jax : entity work.sxkcmf
    port map (dafejwjydn => dvuslshxf);
  
  -- Single-driven assignments
  hyj <= (others => 0.0);
  vdo <= vdo;
  
  -- Multi-driven assignments
  pto <= 'H';
  pto <= pto;
  pto <= 'H';
  thmkrzjmad <= pto;
end v;



-- Seed after: 11020010766993758225,10975587770517081373

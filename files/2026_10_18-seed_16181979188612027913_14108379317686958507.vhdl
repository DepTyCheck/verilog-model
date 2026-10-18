-- Seed: 16181979188612027913,14108379317686958507

entity rjlvjql is
  port (empgdztrz : buffer real; iznrhqba : out integer_vector(3 to 3); rdbp : in real);
end rjlvjql;

architecture gc of rjlvjql is
  
begin
  -- Single-driven assignments
  iznrhqba <= (others => 2#01#);
  empgdztrz <= 16#0_F_A_0_C.0#;
end gc;

library ieee;
use ieee.std_logic_1164.all;

entity peuwkgvcmo is
  port (ltldrzi : in std_logic_vector(3 to 1); zyywcj : buffer time; k : inout bit; mlju : in time);
end peuwkgvcmo;

architecture zzgadk of peuwkgvcmo is
  signal byzrgv : real;
  signal q : integer_vector(3 to 3);
  signal o : real;
begin
  ktfapplut : entity work.rjlvjql
    port map (empgdztrz => o, iznrhqba => q, rdbp => byzrgv);
  
  -- Single-driven assignments
  k <= '0';
  zyywcj <= 16#C_2_A_F.F_A# ps;
  byzrgv <= 0101.3_0_2_0;
end zzgadk;



-- Seed after: 18289919612716590238,14108379317686958507

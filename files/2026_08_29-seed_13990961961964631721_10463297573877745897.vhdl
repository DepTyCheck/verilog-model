-- Seed: 13990961961964631721,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity anbgojwa is
  port (nq : linkage std_logic; uazcmxlxh : in boolean; jcollvexqp : inout boolean_vector(2 to 0); du : in real);
end anbgojwa;

architecture tetqkdtjy of anbgojwa is
  
begin
  
end tetqkdtjy;

entity dg is
  port (pqrtkma : in real; utxzubictd : linkage time_vector(0 to 3));
end dg;

library ieee;
use ieee.std_logic_1164.all;

architecture rk of dg is
  signal dvgjbq : boolean_vector(2 to 0);
  signal zefhtqky : boolean;
  signal hyhhvvjsg : real;
  signal vn : boolean_vector(2 to 0);
  signal tite : boolean;
  signal uyzj : std_logic;
  signal nfzrclhwx : real;
  signal bplrpilgy : boolean_vector(2 to 0);
  signal mg : boolean;
  signal fxsaghemye : boolean_vector(2 to 0);
  signal tigcn : boolean;
  signal ntyq : std_logic;
begin
  gbgkfpqi : entity work.anbgojwa
    port map (nq => ntyq, uazcmxlxh => tigcn, jcollvexqp => fxsaghemye, du => pqrtkma);
  wctcoxfu : entity work.anbgojwa
    port map (nq => ntyq, uazcmxlxh => mg, jcollvexqp => bplrpilgy, du => nfzrclhwx);
  ga : entity work.anbgojwa
    port map (nq => uyzj, uazcmxlxh => tite, jcollvexqp => vn, du => hyhhvvjsg);
  mgwxfhqlgj : entity work.anbgojwa
    port map (nq => uyzj, uazcmxlxh => zefhtqky, jcollvexqp => dvgjbq, du => nfzrclhwx);
  
  -- Single-driven assignments
  zefhtqky <= TRUE;
  tite <= tigcn;
  mg <= FALSE;
  
  -- Multi-driven assignments
  ntyq <= 'L';
end rk;



-- Seed after: 13980343083243683846,10463297573877745897

-- Seed: 17253228073681016245,12260394286515585877

entity o is
  port (riccqtaesb : out character; zvqmy : in boolean_vector(4 downto 2); wlngo : in boolean);
end o;

architecture vrxchnwl of o is
  
begin
  -- Single-driven assignments
  riccqtaesb <= 's';
end vrxchnwl;

entity csybwv is
  port (xshv : out real);
end csybwv;

architecture iyijfkyxmp of csybwv is
  signal tqigm : character;
  signal nu : boolean;
  signal cd : boolean_vector(4 downto 2);
  signal cdiprmgb : character;
begin
  abqkrst : entity work.o
    port map (riccqtaesb => cdiprmgb, zvqmy => cd, wlngo => nu);
  buthnhvp : entity work.o
    port map (riccqtaesb => tqigm, zvqmy => cd, wlngo => nu);
  
  -- Single-driven assignments
  nu <= FALSE;
  cd <= cd;
  xshv <= xshv;
end iyijfkyxmp;

entity k is
  port (yzmfea : in real_vector(0 to 2); lovvcpgj : out time);
end k;

architecture v of k is
  signal xoudcnbsbm : real;
begin
  jyy : entity work.csybwv
    port map (xshv => xoudcnbsbm);
  
  -- Single-driven assignments
  lovvcpgj <= 8#1_0_7# fs;
end v;

library ieee;
use ieee.std_logic_1164.all;

entity cqjijwf is
  port (nk : inout std_logic; v : in std_logic_vector(1 downto 2); gquv : linkage integer);
end cqjijwf;

architecture nbhew of cqjijwf is
  signal vltdxxscw : time;
  signal ggzg : real_vector(0 to 2);
  signal rixzif : boolean;
  signal zphnc : boolean_vector(4 downto 2);
  signal opjv : character;
  signal iweyprzrx : real;
  signal tp : real;
begin
  diqruhjsdw : entity work.csybwv
    port map (xshv => tp);
  ssnmgfceqv : entity work.csybwv
    port map (xshv => iweyprzrx);
  zpvjpgxu : entity work.o
    port map (riccqtaesb => opjv, zvqmy => zphnc, wlngo => rixzif);
  ujvsya : entity work.k
    port map (yzmfea => ggzg, lovvcpgj => vltdxxscw);
  
  -- Single-driven assignments
  rixzif <= FALSE;
  zphnc <= (FALSE, TRUE, FALSE);
end nbhew;



-- Seed after: 11657180981720909051,12260394286515585877

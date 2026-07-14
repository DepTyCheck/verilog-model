-- Seed: 13550128442814770792,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity o is
  port ( yxhz : buffer std_logic_vector(2 to 2)
  ; rejuswm : inout record_value_mirror
  ; pahufjbkjw : inout std_logic_vector(2 to 3)
  ; gts : inout enumeration_value_mirror
  );
end o;

architecture f of o is
  
begin
  -- Multi-driven assignments
  yxhz <= (others => 'L');
  pahufjbkjw <= ('1', 'W');
  yxhz <= yxhz;
  pahufjbkjw <= "LZ";
end f;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ntj is
  port (pmawncsudo : inout protected_subtype_mirror; oxgxcyc : linkage std_logic; yphxbkfywk : out bit_vector(0 downto 1));
end ntj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture kcedvbgv of ntj is
  shared variable zydtl : enumeration_value_mirror;
  signal hpch : std_logic_vector(2 to 3);
  shared variable rky : record_value_mirror;
  shared variable cdbhixjxr : enumeration_value_mirror;
  shared variable ibntz : record_value_mirror;
  shared variable titenadd : enumeration_value_mirror;
  signal syz : std_logic_vector(2 to 3);
  shared variable vhlm : record_value_mirror;
  signal mhsnxrqphi : std_logic_vector(2 to 2);
begin
  qhelfpcf : entity work.o
    port map (yxhz => mhsnxrqphi, rejuswm => vhlm, pahufjbkjw => syz, gts => titenadd);
  xclyk : entity work.o
    port map (yxhz => mhsnxrqphi, rejuswm => ibntz, pahufjbkjw => syz, gts => cdbhixjxr);
  pyilrrhll : entity work.o
    port map (yxhz => mhsnxrqphi, rejuswm => rky, pahufjbkjw => hpch, gts => zydtl);
  
  -- Single-driven assignments
  yphxbkfywk <= yphxbkfywk;
  
  -- Multi-driven assignments
  mhsnxrqphi <= "U";
  mhsnxrqphi <= "L";
  syz <= syz;
  hpch <= "--";
end kcedvbgv;

use std.reflection.all;

entity tqpllai is
  port (dgfk : linkage real; wnerejrks : inout enumeration_value_mirror; bfksk : inout enumeration_value_mirror; yts : inout floating_value_mirror);
end tqpllai;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mdl of tqpllai is
  shared variable pylpkpuzs : enumeration_value_mirror;
  signal muc : std_logic_vector(2 to 3);
  shared variable nldyqhgwa : record_value_mirror;
  signal d : std_logic_vector(2 to 2);
  signal vltjksin : bit_vector(0 downto 1);
  shared variable yrqhvppfj : protected_subtype_mirror;
  signal uienwrirw : bit_vector(0 downto 1);
  signal lbfsmc : std_logic;
  shared variable ceqhvvnqn : protected_subtype_mirror;
begin
  zyrv : entity work.ntj
    port map (pmawncsudo => ceqhvvnqn, oxgxcyc => lbfsmc, yphxbkfywk => uienwrirw);
  elilwmfg : entity work.ntj
    port map (pmawncsudo => yrqhvppfj, oxgxcyc => lbfsmc, yphxbkfywk => vltjksin);
  tcfm : entity work.o
    port map (yxhz => d, rejuswm => nldyqhgwa, pahufjbkjw => muc, gts => pylpkpuzs);
  
  -- Multi-driven assignments
  lbfsmc <= '0';
  lbfsmc <= lbfsmc;
  lbfsmc <= lbfsmc;
  d <= d;
end mdl;



-- Seed after: 3953926107710020712,7726014785203345639

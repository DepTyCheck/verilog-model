-- Seed: 6586783850639752598,6882842853887419669

entity bpaosdbb is
  port (lowmwcipqg : out integer_vector(1 to 3); zxzddb : in real; trmkgz : linkage bit_vector(1 to 3));
end bpaosdbb;

architecture xypaa of bpaosdbb is
  
begin
  -- Single-driven assignments
  lowmwcipqg <= (03243, 3, 2#11111#);
end xypaa;

library ieee;
use ieee.std_logic_1164.all;

entity oh is
  port ( gttbawpip : inout integer
  ; yndjl : inout time_vector(1 to 1)
  ; lgpaszyos : linkage std_logic_vector(2 downto 0)
  ; qhfpvyc : inout std_logic_vector(2 to 2)
  );
end oh;

architecture gyc of oh is
  signal cgn : bit_vector(1 to 3);
  signal jawmydym : real;
  signal ylxsjcyvt : integer_vector(1 to 3);
  signal wtlqle : bit_vector(1 to 3);
  signal ptwraegs : real;
  signal oggl : integer_vector(1 to 3);
begin
  nzzham : entity work.bpaosdbb
    port map (lowmwcipqg => oggl, zxzddb => ptwraegs, trmkgz => wtlqle);
  pcoglonw : entity work.bpaosdbb
    port map (lowmwcipqg => ylxsjcyvt, zxzddb => jawmydym, trmkgz => cgn);
  
  -- Single-driven assignments
  ptwraegs <= 41.0401;
  gttbawpip <= 8#40#;
  yndjl <= (others => 0_4_0_1_1 ps);
  jawmydym <= 2_4_1_0.0_4;
  
  -- Multi-driven assignments
  qhfpvyc <= (others => '0');
  qhfpvyc <= (others => 'L');
  qhfpvyc <= "X";
end gyc;

entity ngpludtvpi is
  port (jxmvkuwb : in time);
end ngpludtvpi;

library ieee;
use ieee.std_logic_1164.all;

architecture zvg of ngpludtvpi is
  signal rhdnvxlo : bit_vector(1 to 3);
  signal xzzblgdadg : real;
  signal rjvlatsmq : integer_vector(1 to 3);
  signal tkptwf : time_vector(1 to 1);
  signal qsqc : integer;
  signal hjdjlendz : std_logic_vector(2 to 2);
  signal fpoyaja : std_logic_vector(2 downto 0);
  signal eyjrarq : time_vector(1 to 1);
  signal olxq : integer;
begin
  iqxuuk : entity work.oh
    port map (gttbawpip => olxq, yndjl => eyjrarq, lgpaszyos => fpoyaja, qhfpvyc => hjdjlendz);
  gbpj : entity work.oh
    port map (gttbawpip => qsqc, yndjl => tkptwf, lgpaszyos => fpoyaja, qhfpvyc => hjdjlendz);
  mts : entity work.bpaosdbb
    port map (lowmwcipqg => rjvlatsmq, zxzddb => xzzblgdadg, trmkgz => rhdnvxlo);
  
  -- Multi-driven assignments
  fpoyaja <= "WH1";
end zvg;



-- Seed after: 18303708110585590735,6882842853887419669

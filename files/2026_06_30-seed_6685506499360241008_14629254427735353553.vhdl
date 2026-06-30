-- Seed: 6685506499360241008,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity dq is
  port (ut : in std_logic_vector(1 downto 2); erub : in real_vector(1 downto 0));
end dq;

architecture zluxf of dq is
  
begin
  
end zluxf;

entity mee is
  port (eeukh : linkage real; xet : out time);
end mee;

library ieee;
use ieee.std_logic_1164.all;

architecture xepuxh of mee is
  signal blbj : real_vector(1 downto 0);
  signal xret : std_logic_vector(1 downto 2);
  signal oxrlgos : real_vector(1 downto 0);
  signal jmkgzez : std_logic_vector(1 downto 2);
  signal dojmoib : real_vector(1 downto 0);
  signal whsf : real_vector(1 downto 0);
  signal cklaize : std_logic_vector(1 downto 2);
begin
  da : entity work.dq
    port map (ut => cklaize, erub => whsf);
  lqq : entity work.dq
    port map (ut => cklaize, erub => dojmoib);
  bxhdbfwtjt : entity work.dq
    port map (ut => jmkgzez, erub => oxrlgos);
  kz : entity work.dq
    port map (ut => xret, erub => blbj);
  
  -- Single-driven assignments
  dojmoib <= (8#6.6_2#, 2#0.0111#);
  whsf <= (2#1_0.0_1_1_1#, 1_0_1_3.03410);
end xepuxh;

library ieee;
use ieee.std_logic_1164.all;

entity rbhyseqe is
  port (jwthnjjtv : out time; asgqabuiv : linkage real; n : in std_logic; vlhmcu : out time);
end rbhyseqe;

library ieee;
use ieee.std_logic_1164.all;

architecture ablrjv of rbhyseqe is
  signal ofn : real_vector(1 downto 0);
  signal yyteeixtg : std_logic_vector(1 downto 2);
begin
  tp : entity work.mee
    port map (eeukh => asgqabuiv, xet => jwthnjjtv);
  umq : entity work.dq
    port map (ut => yyteeixtg, erub => ofn);
  
  -- Single-driven assignments
  vlhmcu <= 8#1.1_1# ms;
  
  -- Multi-driven assignments
  yyteeixtg <= (others => '0');
  yyteeixtg <= "";
  yyteeixtg <= "";
  yyteeixtg <= "";
end ablrjv;



-- Seed after: 7077107587226948523,14629254427735353553

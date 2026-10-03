-- Seed: 6882639494399417018,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (hnjec : in std_logic_vector(1 downto 0); mveatwvi : in time; mgjct : buffer real; q : buffer bit);
end d;

architecture hrljb of d is
  
begin
  -- Single-driven assignments
  q <= q;
  mgjct <= 16#4_E_A_2_7.A_2_8#;
end hrljb;

library ieee;
use ieee.std_logic_1164.all;

entity qsxp is
  port (gouatxyain : buffer severity_level; autqbafsm : in std_logic_vector(4 downto 0); pwdvuhovgh : inout bit_vector(3 to 3));
end qsxp;

library ieee;
use ieee.std_logic_1164.all;

architecture wugq of qsxp is
  signal rgckyf : bit;
  signal xwqqopmra : real;
  signal xk : time;
  signal kxdobwjzt : std_logic_vector(1 downto 0);
begin
  sn : entity work.d
    port map (hnjec => kxdobwjzt, mveatwvi => xk, mgjct => xwqqopmra, q => rgckyf);
  
  -- Single-driven assignments
  pwdvuhovgh <= pwdvuhovgh;
  xk <= 2#11.0_0_1_1_1# ns;
  gouatxyain <= NOTE;
end wugq;

library ieee;
use ieee.std_logic_1164.all;

entity jcjn is
  port (hgtou : inout std_logic_vector(0 to 3); bpflzp : out time; jp : in real);
end jcjn;

library ieee;
use ieee.std_logic_1164.all;

architecture pvo of jcjn is
  signal nds : bit;
  signal ysgimefv : real;
  signal v : bit;
  signal tvikcs : real;
  signal qwsjf : time;
  signal asyacdgsu : std_logic_vector(1 downto 0);
  signal sbvs : bit;
  signal taqle : real;
  signal ct : std_logic_vector(1 downto 0);
  signal cjqskj : bit_vector(3 to 3);
  signal rwg : std_logic_vector(4 downto 0);
  signal ckswi : severity_level;
begin
  dryiffvy : entity work.qsxp
    port map (gouatxyain => ckswi, autqbafsm => rwg, pwdvuhovgh => cjqskj);
  gma : entity work.d
    port map (hnjec => ct, mveatwvi => bpflzp, mgjct => taqle, q => sbvs);
  rspznull : entity work.d
    port map (hnjec => asyacdgsu, mveatwvi => qwsjf, mgjct => tvikcs, q => v);
  spqxbrlyd : entity work.d
    port map (hnjec => ct, mveatwvi => bpflzp, mgjct => ysgimefv, q => nds);
  
  -- Single-driven assignments
  bpflzp <= 8#3.637# ns;
  qwsjf <= 16#C7E# fs;
end pvo;



-- Seed after: 16529581459416320239,6140041381800297705

-- Seed: 15746417964824321457,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity kgfxtjj is
  port (mmaw : inout real; bxrg : in boolean; njbpwxoamg : in std_logic; dtoixi : out std_logic);
end kgfxtjj;

architecture f of kgfxtjj is
  
begin
  -- Single-driven assignments
  mmaw <= 16#20C7.9#;
end f;

library ieee;
use ieee.std_logic_1164.all;

entity esusx is
  port (fayteeqw : buffer std_logic; kdhb : buffer real; j : linkage integer; hihffop : linkage std_logic);
end esusx;

library ieee;
use ieee.std_logic_1164.all;

architecture xaqrnqa of esusx is
  signal xedlnvssw : std_logic;
  signal bstf : real;
  signal cge : boolean;
begin
  zpegucabq : entity work.kgfxtjj
    port map (mmaw => kdhb, bxrg => cge, njbpwxoamg => fayteeqw, dtoixi => fayteeqw);
  lbi : entity work.kgfxtjj
    port map (mmaw => bstf, bxrg => cge, njbpwxoamg => fayteeqw, dtoixi => xedlnvssw);
  
  -- Single-driven assignments
  cge <= FALSE;
  
  -- Multi-driven assignments
  xedlnvssw <= fayteeqw;
  fayteeqw <= 'X';
  fayteeqw <= fayteeqw;
end xaqrnqa;



-- Seed after: 18392646264934371015,5983430343285687595
